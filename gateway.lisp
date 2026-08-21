;;;; gateway.lisp — hosting operandi-gui on a WebRTC (or any) gateway.
;;;;
;;;; The chat CORE (operandi-gui) is transport-agnostic: it offers a projection and a *SPEAK-FN* hook
;;;; and knows nothing about how a reply is voiced or how a phone is reached.  THIS is the fat half —
;;;; the glue that drives voice OUT (chord, sentence by sentence, karaoke ticks), voice IN (stave
;;;; dictation), and the phone panel's control frames.
;;;;
;;;; It has NO transport of its own — no socket, no filesystem path, nothing OS-shaped — because it is
;;;; meant to outlive unix (a from-scratch Lisp machine has no unix sockets to open).  It names WHAT
;;;; it needs and the host provides HOW, through two callbacks:
;;;;
;;;;   *VOICE*     (op &rest args)   — the desktop's voice and ear.  Ops below.
;;;;   *PEER-SEND* (json-string)     — one frame to the phone's chat channel.
;;;;
;;;; The host (a gateway's warp-channel.lisp today) implements those over whatever it has — a UNIX
;;;; seat socket and an SCTP stream on this box; a direct in-image call on a Lisp machine — and this
;;;; file does not change when it does.  The host calls four things in: ENSURE-READY (handing over the
;;;; voice link), APP-SPEC, NOTE-PEER (handing over the phone link), and HANDLE-CONTROL.

(defpackage #:operandi-gui.gateway
  (:use #:cl)
  (:local-nicknames (#:gui #:operandi-gui) (#:llm #:operandi.llm)
                    (#:jzon #:com.inuoe.jzon) (#:bt #:bordeaux-threads))
  (:export #:ensure-ready #:app-spec #:note-peer #:handle-control))
(in-package #:operandi-gui.gateway)

;;; ---- the two host links: no transport here, so nothing here is unix (or anything) -----------
(defvar *voice* nil
  "(lambda (op &rest args)) — the host's link to the desktop's voice + ear.  We name the ops; the host
owns how they travel.  Ops:
    (:speak TEXT)        say TEXT
    (:hush)              stop saying anything, now
    (:speaking-p)   -> generalized boolean: is it still saying something?
    (:hearing-clear)     forget the transcript so far
    (:hearing-start)     begin transcribing the peer's mic
    (:hearing-stop)      stop transcribing
    (:hearing-text) -> a display-ready transcript string (recased, no shouting).")
(defvar *peer-send* nil
  "(lambda (json-string)) — pushes one raw frame to the phone's chat channel.  The host owns the wire.")

(defun voice (op &rest args) (let ((f *voice*)) (when f (apply f op args))))
(defun notify (json) (let ((s *peer-send*)) (when s (ignore-errors (funcall s json)))))

(defun note-peer (send-fn) "The host calls this when the chat channel opens." (setf *peer-send* send-fn))

;;; ---- voice OUT: chord, sentence by sentence, with karaoke ticks ---------------------------
(defvar *speak-gen* 0 "bumped on every new speak/hush; a running speak thread bails when it changes.")
(defvar *lock* (bt:make-lock "operandi-gui-gateway"))

(defun speak-reply (text &optional msg-id)
  "The *SPEAK-FN* the host installs: voice TEXT sentence by sentence, lighting each on the phone as it
plays.  Runs on its own thread — the voice runs on its own clock, so we poll :SPEAKING-P — and bails
the moment a newer reply or a hush bumps *SPEAK-GEN*."
  (let ((gen (bt:with-lock-held (*lock*) (incf *speak-gen*))) (mid (or msg-id -1)))
    (voice :hush)                                          ; clear whatever was still being said
    (bt:make-thread
     (lambda ()
       (ignore-errors
        (let ((sents (gui:split-sentences text)))
          (loop for s in sents for k from 0 while (= gen *speak-gen*) do
            (notify (format nil "{\"a\":\"chat\",\"m\":~a,\"hl\":~a}" mid k))
            (voice :speak (gui:speech-clean s))
            ;; wait for this one sentence to START then FINISH, watching for supersede
            (loop repeat 60 while (and (= gen *speak-gen*) (not (voice :speaking-p))) do (sleep 0.05))
            (loop while (and (= gen *speak-gen*) (voice :speaking-p)) do (sleep 0.15)))
          (when (= gen *speak-gen*)                        ; finished cleanly: clear the highlight
            (notify (format nil "{\"a\":\"chat\",\"m\":~a,\"hl\":-1}" mid))))))
     :name "operandi-gui-speak")))

(defun hush ()
  "Stop the voice now, supersede any running speak loop, and clear the phone's highlight."
  (bt:with-lock-held (*lock*) (incf *speak-gen*))
  (voice :hush)
  (notify "{\"a\":\"chat\",\"hl\":-1}"))

;;; ---- voice IN: the desktop's ear -> the phone's <input> -----------------------------------
;;; DICTATION ONLY — the operator reviews the box and taps Send; nothing is auto-said.
(defvar *listen-gen* 0 "bumped on each listen start/stop; the poll thread bails when it changes.")

(defun notify-dictate (text) (notify (jzon:stringify (llm:ht "a" "chat" "dictate" text))))

(defun listen-start ()
  "Clear + start the ear and poll its (display-ready) transcript into the phone's box until a
stop/new-listen bumps *LISTEN-GEN*."
  (let ((gen (bt:with-lock-held (*lock*) (incf *listen-gen*))))
    (voice :hearing-clear)
    (voice :hearing-start)
    (bt:make-thread
     (lambda ()
       (ignore-errors
        (let ((last ""))
          (loop while (= gen *listen-gen*) do
            (let ((txt (voice :hearing-text)))
              (when txt
                (let ((s (string-trim '(#\Space #\Newline #\Return #\Tab) txt)))
                  (when (string/= s last) (setf last s) (notify-dictate s)))))
            (sleep 0.2)))))
     :name "operandi-gui-listen")))

(defun listen-stop ()
  (bt:with-lock-held (*lock*) (incf *listen-gen*))
  (voice :hearing-stop))

;;; ---- the host's four entry points ---------------------------------------------------------
(defun ensure-ready (voice-fn)
  "VOICE-FN is the host's link to the desktop voice + ear (see *VOICE*).  Start the agent, seat a
greeting (an empty projection sends no frames, which the phone reads as 'not served'), and route
voiced replies to it.  Idempotent."
  (setf *voice* voice-fn)
  (gui:start-agent)
  (gui:new-session!)
  (setf gui:*speak-fn* #'speak-reply)
  t)

(defun app-spec ()
  "The plist the host's warp-app registry wants for the chat: a flat list of messages, no custom
consumer (same shape as the device manager)."
  (list :projection (gui:chat-projection) :view 'gui:chat-view))

(defun handle-control (text)
  "Intercept the phone's out-of-band chat frames before the host's mux (which speaks warp frames
only): {a:chat,say:…} words for the agent, {a:chat,speak:bool} the voice toggle, {a:chat,hush:true}
the stop button, {a:chat,listen:bool} the mic, {a:chat,clear:true} Send resetting the ear.  Returns
T if it handled the frame.  Cheap-gated on a substring so ordinary warp frames never parse."
  (when (and (search "\"chat\"" text)
             (or (search "\"say\"" text) (search "\"speak\"" text) (search "\"hush\"" text)
                 (search "\"listen\"" text) (search "\"clear\"" text)))
    (handler-case
        (let ((o (jzon:parse text)))
          (when (and (hash-table-p o) (equal (gethash "a" o) "chat"))
            (cond
              ((stringp (gethash "say" o)) (gui:say (gethash "say" o)) t)
              ((nth-value 1 (gethash "listen" o))
               (if (gethash "listen" o) (listen-start) (listen-stop)) t)
              ((nth-value 1 (gethash "speak" o))
               (let ((on (and (gethash "speak" o) t)))
                 (setf gui:*speak-enabled* on)
                 (unless on (hush)))                        ; turning it off silences the current line
               t)
              ((nth-value 1 (gethash "hush" o)) (hush) t)
              ((nth-value 1 (gethash "clear" o)) (voice :hearing-clear) t))))
      (error () nil))))

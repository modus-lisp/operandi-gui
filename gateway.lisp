;;;; gateway.lisp — hosting operandi-gui on the glass WebRTC gateway.
;;;;
;;;; The chat CORE (operandi-gui) is transport-agnostic: it offers a projection and a *SPEAK-FN*
;;;; hook and knows nothing about how a reply is voiced or how a phone is reached.  THIS is the fat
;;;; half — the glue that makes the core reachable from a phone through the gateway.  It routes voice
;;;; OUT to the desktop's chord and voice IN from the desktop's stave ear (both over the seat control
;;;; socket the desktop already listens on), pushes dictation + karaoke frames back to the phone, and
;;;; handles the out-of-band control frames the phone's panel sends.
;;;;
;;;; The host (the gateway's warp-channel.lisp) stays thin: it lazy-loads this, calls ENSURE-READY,
;;;; hands APP-SPEC to its warp-app registry, gives NOTE-PEER a send callback when the chat channel
;;;; opens, and offers each inbound frame to HANDLE-CONTROL before its mux.  There is NO glass and NO
;;;; webrtc-data system dependency: the desktop is reached by a UNIX socket at a known path, and the
;;;; phone by a callback the host owns — which is what lets the disposable gateway load this at all.

(defpackage #:operandi-gui.gateway
  (:use #:cl)
  (:local-nicknames (#:gui #:operandi-gui) (#:llm #:operandi.llm)
                    (#:jzon #:com.inuoe.jzon) (#:bt #:bordeaux-threads))
  (:export #:ensure-ready #:app-spec #:note-peer #:handle-control))
(in-package #:operandi-gui.gateway)

;;; ---- the phone: a send callback the host registers ----------------------------------------
(defvar *peer-send* nil
  "(lambda (json-string)) — pushes one raw frame to the phone's chat channel.  The host owns the
transport (an SCTP stream on the gateway); we only need a function to call.")
(defun note-peer (send-fn) "The host calls this when the chat channel opens." (setf *peer-send* send-fn))
(defun notify (json) (let ((s *peer-send*)) (when s (ignore-errors (funcall s json)))))

;;; ---- the desktop: chord + stave, over the seat control socket -----------------------------
(defun ctl (form)
  "Send one Lisp FORM (a string) to the desktop's seat control socket and drain the ack.  The
gateway taps the desktop's audio mix but does not OWN it, so anything to do with the voice — chord
speaking, the stave ear — is asked of the desktop (:3) over the same control socket the operator
uses.  Best-effort and quiet: a box with no voice simply stays silent."
  (ignore-errors
   (let ((sock (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
     (unwind-protect
          (progn
            (sb-bsd-sockets:socket-connect
             sock (namestring (merge-pathnames ".glass/run/seat-0.control" (user-homedir-pathname))))
            (let ((s (sb-bsd-sockets:socket-make-stream sock :input t :output t
                                                             :element-type 'character)))
              (write-string form s) (terpri s) (finish-output s)
              (read-line s nil nil)))               ; drain the ack so the desktop's write completes
       (ignore-errors (sb-bsd-sockets:socket-close sock))))))

;;; ---- voice OUT: chord, sentence by sentence, with karaoke ticks ---------------------------
(defvar *speak-gen* 0 "bumped on every new speak/hush; a running speak thread bails when it changes.")
(defvar *lock* (bt:make-lock "operandi-gui-gateway"))

(defun speaking-p ()
  (let ((r (ctl "(glass::speaking-p)")))
    (and r (string/= "NIL" (string-trim '(#\Space #\Newline #\Return #\Tab) r)))))

(defun speak-reply (text &optional msg-id)
  "The *SPEAK-FN* the host installs: voice TEXT sentence by sentence via chord, lighting each
sentence on the phone as it plays.  Runs on its own thread — the mixer clock is the desktop's, so we
poll SPEAKING-P — and bails the moment a newer reply or a hush bumps *SPEAK-GEN*."
  (let ((gen (bt:with-lock-held (*lock*) (incf *speak-gen*))) (mid (or msg-id -1)))
    (ctl "(glass::hush)")                                  ; clear whatever was still being said
    (bt:make-thread
     (lambda ()
       (ignore-errors
        (let ((sents (gui:split-sentences text)))
          (loop for s in sents for k from 0 while (= gen *speak-gen*) do
            (notify (format nil "{\"a\":\"chat\",\"m\":~a,\"hl\":~a}" mid k))
            (ctl (format nil "(glass::speak ~s)" (gui:speech-clean s)))
            ;; wait for this one sentence to START (synthesis lag) then FINISH, watching for supersede
            (loop repeat 60 while (and (= gen *speak-gen*) (not (speaking-p))) do (sleep 0.05))
            (loop while (and (= gen *speak-gen*) (speaking-p)) do (sleep 0.15)))
          (when (= gen *speak-gen*)                        ; finished cleanly: clear the highlight
            (notify (format nil "{\"a\":\"chat\",\"m\":~a,\"hl\":-1}" mid))))))
     :name "operandi-gui-speak")))

(defun hush ()
  "Stop the desktop's voice now (glass:HUSH drops the queue AND the sentence being synthesised),
supersede any running speak loop, and clear the phone's highlight."
  (bt:with-lock-held (*lock*) (incf *speak-gen*))
  (ctl "(glass::hush)")
  (notify "{\"a\":\"chat\",\"hl\":-1}"))

;;; ---- voice IN: the desktop's stave ear -> the phone's <input> ------------------------------
;;; Symmetric to voice out: the ear lives on the desktop (it owns the mic stream), so we start it
;;; and POLL its transcript, pushing the live text to the phone's box.  DICTATION ONLY — the operator
;;; reviews the box and taps Send; nothing is auto-said.
(defvar *listen-gen* 0 "bumped on each listen start/stop; the poll thread bails when it changes.")

(defun notify-dictate (text)
  (notify (jzon:stringify (llm:ht "a" "chat" "dictate" text))))

(defun listen-start ()
  "Clear + start the desktop's ear (glass:START-LISTENING, source = the peer mic) and poll
HEARING-TEXT — STAVE:SENTENCE-CASE'd (readable, no shouting; no trailing space, a text box wants
none) — into the phone's box until a stop/new-listen bumps *LISTEN-GEN*."
  (let ((gen (bt:with-lock-held (*lock*) (incf *listen-gen*))))
    (ctl "(glass::hearing-clear)")
    (ctl "(glass::start-listening)")
    (bt:make-thread
     (lambda ()
       (ignore-errors
        (let ((last ""))
          (loop while (= gen *listen-gen*) do
            ;; newlines -> spaces so the whole transcript is one control-socket line
            (let ((txt (ctl "(substitute #\\Space #\\Newline (stave:sentence-case (glass::hearing-text)))")))
              (when txt
                (let ((s (string-trim '(#\Space #\Newline #\Return #\Tab) txt)))
                  (when (string/= s last) (setf last s) (notify-dictate s)))))
            (sleep 0.2)))))
     :name "operandi-gui-listen")))

(defun listen-stop ()
  (bt:with-lock-held (*lock*) (incf *listen-gen*))
  (ctl "(glass::stop-listening)"))

;;; ---- the host's four entry points ---------------------------------------------------------
(defun ensure-ready ()
  "Start the agent, seat a greeting (an empty projection sends no frames, which the phone reads as
'not served'), and route voiced replies to chord.  Idempotent."
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
              ((nth-value 1 (gethash "clear" o)) (ctl "(glass::hearing-clear)") t))))
      (error () nil))))

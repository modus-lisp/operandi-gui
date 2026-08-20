;;;; core.lisp — the operandi chat as a loadable warp projection (NO server).
;;;;
;;;; warp renders the CONVERSATION (a keyed, delta-updated, budgeted list — its
;;;; strength). Composing text is done in whatever surface hosts this projection:
;;;; the standalone HTTP server (serve.lisp) offers a browser <input>; the WebRTC
;;;; gateway offers one in payload.js. Either way, free text arrives at SAY and the
;;;; engine reply flows back into the projection on the next tick.
;;;;
;;;; This system carries NO transport and NO framebuffer — it depends on :warp
;;;; (bordeaux-threads and nothing pixel) + :operandi. That is what lets the
;;;; disposable gateway load it in-process without dragging in a display stack.

(defpackage #:operandi-gui
  (:use #:cl)
  (:local-nicknames (#:w #:warp) (#:eng #:operandi.engine)
                    (#:llm #:operandi.llm) (#:tools #:operandi.tools) (#:bt #:bordeaux-threads)
                    (#:session #:operandi.session) (#:jzon #:com.inuoe.jzon))
  (:export #:*model* #:*system-prompt* #:*tool-names*
           #:msg #:msg-id #:msg-text #:msg-kind #:chat-view #:row-type #:rows
           #:*proj* #:chat-projection
           #:say #:new-session! #:open-session! #:sessions-json #:current-session-id
           #:start-agent #:stop-agent
           #:*speak-fn* #:*speak-enabled* #:speech-clean #:split-sentences))
(in-package #:operandi-gui)

;; Chat conversations are operandi sessions (reused wholesale: persisted as
;; <id>.json + .md, listed newest-first, resumable) but kept in their OWN dir so
;; the chat picker shows chats, not every TUI/nostr agent run.
(setf session:*sessions-dir*
      (namestring (merge-pathnames ".operandi/chat-sessions/" (user-homedir-pathname))))

;;; ------------------------------- config -------------------------------
(defparameter *model* (or (uiop:getenv "OPERANDI_CHAT_MODEL") "deepseek/deepseek-v4-flash"))
(defparameter *tool-names* nil "engine tool allow-list; NIL = full toolset.")
(defparameter *system-prompt*
  "You are operandi, in a web chat with your operator. Answer in plain text,
concise but complete. Use your tools to look things up and get things done before
answering. If something is ambiguous or you couldn't find it, say so — never
fabricate a value."
  "Persona/system prompt for chat turns.")

;;; ------------------------------- model --------------------------------
(defclass msg ()
  ((id   :initarg :id   :reader msg-id)
   (role :initarg :role :reader msg-role)       ; "you" | "operandi" | "…"
   (text :initarg :text :accessor msg-text)
   (kind :initarg :kind :reader msg-kind)))     ; :you | :bot | :think | :err
(defvar *messages* '() "conversation, oldest first — the transcript warp renders.")
(defvar *lock* (bt:make-lock "operandi-gui"))
(defvar *seq* 0)
(defvar *session* nil "the live operandi session (its :history threads across turns), or NIL when cold.")

(defun add-msg (role text kind)
  (bt:with-lock-held (*lock*)
    (setf *messages* (append *messages*
                             (list (make-instance 'msg :id (incf *seq*) :role role
                                                  :text text :kind kind)))))
  (car (last *messages*)))

;;; ---------------------------- sessions --------------------------------
(defparameter *greeting* "Hi — I'm operandi. Ask me anything.")

(defun current-session-id () (and *session* (gethash :id *session*)))

(defun messages-from-history (hist)
  "Rebuild the visible transcript from a resumed session's engine history: show the
   user + assistant TEXT turns; skip system, tool results, and tool-call-only turns."
  (loop for m in hist
        for role = (gethash "role" m)
        for content = (gethash "content" m)
        when (and (member role '("user" "assistant") :test #'string=)
                  (stringp content) (plusp (length content)))
          collect (make-instance 'msg :id (incf *seq*)
                                 :role (if (string= role "user") "you" "operandi")
                                 :text content
                                 :kind (if (string= role "user") :you :bot))))

(defun new-session! ()
  "Start a fresh chat: a new session, an empty transcript, a greeting."
  (bt:with-lock-held (*lock*)
    (setf *session* (session:make-session)
          *messages* (list (make-instance 'msg :id (incf *seq*) :role "operandi"
                                          :text *greeting* :kind :bot))))
  (gethash :id *session*))

(defun open-session! (id)
  "Resume a saved chat by id: load its history and rebuild the transcript. Returns
   the id, or NIL if it isn't there."
  (let ((s (session:make-session)))
    (when (session:resume-session! s id)
      (bt:with-lock-held (*lock*)
        (setf *session* s
              *messages* (or (messages-from-history (session:session-history s))
                             (list (make-instance 'msg :id (incf *seq*) :role "operandi"
                                                  :text *greeting* :kind :bot)))))
      id)))

(defun sessions-json ()
  "The saved chats as JSON for the picker: newest first, each {id,title,turns}."
  (jzon:stringify
   (coerce
    (loop for (id turns first) in (session:list-sessions)
          collect (llm:ht "id" id
                          "turns" (or turns 0)
                          "title" (let ((s (or first "New chat")))
                                    (if (> (length s) 72) (subseq s 0 72) s))))
    'vector)))

;;; ---------------------------- warp projection -------------------------
;;; A message is a typed presentation; its cells are (text kind-tag). The DOM
;;; client renders cell0 into .v and cell1 into .l; the surface hides .l and reads
;;; it to pick the bubble class (warp's trend() only knows bad/warn, so sender
;;; styling is applied from the kind tag).
(w:define-presentation-key msg (m) (msg-id m))
(defmethod w:present ((m msg) (type (eql 'msg)) (view (eql 'chat-view)))
  (list (msg-text m) (string-downcase (symbol-name (msg-kind m)))))

(defun rows ()
  "The QUERY: the conversation as domain objects (a transient 'thinking' row while
   the agent works, so the client sees progress)."
  (bt:with-lock-held (*lock*)
    (copy-list *messages*)))
(defun row-type (o) (declare (ignore o)) 'msg)

(defvar *proj* (w:make-projection #'rows :type-fn #'row-type))
(defun chat-projection () *proj*)

;;; ------------------------------- the agent ----------------------------
(defvar *queue* '()) (defvar *qlock* (bt:make-lock "chat-q")) (defvar *qcv* (bt:make-condition-variable))
(defvar *worker* nil) (defvar *running* nil) (defvar *think* nil "the live 'thinking' msg, or NIL.")

;; Voice output is the host's to provide (the gateway routes it to the desktop's chord voice; a
;; local server could synthesize to the browser). Core just offers the hook + the toggle.
(defvar *speak-enabled* nil "when T, each finished bot reply is handed to *SPEAK-FN*.")
(defvar *speak-fn* nil "(lambda (text)) — voice TEXT, or NIL for no voice.")

(defun say (text)
  "Operator sent TEXT: show it, and enqueue a turn for the agent."
  (let ((s (string-trim '(#\Space #\Newline #\Return #\Tab) text)))
    (when (plusp (length s))
      (unless *session* (new-session!))     ; typing implies a chat; open one if cold
      (add-msg "you" s :you)
      (bt:with-lock-held (*qlock*)
        (setf *queue* (nconc *queue* (list s)))
        (bt:condition-notify *qcv*)))))

(defun answer (text)
  "Run one engine turn threaded onto the live session's history, and persist it."
  (let ((base (and *session* (session:session-history *session*))))
    (multiple-value-bind (reply hist)
        (eng:run text
                 ;; :history NIL on the first turn lets the engine seat the system
                 ;; prompt; thereafter we thread the accumulated history + new message.
                 :history (when base
                            (append base (list (llm:ht "role" "user" "content" text))))
                 :system *system-prompt*
                 :tool-names (or *tool-names* (tools:default-tools))
                 :verbose nil)
      (when *session*
        (setf (gethash :history *session*) hist)
        (incf (gethash :turns *session*))
        (ignore-errors (session:persist-session *session*)))     ; crash never loses a turn
      (let ((r (string-trim '(#\Space #\Newline #\Return #\Tab) (or reply ""))))
        (if (plusp (length r)) r "(no reply produced)")))))

(defun %strip-md-inline (text)
  "Drop inline markdown glyphs a voice shouldn't read — **bold**, `code`, ### , > blockquote —
   and turn [label](url) into just the label.  Leaves - and _ (hyphens, identifiers) alone."
  (with-output-to-string (s)
    (let ((n (length text)) (i 0))
      (loop while (< i n) do
        (let ((c (char text i)))
          (cond
            ((char= c #\[)                              ; [label](url) -> label
             (incf i)
             (loop while (and (< i n) (char/= (char text i) #\])) do
               (write-char (char text i) s) (incf i))
             (when (< i n) (incf i))                    ; past ]
             (when (and (< i n) (char= (char text i) #\())
               (loop while (and (< i n) (char/= (char text i) #\))) do (incf i))
               (when (< i n) (incf i))))                ; past (url)
            ((and (char= c #\-)                          ; a --- rule: drop a run of 3+ hyphens
                  (let ((j i)) (loop while (and (< j n) (char= (char text j) #\-)) do (incf j))
                    (when (>= (- j i) 3) (setf i j) t))))
            ((member c '(#\* #\` #\# #\~ #\>)) (incf i)) ; emphasis / heading / rule / quote glyphs
            (t (write-char c s) (incf i))))))))

(defun speech-clean (text)
  "A markdown reply as speech-friendly text: keep the words, drop the syntax a voice reads
   awkwardly (headings, bold/italic stars, backticks, bullet/number markers, link URLs), and
   collapse blank runs to a single pause.  The DISPLAYED reply keeps its markdown; only the voice
   gets this."
  (let ((out (make-string-output-stream)) (blank 0))
    (dolist (raw (uiop:split-string (%strip-md-inline text) :separator '(#\Newline)))
      (let* ((line (string-trim '(#\Space #\Tab #\Return) raw))
             (line (cond
                     ((and (>= (length line) 2) (member (char line 0) '(#\- #\+))
                           (char= (char line 1) #\Space))
                      (string-left-trim '(#\Space) (subseq line 2)))          ; "- item" / "+ item"
                     (t (let ((dot (position #\. line)))                       ; "3. item"
                          (if (and dot (< 0 dot 4) (< (1+ dot) (length line))
                                   (char= (char line (1+ dot)) #\Space)
                                   (every #'digit-char-p (subseq line 0 dot)))
                              (string-left-trim '(#\Space) (subseq line (1+ dot)))
                              line))))))
        (if (zerop (length line))
            (when (zerop blank) (terpri out) (setf blank 1))
            (progn (setf blank 0) (write-line line out)))))
    (string-trim '(#\Space #\Tab #\Newline #\Return) (get-output-stream-string out))))

(defun split-sentences (text)
  "Break TEXT into highlight chunks: at sentence-enders (. ! ?) followed by space/end, and at line
   breaks.  MUST stay identical to the JS splitter in payload.js so chunk K here == chunk K there."
  (let ((out '()) (start 0) (n (length text)))
    (flet ((emit (end)
             (let ((chunk (string-trim '(#\Space #\Tab #\Return #\Newline) (subseq text start end))))
               (when (plusp (length chunk)) (push chunk out)))))
      (loop for i from 0 below n for c = (char text i) do
        (cond ((char= c #\Newline) (emit i) (setf start (1+ i)))
              ((and (member c '(#\. #\! #\?))
                    (or (= (1+ i) n) (member (char text (1+ i)) '(#\Space #\Newline)))
                    ;; not a list number "1." or a decimal "3.14": a '.' right after a digit
                    (not (and (char= c #\.) (> i 0) (digit-char-p (char text (1- i))))))
               (emit (1+ i)) (setf start (1+ i)))))
      (emit n))
    (nreverse out)))

(defun worker-loop ()
  (loop while *running* do
    (let ((text (bt:with-lock-held (*qlock*)
                  (loop until (or (not *running*) *queue*)
                        do (bt:condition-wait *qcv* *qlock*))
                  (and *queue* (pop *queue*)))))
      (when text
        (setf *think* (add-msg "operandi" "…thinking…" :think))
        (multiple-value-bind (reply kind)
            (handler-case (values (answer text) :bot)
              (serious-condition (e) (values (format nil "Something went wrong: ~A" e) :err)))
          ;; replace the thinking row's text in place (same id -> a :changed delta)
          (let ((mid (msg-id *think*)))
            (bt:with-lock-held (*lock*)
              (setf (msg-text *think*) reply
                    (slot-value *think* 'kind) kind))
            (setf *think* nil)
            ;; voice a real answer (not an error) if the operator turned it on.  We hand the RAW
            ;; reply + its row id; the host splits into sentences, cleans each for the voice, and
            ;; drives the highlight (payload.js re-splits the SAME raw text to find the spans).
            (when (and (eq kind :bot) *speak-enabled* *speak-fn*)
              (ignore-errors (funcall *speak-fn* reply mid)))))))))

;;; ------------------------------ lifecycle -----------------------------
(defun start-agent (&key (model *model*))
  "Configure the LLM backend and start the single worker thread (idempotent)."
  (when model (llm:use-openrouter :model model))
  (ensure-directories-exist session:*sessions-dir*)
  (unless (and *worker* (bt:thread-alive-p *worker*))
    (setf *running* t
          *worker* (bt:make-thread #'worker-loop :name "chat-agent")))
  t)

(defun stop-agent ()
  (setf *running* nil)
  (bt:with-lock-held (*qlock*) (bt:condition-notify *qcv*)))

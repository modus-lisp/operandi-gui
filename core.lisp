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
                    (#:hooks #:operandi.hooks)
                    (#:session #:operandi.session) (#:jzon #:com.inuoe.jzon))
  (:export #:*model* #:*system-prompt* #:*tool-names*
           #:msg #:msg-id #:msg-text #:msg-kind #:chat-view #:row-type #:rows
           #:*proj* #:chat-projection
           #:say #:new-session! #:open-session! #:sessions-json #:current-session-id
           #:start-agent #:stop-agent
           #:*speak-fn* #:*speak-enabled* #:speech-clean #:split-sentences
           #:set-model #:handle-command #:effort-label #:context-budget-k #:compact-at-k))
(in-package #:operandi-gui)

;; Chat conversations are operandi sessions (reused wholesale: persisted as
;; <id>.json + .md, listed newest-first, resumable) but kept in their OWN dir so
;; the chat picker shows chats, not every TUI/nostr agent run.
(setf session:*sessions-dir*
      (namestring (merge-pathnames ".operandi/chat-sessions/" (user-homedir-pathname))))

;;; ------------------------------- config -------------------------------
(defparameter *model* (or (uiop:getenv "OPERANDI_CHAT_MODEL") "z-ai/glm-5.3-flash")
  "The model a fresh chat starts on.  Changed live with /model — which pings the new one and keeps
   the old if it does not answer, so the default here is a starting point and not a commitment.")
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
(defvar *steer* '()
  "Words written while a turn is ALREADY running.  They do not wait for it —
   ENG:*STEER-FN* drains this between iterations and appends them to the running
   conversation, so they redirect the work in flight.  Queuing them instead is
   what made \"continue\" the only usable thing to say: by the time a queued
   message is read, the turn you wanted to change is over.")
(defvar *worker* nil) (defvar *running* nil) (defvar *think* nil "the live 'thinking' msg, or NIL.")

;; Voice output is the host's to provide (the gateway routes it to the desktop's chord voice; a
;; local server could synthesize to the browser). Core just offers the hook + the toggle.
(defvar *speak-enabled* nil "when T, each finished bot reply is handed to *SPEAK-FN*.")
(defvar *speak-fn* nil "(lambda (text)) — voice TEXT, or NIL for no voice.")

;;; ------------------------------ /commands ----------------------------
;;; Typed into the same box as everything else, because on a phone that box is the only input this
;;; app has.  A line beginning with / is the GUI's own and never reaches the agent.

(defun set-model (slug)
  "Switch the agent to SLUG — but PROVE it first.  ENG:PREFLIGHT-MODEL sends a one-token ping and
   reads the provider's own reason, so a typo, a retired slug, or a provider-allowlist miss is
   caught here rather than turning every later turn into a blank reply.  A switch that does not
   preflight is rolled back: better the model you had than one that cannot answer."
  (let ((old *model*))
    (llm:use-openrouter :model slug)
    (multiple-value-bind (ok reason) (eng:preflight-model)
      (cond (ok (setf *model* slug)
                (format nil "Model is now ~a." slug))
            (t (llm:use-openrouter :model old)
               (format nil "~a did not answer, so I kept ~a.~@[~%~%~a~]" slug old reason))))))

;;; Reasoning effort and the context budget are engine settings, not chat settings, so they live
;;; where the engine keeps them (LLM:*LLM-EFFORT*, ENG:*CONTEXT-TOKEN-BUDGET*) and these commands
;;; only read and write those places.  The TUI already spells effort `/effort'; the same word here
;;; means the same thing, because an operator who learned one should not have to learn the other.

(defun effort-label ()
  "The current reasoning effort as the word a person would type."
  (if llm:*llm-effort* (string-downcase (symbol-name llm:*llm-effort*)) "default"))

(defun context-budget-k ()
  "The context budget in thousands of tokens — the unit the panel and /model both show."
  (round eng:*context-token-budget* 1000))

(defun compact-at-k ()
  "Where compaction actually fires, in thousands of tokens.  Shown next to the
   budget because the two differ by the headroom, and a budget that never seems
   to be reached is otherwise just confusing."
  (round (eng:compact-threshold) 1000))

(defun set-effort (arg)
  "Set reasoning effort from ARG, or explain what ARG could have been."
  (multiple-value-bind (e ok) (llm:parse-effort arg)
    (cond ((not ok)
           (format nil "`~a` is not an effort. Try `off`, `low`, `medium`, `high`, or `default`."
                   arg))
          (t (setf llm:*llm-effort* e)
             (format nil "Effort is now ~a." (effort-label))))))

(defun set-context-budget (arg)
  "Set the context budget from ARG, read as thousands of tokens (`24` and `24k` both mean 24000).
   Bounded below because a budget under a few thousand tokens compacts away the turn in progress,
   and above because the point of the number is to stay under the model's window."
  (let* ((txt (string-right-trim "kK" (string-trim " " arg)))
         (n (ignore-errors (parse-integer txt))))
    (cond ((null n) (format nil "`~a` is not a number of thousands of tokens. Try `/context 24`."
                            arg))
          ((< n 4) "That is too small — under 4k the budget compacts away the turn in progress.")
          ((> n 1000) "That is larger than any context window I can use. Try something under 1000.")
          (t (setf eng:*context-token-budget* (* n 1000))
             (format nil "Context budget is now ~dk tokens; I compact at ~dk."
                     n (compact-at-k))))))

(defun handle-command (line)
  "Answer a /command, or return NIL if LINE is not one."
  (let* ((s (string-trim '(#\Space #\Tab) line))
         (sp (position #\Space s))
         (verb (string-downcase (subseq s 0 (or sp (length s)))))
         (arg (and sp (string-trim '(#\Space #\Tab) (subseq s sp)))))
    (cond
      ((not (and (plusp (length s)) (char= (char s 0) #\/))) nil)
      ((string= verb "/model")
       (if (and arg (plusp (length arg)))
           (set-model arg)
           (format nil "Model is `~a`.~%~%Change it with `/model <slug>` — I ping the model and keep ~
                        the old one if it doesn't answer." *model*)))
      ((string= verb "/effort")
       (if (and arg (plusp (length arg)))
           (set-effort arg)
           (format nil "Effort is `~a`.~%~%Change it with `/effort off|low|medium|high|default`."
                   (effort-label))))
      ((string= verb "/context")
       (if (and arg (plusp (length arg)))
           (set-context-budget arg)
           (format nil "Context budget is `~dk` tokens; I compact at `~dk`, leaving ~dk of ~
                        headroom for a big tool result.~%~%Change it with `/context <k>`, e.g. ~
                        `/context 24`."
                   (context-budget-k) (compact-at-k)
                   (- (context-budget-k) (compact-at-k)))))
      ((string= verb "/new")
       (new-session!)
       "Started a fresh conversation.")
      ((member verb '("/help" "/?") :test #'string=)
       (format nil "- `/model [slug]` — show or switch the model~%~
                    - `/effort [lvl]` — reasoning effort: off, low, medium, high, default~%~
                    - `/context [k]` — context budget in thousands of tokens~%~
                    - `/new` — start a fresh conversation~%~
                    - `/help` — this"))
      (t (format nil "I don't know `~a`. Try `/help`." verb)))))

(defun say (text)
  "Operator sent TEXT: show it, and enqueue a turn for the agent — unless it is a /command, which
   the GUI answers itself and the agent never sees."
  (let ((s (string-trim '(#\Space #\Newline #\Return #\Tab) text)))
    (when (plusp (length s))
      (unless *session* (new-session!))     ; typing implies a chat; open one if cold
      (add-msg "you" s :you)
      (let ((cmd (and (char= (char s 0) #\/)
                      (handler-case (handle-command s)
                        (serious-condition (e) (format nil "That went wrong: ~a" e))))))
        (cond
          ;; the GUI is answering, so it lands as a reply and nothing is queued for the engine
          (cmd (add-msg "operandi" cmd :bot))
          ;; A turn is in flight: STEER it rather than queue behind it.  *THINK* is
          ;; the live turn's own row, so it is exactly "is the worker busy" without
          ;; a second flag that could disagree with it.
          (*think*
           (bt:with-lock-held (*qlock*)
             (setf *steer* (nconc *steer* (list s))))
           (%think! "↪ steering…"))
          (t
           (bt:with-lock-held (*qlock*)
             (setf *queue* (nconc *queue* (list s)))
             (bt:condition-notify *qcv*))))))))

;;; --------------------------- live status ------------------------------
;;; A turn can run for minutes across dozens of tool calls, and the panel said
;;; "...thinking..." for all of it -- indistinguishable from a hang, which is the
;;; thing a chat UI most needs to not look like.  The row is already updated in
;;; place at the end of the turn (same id -> a :changed delta), so saying more
;;; costs nothing new: rewrite the SAME row as the work happens.

(defun %think! (fmt &rest args)
  "Rewrite the in-flight thinking row.  A no-op when no turn is running, so a
   stray hook from a finished run cannot resurrect a row or clobber a reply."
  (let ((m *think*))
    (when m
      (bt:with-lock-held (*lock*)
        (setf (msg-text m) (apply #'format nil fmt args))))))

(defun %arg-brief (args)
  "The one argument worth showing from a tool call -- the path, pattern or command
   -- trimmed to fit a phone.  Deliberately not the whole argument hash: this is a
   status line, and a Bash heredoc would bury the panel."
  (let ((v (and (hash-table-p args)
                (or (gethash "path" args) (gethash "file_path" args)
                    (gethash "pattern" args) (gethash "command" args)
                    (gethash "query" args) (gethash "url" args)))))
    (when (stringp v)
      (let* ((one (substitute #\Space #\Newline v))
             (cut (if (> (length one) 48)
                      (concatenate 'string (subseq one 0 48) "...")
                      one)))
        cut))))

(defun answer (text)
  "Run one engine turn threaded onto the live session's history, and persist it."
  (let ((base (and *session* (session:session-history *session*)))
        (calls 0)
        (toks 0))
    (multiple-value-bind (reply hist)
        (let* ((hooks:*pre-tool-hooks*
                 (cons (lambda (name args)
                         (incf calls)
                         (let ((b (%arg-brief args)))
                           (%think! "⚙ ~a~@[ ~a~]~@[ · ~a~]" name b
                                    (and (plusp calls)
                                         (format nil "~d tool call~:p" calls)))))
                       hooks:*pre-tool-hooks*))
               (hooks:*post-tool-hooks*
                 (cons (lambda (name args result error ms)
                         (declare (ignore args result ms))
                         (when error (%think! "⚠ ~a failed — recovering…" name)))
                       hooks:*post-tool-hooks*))
               ;; Compaction is the longest silent pause in a run: it summarises the
               ;; whole span with a second model call.  Name it, or it reads as a hang.
               (eng:*on-compact*
                 (lambda (before after path)
                   (declare (ignore path))
                   (%think! "🗜 compacting context ~dk → ~dk…"
                            (round before 1000) (round after 1000))))
               ;; Streamed reasoning, throttled: every token would be a delta to the
               ;; phone, so only redraw about once per 800 characters.  The count is
               ;; the point -- it moves, which is what says running rather than wedged.
               ;; Hand the engine anything said since the last iteration.  One string,
               ;; blank-joined, so two quick messages arrive as one turn rather than
               ;; two consecutive user turns the model has to reconcile.
               (eng:*steer-fn*
                 (lambda ()
                   (let ((pending (bt:with-lock-held (*qlock*)
                                    (prog1 *steer* (setf *steer* '())))))
                     (when pending
                       (format nil "~{~A~^~%~%~}" pending)))))
               (eng:*on-token*
                 (lambda (chunk)
                   (incf toks (length chunk))
                   (when (> toks 800)
                     (setf toks 0)
                     (%think! "…thinking~@[ · ~a so far~]"
                              (and (plusp calls)
                                   (format nil "~d tool call~:p" calls)))))))
          (eng:run text
                 ;; :history NIL on the first turn lets the engine seat the system
                 ;; prompt; thereafter we thread the accumulated history + new message.
                 :history (when base
                            (append base (list (llm:ht "role" "user" "content" text))))
                 :system *system-prompt*
                 :tool-names (or *tool-names* (tools:default-tools))
                 :verbose nil))
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
            ;; A steer written in the gap between the engine's last drain and the
            ;; turn ending has nowhere to land — it must not evaporate.  Promote it
            ;; to the turn queue so it starts the next turn instead.
            (let ((late (bt:with-lock-held (*qlock*)
                          (prog1 *steer* (setf *steer* '())))))
              (when late
                (bt:with-lock-held (*qlock*)
                  (setf *queue* (nconc *queue* late))
                  (bt:condition-notify *qcv*))))
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

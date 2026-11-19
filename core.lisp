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
                    (#:session #:operandi.session) (#:json #:json-simple))
  (:export #:*model* #:*system-prompt* #:*tool-names*
           #:msg #:msg-id #:msg-text #:msg-kind #:chat-view #:row-type #:rows
           #:*proj* #:chat-projection
           #:say #:stored-file #:file-payload #:new-session! #:open-session! #:delete-session! #:upload! #:sessions-json #:current-session-id
           #:start-agent #:stop-agent #:busy-p #:*row-previews* #:configure! #:preflight! #:*preflight-fn* #:url-label
           #:*speak-fn* #:*speak-enabled* #:speech-clean #:split-sentences
           #:set-model #:handle-command #:effort-label #:context-budget-k #:compact-at-k))
(in-package #:operandi-gui)

;; Chat conversations are operandi sessions (reused wholesale: persisted as
;; <id>.json + .md, listed newest-first, resumable) but kept in their OWN dir so
;; the chat picker shows chats, not every TUI/nostr agent run.
(setf session:*sessions-dir*
      (namestring (merge-pathnames ".operandi/chat-sessions/" (user-homedir-pathname))))

;;; ------------------------------- config -------------------------------
(defparameter *chat-url* (uiop:getenv "OPERANDI_CHAT_URL")
  "A local OpenAI-compatible chat endpoint (llama.cpp, illatio: http://host:port/v1/chat/completions)
   to talk to instead of OpenRouter.  NIL = OpenRouter.")
(defparameter *model* (or (uiop:getenv "OPERANDI_CHAT_MODEL") (unless *chat-url* "z-ai/glm-5.3-flash"))
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
   (kind :initarg :kind :reader msg-kind)))     ; :you | :bot | :think | :err | :file (text: JSON, FILE-ROW-TEXT)
(defvar *messages* '() "conversation, oldest first — the transcript warp renders.")
(defvar *lock* (bt:make-lock "operandi-gui"))
(defvar *seq* 0)
(defvar *session* nil "the live operandi session (its :history threads across turns), or NIL when cold.")

;;; ------------------------------- the agent ----------------------------
;;; THE WORKER SHARES NOTHING WITH THE HOST.  The transcript (*MESSAGES*, *SESSION*, *UPLOADS*) is the
;;; HOST's -- the thread that calls SAY and ROWS: a desk's frame loop, a server's connections.  The
;;; worker owns the agent's side: the engine session it loads by id and saves after each turn, the live
;;; status of the turn it is running.  They talk in two mailboxes of plain data: the host sends turns and
;;; steers, the worker sends back what the chat should show (rows to open, text to put in them, the
;;; reply), and ROWS applies that before every read.
;;;
;;; WHY.  On modus a thread's objects live in its own heap and the runtime refuses a store that would let
;;; another thread reach one, so a worker that pushed rows into *MESSAGES* (as this one used to) has its
;;; first reply refused.  A value crosses by being SENT -- copied -- which is what a modus channel does;
;;; elsewhere the mailbox is a locked list, and the same code runs on both.

(defstruct (mailbox (:constructor %make-mailbox)) chan lock cv (items '()))

(defun make-mailbox (name)
  "A mailbox between threads.  Made at run time, never in a saved image: a modus channel's lock and ring
   belong to the process that made them."
  #+modus (%make-mailbox :chan (modus:make-channel :name name))
  #-modus (%make-mailbox :lock (bt:make-lock name) :cv (bt:make-condition-variable)))

(defun mb-send (mb value)
  "Send VALUE (plain data: lists, strings, numbers, symbols, hash tables) to MB's reader."
  #+modus (modus:channel-send (mailbox-chan mb) value)
  #-modus (bt:with-lock-held ((mailbox-lock mb))
            (setf (mailbox-items mb) (nconc (mailbox-items mb) (list value)))
            (bt:condition-notify (mailbox-cv mb)))
  t)

(defun mb-receive (mb)
  "The next value in MB, waiting for one."
  #+modus (values (modus:channel-receive (mailbox-chan mb)))
  #-modus (bt:with-lock-held ((mailbox-lock mb))
            (loop until (mailbox-items mb)
                  do (bt:condition-wait (mailbox-cv mb) (mailbox-lock mb)))
            (pop (mailbox-items mb))))

(defun mb-try (mb)
  "(values value T) for the next value in MB, or NIL NIL when there is none."
  #+modus (modus:channel-try-receive (mailbox-chan mb))
  #-modus (bt:with-lock-held ((mailbox-lock mb))
            (if (mailbox-items mb) (values (pop (mailbox-items mb)) t) (values nil nil))))

;; the host's side
(defvar *inbox* nil "Host -> worker: (:turn TEXT UPLOADS SID), (:steer TEXT SID), (:stop).")
(defvar *outbox* nil "Worker -> host: what the chat shows (see APPLY-EVENT).")
(defvar *worker* nil) (defvar *running* nil)
(defvar *pending* 0
  "Turns sent and not yet finished.  Non-zero means a turn is running, so what the operator writes now
   STEERS it (see SAY) rather than waiting behind it.")
(defun busy-p () "A turn is running: what is said now steers it." (plusp *pending*))
(defvar *think* nil "The live 'thinking' msg, or NIL -- the host's row for the worker's live status.")
(defvar *row-of* (make-hash-table) "The worker's row tokens -> the host's msgs, for the running turn.")
(defvar *pump-lock* (bt:make-lock "operandi-gui pump"))
(defvar *replies* nil "Host -> worker: (ID RESULT), the answer to a tool the host ran for it (HOST-TOOLS).")

;; TOOLS THAT RUN ON THE HOST.  On modus a worker thread cannot EVAL: compiling a form stores into the
;; image's shared tables, and the store is refused.  So the tools that evaluate are run by the host, in
;; its PUMP, and the worker waits for the answer.  Elsewhere a worker evaluates as well as any thread.
(defparameter *host-tools* #+modus '("Eval") #-modus '()
  "Tools whose bodies the HOST runs for the worker.")
(defvar *host-tool-impls* (make-hash-table :test 'equal) "Name -> the tool's own implementation.")
(defvar *host-call-n* 0)

;; the worker's side, bound in its thread only
(defvar *worker-out* nil "The outbox, in the worker; NIL on the host.  How a function knows which side it is on.")
(defvar *wsession* nil "The worker's engine session.")
(defvar *think-k* nil "The worker's token for its live row, or NIL.")
(defvar *think-n* 0)
(defvar *later* '() "Turns the worker read while draining steers, to run after this one.")
(defvar *stopping* nil)

;;; ------------------------------- files --------------------------------
;;; A file the agent hands over -- a picture Imagine made or Read opened, anything SendFile sends --
;;; is COPIED into this chat's own store, ~/.operandi/chat-files/<session>/<id>/<name>, and its row
;;; refers to the copy: a transcript must not break when the original moves.  The id is the start
;;; of the content's SHA-256 (natrium's, which operandi already carries -- sb-md5 is SBCL's alone), so the
;;; same bytes sent twice are one file.  A row's text is JSON
;;; {id,name,size,mime,href,preview}: HREF downloads it from the HTTP host; the phone, which has
;;; no HTTP, asks the gateway for the bytes (FILE-PAYLOAD); PREVIEW, for a picture, is a data: URL
;;; shrunk to *PICTURE-SIDE*, so the row shows it at a glance.

(defparameter *files-root* (merge-pathnames ".operandi/chat-files/" (user-homedir-pathname)))
(defparameter *picture-side* 768 "Longest side, in pixels, of a picture's preview.")
(defparameter *phone-file-max* (* 25 1024 1024)
  "The largest file the phone fetches in one frame; larger ones are for the HTTP page.")

(defparameter +mime-types+
  '(("txt" . "text/plain") ("md" . "text/markdown") ("csv" . "text/csv") ("json" . "application/json")
    ("html" . "text/html") ("pdf" . "application/pdf") ("zip" . "application/zip")
    ("svg" . "image/svg+xml") ("mp3" . "audio/mpeg") ("wav" . "audio/wav") ("mp4" . "video/mp4")))

(defun file-mime (name)
  (or (llm:image-file-p name)
      (cdr (assoc (pathname-type (pathname name)) +mime-types+ :test #'equalp))
      "application/octet-stream"))

(defun read-octets (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length in) :element-type '(unsigned-byte 8)))) (read-sequence v in) v)))

(defun files-dir (sid) (merge-pathnames (format nil "~a/" sid) *files-root*))

(defun file-id-p (id)
  (and (stringp id) (= (length id) 12) (every (lambda (c) (digit-char-p c 16)) id)))

(defun stored-file (id &optional (sid (current-session-id)))
  "(values path name size mime) of stored file ID in chat SID, or NIL."
  (when (and sid (file-id-p id) (every (lambda (c) (or (alphanumericp c) (char= c #\-))) sid))
    (let ((f (first (uiop:directory-files (merge-pathnames (format nil "~a/" id) (files-dir sid))))))
      (when f
        (let ((name (file-namestring f)))
          (values f name (with-open-file (in f :element-type '(unsigned-byte 8)) (file-length in))
                  (file-mime name)))))))

(defun index-file (sid) (merge-pathnames "index.sexp" (files-dir sid)))

(defun stored-id-of (path sid)
  "The id PATH was stored under in chat SID, if it was (the original may since have moved)."
  (let ((f (index-file sid)))
    (when (probe-file f)
      (cdr (assoc path (ignore-errors (with-open-file (in f) (let ((*read-eval* nil)) (read in nil))))
                  :test #'equal)))))

(defun store-octets (bytes name &optional (sid (current-session-id)))
  "BYTES, a file named NAME, in chat SID's store; its id."
  (let* ((id (string-downcase (subseq (format nil "~{~2,'0x~}" (coerce (natrium:sha256 (coerce bytes '(simple-array (unsigned-byte 8) (*)))) 'list)) 0 12)))
         (dest (merge-pathnames (format nil "~a/~a" id name) (files-dir sid))))
    (unless (stored-file id sid)
      (ensure-directories-exist dest)
      (with-open-file (o dest :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
        (write-sequence bytes o)))
    id))

(defun store-file (path &optional (sid (current-session-id)))
  "Copy PATH into chat SID's store; its id."
  (let ((id (store-octets (read-octets path) (file-namestring path) sid)))
    (let* ((f (index-file sid))
           (index (and (probe-file f) (ignore-errors (with-open-file (in f) (let ((*read-eval* nil)) (read in nil)))))))
      (unless (equal (cdr (assoc path index :test #'equal)) id)
        (ensure-directories-exist f)
        (with-open-file (o f :direction :output :if-exists :supersede)
          (with-standard-io-syntax (prin1 (acons path id (remove path index :key #'car :test #'equal)) o)))))
    id))

(defvar *row-previews* t
  "Whether a picture's row carries a small copy of it (\"preview\", a data: URL) for a client that
   draws from the row alone -- the browser.  Making one decodes the picture in Lisp; a phone draws
   its own thumbnails natively and turns this off.")

(defun file-row-text (id sid &optional note mine)
  (multiple-value-bind (path name size mime) (stored-file id sid)
    (when path
      (let ((h (llm:ht "id" id "name" name "size" size "mime" mime
                       "href" (format nil "/file/~a/~a" sid id))))
        (when mine (setf (gethash "from" h) "you"))          ; an upload: on the operator's side
        (when (and (stringp note) (plusp (length note))) (setf (gethash "note" h) note))
        (when (and *row-previews* (llm:image-file-p name))
          (let ((url (ignore-errors (gethash "url" (gethash "image_url" (llm:image-part path :max-side *picture-side*))))))
            (when url (setf (gethash "preview" h) url))))
        (json:stringify h)))))

(defun file-msg (id &optional (sid (current-session-id)) note mine)
  "A row for stored file ID, captioned NOTE (an error row when it is gone); MINE: the operator's upload."
  (let ((text (and id (file-row-text id sid note mine))))
    (make-instance 'msg :id (incf *seq*) :role "operandi"
                        :text (or text "(a file that is no longer stored)") :kind (if text :file :err))))

;;; An upload: the operator's file, stored as the agent's are, its row on their side.  It goes with
;;; their next message: a note with its stored path (for tools: Imagine's image, Read) and, when it
;;; is a picture, the picture, so the model sees it.
(defvar *uploads* '() "Paths of the files uploaded since the operator's last message, oldest first.")
(defparameter +upload-tag+ "[The user uploaded: ")
(defparameter *upload-side* 1024 "Longest side, in pixels, of an uploaded picture as the model is shown it.")

(defun upload-name (name)
  "NAME made safe as a file name: its last component, letters, digits, . _ - kept."
  (let* ((base (subseq name (1+ (or (position-if (lambda (c) (member c '(#\/ #\\))) name :from-end t) -1))))
         (safe (map 'string (lambda (c) (if (or (alphanumericp c) (member c '(#\. #\_ #\-))) c #\_)) base))
         (safe (string-left-trim "." safe)))
    (if (plusp (length safe)) (subseq safe 0 (min 120 (length safe))) "upload")))

(defun upload! (name bytes)
  "The operator uploaded BYTES as NAME: stored in the open chat (one is opened if none is), its row
   added, and held for their next message.  The stored id."
  (unless *session* (new-session!))
  (let* ((sid (current-session-id)) (id (store-octets bytes (upload-name name) sid)))
    (bt:with-lock-held (*lock*)
      (setf *messages* (append *messages* (list (file-msg id sid nil t)))
            *uploads* (append *uploads* (list (namestring (stored-file id sid))))))
    id))

(defun user-content (text uploads)
  "A user message's content: TEXT, and with UPLOADS a parts array, each file noted after it by path
   and each picture shown."
  (if (null uploads)
      text
      (coerce (cons (llm:ht "type" "text" "text" text)
                    (loop for p in uploads
                          collect (llm:ht "type" "text" "text" (format nil "~%~a~a]" +upload-tag+ p))
                          when (llm:image-file-p p)
                            collect (llm:image-part p :max-side *upload-side*)))
              'vector)))

(defun upload-parts (content)
  "(values text paths) of a user message's CONTENT made by USER-CONTENT."
  (let ((text '()) (paths '()))
    (loop for part across content
          for x = (and (hash-table-p part) (equal (gethash "type" part) "text") (gethash "text" part))
          for line = (and x (string-trim '(#\Newline #\Space) x))
          do (cond ((null line))
                   ((and (eql 0 (search +upload-tag+ line)) (char= (char line (1- (length line))) #\]))
                    (push (subseq line (length +upload-tag+) (1- (length line))) paths))
                   (t (push x text))))
    (values (format nil "~{~a~}" (reverse text)) (reverse paths))))

(defun path-file-msg (path &optional (sid (current-session-id)))
  "A row for the file at PATH: its stored copy, storing it now if it never was."
  (let ((id (or (stored-id-of path sid)
                (and sid (probe-file path) (ignore-errors (store-file path sid))))))
    (if id
        (file-msg id sid)
        (make-instance 'msg :id (incf *seq*) :role "operandi"
                            :text (format nil "(file no longer available: ~a)" path) :kind :err))))

(defun file-payload (id)
  "The phone's download: {a:chat, file:{id,name,mime,b64}} for stored file ID, or an error."
  (multiple-value-bind (path name size mime) (stored-file id)
    (json:stringify
     (llm:ht "a" "chat"
             "file" (cond ((null path) (llm:ht "id" id "error" "no such file"))
                          ((> size *phone-file-max*)
                           (llm:ht "id" id "error" (format nil "~a is ~:d bytes: download it from the desktop page" name size)))
                          (t (llm:ht "id" id "name" name "mime" mime
                                     "b64" (operandi.http:base64-string (read-octets path)))))))))

(defun tool-picture-paths (content)
  "The paths in an engine tool-pictures message (operandi.engine's TOOL-IMAGES-MSG), or NIL when
   CONTENT is not one: its first text part is the tag, then one \"<path>:\" part per picture."
  (when (and (vectorp content) (plusp (length content)) (hash-table-p (aref content 0))
             (eql 0 (search "[Pictures from the tool calls above" (or (gethash "text" (aref content 0)) ""))))
    (loop for part across content
          for text = (and (hash-table-p part) (equal (gethash "type" part) "text") (gethash "text" part))
          for line = (and text (string-trim '(#\Newline #\Space) text))
          when (and line (> (length line) 1) (char= (char line (1- (length line))) #\:)
                    (char= (char line 0) #\/))
            collect (subseq line 0 (1- (length line))))))

(defun sent-file-id (content)
  "(values id note) from a SendFile result (\"Sent NAME to the user [file ID]: NOTE\"), or NIL."
  (let ((i (and (stringp content) (search "[file " content))))
    (when i
      (let ((id (subseq content (+ i 6) (min (length content) (+ i 18)))))
        (when (file-id-p id)
          (values id (let ((j (search "]: " content :start2 i))) (and j (subseq content (+ j 3))))))))))

(defun file-row-id (m)
  "The id of the file row M, or NIL."
  (and (eq (msg-kind m) :file)
       (ignore-errors (gethash "id" (json:parse (msg-text m))))))

(defun add-row (row)
  (bt:with-lock-held (*lock*) (setf *messages* (append *messages* (list row)))))

(defun %emit (&rest event)
  "From the worker: tell the host what the chat should show (APPLY-EVENT)."
  (mb-send *worker-out* event))

(defun add-file-row (id sid &optional note)
  "A row for stored file ID -- unless the chat already shows it, as when SendFile hands over the
   picture Imagine just made; a NOTE then captions the row already there.  From the worker, the host
   is asked to (the row is the host's)."
  (when *worker-out* (return-from add-file-row (%emit :file id sid note)))
  (let ((there (bt:with-lock-held (*lock*) (find id *messages* :key #'file-row-id :test #'equal))))
    (cond ((null there) (add-row (file-msg id sid note)))
          ((and (stringp note) (plusp (length note)))
           (let ((text (file-row-text id sid note)))
             (when text (bt:with-lock-held (*lock*) (setf (msg-text there) text))))))))

(tools:register-tool
 "SendFile"
 "Hand the user a file: it appears in the chat as a download (a picture with its preview), and stays
there even if the original later moves.  Use it for anything you made for them to keep -- a report,
a CSV, an archive, a picture.  NOTE, optional, is a line shown with it."
 (llm:ht "type" "object"
         "properties" (llm:ht "path" (llm:ht "type" "string" "description" "The file to send.")
                              "note" (llm:ht "type" "string" "description" "A line shown with it."))
         "required" (vector "path"))
 (lambda (args)
   (let ((path (gethash "path" args)) (sid (current-session-id)))
     (cond ((not sid) "SendFile: no chat is open")
           ;; a file, not a directory: PROBE-FILE alone (modus's UIOP has no FILE-EXISTS-P)
           ((not (and (stringp path)
                      (let ((p (probe-file path))) (and p (pathname-name p)))))
            (format nil "SendFile: no such file: ~a" path))
           (t (let ((id (store-file (namestring (truename path)) sid))
                    (note (let ((n (gethash "note" args)))
                            (and (stringp n) (plusp (length n)) (substitute #\Space #\Newline n)))))
                (add-file-row id sid note)
                (format nil "Sent ~a to the user [file ~a]~@[: ~a~]" (file-namestring path) id note)))))))

(defun add-msg (role text kind)
  (bt:with-lock-held (*lock*)
    (setf *messages* (append *messages*
                             (list (make-instance 'msg :id (incf *seq*) :role role
                                                  :text text :kind kind)))))
  (car (last *messages*)))

;;; ---------------------------- sessions --------------------------------
(defparameter *greeting* "Hi — I'm operandi. Ask me anything.")

(defun current-session-id ()
  "The open chat's id -- in the worker, the chat of the turn it is running."
  (let ((s (or *wsession* *session*))) (and s (gethash :id s))))

(defun messages-from-history (hist)
  "Rebuild the visible transcript from a resumed session's engine history: the user and
   assistant text turns, and before each reply the row of what led to it -- its reasoning and its
   tool calls, as a live turn shows them (summary first, a tap for the rest)."
  (let ((out '()) (log '()) (calls 0) (pics '()) (sid (current-session-id))
        (seen (make-hash-table :test 'equal)))   ; file id -> its row: one row a file
    (flet ((flush ()
             (when log
               (push (make-instance 'msg :id (incf *seq*) :role "operandi" :kind :think
                                    :text (format nil "~a~%~{~a~%~}" (%think-summary log) (reverse log)))
                     out)
               (setf log '()))
             ;; the pictures the calls made, after their row, as a live turn adds them
             (dolist (p (reverse pics))
               (destructuring-bind (kind x &optional note) p
                 (let ((id (if (eq kind :id) x (or (stored-id-of x sid) (and sid (probe-file x) (ignore-errors (store-file x sid)))))))
                   (cond ((null id) (push (path-file-msg x sid) out))
                         ((gethash id seen) (when note (let ((m (gethash id seen)) (text (file-row-text id sid note)))
                                                         (when text (setf (msg-text m) text)))))
                         (t (let ((m (file-msg id sid note))) (setf (gethash id seen) m) (push m out)))))))
             (setf pics '())))
      (dolist (m hist)
        (let ((role (gethash "role" m)) (content (gethash "content" m))
              (reason (or (gethash "reasoning_content" m) (gethash "reasoning" m))))
          (cond ((and (string= role "user") (tool-picture-paths content))
                 ;; after the row holding the calls that made them; with no such row pending (the
                 ;; reply that made the calls already closed it), right here
                 (dolist (p (tool-picture-paths content)) (pushnew (list :path p) pics :test #'equal))
                 (unless log (flush)))
                ((and (string= role "tool") (sent-file-id content))
                 (multiple-value-bind (id note) (sent-file-id content)
                   (push (list :id id note) pics)))
                ;; a message with uploads: a parts VECTOR -- not a string, which is a vector too, and
                ;; was taken for one, so every plain message vanished from a reopened chat
                ((and (string= role "user") (vectorp content) (not (stringp content)))
                 (flush) (setf calls 0)
                 (multiple-value-bind (text paths) (upload-parts content)
                   (dolist (p paths)
                     (let ((id (car (last (pathname-directory (pathname p))))))   ; the store's <id>/<name>
                       (push (if (stored-file id sid) (file-msg id sid nil t) (path-file-msg p sid)) out)))
                   (when (plusp (length (string-trim " " text)))
                     (push (make-instance 'msg :id (incf *seq*) :role "you" :text text :kind :you) out))))
                ((string= role "user")
                 (flush) (setf calls 0)
                 (when (and (stringp content) (plusp (length content))
                            (string/= content eng:+empty-turn-nudge+))     ; the engine's, not yours
                   (push (make-instance 'msg :id (incf *seq*) :role "you" :text content :kind :you) out)))
                ((string= role "assistant")
                 (when (and (stringp reason) (plusp (length (string-trim " " reason))))
                   (push (%clip-reason reason) log))
                 (map nil (lambda (tc)
                            (let* ((fn (and (hash-table-p tc) (gethash "function" tc)))
                                   (args (and fn (gethash "arguments" fn)))
                                   (args (if (stringp args) (ignore-errors (json:parse args)) args)))
                              (when fn
                                (push (format nil "~2,' d. ~a~@[ ~a~]" (incf calls) (gethash "name" fn)
                                              (%arg-detail args))
                                      log))))
                      (or (gethash "tool_calls" m) #()))
                 (when (and (stringp content) (plusp (length content)))
                   (flush)
                   (push (make-instance 'msg :id (incf *seq*) :role "operandi" :text content :kind :bot) out))))))
      (flush))
    (nreverse out)))

(defun new-session! ()
  "Start a fresh chat: a new session, an empty transcript, a greeting."
  (bt:with-lock-held (*lock*)
    (setf *session* (session:make-session) *uploads* '()
          *messages* (list (make-instance 'msg :id (incf *seq*) :role "operandi"
                                          :text *greeting* :kind :bot))))
  (gethash :id *session*))

(defun open-session! (id)
  "Resume a saved chat by id: load its history and rebuild the transcript. Returns
   the id, or NIL if it isn't there."
  (let ((s (session:make-session)))
    (when (session:resume-session! s id)
      (bt:with-lock-held (*lock*)
        (setf *session* s *uploads* '()
              *messages* (or (messages-from-history (session:session-history s))
                             (list (make-instance 'msg :id (incf *seq*) :role "operandi"
                                                  :text *greeting* :kind :bot)))))
      id)))

(defparameter *trash-root*
  (merge-pathnames ".operandi/chat-trash/" (user-homedir-pathname))
  "Where a deleted chat goes, whole (its session, its transcript, its files),
   in a directory of its id: deleting is moving, undone by moving back.")

(defun session-id-p (id)
  "Whether ID looks like a session's id (YYYYMMDD-HHMMSS...): nothing that
   reaches outside the sessions directory."
  (and (stringp id) (<= 15 (length id) 40)
       (every (lambda (c) (or (digit-char-p c) (char= c #\-))) id)))

(defun delete-session! (id)
  "Move chat ID (its .json, its .md and its files) to *TRASH-ROOT*/ID/.
   Returns :DELETED, :MISSING, or :BUSY (the open chat, mid-turn: its turn
   would write it back).  Deleting the open chat closes it."
  (unless (session-id-p id) (return-from delete-session! :missing))
  (let* ((dir session:*sessions-dir*)
         (json (merge-pathnames (format nil "~A.json" id) dir))
         (md (merge-pathnames (format nil "~A.md" id) dir))
         (files (merge-pathnames (format nil "~A/" id) *files-root*))
         (to (merge-pathnames (format nil "~A/" id) *trash-root*)))
    (unless (or (probe-file json) (probe-file md)) (return-from delete-session! :missing))
    (bt:with-lock-held (*lock*)
      (when (equal id (current-session-id))
        (when (plusp *pending*) (return-from delete-session! :busy))
        (setf *session* nil *messages* '())))
    (ensure-directories-exist to)
    (flet ((move (from to)            ; rename(2): the same filesystem, files or directories
             #+sbcl (multiple-value-bind (ok err) (sb-unix:unix-rename from to)
                      (unless ok (error "cannot move ~A to ~A: ~A" from to (sb-int:strerror err))))
             #-sbcl (rename-file from to)))
      (dolist (f (list json md))
        (when (probe-file f) (move (namestring f) (namestring (merge-pathnames (file-namestring f) to)))))
      (when (ignore-errors (probe-file files))       ; modus's UIOP has no DIRECTORY-EXISTS-P
        (move (string-right-trim "/" (namestring files)) (namestring (merge-pathnames "files" to)))))
    :deleted))

(defun sessions-json ()
  "The saved chats as JSON for the picker: newest first, each {id,title,turns}."
  (json:stringify
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
   the agent works, so the client sees progress).  What the worker has sent is applied first, so every
   read is current and nothing else has to pump."
  (pump)
  (bt:with-lock-held (*lock*)
    (copy-list *messages*)))
(defun row-type (o) (declare (ignore o)) 'msg)

(defvar *proj* (w:make-projection #'rows :type-fn #'row-type))
(defun chat-projection () *proj*)


;; Voice output is the host's to provide (the gateway routes it to the desktop's chord voice; a
;; local server could synthesize to the browser). Core just offers the hook + the toggle.
(defvar *speak-enabled* nil "when T, each finished bot reply is handed to *SPEAK-FN*.")
(defvar *speak-fn* nil "(lambda (text)) — voice TEXT, or NIL for no voice.")

;;; ------------------------------ /commands ----------------------------
;;; Typed into the same box as everything else, because on a phone that box is the only input this
;;; app has.  A line beginning with / is the GUI's own and never reaches the agent.

(defvar *backend* :openrouter
  "Where the chat's model runs when *CHAT-URL* names no local server: :OPENROUTER, :PPQ, :ROUTSTR,
   :TINFOIL or :MAPLE (CONFIGURE! sets it; :PPQ and :OPENROUTER read their key files when it has not).")
(defvar *backend-token* nil
  "The key CONFIGURE! was given (a phone's keychain), or NIL: the setters then read their key files.")
(defvar *backend-url* nil "Routstr's node, when CONFIGURE! named one.")

(defun %set-backend (backend slug)
  "Point the engine at SLUG on BACKEND, with the key and node CONFIGURE! was given.  T, or NIL when the
   setter refused (no key)."
  (let ((tok *backend-token*))
    (ecase backend
      (:openrouter (llm:set-backend-openrouter! :model slug :token tok))
      (:ppq (llm:set-backend-ppq! :model slug :token tok))
      (:routstr (apply #'llm:set-backend-routstr! :model slug :token tok
                       (and *backend-url* (list :url *backend-url*))))
      (:tinfoil (llm:set-backend-tinfoil! :model slug :token tok))
      (:maple (llm:set-backend-maple! :model slug :token tok)))))

(defun use-model (slug)
  "Point the engine at SLUG: on the local server when *CHAT-URL* is set (SLUG NIL asks it what it
   serves), else on *BACKEND*."
  (if *chat-url*
      (llm:set-backend-llama! :url *chat-url* :model slug)
      (%set-backend *backend* slug)))

;;; THE SETTINGS PANEL'S ENTRY.  Everything the agent's inference depends on, set at once and checked
;;; before it is kept: a choice that cannot work (no key; --confidential on a path that is not sealed, or
;;; whose operator TRUST does not name) leaves the agent as it was and says why.  It changes globals the
;;; worker reads, so it runs on the HOST and not while a turn is running.

(defparameter +config-vars+
  '(llm:*llm-backend* llm:*llm-url* llm:*llm-auth-token* llm:*llm-model* llm:*llm-read-timeout*
    llm:*sealed-post* llm:*confidential* llm:*trust* llm:*maple-mode* llm:*llm-effort*
    *chat-url* *backend* *backend-token* *backend-url* *model*)
  "What CONFIGURE! may change, and so puts back when the new choice is refused.")

(defun configure! (&key backend token model url confidential trust effort)
  "Point the agent at BACKEND -- :OPENROUTER :PPQ :ROUTSTR :TINFOIL :MAPLE, or :LOCAL (an OpenAI-compatible
   server at URL: illatio, llama.cpp) -- with TOKEN (its key), MODEL (NIL: the backend's default, or what a
   local server serves), URL (a local server's address; Routstr's node, NIL for the default),
   CONFIDENTIAL (every request sealed to an attested enclave, else refused), TRUST (the --trust names, a
   list of strings) and EFFORT (NIL, :OFF, :LOW, :MEDIUM, :HIGH).
   Returns (values T label) when it took, else (values NIL reason) with nothing changed."
  (when (busy-p)
    (return-from configure! (values nil "A turn is running; change the settings when it has finished.")))
  (let ((saved (mapcar #'symbol-value +config-vars+)))
    (flet ((refuse (why)
             (mapc #'set +config-vars+ saved)
             (return-from configure! (values nil why))))
      (handler-case
          (progn
            (setf llm:*confidential* nil
                  llm:*trust* (copy-list trust)
                  llm:*llm-effort* effort
                  *backend-token* (and token (plusp (length token)) token))
            (cond ((eq backend :local)
                   (unless (and url (plusp (length url))) (refuse "A local server needs its address."))
                   (setf *chat-url* (llm:llama-chat-url url))
                   (llm:set-backend-llama! :url *chat-url* :model (and model (plusp (length model)) model)))
                  (t
                   (setf *chat-url* nil *backend* backend
                         *backend-url* (and (eq backend :routstr) url (plusp (length url)) url))
                   (unless *backend-token* (refuse "This backend needs an API key."))
                   (unless (if (and model (plusp (length model)))
                               (%set-backend backend model)
                               ;; the setter's own default model
                               (let ((tok *backend-token*))
                                 (ecase backend
                                   (:openrouter (llm:set-backend-openrouter! :token tok))
                                   (:ppq (llm:set-backend-ppq! :token tok :confidential confidential))
                                   (:routstr (apply #'llm:set-backend-routstr! :token tok :confidential confidential
                                                    (and *backend-url* (list :url *backend-url*))))
                                   (:tinfoil (llm:set-backend-tinfoil! :token tok))
                                   (:maple (llm:set-backend-maple! :token tok)))))
                     (refuse "The backend refused those settings."))))
            (when confidential
              (llm:enable-confidential!)
              (let ((why (llm:confidential-refusal)))
                (when why (refuse (format nil "Confidential: ~a." why)))))
            (setf *model* llm:*llm-model*)
            (values t (format nil "~a on ~a~:[~; (confidential)~]"
                              (or llm:*llm-model* "the server's model")
                              (case backend
                                (:local (url-label *chat-url*)) (:openrouter "OpenRouter") (:ppq "PPQ") (:routstr "Routstr")
                                (:tinfoil "Tinfoil") (:maple "Maple") (t backend))
                              llm:*confidential*)))
        (error (e) (refuse (princ-to-string e)))))))

(defun url-label (url)
  "A server's address as a person reads it: no scheme, no /v1/chat/completions."
  (let* ((u (or url ""))
         (u (let ((p (search "://" u))) (if p (subseq u (+ p 3)) u)))
         (p (search "/v1/chat/completions" u)))
    (if p (subseq u 0 p) (string-right-trim "/" u))))

(defvar *preflight-fn* nil
  "(lambda (ok reason)), called on the host when a PREFLIGHT! the worker ran has an answer.")

(defun preflight! ()
  "Ask the worker to send the current backend a one-token ping (ENG:PREFLIGHT-MODEL): on the worker, so
   the window that asked does not stop while it waits.  *PREFLIGHT-FN* gets the answer.  NIL when there is
   no worker or a turn is running."
  (when (and *inbox* (not (busy-p)))
    (mb-send *inbox* (list :preflight))
    t))

(defun set-model (slug)
  "Switch the agent to SLUG — but PROVE it first.  ENG:PREFLIGHT-MODEL sends a one-token ping and
   reads the provider's own reason, so a typo, a retired slug, or a provider-allowlist miss is
   caught here rather than turning every later turn into a blank reply.  A switch that does not
   preflight is rolled back: better the model you had than one that cannot answer."
  (let ((old *model*))
    (use-model slug)
    (multiple-value-bind (ok reason) (eng:preflight-model)
      (cond (ok (setf *model* slug)
                (format nil "Model is now ~a." slug))
            (t (use-model old)
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
          ((null *inbox*)
           (add-msg "operandi" "The agent isn't running." :err))
          ;; A turn is in flight: STEER it rather than queue behind it.  Words that wait for
          ;; the turn to end arrive after the turn you wanted to change is over -- which is
          ;; what made "continue" the only usable thing to say.  The worker hands them to the
          ;; engine between iterations (ENG:*STEER-FN*) and splits its live row there, so the
          ;; calls the steer caused sit below it (WORKER-STEERS).
          ((plusp *pending*)
           (mb-send *inbox* (list :steer s (current-session-id))))
          (t
           (incf *pending*)
           (mb-send *inbox* (list :turn s (bt:with-lock-held (*lock*) (prog1 *uploads* (setf *uploads* '())))
                                  (current-session-id)))))))))

;;; --------------------------- live status ------------------------------
;;; A turn can run for minutes across dozens of tool calls, and the panel said
;;; "...thinking..." for all of it -- indistinguishable from a hang, which is the
;;; thing a chat UI most needs to not look like.  The row is already updated in
;;; place at the end of the turn (same id -> a :changed delta), so saying more
;;; costs nothing new: rewrite the SAME row as the work happens.
;;;
;;; ALL OF THIS RUNS IN THE WORKER (its variables are bound there, WORKER-LOOP): the row is the
;;; host's, so the worker names it by a token of its own and sends its text (%EMIT).

(defvar *think-head* "" "The one-line status: what the turn is doing RIGHT NOW.")
(defvar *tool-log* '() "What it has already done this leg, newest first.")
(defparameter *tool-log-keep* 40
  "How many calls the detail list keeps.  The row's whole text is resent on every
   update, so an unbounded log would put a growing payload on the data channel
   once per tool call — and forty is already more than anyone reads on a phone.")

(defvar *think-reason* (make-string-output-stream)
  "The reasoning streaming in now, not yet in the log: it goes in as one entry when a call starts
   or the turn ends, so the detail reads in order -- what it thought, what it did, what it thought.")
(defvar *think-reason-n* 0 "Characters of it so far.")
(defparameter *reason-clip* 1500 "How much of one stretch of reasoning a row keeps.")

(defun %clip-reason (text)
  "One stretch of reasoning as one log entry.  Entries are lines, so its own line breaks travel as
   U+2028 -- the page turns those back into breaks, and a numbered list inside a thought can't be
   mistaken for the calls around it."
  (let* ((r (string-trim '(#\Space #\Newline #\Tab #\Return) (remove #\Return text)))
         (r (if (> (length r) *reason-clip*) (concatenate 'string (subseq r 0 *reason-clip*) "…") r)))
    (format nil "✻ ~a" (substitute (code-char #x2028) #\Newline r))))

(defun %reason-flush ()
  "Close the reasoning streaming in now into one log entry."
  (when (plusp *think-reason-n*)
    (push (%clip-reason (get-output-stream-string *think-reason*)) *tool-log*)
    (setf *think-reason-n* 0)))

(defun %think-summary (log)
  "The status a finished row keeps: its calls counted, or that it thought."
  (let ((calls (count-if-not (lambda (e) (eql 0 (search "✻" e))) log)))
    (if (plusp calls) (format nil "⚙ ~d tool call~:p" calls) "✻ thought")))

(defun %think-repaint ()
  "Compose the row: status line first, detail below.  FIRST is deliberate — the
   collapsed row shows only its first line, so the live status has to be that
   line, and the detail is what tapping reveals."
  (let ((k *think-k*))
    (when k
      (let* ((live (and (plusp *think-reason-n*)
                        (%clip-reason (let ((r (get-output-stream-string *think-reason*)))
                                        (write-string r *think-reason*)   ; read without losing it
                                        r))))
             (log (append (reverse (subseq *tool-log* 0 (min *tool-log-keep* (length *tool-log*))))
                          (and live (list live))))
             (text (if log (format nil "~a~%~{~a~%~}" *think-head* log) *think-head*)))
        (%emit :think-text k text)))))

(defun %think! (fmt &rest args)
  "Set the status line.  A no-op when no turn is running, so a stray hook from a
   finished run cannot resurrect a row or clobber a reply."
  (setf *think-head* (apply #'format nil fmt args))
  (%think-repaint))

(defun %tool-log! (fmt &rest args)
  "Add one finished call to the detail list."
  (push (apply #'format nil fmt args) *tool-log*)
  (%think-repaint))

(defun %think-close! ()
  "Leave the live row as what its leg did: a summary over its calls and reasoning, or nothing at
   all when it did neither (a bare \"…thinking…\" is no record of anything)."
  (%reason-flush)
  (when *think-k*
    (if *tool-log*
        (%think! "~a" (%think-summary *tool-log*))
        (progn (%emit :think-drop *think-k*) (setf *think-k* nil)))))

(defun %think-open! (head)
  "Start a FRESH rollup row with HEAD as its status, leaving the previous row
   frozen where it is.  Used when a steering message arrives: the calls before it
   and the calls after it are different answers to different instructions, and
   rolling them into one line puts the work above the words that caused it."
  (%think-close!)
  (setf *tool-log* '() *think-head* head *think-reason-n* 0)
  (setf *think-k* (incf *think-n*))
  (%emit :think-open *think-k* head))

(defun %arg-detail (args)
  "Like %ARG-BRIEF but roomier — the detail list is read deliberately, so it can
   afford the whole path or most of a command."
  (let ((v (and (hash-table-p args)
                (or (gethash "path" args) (gethash "file_path" args)
                    (gethash "pattern" args) (gethash "command" args)
                    (gethash "query" args) (gethash "url" args)))))
    (when (stringp v)
      (let ((one (substitute #\Space #\Newline v)))
        (if (> (length one) 120) (concatenate 'string (subseq one 0 120) "...") one)))))

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

(defun answer (text uploads)
  "In the worker: run one engine turn threaded onto its session's history, and persist it."
  (let ((base (and *wsession* (session:session-history *wsession*)))
        ;; the files uploaded since the last message go with this one
        (content (user-content text uploads))
        (calls 0)
        (toks 0)
        (shown '()))                     ; pictures already given a row this turn
    (multiple-value-bind (reply hist)
        (let* ((hooks:*pre-tool-hooks*
                 (cons (lambda (name args)
                         (%reason-flush)
                         (incf calls)
                         (let ((b (%arg-brief args)))
                           (%think! "⚙ ~a~@[ ~a~]~@[ · ~a~]" name b
                                    (and (plusp calls)
                                         (format nil "~d tool call~:p" calls)))))
                       hooks:*pre-tool-hooks*))
               ;; The detail line is written on POST, not PRE, because that is when the
               ;; outcome exists — how long it took and whether it worked is most of what
               ;; makes the list worth opening.
               (hooks:*post-tool-hooks*
                 (cons (lambda (name args result error ms)
                         (%tool-log! "~2,' d. ~a~@[ ~a~] ~a"
                                     calls name (%arg-detail args)
                                     (cond (error (format nil "— FAILED: ~a" error))
                                           ((stringp result)
                                            (format nil "— ~:d chars~@[, ~a ms~]"
                                                    (length result) ms))
                                           (t (format nil "— done~@[ in ~a ms~]" ms))))
                         (when error (%think! "⚠ ~a failed — recovering…" name))
                         ;; a picture it made or opened: a row now, not when the turn ends
                         (when (consp tools:*tool-images*)
                           (dolist (p (reverse tools:*tool-images*))
                             (unless (member p shown :test #'string=)
                               (push p shown)
                               (let ((id (ignore-errors (store-file p))))
                                 (if id
                                     (add-file-row id (current-session-id))
                                     (%emit :path p (current-session-id))))))))
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
                   (let ((pending (worker-steers)))
                     (when pending
                       ;; SPLIT THE ROLLUP: the calls before the steer and the calls it causes are
                       ;; answers to different instructions -- a fresh row, below the steer's
                       (%think-open! "↪ steering…")
                       (format nil "~{~A~^~%~%~}" pending)))))
               (eng:*on-reasoning*
                 (lambda (chunk)
                   (let ((before *think-reason-n*))
                     (write-string chunk *think-reason*)
                     (incf *think-reason-n* (length chunk))
                     ;; redraw every ~200 characters: it moves, which says running, not wedged
                     (when (/= (floor before 200) (floor *think-reason-n* 200))
                       (%think-repaint)))))
               (eng:*on-token*
                 (lambda (chunk)
                   (incf toks (length chunk))
                   (when (> toks 800)
                     (setf toks 0)
                     (%think! "…thinking~@[ · ~a so far~]"
                              (and (plusp calls)
                                   (format nil "~d tool call~:p" calls)))))))
          (eng:run content
                 ;; :history NIL on the first turn lets the engine seat the system
                 ;; prompt; thereafter we thread the accumulated history + new message.
                 :history (when base
                            (append base (list (llm:ht "role" "user" "content" content))))
                 :system *system-prompt*
                 :tool-names (append (or *tool-names* (tools:default-tools)) (list "SendFile"))
                 :verbose nil))
      (when *wsession*
        (setf (gethash :history *wsession*) hist)
        (incf (gethash :turns *wsession*))
        (ignore-errors (session:persist-session *wsession*)))     ; crash never loses a turn
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

;;; ------------------------------ the worker ----------------------------
(defun worker-session (sid)
  "The worker's session for chat SID: the one it has, else read from disk, else a fresh one under that
   id (a chat the host opened and nothing has saved yet)."
  (unless (and *wsession* (equal (gethash :id *wsession*) sid))
    (setf *wsession* (let ((s (session:make-session)))
                       (unless (and sid (session:resume-session! s sid))
                         (when sid (setf (gethash :id s) sid)))
                       s)))
  *wsession*)

(defun worker-steers ()
  "Drain the inbox between engine iterations: the steers' texts, oldest first.  A turn read here waits
   in *LATER*; a stop is remembered."
  (let ((out '()))
    (loop (multiple-value-bind (m ok) (mb-try *inbox*)
            (unless ok (return))
            (case (first m)
              (:steer (push (second m) out))
              (:turn (setf *later* (append *later* (list m))))
              (:stop (setf *stopping* t)))))
    (nreverse out)))

(defun worker-turn (text uploads sid)
  (worker-session sid)
  (setf *tool-log* '() *think-head* "…thinking…" *think-reason-n* 0)
  (get-output-stream-string *think-reason*)
  (setf *think-k* (incf *think-n*))
  (%emit :think-open *think-k* "…thinking…")
  (multiple-value-bind (reply kind)
      (handler-case (values (answer text uploads) :bot)
        (serious-condition (e) (values (format nil "Something went wrong: ~A" e) :err)))
    ;; A turn that used tools keeps its row, the status now a summary and the calls still
    ;; under it, and the reply is a row of its own; a turn that did not is just its reply,
    ;; written over the thinking row (same id -> a :changed delta).
    (%reason-flush)
    (if *tool-log*
        (progn (%think-close!) (%emit :reply nil reply kind))
        (%emit :reply *think-k* reply kind))
    (setf *think-k* nil)
    ;; A steer written in the gap between the engine's last drain and the turn ending has
    ;; nowhere to land -- it must not evaporate.  It starts the next turn instead (counted
    ;; before this one is finished, so the host never sees the agent idle in between).
    (let ((late (worker-steers)))
      (when late
        (%emit :extra-turn)
        (setf *later* (append *later* (list (list :turn (format nil "~{~A~^~%~%~}" late) nil sid))))))
    (%emit :turn-done)))

(defun worker-loop (inbox outbox)
  "The worker thread: turns from INBOX, one at a time; what the chat should show to OUTBOX.  Every
   variable it writes is bound here, so it is this thread's own."
  (let ((*inbox* inbox) (*worker-out* outbox) (*wsession* nil)
        (*think-k* nil) (*think-n* 0) (*later* '()) (*stopping* nil) (*host-call-n* 0)
        (*think-head* "") (*tool-log* '()) (*think-reason* (make-string-output-stream)) (*think-reason-n* 0))
    (loop until *stopping* do
      (let ((m (or (pop *later*) (mb-receive inbox))))
        (case (first m)
          (:stop (setf *stopping* t))
          (:preflight (multiple-value-bind (ok reason) (eng:preflight-model)
                        (%emit :preflight (and ok t) (and reason (princ-to-string reason)))))
          ;; a steer that finds no turn running is a turn
          ((:turn :steer)
           (destructuring-bind (text &optional uploads sid)
               (if (eq (first m) :steer) (list (second m) nil (third m)) (rest m))
             (when (eq (first m) :steer) (%emit :extra-turn))
             (handler-case (worker-turn text uploads sid)
               (serious-condition (e)
                 (ignore-errors (%emit :reply nil (format nil "Something went wrong: ~A" e) :err))
                 (ignore-errors (%emit :turn-done)))))))))))

;;; ------------------------------ the host ------------------------------
(defun apply-event (ev)
  "On the host: one thing the worker said the chat should show."
  (destructuring-bind (kind &rest args) ev
    (ecase kind
      (:preflight (when *preflight-fn* (apply *preflight-fn* args)))
      (:think-open (destructuring-bind (k text) args
                     (setf *think* (add-msg "operandi" text :think)
                           (gethash k *row-of*) *think*)))
      (:think-text (destructuring-bind (k text) args
                     (let ((m (gethash k *row-of*)))
                       (when m (bt:with-lock-held (*lock*) (setf (msg-text m) text))))))
      (:think-drop (let ((m (gethash (first args) *row-of*)))
                     (when m
                       (remhash (first args) *row-of*)
                       (when (eq m *think*) (setf *think* nil))
                       (bt:with-lock-held (*lock*) (setf *messages* (remove m *messages*))))))
      (:reply (destructuring-bind (k reply kind) args
                (let* ((m (and k (gethash k *row-of*)))
                       (mid (if m
                                (progn (bt:with-lock-held (*lock*)
                                         (setf (msg-text m) reply (slot-value m 'kind) kind))
                                       (msg-id m))
                                (msg-id (add-msg "operandi" reply kind)))))
                  (setf *think* nil)
                  ;; voice a real answer (not an error) if the operator turned it on.  We hand the RAW
                  ;; reply + its row id; the host splits into sentences, cleans each for the voice, and
                  ;; drives the highlight (payload.js re-splits the SAME raw text to find the spans).
                  (when (and (eq kind :bot) *speak-enabled* *speak-fn*)
                    (ignore-errors (funcall *speak-fn* reply mid))))))
      (:file (destructuring-bind (id sid note) args (add-file-row id sid note)))
      (:path (destructuring-bind (path sid) args (add-row (path-file-msg path sid))))
      (:tool (destructuring-bind (id name args) args
               (let ((impl (gethash name *host-tool-impls*)))
                 (mb-send *replies*
                          (list id (if impl
                                       (handler-case (funcall impl args)
                                         (serious-condition (e) (format nil "TOOL ERROR: ~a" e)))
                                       (format nil "TOOL ERROR: ~a is not a host tool" name)))))))
      (:extra-turn (incf *pending*))
      (:turn-done (setf *pending* (max 0 (1- *pending*)) *think* nil)
                  (clrhash *row-of*)))))

(defun host-tool-call (name args)
  "In the worker: have the host run tool NAME on ARGS, and wait for its answer."
  (let ((id (incf *host-call-n*)))
    (%emit :tool id name args)
    (loop (destructuring-bind (rid result) (mb-receive *replies*)
            (when (eql rid id) (return result))))))

(defun install-host-tools ()
  "On the host, once: each of *HOST-TOOLS* keeps its implementation here and is replaced, for the
   engine, by a call to the host."
  (let ((tools (symbol-value (find-symbol "*TOOLS*" "OPERANDI.TOOLS"))))
    (dolist (name *host-tools*)
      (let ((tool (gethash name tools)))
        (when (and tool (not (gethash name *host-tool-impls*)))
          (setf (gethash name *host-tool-impls*) (getf tool :impl))
          (setf (gethash name tools)
                (list* :impl (let ((n name)) (lambda (args) (host-tool-call n args)))
                       (loop for (k v) on tool by #'cddr unless (eq k :impl) nconc (list k v)))))))))

(defun pump ()
  "Apply everything the worker has sent.  ROWS calls it, so a host that reads the chat pumps it."
  (when *outbox*
    (bt:with-lock-held (*pump-lock*)
      (loop (multiple-value-bind (ev ok) (mb-try *outbox*)
              (unless ok (return))
              (handler-case (apply-event ev)
                (serious-condition (e) (format *error-output* "~&operandi-gui: event ~s: ~a~%" (first ev) e))))))))

;;; ------------------------------ lifecycle -----------------------------
(defun start-agent (&key (model *model*))
  "Configure the LLM backend and start the single worker thread (idempotent)."
  ;; The environment of the RUN, not of the build: *CHAT-URL* and *MODEL* were read when this file was
  ;; loaded, which for a saved image (the phone's) was on another machine.
  (let ((u (uiop:getenv "OPERANDI_CHAT_URL")) (m (uiop:getenv "OPERANDI_CHAT_MODEL")))
    (when (and u (plusp (length u))) (setf *chat-url* u))
    (when (and m (plusp (length m)) (equal model *model*)) (setf model m *model* m)))
  (when (or model *chat-url*)
    (use-model model)
    (setf *model* llm:*llm-model*))
  (ensure-directories-exist session:*sessions-dir*)
  (unless (and *worker* (bt:thread-alive-p *worker*))
    (install-host-tools)
    (let ((in (make-mailbox "operandi-gui in")) (out (make-mailbox "operandi-gui out"))
          (replies (make-mailbox "operandi-gui replies")))
      (setf *inbox* in *outbox* out *replies* replies *pending* 0 *running* t
            *worker* (bt:make-thread (lambda () (worker-loop in out)) :name "chat-agent"))))
  t)

(defun stop-agent ()
  (setf *running* nil)
  (when *inbox* (mb-send *inbox* (list :stop))))

;;;; desk.lisp — the desktop, as functions the agent can just call.
;;;;
;;;; ============================================================================================
;;;; WHY THIS IS NOT A SET OF TOOLS
;;;; ============================================================================================
;;;;
;;;; The agent already has EVAL, and Eval runs in a live image where every loaded package is
;;;; callable.  So a new capability does not need a new tool: it needs a FUNCTION with a good name
;;;; and a docstring.  A tool costs a JSON schema, a slot in every request's tool array, and a
;;;; description the model reads whether or not it is relevant; a function costs one DEFUN and is
;;;; composable with everything else in the image -- the agent can map over it, bind it, wrap it,
;;;; or redefine it, none of which a tool-call can do.
;;;;
;;;; The one thing a tool has that a function does not is that the model is TOLD it exists.  That
;;;; is the only real problem to solve here, and it is solved by HELP plus one line in the agent's
;;;; notes -- which are already read into every system prompt.  So: functions for capability,
;;;; notes for discovery, and the tool array stays the size it was.
;;;;
;;;; ============================================================================================
;;;; WHAT BELONGS IN HERE
;;;; ============================================================================================
;;;;
;;;; Things the agent might reasonably want to DO to the machine it is living in, named the way it
;;;; would ask for them.  Not a wrapper for everything glass exports -- that is what the packages
;;;; themselves are for, and Eval reaches them directly.  This is the short list worth knowing
;;;; without being told, which is why HELP prints it rather than making anyone read a file.
;;;;
;;;; EVERY FUNCTION GOES THROUGH THE HOST'S VOICE LINK, not through glass directly, because the
;;;; host may be a gateway in another process.  The link already answers "I cannot do that from
;;;; here" as NIL, and each function below turns that into a sentence rather than a blank.

(defpackage #:operandi-gui.desk
  (:use #:cl)
  (:nicknames #:desk)
  (:export #:help #:voices #:voice #:set-voice #:say #:hush #:speaking-p
           #:say-and-wait #:say-in #:demo-voices #:listen-for))

(in-package #:operandi-gui.desk)

(defun %ask (op &rest args)
  "One op on the host's voice link.  NIL when the host cannot answer."
  (let ((f (find-symbol "VOICE" "OPERANDI-GUI.GATEWAY")))
    (and f (fboundp f) (apply f op args))))

(defun voices ()
  "Every text-to-speech voice this desktop can speak with, as a list of names.
The current one is first.  See SET-VOICE to change it."
  (mapcar #'car (%ask :voices)))

(defun voice ()
  "The name of the voice this desktop is speaking with right now, or NIL if it cannot be read."
  (car (first (%ask :voices))))

(defun set-voice (name)
  "Speak with NAME from the next utterance on.  NAME is one of (VOICES).

Anything already being said finishes in the voice it started in -- a sentence does not change
voice half way through.  Returns the name on success, NIL if there is no such voice."
  (let ((got (%ask :set-voice (string name))))
    (and got (pathname-name (pathname got)))))

(defun say (text)
  "Say TEXT out loud on the desktop, now.  Returns T if the voice took it."
  (and (%ask :speak (string text)) t))

(defun hush ()
  "Stop talking immediately and drop whatever was queued."
  (%ask :hush) t)

(defun speaking-p ()
  "True while there is still something being said."
  (and (%ask :speaking-p) t))

;;; ---- functions that finish the job ------------------------------------------------------------
;;;
;;; A FUNCTION THE AGENT CALLS SHOULD DO A WHOLE JOB, because every call is an ITERATION of the
;;; agent loop and the loop has a budget.  SAY plus a poll on SPEAKING-P is the natural way to
;;; write "say this and wait", and it is the expensive way: five voices demonstrated that way spent
;;; eighty iterations and ended in `[max-iterations exceeded]` with nothing to show.  The same work
;;; through SAY-AND-WAIT is five calls; through DEMO-VOICES it is one.
;;;
;;; THE CEILING IS THE EVAL TOOL'S TIMEOUT, sixty seconds, and it is a hard kill: a form still
;;; running when it expires returns nothing at all, so a function that blocks has to finish inside
;;; it or stop early ON PURPOSE.  Every wait below is bounded and every one of them REPORTS what it
;;; got rather than pretending: :TIMEOUT is an answer, a killed Eval is not.

(defparameter *wait-slice* 0.1
  "How often a wait checks.  Short enough to feel immediate, long enough not to spin.")

(defparameter *say-timeout* 40
  "Seconds SAY-AND-WAIT will wait.  Comfortably inside the Eval tool's sixty, because the
answer has to get back through it.")

(defun %wait-quiet (timeout)
  "Block until nothing is being said, or TIMEOUT.  T if it went quiet, :TIMEOUT if it did not."
  (let ((deadline (+ (get-internal-real-time)
                     (* timeout internal-time-units-per-second))))
    ;; Wait for it to START first, briefly: SAY queues and a synthesiser takes a moment, so
    ;; checking `is it quiet' immediately would answer yes about the silence BEFORE the sentence.
    ;; That is the bug this function exists to stop anyone writing again.
    (loop repeat (round 3 *wait-slice*)
          until (speaking-p) do (sleep *wait-slice*))
    (loop while (speaking-p)
          do (when (> (get-internal-real-time) deadline) (return-from %wait-quiet :timeout))
             (sleep *wait-slice*))
    t))

(defun say-and-wait (text &key (timeout *say-timeout*))
  "Say TEXT and return when it has FINISHED being said.

This is the one to reach for.  SAY returns the moment the words are queued, so anything that needs
to know the sentence is over -- saying a second one, changing voice, asking a question -- has to
wait, and waiting by polling from the agent loop costs an iteration per poll.  Here it costs none.

Returns T when it finished, :TIMEOUT if it was still going after TIMEOUT seconds (the voice is left
alone; call HUSH if you want it stopped)."
  (if (say text) (%wait-quiet timeout) nil))

(defun say-in (voice text &key (timeout *say-timeout*) (restore t))
  "Say TEXT in VOICE, wait for it to finish, and put the previous voice back.

RESTORE NIL leaves VOICE selected.  Returns T, :TIMEOUT, or NIL if there is no such voice."
  (let ((was (and restore (desk:voice))))
    (if (not (set-voice voice))
        nil
        (unwind-protect (say-and-wait text :timeout timeout)
          (when (and was (string/= was voice)) (set-voice was))))))

(defun demo-voices (&key names (text "This is how I sound.") (budget 50))
  "Say TEXT once in each of NAMES, in one call, and put the original voice back.

NAMES defaults to every voice on the box.  Returns a list of (NAME . RESULT) so a voice that did
not play is visible rather than merely absent.

BUDGET is the whole call's second-budget, and it stops early rather than being killed: the Eval
tool kills a form at sixty seconds and returns NOTHING, so an honest partial list beats a complete
one nobody receives.  Voices not reached come back as :SKIPPED."
  (let* ((all (or names (voices)))
         (was (desk:voice))
         (deadline (+ (get-internal-real-time) (* budget internal-time-units-per-second)))
         (out '()))
    (unwind-protect
         (dolist (n all (nreverse out))
           (push (cons n (if (> (get-internal-real-time) deadline)
                             :skipped
                             (or (say-in n text :restore nil) :no-such-voice)))
                 out))
      (when was (set-voice was)))))

(defun listen-for (seconds)
  "Transcribe the phone's microphone for SECONDS, then stop and return what was heard.

The dictation equivalent of SAY-AND-WAIT, and it exists for the same reason: start / poll / stop is
three or more iterations and this is one.  SECONDS is clamped to something that fits inside the
Eval tool's timeout."
  (let ((secs (max 1 (min 40 seconds))))
    (%ask :hearing-clear)
    (%ask :hearing-start)
    (unwind-protect (sleep secs)
      (%ask :hearing-stop))
    (or (%ask :hearing-text) "")))

(defun help ()
  "Print every function in this package with what it does.

This exists because a function the agent does not know about is a function it will not call.  It
is the cheap half of what a tool definition buys, without the cost of the other half."
  (format t "~&desk: the desktop, as functions.  Call them with Eval, e.g. (desk:voices).~%~%")
  (let ((names '()))
    (do-external-symbols (s (find-package "OPERANDI-GUI.DESK"))
      (when (fboundp s) (push s names)))
    (dolist (s (sort names #'string< :key #'symbol-name))
      (let ((doc (documentation s 'function)))
        (format t "~&(desk:~(~a~)~{ ~(~a~)~})~%~@[    ~a~%~]~%"
                (symbol-name s)
                (mapcar #'symbol-name
                        (remove-if (lambda (x) (member x lambda-list-keywords))
                                   (sb-introspect:function-lambda-list s)))
                (and doc (substitute #\Space #\Newline doc))))))
  (values))

;;;; bin/standalone.lisp — run the local HTTP chat server (127.0.0.1:8790).
;;;;   sbcl --non-interactive --load ~/operandi-gui/bin/standalone.lisp
;;;; OPERANDI_CHAT_PORT / OPERANDI_CHAT_MODEL override the defaults.
(require :asdf)
(let ((q (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file q) (load q)))
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (asdf:load-system :operandi-gui/serve)))
(funcall (find-symbol "START" :operandi-gui))
(handler-case (loop (sleep 3600))
  (#+sbcl sb-sys:interactive-interrupt #-sbcl error ()
    (funcall (find-symbol "STOP" :operandi-gui))))

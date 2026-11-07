;;;; bin/standalone.lisp — run the local HTTP chat server (127.0.0.1:8790).
;;;;   bin/operandi-gui [flags]      (bin/operandi-gui --help lists them), or
;;;;   sbcl --non-interactive --load ~/operandi-gui/bin/standalone.lisp [--end-toplevel-options flags]
(require :asdf)
(let ((q (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file q) (load q)))
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (asdf:load-system :operandi-gui/serve)))
(funcall (find-symbol "MAIN" :operandi-gui)
         (rest sb-ext:*posix-argv*))   ; SBCL keeps only "sbcl" and what follows --end-toplevel-options
(handler-case (loop (sleep 3600))
  (#+sbcl sb-sys:interactive-interrupt #-sbcl error ()
    (funcall (find-symbol "STOP" :operandi-gui))))

(load "~/quicklisp/setup.lisp")
(ql:quickload "hunchentoot")
(ql:quickload "cl-who")

(load "server.lisp")


(defun build-lisp-image (dump-name)
  #+:CLISP (ext:saveinitmem dump-name :init-function (lambda () (my-server:start-server) (ext:quit)) :executable t)
  #+:SBCL (sb-ext:save-lisp-and-die dump-name :toplevel (lambda () (my-server:start-server) (sb-ext:exit)) :executable t)
  #+:CMU (ext:save-lisp dump-name :init-function (lambda () (my-server:start-server) (ext:quit)) :executable t)
  #+:CCL (ccl:save-application dump-name :toplevel-function (lambda () (my-server:start-server) (ccl:quit))
                               :prepend-kernel t)
  #+:ECL (c:build-program dump-name :epilogue-code '(my-server:start-server)))


(build-lisp-image "my-server-test")
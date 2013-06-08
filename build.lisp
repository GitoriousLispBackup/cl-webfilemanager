(load "~/quicklisp/setup.lisp")
(ql:quickload "hunchentoot")
(ql:quickload "cl-who")
(ql:quickload "parenscript")

(load "cl-webfilemanager.asd")
(require :cl-webfilemanager)




(defun build-lisp-image (dump-name)
  #+:CLISP (ext:saveinitmem dump-name :init-function
                            (lambda ()
                              (cl-webfilemanager:start-server-loop)
                              (ext:quit))
                            :executable t)
  #+:SBCL (sb-ext:save-lisp-and-die dump-name :toplevel
                                    (lambda ()
                                      (cl-webfilemanager:start-server-loop)
                                      (sb-ext:exit))
                                    :executable t)
  #+:CMU (ext:save-lisp dump-name :init-function
                        (lambda ()
                          (cl-webfilemanager:start-server-loop)
                          (ext:quit))
                        :executable t)
  #+:CCL (ccl:save-application dump-name :toplevel-function
                               (lambda ()
                                 (cl-webfilemanager:start-server-loop)
                                 (ccl:quit))
                               :prepend-kernel t)
  #+:ECL (c:build-program dump-name :epilogue-code '(cl-webfilemanager:start-server-loop)))


(build-lisp-image "cl-webfilemanager")

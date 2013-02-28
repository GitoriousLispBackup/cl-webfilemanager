(in-package :cl-webfilemanager)



(defmacro define-action (name lists args &body body)
  `(progn
     ,(when (member :admin lists)
            `(push ',name *action-list*))
     ,(when (member :guest lists)
            `(push ',name *action-guest-list*))
     (defun ,name ,(append '(param) args)
       (declare (ignorable param))
       ,@body)))



(define-action cd (:admin :guest) (pathname)
  (setf (nth (param-current-tab param) (param-tab-list param)) pathname))

(define-action clean-auth (:admin) ()
  (setf *auth-admin* (list (param-identified param)) *auth-guest* nil))

(define-action deconnexion (:admin :guest) ()
  (remove-in-auth-admin (param-identified param))
  (setf (param-identified param) "false"))

(define-action options (:admin :guest) (pathname)
  (setf (param-additional-html param)
        (with-html-output-to-string (*standard-output*)
          (:button (str (format nil "plop ~A plop" pathname))))))


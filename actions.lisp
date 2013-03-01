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


(defmacro current-tab-pathname (param)
  `(nth (param-current-tab ,param) (param-tab-list ,param)))


(defun ask-text (param message confirm-message action)
  (setf (param-additional-html param)
        (with-html-output-to-string (*standard-output*)
          (:p (:form
               :action "/"
               :method :post
               (:input :type :hidden :name "identified" :value (param-identified param))
               (:input :type :hidden :name "tab-list" :value (to-string (param-tab-list param)))
               (:input :type :hidden :name "current-tab" :value (param-current-tab param))
               (:p message
                   (:input :type :text :name "data")
                   (:button :name "action" :value (str (to-string action)) (str confirm-message))
                   (:input :type :submit :value "Cancel")))))))


(define-action cd (:admin :guest) (pathname)
  (setf (current-tab-pathname param) pathname))

(define-action clean-auth (:admin) ()
  (setf *auth-admin* (list (param-identified param)) *auth-guest* nil))

(define-action deconnexion (:admin :guest) ()
  (remove-in-auth-admin (param-identified param))
  (setf (param-identified param) "false"))

(define-action options (:admin :guest) (pathname)
  (setf (param-additional-html param)
        (with-html-output-to-string (*standard-output*)
          (:button (str (format nil "plop ~A plop" pathname))))))

(define-action ask-make-dir (:admin) ()
  (ask-text param "Create a new directory: " "Create" '(do-make-dir)))

(define-action do-make-dir (:admin) ()
  (let ((dirname (pathname-as-directory
                  (format nil "~A~A" (current-tab-pathname param) (param-data param)))))
    (do-shell-output "mkdir -p ~S" (namestring dirname))
    (setf (param-additional-html param)
          (with-html-output-to-string (*standard-output*)
            (:p "Creating : " (str dirname))))))



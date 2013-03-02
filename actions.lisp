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
          (:p (:p message
                  (:input :type :text :name "data")
                  (:button :name "action" :value (str (to-string action)) (str confirm-message))
                  (:input :type :submit :value "Cancel")))
          (:hr))))

(defun ask-yes-or-no (param message action)
  (setf (param-additional-html param)
        (with-html-output-to-string (*standard-output*)
          (:p (:p (str message)
                  (:button :name "action" :value (str (to-string action)) "Yes")
                  (:input :type :submit :value "No")))
          (:hr))))


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
            (:p "Creating: " (str dirname))
            (:hr)))))



(define-action ask-delete-selected (:admin) ()
  (ask-yes-or-no param
                 (format nil "Delete:<ul>~{<li>~A</li>~}</ul>" (param-selected-file param))
                 '(do-delete-selected)))

(define-action do-delete-selected (:admin) ()
  (let ((files (param-selected-file param)))
    (dolist (file files)
      (do-shell-output "rm -rf ~S" file))
    (setf (param-additional-html param)
          (with-html-output-to-string (*standard-output*)
            (:p (str (format nil "Deleted:<ul>~{<li>~A</li>~}</ul>" files)))
            (:hr)))))



(define-action deselect-all (:admin :guest) ()
  (setf (param-selected-file param) nil))

(define-action select-all (:admin :guest) ()
  (multiple-value-bind (dirs files)
      (get-directory-list (current-tab-pathname param))
    (setf (param-selected-file param)
          (let ((acc nil))
            (dolist (dir dirs) (push (namestring dir) acc))
            (dolist (file files) (push (namestring file) acc))
            acc))))



(define-action system-management (:admin) ()
  (setf (param-additional-html param)
        (with-html-output-to-string (*standard-output*)
          (:p
           (:button :name "action" :value (str (to-string '(reboot))) "Reboot") " "
           (:button :name "action" :value (str (to-string '(halt))) "Halt") " "
           (:button :name "action" :value (str (to-string '(restart-X))) "Restart X"))
          (:hr))))


(define-action reboot (:admin) ()
  (let ((output (do-shell-output "sudo /sbin/reboot")))
    (setf (param-additional-html param)
          (with-html-output-to-string (*standard-output*)
            (:p (str (format nil "Rebooted. ~A" output)))
            (:hr)))))

(define-action halt (:admin) ()
  (let ((output (do-shell-output "sudo /sbin/halt")))
    (setf (param-additional-html param)
          (with-html-output-to-string (*standard-output*)
            (:p (str (format nil "Halted. ~A" output)))
            (:hr)))))

(define-action restart-X (:admin) ()
  (let ((output (do-shell-output "sudo /etc/init.d/slim restart")))
    (setf (param-additional-html param)
          (with-html-output-to-string (*standard-output*)
            (:p (str (format nil "Restart X. ~A" output)))
            (:hr)))))


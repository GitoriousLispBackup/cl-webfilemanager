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
          (:div :class "topbar"
                (str message)
                (:input :type :text :name "data")
                (:button :name "action" :value (str (to-string action)) (str confirm-message))
                (:input :type :submit :value "Cancel")))))

(defun info-text (param formatter &rest args)
  (setf (param-additional-html param)
        (with-html-output-to-string (*standard-output*)
          (:div :class "topbar"
                (str (apply #'format nil formatter args))))))




(defun ask-yes-or-no (param action formatter &rest args)
  (setf (param-additional-html param)
        (with-html-output-to-string (*standard-output*)
          (:div :class "topbar"
                (str (apply #'format nil formatter args))
                (:button :name "action" :value (str (to-string action)) "Yes")
                (:input :type :submit :value "No")))))



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
          (:div :class "topbar"
                (:button (str (format nil "plop ~A plop" pathname)))))))

(define-action ask-make-dir (:admin) ()
  (ask-text param "Create a new directory: " "Create" '(do-make-dir)))

(define-action do-make-dir (:admin) ()
  (let ((dirname (pathname-as-directory
                  (format nil "~A~A" (current-tab-pathname param) (param-data param)))))
    (do-shell-output "mkdir -p ~S" (namestring dirname))
    (info-text param "Creating: ~A" dirname)))



(define-action ask-delete-selected (:admin) ()
  (ask-yes-or-no param '(do-delete-selected)
                 "Delete:<ul>~{<li>~A</li>~}</ul>" (param-selected-file param)))

(define-action do-delete-selected (:admin) ()
  (let ((files (param-selected-file param)))
    (dolist (file files)
      (do-shell-output "rm -rf ~S" file))
    (setf (param-additional-html param)
          (info-text param "Deleted:<ul>~{<li>~A</li>~}</ul>" files))))



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


(define-action select-tab (:admin :guest) (tab-num)
  (setf (param-current-tab param) tab-num))



(defun create-new-tab (param directory)
  (setf (param-tab-list param) (append (param-tab-list param)
                                       (list directory)))
  (setf (param-current-tab param) (1- (length (param-tab-list param)))))


(define-action add-new-tab (:admin :guest) ()
  (create-new-tab param (user-homedir-pathname)))

(define-action clone-tab (:admin :guest) ()
  (create-new-tab param (current-tab-pathname param)))

(define-action delete-tab (:admin :guest) ()
  (when (> (length (param-tab-list param)) 1)
    (let ((acc nil))
      (dotimes (i (length (param-tab-list param)))
        (unless (= i (param-current-tab param))
          (push (nth i (param-tab-list param)) acc)))
      (setf (param-tab-list param) (nreverse acc))
      (setf (param-current-tab param) (max (1- (param-current-tab param)) 0)))))




(define-action system-management (:admin) ()
  (setf (param-additional-html param)
        (with-html-output-to-string (*standard-output*)
          (:div :class "topbar"
                (:button :name "action" :value (str (to-string '(reboot))) "Reboot") " "
                (:button :name "action" :value (str (to-string '(halt))) "Halt") " "
                (:button :name "action" :value (str (to-string '(restart-X))) "Restart X")))))



(define-action reboot (:admin) ()
  (let ((output (do-shell-output "sudo /sbin/reboot")))
    (setf (param-additional-html param)
          (with-html-output-to-string (*standard-output*)
            (:div :class "topbar"
                  (str (format nil "Rebooted. ~A" output)))))))

(define-action halt (:admin) ()
  (let ((output (do-shell-output "sudo /sbin/halt")))
    (setf (param-additional-html param)
          (with-html-output-to-string (*standard-output*)
            (:div :class "topbar"
                  (str (format nil "Halted. ~A" output)))))))

(define-action restart-X (:admin) ()
  (let ((output (do-shell-output "sudo /etc/init.d/slim restart")))
    (setf (param-additional-html param)
          (with-html-output-to-string (*standard-output*)
            (:div :class "topbar"
                  (str (format nil "Restart X. ~A" output)))))))


(defparameter *open-actions*
  '(("mpeg" "vlc")
    ("jpeg" open-qiv)
    ("fig image" "xfig")))


(defun open-qiv (file)
  (let ((id :qiv))
    (push (list id
                (with-html-output-to-string (*standard-output*)
                  (:div :class "topbar"
                        (:button :name "action" :class "btn"
                                 :value (str (to-string `(close-qiv ,file ,id)))
                                 (str (format nil "Close ~A" file))))))
          *permanent-html*)
    (fdo-shell "pkill qiv && qiv -t -m ~A" file)))

(define-action close-qiv (:admin :guest) (file id)
  (declare (ignore file))
  (fdo-shell "pkill qiv")
  (setf *permanent-html* (remove-if (lambda (x) (eql id (first x))) *permanent-html*)))




(defun find-open-action (file)
  (let ((type (file-type file)))
    (dolist (action *open-actions*)
      (when (search (first action) type :test #'string-equal)
        (return-from find-open-action (values (second action) t))))
    (values (format nil "Not found: ~A" type) nil)))

(defun do-open-file (param file)
  (multiple-value-bind (action found)
      (find-open-action file)
    (if found
        (typecase action
          ((or function symbol) (funcall action (namestring file)))
          (t (fdo-shell "~A ~S &" action (namestring file))
             (info-text param "Openning: ~A with ~A" file action)))
        (info-text param "Action not found: ~A<br>File type: ~A"
                   file (file-type file)))))




(define-action file-open (:admin :guest) (file)
  (do-open-file param file))

(define-action open-selected (:admin :guest) ()
  (dolist (file (param-selected-file param))
    (file-open param file)))

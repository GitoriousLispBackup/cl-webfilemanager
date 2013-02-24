(in-package :cl-user)

(defpackage :cl-webfilemanager
  (:use :common-lisp :hunchentoot :cl-who :cl-fad)
  (:export :start-server
           :start-server-loop))

(in-package :cl-webfilemanager)

(setf *show-lisp-errors-p* t)
(defparameter *port* 4343)


(defparameter *auth-admin* ())
(defparameter *auth-guest* ())


(defstruct tab-dir dirname dirs files show-hidden selected-file)

(defparameter *tab-list* (list (make-tab-dir :dirname (user-homedir-pathname) :selected-file nil
                                             :dirs nil :files nil
                                             :show-hidden nil)))

(defparameter *current-tab* 0)

(defparameter *num-columns* 1)


(defun split-string (string &optional (separator #\Space))
  "Return a list from a string splited at each separators"
  (loop for i = 0 then (1+ j)
     as j = (position separator string :start i)
     as sub = (subseq string i j)
     unless (string= sub "") collect sub
     while j))


(defun create-auth-md5 (login password)
  (remove #\space (format nil "~A"
                          (md5:md5sum-sequence
                           (format nil "~A ~A ~A" login password (random 100000))))))

(defun remove-in-auth-admin (identified)
  (setf *auth-admin* (remove identified *auth-admin* :test #'string=)))

(defun remove-in-auth-guest (identified)
  (setf *auth-guest* (remove identified *auth-guest* :test #'string=)))


(defun add-in-auth-admin (login password)
  (let ((auth-md5 (create-auth-md5 login password)))
    (pushnew auth-md5 *auth-admin*)
    auth-md5))

(defun add-in-auth-guest (login password)
  (let ((auth-md5 (create-auth-md5 login password)))
    (pushnew auth-md5 *auth-guest*)
    auth-md5))


(defun is-hidden-file (file)
  (eql #\. (char (pathname-name file) 0)))

(defun is-hidden-directory (dirname)
  (dolist (dir (rest (pathname-directory dirname)))
    (when (eql #\. (char dir 0))
      (return-from is-hidden-directory t))))

(defun sort-file (filelist)
  (sort filelist
        (lambda (x y)
          (string< (string-upcase (namestring x)) (string-upcase (namestring y))))))

;;(defun sort-file (filelist)
;;  (sort filelist
;;        (lambda (x y)
;;          (string< (string-upcase x) (string-upcase y)))))


(defun get-directory-list (dirname &optional show-hidden)
  (let ((acc-file nil)
        (acc-dir nil))
    (dolist (item (list-directory dirname))
      (if (directory-pathname-p item)
          (when (or show-hidden (not (is-hidden-directory item)))
            (push item acc-dir))
          (when (or show-hidden (and (not (is-hidden-file item)) (not (is-hidden-directory item))))
            (push item acc-file))))
    (values (sort-file acc-dir) (sort-file acc-file))))

;;(defun get-directory-list (dirname &optional show-hidden)
;;  (let ((acc-file nil)
;;        (acc-dir nil))
;;    (dolist (item (list-directory dirname))
;;      (if (directory-pathname-p item)
;;          (when (or show-hidden (not (is-hidden-directory item)))
;;            (push (first (last (pathname-directory item))) acc-dir))
;;          (when (or show-hidden (and (not (is-hidden-file item)) (not (is-hidden-directory item))))
;;            (push (format nil "~A~A" (pathname-name item)
;;                          (if (pathname-type item)
;;                              (format nil ".~A" (pathname-type item))
;;                              ""))
;;                  acc-file))))
;;    (values (sort-file acc-dir) (sort-file acc-file))))




(defun update-tab-content (tab)
  (multiple-value-bind (dirs files)
      (get-directory-list (tab-dir-dirname tab) (tab-dir-show-hidden tab))
    (setf (tab-dir-dirs tab) dirs
          (tab-dir-files tab) files)))


(defun current-tab ()
  (nth *current-tab* *tab-list*))

;;(defun css-style ()
;;  "h1 { color: #FF0000; text-align: center; }
;;   input.btn {   color:#050;   font: bold small 'trebuchet ms',helvetica,sans-serif; }
;;   input.btn2 {   color:#050;   font: bold 84% 'trebuchet ms',helvetica,sans-serif;
;;                  background-color:#fed;   border:1px solid;   border-color: #696 #363 #363 #696; }
;;   input.btnhov {   border-color: #c63 #930 #930 #c63; }
;;")


;;(defun css-style ()
;;  "h1 { color: #FF0000; text-align: center; }
;;   input.btn {   color:blue;   font: bold 84% 'trebuchet ms',helvetica,sans-serif;
;;                 background-color:white;   border:1px solid;   border-color: white white white white; }
;;   input.btnhov {   border-color: black black black; }
;;")

(defun css-style ()
  "h1 { color: #FF0000; text-align: center; }
   input.btnblue { color: blue; }
   input.btn {   background-color:white; font: normal 100% sans-serif;  border:1px solid;
                 border-color: white white white white; }
   input.btnhov {   border-color: black black black; }
")



(defun send-login-page (login password identified)
  (with-html-output-to-string (*standard-output* nil :prologue t)
    (:html
     (:head (:title "Hello, world!"))
     (:body
      :onload "document.getElementById(\"login\").focus();"
      :style "margin: 20px"
      (:h1 "Hello, world!")
      (:p (:form
           :action "/"
           :method :post
	   (:input :type :hidden :name "identified" :value identified)
	   (:p "Login : "
	       (:input :type :text :id "login" :name "login")) ; :value login))
	   (:p "Mot de passe : "
	       (:input :type :password :name "password")) ;; :value password))
	   (:p (:input :type :submit :value "Submit"))))
      (:p "Login : " (str login))
      (:p "Mot de passe : " (str password))
      (:p "Identifié : " (str identified))
      (:p "Auth admin : " (str *auth-admin*))
      (:p "Auth guest : " (str *auth-guest*))))))


(defun button-in (value)
  (with-html-output-to-string (*standard-output*)
    (:input :type :submit :name "action" :class "btn" :value value
            :onmouseover "this.className=\"btn btnhov\";"
            :onmouseout "this.className=\"btn\";")))


(defun group-by (list n)
  (let ((acc nil) (subacc nil) (i 0))
    (dolist (l list)
      (incf i)
      (push l subacc)
      (when (zerop (mod i n))
        (push (nreverse subacc) acc)
        (setf subacc nil)))
    (when subacc
      (push (nreverse subacc) acc))
    (nreverse acc)))

(defun spaces (n)
  (with-output-to-string (str)
    (dotimes (i n)
      (princ #\space str))))

(defun limit-char (string &optional (limit 20))
  (let ((len (length string)))
    (if (> len limit)
        (subseq string 0 limit)
        (format nil "~A~A" string (spaces (- limit len))))))


(defun make-dir-button (dir)
  (with-html-output-to-string (*standard-output*)
    (:input :type :submit :name "action" :class "btnblue btn"
            :value (namestring dir) :text "plop"
            :onmouseover "this.className=\"btnblue btn btnhov\";"
            :onmouseout "this.className=\"btnblue btn\";")))

(defun make-file-button (file)
  (with-html-output-to-string (*standard-output*)
    (:input :type :submit :name "action" :class "btn"
            :value (namestring file)
            :onmouseover "this.className=\"btn btnhov\";"
            :onmouseout "this.className=\"btn\";")))


(defun generic-build-list (list fun-button)
  (with-html-output-to-string (*standard-output*)
    (:table
     (dolist (grp (group-by list *num-columns*))
       do (htm (:tr
                (dolist (dir grp)
                  (htm (:td (str (funcall fun-button dir)))))))))))


(defun build-dir-list ()
  (generic-build-list (tab-dir-dirs (current-tab)) #'make-dir-button))


(defun build-file-list ()
  (generic-build-list (tab-dir-files (current-tab)) #'make-file-button))

;;(defun build-file-list ()
;;  (let ((tab (current-tab)))
;;    ;;(multiple-value-bind (dirs files)
;;    ;;    (get-directory-list (tab-dir-dirname tab))
;;    (with-html-output-to-string (*standard-output*)
;;      (:table
;;       (dolist (grp (group-by (tab-dir-dirs tab) *num-columns*))
;;         do (htm (:tr
;;                  (dolist (dir grp)
;;                    (htm (:td (str (make-dir-button dir)))))))))
;;      (:table
;;       (dolist (grp (group-by (tab-dir-files tab) *num-columns*))
;;         do (htm (:tr
;;                  (dolist (file grp)
;;                    (htm (:td (str (make-file-button file))))))))))))



(defun build-dir-path ()
  (let ((tab (current-tab)))
    (with-html-output-to-string (*standard-output*)
      "/"
      (dolist (dir (rest (pathname-directory (tab-dir-dirname tab))))
        (htm
         (str (make-dir-button dir)) "/")))))


(defun send-identified (identified action)
  (when action
    (when (string= action "Deconnexion")
      (remove-in-auth-admin identified)
      (setf identified "false")
      (return-from send-identified (send-login-page "" "" "false")))
    (when (string= action "Clean auth")
      (setf *auth-admin* nil *auth-guest* nil))
    (when (string= action "Home")
      (setf (tab-dir-dirname (current-tab)) (user-homedir-pathname)))
    (when (member (pathname-as-directory action) (tab-dir-dirs (current-tab)) :test #'equal)
      (setf (tab-dir-dirname (current-tab)) action)
      ;;(pathname-as-directory (format nil "~A/~A" (tab-dir-dirname (current-tab)) action))))
      )
    (let* ((dir (pathname-directory (tab-dir-dirname (current-tab))))
           (pos (position action dir :test #'string=)))
      (when pos
        (setf (tab-dir-dirname (current-tab)) (make-pathname :directory (subseq dir 0 (1+ pos)))))))
  (update-tab-content (current-tab))
  (with-html-output-to-string (*standard-output* nil :prologue t)
    (:html
     (:head (:title "TODO")
            (:style (str (css-style))))
     (:body
      ;;:style "margin: 20px"
      (:p (:form :name "plop"
                 :method :post
                 :action "/"
                 (:input :type :hidden :name "identified" :value identified)
                 (str (build-dir-path))
                 (:hr)
                 (str (build-dir-list))
                 (:hr)
                 (str (build-file-list))
                 (:hr)
                 (:p (:input :type :submit :name "action" :value "Home")
                     (:input :type :submit :name "action" :value "Clean auth")
                     (:input :type :submit :name "action" :value "Deconnexion"))
                 ;;(:p (:input :type :submit :name "action" :class "btn" :value "Lance") "&nbsp;"
                 ;;    (:input :type :submit :name "action" :class "btn" :value "Plop") "&nbsp;"
                 ;;    (:input :type :submit :name "action" :class "btn" :value "Paf"
                 ;;            :onmouseover "this.className=\"btn btnhov\";"
                 ;;            :onmouseout "this.className=\"btn\";"))
                 ;;(:p (str (button-in "Pif")) (str (button-in "Plaf")) (str (button-in "Plouf")))
                 (:p (:input :type :submit :value "Submit"))
                 (:p "Identifié : " (str identified))
                 ;;(:p "Select-salle : " (str select-salle))
                 (:p "Action : " (str action))
                 (:p "Auth admin : " (str *auth-admin*))
                 (:p "Auth guest : " (str *auth-guest*))))))))




(defun send-identified-guest (identified select-salle action)
  (let ((str-action "???"))
    (when (string= action "Deconnexion")
      (remove-in-auth-guest identified)
      (setf identified "false")
      (return-from send-identified-guest (send-login-page "" "" "false")))
    (with-html-output-to-string (*standard-output* nil :prologue t)
      (:html
       (:head (:title "Hello, world!")
              (:style (str (css-style))))
       (:body
        :style "margin: 20px"
        (:h1 "Hello, world!")
        (:p (:form :name "plop"
                   :method :post
                   :action "/"
                   (:input :type :hidden :name "identified" :value identified)
                   (:p (str (button-in "Pif")) (str (button-in "Plaf")) (str (button-in "Plouf")))
                   (:p (:input :type :submit :value "Submit"))
                   (:p "Identifié : " (str identified))
                   (:p "Select-salle : " (str select-salle))
                   (:p "Action : " (str action))
                   (:p "String action : " (str str-action))
                   (:p "Auth admin : " (str *auth-admin*))
                   (:p "Auth guest : " (str *auth-guest*))
                   (:p (:input :type :submit :name "action" :value "Deconnexion")))))))))




(define-easy-handler (login-page :uri "/" :default-request-type :post)
    ((login :parameter-type 'string)
     (password :parameter-type 'string)
     (identified :parameter-type 'string)
     (selected-file :parameter-type 'list)
     (action :parameter-type 'string))
  (labels ((handle-auth-admin-request ()
             (remove-in-auth-admin identified)
             (setf identified (add-in-auth-admin login password))
             (send-identified identified action))
           (handle-auth-guest-request ()
             (remove-in-auth-guest identified)
             (setf identified (add-in-auth-guest login password))
             (send-identified-guest identified selected-file action)))
    (when (and (string-equal login "phil") (string-equal password "toto"))
      (setf identified (add-in-auth-admin login password)))
    (when (and (string-equal login "toto") (string-equal password "toto"))
      (setf identified (add-in-auth-guest login password)))
    (cond ((member identified *auth-admin* :test #'string=) (handle-auth-admin-request))
          ((member identified *auth-guest* :test #'string=) (handle-auth-guest-request))
          (t (send-login-page login password identified)))))


(defun 404-dispatcher (request)
  (declare (ignore request))
  '404-page)

(defun 404-page ()
  ;;"404 is here!")
  (send-login-page nil nil nil))


(setf *dispatch-table*
      (list #'dispatch-easy-handlers
            ;;#'default-dispatcher
            #'404-dispatcher))



(defun start-server (&optional (port *port*))
  (start (make-instance 'ssl-acceptor :port port
                        :ssl-certificate-file "./server.crt"
                        :ssl-privatekey-file "./server.key"))
  (format t "Server started on port ~A with ssl/https~%" *port*))


;;(start-server)

(defun start-server-loop ()
  (start-server)
  (loop (sleep 10)))






;;(defun send-identified-test (identified selected-file action)
;;  (let ((str-action "???"))
;;    (when (string= action "Deconnexion")
;;      (remove-in-auth-admin identified)
;;      (setf identified "false")
;;      (return-from send-identified-test (send-login-page "" "" "false")))
;;    (when (string= action "Lance")
;;      (setf str-action "--> Lance"))
;;    (when (string= action "Plop")
;;      (setf str-action "--> Plop plop"))
;;    (when (string= action "Clean auth")
;;      (setf *auth-admin* nil
;;            *auth-guest* nil)
;;      (setf str-action "Clean auth"))
;;    (with-html-output-to-string (*standard-output* nil :prologue t)
;;      (:html
;;       (:head (:title "Hello, world!")
;;              (:style (str (css-style))))
;;       (:body
;;        :style "margin: 20px"
;;        (:h1 "Hello, world!")
;;        ;;(:p (:a :href "/" "Deconnexion"))
;;        (:p (:form :name "plop"
;;                   :method :post
;;                   :action "/"
;;                   (:input :type :hidden :name "identified" :value identified)
;;                   ;;(:input :type :checkbox :name "select-salle" :value "1"
;;                   ;;        :checked (member "1" select-salle :test 'string=)
;;                   ;;        "Salle 1")
;;                   ;;(:input :type :checkbox :name "select-salle" :value "2"
;;                   ;;        :checked (member "2" select-salle :test 'string=)
;;                   ;;        "Salle 2")
;;                   ;;(:input :type :checkbox :name "select-salle" :value "3"
;;                   ;;        :checked (member "3" select-salle :test 'string=)
;;                   ;;        "Salle 3")
;;                   (:p (:input :type :submit :name "action" :class "btn" :value "Lance") "&nbsp;"
;;                       (:input :type :submit :name "action" :class "btn2" :value "Plop") "&nbsp;"
;;                       (:input :type :submit :name "action" :class "btn2" :value "Paf"
;;                               :onmouseover "this.className=\"btn2 btnhov\";"
;;                               :onmouseout "this.className=\"btn2\";"))
;;                   (:p (str (button-in "Pif")) (str (button-in "Plaf")) (str (button-in "Plouf")))
;;                   (:p (:input :type :submit :value "Submit"))
;;                   (:p "Identifié : " (str identified))
;;                   ;;(:p "Select-salle : " (str select-salle))
;;                   (:p "Action : " (str action))
;;                   (:p "String action : " (str str-action))
;;                   (:p "Auth admin : " (str *auth-admin*))
;;                   (:p "Auth guest : " (str *auth-guest*))
;;                   (:p (str (button-in "Clean auth"))
;;                       (:input :type :submit :name "action" :value "Deconnexion")))))))))
(in-package :cl-user)

(defpackage :my-server
  (:use :common-lisp :hunchentoot :cl-who)
  (:export :start-server))

(in-package :my-server)

(setf *show-lisp-errors-p* t)
(defparameter *port* 4343)


(defparameter *auth-admin* ())
(defparameter *auth-guest* ())



;; (file-namestring #P"/home/initrd.img.slackware/")
;; (cl-fad:list-directory "/home")
;; (cl-fad:directory-pathname-p #P"/home/phil/tmp/")






(defun css-style ()
  "h1 { color: #FF0000; text-align: center; }
   input.btn {   color:#050;   font: bold small 'trebuchet ms',helvetica,sans-serif; }
   input.btn2 {   color:#050;   font: bold 84% 'trebuchet ms',helvetica,sans-serif;
                  background-color:#fed;   border:1px solid;   border-color: #696 #363 #363 #696; }
   input.btnhov {   border-color: #c63 #930 #930 #c63; }
")


(defun css-style ()
  "h1 { color: #FF0000; text-align: center; }
   input.btn {   color:blue;   font: bold small 'trebuchet ms',helvetica,sans-serif; }
   input.btn2 {   color:blue;   font: bold 84% 'trebuchet ms',helvetica,sans-serif;
                  background-color:white;   border:1px solid;   border-color: white white white white; }
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
  (with-html-output-to-string (*standard-output* nil :prologue nil)
    (:input :type :submit :name "action" :class "btn2" :value value
            :onmouseover "this.className=\"btn2 btnhov\";"
            :onmouseout "this.className=\"btn2\";")
    "&nbsp;" "/" "&nbsp;"))


(defun send-identified (identified select-salle action)
  (let ((str-action "???"))
    (when (string= action "Deconnexion")
      (setf *auth-admin* (remove identified *auth-admin* :test #'string=))
      (setf identified "false")
      (return-from send-identified (send-login-page "" "" "false")))
    (when (string= action "Lance")
      (setf str-action "--> Lance"))
    (when (string= action "Plop")
      (setf str-action "--> Plop plop"))
    (when (string= action "Clean auth")
      (setf *auth-admin* nil
            *auth-guest* nil)
      (setf str-action "Clean auth"))
    (with-html-output-to-string (*standard-output* nil :prologue t)
      (:html
       (:head (:title "Hello, world!")
              (:style (str (css-style))))
       (:body
        :style "margin: 20px"
        (:h1 "Hello, world!")
        ;;(:p (:a :href "/" "Deconnexion"))
        (:p (:form :name "plop"
                   :method :post
                   :action "/"
                   (:input :type :hidden :name "identified" :value identified)
                   (:input :type :checkbox :name "select-salle" :value "1"
                           :checked (member "1" select-salle :test 'string=)
                           "Salle 1")
                   (:input :type :checkbox :name "select-salle" :value "2"
                           :checked (member "2" select-salle :test 'string=)
                           "Salle 2")
                   (:input :type :checkbox :name "select-salle" :value "3"
                           :checked (member "3" select-salle :test 'string=)
                           "Salle 3")
                   (:p (:input :type :submit :name "action" :class "btn" :value "Lance") "&nbsp;"
                       (:input :type :submit :name "action" :class "btn2" :value "Plop") "&nbsp;"
                       (:input :type :submit :name "action" :class "btn2" :value "Paf"
                               :onmouseover "this.className=\"btn2 btnhov\";"
                               :onmouseout "this.className=\"btn2\";"))
                   (:p (str (button-in "Pif")) (str (button-in "Plaf")) (str (button-in "Plouf")))
                   (:p (:input :type :submit :value "Submit"))
                   (:p "Identifié : " (str identified))
                   (:p "Select-salle : " (str select-salle))
                   (:p "Action : " (str action))
                   (:p "String action : " (str str-action))
                   (:p "Auth admin : " (str *auth-admin*))
                   (:p "Auth guest : " (str *auth-guest*))
                   (:p (str (button-in "Clean auth"))
                       (:input :type :submit :name "action" :value "Deconnexion")))))))))




(defun send-identified-guest (identified select-salle action)
  (let ((str-action "???"))
    (when (string= action "Deconnexion")
      (setf *auth-guest* (remove identified *auth-guest* :test #'string=))
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




(defun create-auth-md5 (login password)
  (remove #\space (format nil "~A"
                          (md5:md5sum-sequence
                           (format nil "~A ~A ~A" login password (random 10000))))))


(define-easy-handler (login-page :uri "/" :default-request-type :post)
    ((login :parameter-type 'string)
     (password :parameter-type 'string)
     (identified :parameter-type 'string)
     (select-salle :parameter-type 'list)
     (action :parameter-type 'string))
  (when (and (string-equal login "phil") (string-equal password "toto"))
    (let ((auth-md5 (create-auth-md5 login password)))
      (pushnew auth-md5 *auth-admin*)
      (setf identified auth-md5)))
  (when (and (string-equal login "toto") (string-equal password "toto"))
    (let ((auth-md5 (create-auth-md5 login password)))
      (pushnew auth-md5 *auth-guest*)
      (setf identified auth-md5)))
  (cond ((member identified *auth-admin* :test #'string=)
         (send-identified identified select-salle action))
        ((member identified *auth-guest* :test #'string=)
         (send-identified-guest identified select-salle action))
        (t (send-login-page login password identified))))


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
  (format t "Server started on port ~A with ssl/https~%" *port*)
  (loop (sleep 10)))


;;(start-server)

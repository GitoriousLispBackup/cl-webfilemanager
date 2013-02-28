(in-package :cl-webfilemanager)


(defun css-style ()
  "h1 { color: #FF0000; text-align: center; }
   input.btnblue { color: blue; }
   input.btn {   background-color:white; font: normal 100% sans-serif;  border:1px solid;
                 border-color: white white white white; }
   input.btnhov {   border-color: black black black; }

   button.btnblue { color: blue; }
   button.btn {   background-color:white; font: normal 100% sans-serif;  border:1px solid;
                 border-color: white white white white; }
   button.btnhov {   border-color: black black black; }
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
	       (:input :type :text :id "login" :name "login"))
	   (:p "Mot de passe : "
	       (:input :type :password :name "password"))
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


(defun make-dir-button (dir selected-file)
  (let* ((dirname (first (last (pathname-directory dir))))
         (dirstr (if (equal dirname :absolute) "Root" dirname)))
    (with-html-output-to-string (*standard-output*)
      (:input :type :checkbox :name "selected-file" :value dir
              :checked (member (namestring dir) selected-file :test #'equal))
      (:button :type :submit :name "action" :class "btnblue btn"
               :value (str (to-string `(cd ,dir)))
               :onmouseover "this.className=\"btnblue btn btnhov\";"
               :onmouseout "this.className=\"btnblue btn\";"
               (str dirstr)))))

(defun make-file-button (file selected-file)
  (with-html-output-to-string (*standard-output*)
    (:input :type :checkbox :name "selected-file" :value file
            :checked (member (namestring file) selected-file :test #'equal))
    (:button :type :submit :name "action" :class "btn"
             :value (str (to-string `(options ,file)))
             :onmouseover "this.className=\"btn btnhov\";"
             :onmouseout "this.className=\"btn\";"
             "o")
    (:button :type :submit :name "action" :class "btn"
             :value (str (to-string `(open ,file)))
             :onmouseover "this.className=\"btn btnhov\";"
             :onmouseout "this.className=\"btn\";"
             (str (pathname-name-type file)))))

(defun generic-build-list (list fun-button selected-file)
  (with-html-output-to-string (*standard-output*)
    (dolist (item list)
      (htm (str (funcall fun-button item selected-file))
           (:br)))))



(defun build-dir-list (dirs selected-file)
  (generic-build-list dirs #'make-dir-button selected-file))


(defun build-file-list (files selected-file)
  (generic-build-list files #'make-file-button selected-file))



(defun build-dir-path (tab-pathname selected-file)
  (with-html-output-to-string (*standard-output*)
    "/"
    (dolist (dir (extract-all-pathname tab-pathname))
      (htm
       (str (make-dir-button dir selected-file)) "/"))))


(defun control-button ()
  (with-html-output-to-string (*standard-output*)
    (:p (:button :name "action" :value (str (to-string `(cd ,(user-homedir-pathname))))
                 "Home")
        (:button :name "action" :value (str (to-string '(clean-auth)))
                 "Clean Auth")
        (:button :name "action" :value (str (to-string '(deconnexion)))
                 "Deconnexion"))))


(defun send-identified (identified action selected-file tab-list current-tab)
  (let* ((tab-list (read-from-string tab-list))
         (param (make-param :identified identified
                            :action action
                            :selected-file selected-file
                            :tab-list tab-list
                            :current-tab current-tab
                            :additional-html nil)))
    (labels ((do-default-action (action)
               (when action
                 (let ((cmd (read-from-string action)))
                   (when (member (first cmd) *action-list*)
                     (apply (first cmd) param (rest cmd))
                     (setf identified (param-identified param)
                           action (param-action param)
                           selected-file (param-selected-file param)
                           tab-list (param-tab-list param)
                           current-tab (param-current-tab param)))))))
      (do-default-action action)
      (unless (member identified *auth-admin* :test #'string=)
        (return-from send-identified (send-login-page "" "" "false")))
      (let ((tab-pathname (nth current-tab tab-list)))
        (multiple-value-bind (dirs files)
            (get-directory-list tab-pathname) ;; TODO showhidden
          (with-html-output-to-string (*standard-output* nil :prologue t)
            (:html
             (:head (:title "TODO")
                    (:style (str (css-style))))
             (:body
              (:p (str (param-additional-html param)))
              (:p (:form :method :post
                         :action "/"
                         (:input :type :hidden :name "identified" :value identified)
                         (:input :type :hidden :name "tab-list" :value (to-string tab-list))
                         (:input :type :hidden :name "current-tab" :value current-tab)
                         (str (control-button))
                         (:hr)
                         (str (build-dir-path tab-pathname selected-file))
                         (:hr)
                         (str (build-dir-list dirs selected-file))
                         (:hr)
                         (str (build-file-list files selected-file))
                         (:hr)
                         (str (control-button))
                         (:hr)
                         (:p "Identifié : " (str identified))
                         (:p "Tab list : " (str (to-string tab-list)))
                         (:p "Current tab : " (str current-tab))
                         (:p "Selected-File : " (str selected-file))
                         (:p "Action : " (str action))
                         (:p "Auth admin : " (str *auth-admin*))
                         (:p "Auth guest : " (str *auth-guest*))))))))))))




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
     (action :parameter-type 'string)
     (tab-list :parameter-type 'string)
     (current-tab :parameter-type 'integer))
  (labels ((handle-auth-admin-request ()
             (remove-in-auth-admin identified)
             (setf identified (add-in-auth-admin login password))
             (send-identified identified action selected-file tab-list current-tab))
           (handle-auth-guest-request ()
             (remove-in-auth-guest identified)
             (setf identified (add-in-auth-guest login password))
             (send-identified-guest identified selected-file action)))
    (when (and (string-equal login "phil") (string-equal password "toto"))
      (setf identified (add-in-auth-admin login password)
            tab-list (to-string (list (user-homedir-pathname)))
            current-tab 0
            login "???"
            password "???"))
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
(in-package :cl-user)

;; TODO: maintenir une hashtable avec liste des pathnames visités
;; au cd ou open
;;   (push file/pathname (gethash pathname-orig))

(defpackage :cl-webfilemanager
  (:use :common-lisp :hunchentoot :cl-who :cl-fad :parenscript)
  (:export :start-server
           :start-server-loop
           :stop-server))

(in-package :cl-webfilemanager)

(setf *show-lisp-errors-p* t)

(defparameter *port* 4343)


(defparameter *server-instance* nil)

(defparameter *auth-admin* ())
(defparameter *auth-guest* ())

(defparameter *admin-login-list* nil
  "A list of login/password pair")
(defparameter *guest-login-list* nil
  "A list of login/password pair")

(defparameter *action-list* nil)
(defparameter *action-guest-list* nil)

(defparameter *permanent-html* nil)
(defparameter *permanent-html-id* 0)

(defstruct param identified action selected-file tab-list current-tab data additional-html)


(in-package :cl-user)

;; TODO: maintenir une hashtable avec liste des pathnames visités
;; au cd ou open
;;   (push file/pathname (gethash pathname-orig))

(defpackage :cl-webfilemanager
  (:use :common-lisp :hunchentoot :cl-who :cl-fad)
  (:export :start-server
           :start-server-loop))

(in-package :cl-webfilemanager)

(setf *show-lisp-errors-p* t)
(defparameter *port* 4343)


(defparameter *auth-admin* ())
(defparameter *auth-guest* ())


(defparameter *num-columns* 4)

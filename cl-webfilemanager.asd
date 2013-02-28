 ;;;; -*- Mode: Lisp -*-
;;;; Author: Philippe Brochard <pbrochard@common-lisp.net>
;;;; ASDF System Definition
;;;

(in-package #:asdf)

(defsystem cl-webfilemanager
  :description "CL-WEBMANAGER: A web file manager"
  :version "0.0"
  :author "Philippe Brochard  <pbrochard@common-lisp.net>"
  :licence "GNU Public License (GPL)"
  :components ((:file "package")
               (:file "tools"
                      :depends-on ("package"))
               (:file "cl-webfilemanager"
                      :depends-on ("package" "tools"))))






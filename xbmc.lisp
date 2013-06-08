(in-package :cl-webfilemanager)

(defparameter *xbmc-server* "http://localhost:8080")

;;(defun handle-key-down ()
;;  (ps-inline
;;   (let ((span (chain document (get-element-by-id "plop"))))
;;     (alert (chain span inner-h-t-m-l))
;;     (setf (@ (chain document (get-element-by-id "plop")) inner-h-t-m-l) "BLA"))))


(defun main-js-code ()
  (ps
    (defun url-go-to (url)
      (setf (@ (chain window location href)) url))

    (setf (@ document onkeydown)
          (lambda (e)
            (let ((keycode (getprop e 'key-code)))
              ;;(setf (@ (chain document (get-element-by-id "plop")) inner-h-t-m-l) keycode)
              (case keycode
                (39 (url-go-to "/xbmc-right"))
                (37 (url-go-to "/xbmc-left"))
                (38 (url-go-to "/xbmc-up"))
                (40 (url-go-to "/xbmc-down"))
                (33 (url-go-to "/xbmc-page_up"))
                (34 (url-go-to "/xbmc-page_down"))
                (8 (url-go-to "/xbmc-back"))
                (27 (url-go-to "/xbmc-back"))
                (32 (url-go-to "/xbmc-context"))
                (17 (url-go-to "/xbmc-context"))
                (72 (url-go-to "/xbmc-home"))
                (36 (url-go-to "/xbmc-home"))
                (13 (url-go-to "/xbmc-select"))))))))


;;    (defun handle-key-down (e)
;;      ;;(alert (getprop e 'key-code)))))
;;      (setf (@ (chain document (get-element-by-id "plop")) inner-h-t-m-l) (getprop e 'key-code))
;;      true)))

;; Home   Up   Back
;; Left Select Right
;; PgUp  Down  PgDn
;; Context
(defun xbmc-remote-page ()
  (with-html-output-to-string (*standard-output* nil :prologue t)
    (:html
     (:head (:title "XBMC Remote")
            (:script :type "text/javascript"
                     (str (main-js-code))))
     (:body
      (:a :href "xbmc-home" (:img :src "pics/home.png" :width "100px"))
      (:a :href "xbmc-up"(:img :src "pics/up.png" :width "100px"))
      (:a :href "xbmc-back" (:img :src "pics/back.png" :width "100px"))
      (:br)
      (:a :href "xbmc-left" (:img :src "pics/left.png" :width "100px"))
      (:a :href "xbmc-select" (:img :src "pics/select.png" :width "100px"))
      (:a :href "xbmc-right" (:img :src "pics/right.png" :width "100px")) (:br)
      (:a :href "xbmc-page_up" (:img :src "pics/page_up.png" :width "100px"))
      (:a :href "xbmc-down" (:img :src "pics/down.png" :width "100px"))
      (:a :href "xbmc-page_down" (:img :src "pics/page_down.png" :width "100px")) (:br)
      (:a :href "xbmc-context" (:img :src "pics/context.png" :width "100px"))
      ;;(:span :id "plop" "plop")
      ;;(:input :type "text" :onkeydown (ps (handle-key-down event)))
      ))))

;; {"jsonrpc": "2.0", "method": "Playlist.Add","params": {"playlistid": 1, "item": {"file" : "smb://homestorage/XYZ.JPG"}}, "id": 1}

(defun curl-cmd (cmd)
  (format nil "curl -i -X POST -H \"Content-Type: application/json\" -d '{\"jsonrpc\": \"2.0\", \"method\": ~S, \"params\": {  }, \"id\": 1 }' ~A/jsonrpc > /dev/null 2> /dev/null &" cmd *xbmc-server*))


(define-easy-handler (xbmc :uri "/xbmc") ()
  (xbmc-remote-page))

(define-easy-handler (xbmc-up :uri "/xbmc-up") ()
  (fdo-shell (curl-cmd "Input.Up"))
  (xbmc-remote-page))

(define-easy-handler (xbmc-down :uri "/xbmc-down") ()
  (fdo-shell (curl-cmd "Input.Down"))
  (xbmc-remote-page))

(define-easy-handler (xbmc-left :uri "/xbmc-left") ()
  (fdo-shell (curl-cmd "Input.Left"))
  (xbmc-remote-page))

(define-easy-handler (xbmc-right :uri "/xbmc-right") ()
  (fdo-shell (curl-cmd "Input.Right"))
  (xbmc-remote-page))

(define-easy-handler (xbmc-back :uri "/xbmc-back") ()
  (fdo-shell (curl-cmd "Input.Back"))
  (xbmc-remote-page))

(define-easy-handler (xbmc-home :uri "/xbmc-home") ()
  (fdo-shell (curl-cmd "Input.Home"))
  (xbmc-remote-page))

(define-easy-handler (xbmc-select :uri "/xbmc-select") ()
  (fdo-shell (curl-cmd "Input.Select"))
  (xbmc-remote-page))

(define-easy-handler (xbmc-page_down :uri "/xbmc-page_down") ()
  (dotimes (i 10)
    (fdo-shell (curl-cmd "Input.Down")))
  (xbmc-remote-page))

(define-easy-handler (xbmc-page_up :uri "/xbmc-page_up") ()
  (dotimes (i 10)
    (fdo-shell (curl-cmd "Input.Up")))
  (xbmc-remote-page))

(define-easy-handler (xbmc-context :uri "/xbmc-context") ()
  (fdo-shell (curl-cmd "Input.ContextMenu"))
  (xbmc-remote-page))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Light version
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun main-light-js-code ()
  (ps
    (defun url-go-to (url)
      (setf (@ (chain window location href)) url))

    (setf (@ document onkeydown)
          (lambda (e)
            (let ((keycode (getprop e 'key-code)))
              ;;(setf (@ (chain document (get-element-by-id "plop")) inner-h-t-m-l) keycode)
              (case keycode
                (39 (url-go-to "/xbmcl-right"))
                (37 (url-go-to "/xbmcl-left"))
                (38 (url-go-to "/xbmcl-up"))
                (40 (url-go-to "/xbmcl-down"))
                (33 (url-go-to "/xbmcl-page_up"))
                (34 (url-go-to "/xbmcl-page_down"))
                (8 (url-go-to "/xbmcl-back"))
                (27 (url-go-to "/xbmcl-back"))
                (32 (url-go-to "/xbmcl-context"))
                (17 (url-go-to "/xbmcl-context"))
                (72 (url-go-to "/xbmcl-home"))
                (36 (url-go-to "/xbmcl-home"))
                (13 (url-go-to "/xbmcl-select"))))))))


;;    (defun handle-key-down (e)
;;      ;;(alert (getprop e 'key-code)))))
;;      (setf (@ (chain document (get-element-by-id "plop")) inner-h-t-m-l) (getprop e 'key-code))
;;      true)))

;; Home   Up   Back
;; Left Select Right
;; PgUp  Down  PgDn
;; Context
(defun xbmc-light-remote-page ()
  (with-html-output-to-string (*standard-output* nil :prologue t)
    (:html
     (:head (:title "XBMC Remote")
            (:script :type "text/javascript"
                     (str (main-light-js-code))))
     (:body
      (:a :href "xbmcl-home" "Home") " "
      (:a :href "xbmcl-up" "Up") " "
      (:a :href "xbmcl-back" "Back")
      (:br)
      (:a :href "xbmcl-left" "Left") " "
      (:a :href "xbmcl-select" "Select") " "
      (:a :href "xbmcl-right" "Right") (:br)
      (:a :href "xbmcl-page_up" "PgUp") " "
      (:a :href "xbmcl-down" "Down") " "
      (:a :href "xbmcl-page_down" "PgDown") (:br)
      (:a :href "xbmcl-context" "Context")
      ;;(:span :id "plop" "plop")
      ;;(:input :type "text" :onkeydown (ps (handle-key-down event)))
      ))))

;; {"jsonrpc": "2.0", "method": "Playlist.Add","params": {"playlistid": 1, "item": {"file" : "smb://homestorage/XYZ.JPG"}}, "id": 1}


(define-easy-handler (xbmc-light :uri "/xbmcl") ()
  (xbmc-light-remote-page))

(define-easy-handler (xbmc-light-up :uri "/xbmcl-up") ()
  (fdo-shell (curl-cmd "Input.Up"))
  (xbmc-light-remote-page))

(define-easy-handler (xbmc-light-down :uri "/xbmcl-down") ()
  (fdo-shell (curl-cmd "Input.Down"))
  (xbmc-light-remote-page))

(define-easy-handler (xbmc-light-left :uri "/xbmcl-left") ()
  (fdo-shell (curl-cmd "Input.Left"))
  (xbmc-light-remote-page))

(define-easy-handler (xbmc-light-right :uri "/xbmcl-right") ()
  (fdo-shell (curl-cmd "Input.Right"))
  (xbmc-light-remote-page))

(define-easy-handler (xbmc-light-back :uri "/xbmcl-back") ()
  (fdo-shell (curl-cmd "Input.Back"))
  (xbmc-light-remote-page))

(define-easy-handler (xbmc-light-home :uri "/xbmcl-home") ()
  (fdo-shell (curl-cmd "Input.Home"))
  (xbmc-light-remote-page))

(define-easy-handler (xbmc-light-select :uri "/xbmcl-select") ()
  (fdo-shell (curl-cmd "Input.Select"))
  (xbmc-light-remote-page))

(define-easy-handler (xbmc-light-page_down :uri "/xbmcl-page_down") ()
  (dotimes (i 10)
    (fdo-shell (curl-cmd "Input.Down")))
  (xbmc-light-remote-page))

(define-easy-handler (xbmc-light-page_up :uri "/xbmcl-page_up") ()
  (dotimes (i 10)
    (fdo-shell (curl-cmd "Input.Up")))
  (xbmc-light-remote-page))

(define-easy-handler (xbmc-light-context :uri "/xbmcl-context") ()
  (fdo-shell (curl-cmd "Input.ContextMenu"))
  (xbmc-light-remote-page))

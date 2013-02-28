(in-package :cl-webfilemanager)

(defun split-string (string &optional (separator #\Space))
  "Return a list from a string splited at each separators"
  (loop for i = 0 then (1+ j)
     as j = (position separator string :start i)
     as sub = (subseq string i j)
     unless (string= sub "") collect sub
     while j))


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

(defun limit-char (string &optional (limit 13))
  (let ((len (length string)))
    (if (> len limit)
        (format nil "~A___" (subseq string 0 limit))
        string)))


(defun directory-to-pathname (dir)
  (make-pathname :directory dir))

(defun pathname-name-type (pathname)
  (format nil "~A~A"
          (pathname-name pathname)
          (if (pathname-type pathname)
              (format nil ".~A" (pathname-type pathname))
              "")))

(defun extract-all-pathname (pathname)
  (let ((dir (pathname-directory pathname))
        (acc nil))
    (dotimes (i (length dir))
      (push (directory-to-pathname (subseq dir 0 (1+ i))) acc))
    (nreverse acc)))



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


(defun to-string (list)
  (format nil "~S" list))

(in-package :cl-webfilemanager)

(defun split-string (string &optional (separator #\Space))
  "Return a list from a string splited at each separators"
  (loop for i = 0 then (1+ j)
     as j = (position separator string :start i)
     as sub = (subseq string i j)
     unless (string= sub "") collect sub
     while j))


(declaim (inline elast))
(defun elast (list)
  "Return the last element in list"
  (first (last list)))

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
  (let ((dir (pathname-directory (pathname-as-directory pathname)))
        (acc nil))
    (dotimes (i (length dir))
      (push (directory-to-pathname (subseq dir 0 (1+ i))) acc))
    (nreverse acc)))


(defun last-directory-path (pathname)
  (let ((dir (elast (pathname-directory (pathname-as-directory pathname)))))
    (if (eql dir :absolute) "Root" dir)))



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
  (or (not (pathname-name file))
      (eql #\. (char (pathname-name file) 0))))

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
    (dolist (item (list-directory dirname :follow-symlinks nil))
      (if (directory-pathname-p item)
          (when (or show-hidden (not (is-hidden-directory item)))
            (push item acc-dir))
          (when (or show-hidden (and (not (is-hidden-file item)) (not (is-hidden-directory item))))
            (push item acc-file))))
    (values (sort-file acc-dir) (sort-file acc-file))))


(defun to-string (list)
  (format nil "~S" list))


;;; Shell part (taken from ltk)
(defun do-execute (program args &optional (wt nil) (io :stream))
  "execute program with args a list containing the arguments passed to
the program   if wt is non-nil, the function will wait for the execution
of the program to return.
   returns a two way stream connected to stdin/stdout of the program"
  #-CLISP (declare (ignore io))
  (let ((fullstring program))
    (dolist (a args)
      (setf fullstring (concatenate 'string fullstring " " a)))
    #+:cmu (let ((proc (ext:run-program program args :input :stream :output :stream :wait wt)))
             (unless proc
               (error "Cannot create process."))
             (make-two-way-stream
              (ext:process-output proc)
              (ext:process-input proc)))
    #+:clisp (ext:run-program program :arguments args :input io :output io :wait wt)
    #+:sbcl (let ((proc (sb-ext:run-program program args :input :stream :output :stream :wait wt)))
	      (unless proc
		(error "Cannot create process."))
	      (make-two-way-stream
	       (sb-ext:process-output proc)
	       (sb-ext:process-input proc)))
    #+:lispworks (system:open-pipe fullstring :direction :io)
    #+:allegro (let ((proc (excl:run-shell-command
			    (apply #'vector program program args)
			    :input :stream :output :stream :wait wt)))
		 (unless proc
		   (error "Cannot create process."))
		 proc)
    #+:ecl (ext:run-program program args :input :stream :output :stream
                            :error :output)
    #+:openmcl (let ((proc (ccl:run-program program args :input
							 :stream :output
							 :stream :wait wt)))
		 (unless proc
		   (error "Cannot create process."))
		 (make-two-way-stream
		  (ccl:external-process-output-stream proc)
		  (ccl:external-process-input-stream proc)))))

(defun do-shell (program &optional args (wait nil) (io :stream))
  (do-execute "/bin/sh" `("-c" ,program ,@args) wait io))

(defun fdo-shell (formatter &rest args)
  (do-shell (apply #'format nil formatter args)))

(defun do-shell-output (formatter &rest args)
  (let ((output (do-shell (apply #'format nil formatter args) nil t)))
    (loop for line = (read-line output nil nil)
       while line
       collect line)))



(defun getenv (var)
  "Return the value of the environment variable."
  #+allegro (sys::getenv (string var))
  #+clisp (ext:getenv (string var))
  #+(or cmu scl)
  (cdr (assoc (string var) ext:*environment-list* :test #'equalp
              :key #'string))
  #+gcl (si:getenv (string var))
  #+lispworks (lw:environment-variable (string var))
  #+lucid (lcl:environment-variable (string var))
  #+(or mcl ccl) (ccl::getenv var)
  #+sbcl (sb-posix:getenv (string var))
  #+ecl (si:getenv (string var))
  #-(or allegro clisp cmu gcl lispworks lucid mcl sbcl scl ecl ccl)
  (error 'not-implemented :proc (list 'getenv var)))


(defun (setf getenv) (val var)
  "Set an environment variable."
  #+allegro (setf (sys::getenv (string var)) (string val))
  #+clisp (setf (ext:getenv (string var)) (string val))
  #+(or cmu scl)
  (let ((cell (assoc (string var) ext:*environment-list* :test #'equalp
							 :key #'string)))
    (if cell
        (setf (cdr cell) (string val))
        (push (cons (intern (string var) "KEYWORD") (string val))
              ext:*environment-list*)))
  #+gcl (si:setenv (string var) (string val))
  #+lispworks (setf (lw:environment-variable (string var)) (string val))
  #+lucid (setf (lcl:environment-variable (string var)) (string val))
  #+sbcl (sb-posix:putenv (format nil "~A=~A" (string var) (string val)))
  #+ecl (si:setenv (string var) (string val))
  #+ccl (ccl::setenv (string var) (string val))
  #-(or allegro clisp cmu gcl lispworks lucid sbcl scl ecl ccl)
  (error 'not-implemented :proc (list '(setf getenv) var)))


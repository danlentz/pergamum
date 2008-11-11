;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PERGAMUM; Base: 10 -*-
;;;

(in-package :pergamum)

(define-condition pathname-busy (file-error) ())

(define-condition pathname-not-present (file-error) ())

(defun subfile (directory-pathname sub)
  "Return a file pathname with name SUB in DIRECTORY-PATHNAME."
  (merge-pathnames (make-pathname :name sub) directory-pathname))

(defun subdirectory (directory-pathname sub)
  "Return a subdirectory pathname with name SUB in DIRECTORY-PATHNAME."
  (merge-pathnames (make-pathname :directory `(:relative ,sub)) directory-pathname))

(defun change-directory (pathname)
  "Change both Lisp's and POSIX's ideas of the current directory."
  (declare (type pathname pathname))
  (setf *default-pathname-defaults* pathname)
  (zerop (sb-posix:chdir (namestring pathname))))

(defmacro within-directory ((dirvar dirform &key (when-exists :continue) (when-does-not-exist :error)) &body body)
  "Execute BODY with both the Lisp's and POSIX's 
   ideas of current directory set to DIRVAR,
   which is bound to the value of DIRFORM.
   Defined keywords:
    :IF-EXISTS - one of :ERROR or :CONTINUE
    :IF-DOES-NOT-EXIST - one of :ERROR or :CREATE
  See the manual for details."
  (flet ((wrap-body (dirsym oldsym body)
           `(let ((,oldsym (sb-posix:getcwd))
                  (*default-pathname-defaults* (parse-namestring ,dirsym)))
              (sb-posix:chdir ,dirsym)
              (unwind-protect (progn ,@body)
                (sb-posix:chdir ,oldsym)))))
    (with-gensyms (old exists-p)
      `(if-let* ((,dirvar ,dirform)
                 (,exists-p (directory-exists-p ,dirvar)))
                ,(ecase when-exists
                        (:continue (wrap-body dirvar old body))
                        (:error `(error 'pathname-busy :pathname ,dirvar)))
                ,(ecase when-does-not-exist
                        (:create `(progn
                                    (ensure-directories-exist ,dirvar)
                                    ,(wrap-body dirvar old body)))
                        (:error `(error 'pathname-not-present :pathname ,dirvar)))))))

(defun file-as-vector (filename &rest rest &key (element-type '(unsigned-byte 8)) &allow-other-keys)
  "Return contents of FILENAME as simple vector with element type (UNSIGNED-BYTE 8)."
  (with-open-stream (s (apply #'open filename :element-type element-type (remove-from-plist rest :element-type)))
    (stream-as-vector s (file-length s) :element-type element-type)))

(defun file-as-string (filename &rest rest &key (element-type 'character) &allow-other-keys)
  "Return contents of FILENAME as simple vector with element type CHARACTER."
  (with-open-stream (s (apply #'open filename :element-type element-type (remove-from-plist rest :element-type)))
    (stream-as-vector s (file-length s) :element-type element-type)))

(defmacro with-output-to-file ((stream filespec &rest options &key (if-does-not-exist :create) (if-exists :supersede)) &body body)
  "Like WITH-OPEN-FILE, but with defaults conveniently set for file creation/overwriting."
  `(with-open-file (,stream ,filespec :direction :output :if-does-not-exist ,if-does-not-exist :if-exists ,if-exists
                            ,@(remove-from-plist options :direction :if-does-not-exist :if-exists))
     ,@body))
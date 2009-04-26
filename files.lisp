;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PERGAMUM; Base: 10 -*-
;;;

(in-package :pergamum)

(define-condition pathname-busy (file-error) ())

(define-condition pathname-not-present (file-error) ())

(defun posix-working-directory ()
  "Return the POSIX idea of the current working directory."
  #+sbcl (sb-posix:getcwd)
  #+ecl (si:getcwd))

(defun set-posix-working-directory (pathname)
  "Change the POSIX view of the current directory."
  (zerop (#+sbcl sb-posix:chdir
          #+ecl  si:chdir
                 pathname)))

(defsetf posix-working-directory set-posix-working-directory)

(defmacro within-directory ((directory-form &key (as (gensym)) (lisp t) (posix t) (if-exists :continue) (if-does-not-exist :error)) &body body)
  "Execute BODY with the idea of current directory changed to the
   value of DIRECTORY-FORM, which must evaluate to a string, which is 
   optionally captured in the binding of a variable named by AS.

   Defined keywords:
    :AS - a symbol, optionally specifying the name of the variable
          to be bound to the value of DIRECTORY-FORM
    :LISP - a boolean, indicating a request to bind *D-P-D*
    :POSIX - a boolean, indicating a request to change POSIX's idea
             of the current directory
    :IF-EXISTS - one of :ERROR or :CONTINUE
    :IF-DOES-NOT-EXIST - one of :ERROR or :CREATE
   See the manual for details."
  (flet ((wrap-body (dirsym oldsym body)
           `(let (,@(when posix `((,oldsym (posix-working-directory))))
                  ,@(when lisp `((*default-pathname-defaults* (parse-namestring ,dirsym)))))
              ,@(if posix
                    `((set-posix-working-directory ,dirsym)
                      (unwind-protect (progn ,@body)
                        (set-posix-working-directory ,oldsym)))
                    body))))
    (with-gensyms (old)
      `(let ((,as ,directory-form))
         (if (directory-exists-p ,as)
             ,(ecase if-exists
                     (:continue (wrap-body as old body))
                     (:error `(error 'pathname-busy :pathname ,as)))
             ,(ecase if-does-not-exist
                     (:create `(progn
                                 (ensure-directories-exist ,as)
                                 ,(wrap-body as old body)))
                     (:error `(error 'pathname-not-present :pathname ,as))))))))

(defun file-as-vector (filename &rest rest &key (element-type '(unsigned-byte 8)) &allow-other-keys)
  "Return contents of FILENAME as simple vector with element type (UNSIGNED-BYTE 8)."
  (with-open-stream (s (apply #'open filename :element-type element-type (remove-from-plist rest :element-type)))
    (stream-as-vector s (file-length s) :element-type element-type)))

(defun file-as-string (filename &rest rest &key (element-type 'character) &allow-other-keys)
  "Return contents of FILENAME as simple vector with element type CHARACTER."
  (with-open-stream (s (apply #'open filename :element-type element-type (remove-from-plist rest :element-type)))
    (stream-as-vector s (file-length s) :element-type element-type)))

(defmacro with-output-to-file ((stream filespec &rest options &key (if-does-not-exist :create) (if-exists :supersede) &allow-other-keys) &body body)
  "Like WITH-OPEN-FILE, but with defaults conveniently set for file creation/overwriting."
  `(with-open-file (,stream ,filespec :direction :output :if-does-not-exist ,if-does-not-exist :if-exists ,if-exists
                            ,@(remove-from-plist options :direction :if-does-not-exist :if-exists))
     ,@body))

(defun symlink-to-p (symlink path)
  "See if SYMLINK does point at PATH."
  (if-let ((destination (file-exists-p symlink)))
    (pathname-match-p symlink path)))

(defun symlink-target-file-present-p (symlink)
  "See if the file SYMLINK points at exists, and return that."
  (when-let ((destination (file-exists-p symlink)))
    (file-exists-p destination)))

(defun ensure-symlink (symlink target)
  "Ensure that file at SYMLINK is a symbolic link pointing to TARGET."
  #-win32
  (unless (symlink-to-p symlink target)
    (when (file-exists-p symlink)
      (delete-file symlink))
    #+sbcl (sb-posix:symlink target symlink)
    t)
  #+win32 nil)

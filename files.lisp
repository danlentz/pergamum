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

(defun check-pathname-not-occupied (pathname)
  "Check that PATHNAME is not occupied, and raise a error of type
PATHNAME-BUSY otherwise."
  (or (not (file-exists-p pathname))
      (error 'pathname-busy :pathname pathname)))

(defmacro within-directory ((directory-form &key (lisp t) (posix t) (if-exists :continue) (if-does-not-exist :error)) &body body)
  "Evaluate DIRECTORY-FORM, and then execute BODY with the idea of
current directory changed to the resulting value, which must
be a pathname specifier -- either a pathname, or a namestring.

Defined keywords:
 :LISP - a boolean, indicating a request to bind *D-P-D*
 :POSIX - a boolean, indicating a request to change POSIX's idea
          of the current directory
 :IF-EXISTS - one of :ERROR or :CONTINUE
 :IF-DOES-NOT-EXIST - one of :ERROR or :CREATE
See the manual for details.

Within the lexical context established by the WITHIN-DIRECTORY form,
 (DIRECTORY-EXISTED-P) evaluates to a boolean value representing
whether the directory had to be created, when IF-DOES-NOT-EXIST
is :CREATE. (DIRECTORY-CREATED-P) evaluates to the negation of that
value."
  (with-gensyms (existsp directory old)
    (once-only (directory-form)
      `(let ((,existsp (directory-exists-p ,directory-form)))
         (macrolet ((directory-created-p () `(not ,',existsp))
                    (directory-existed-p () ',existsp))
           (flet ((invoke-within-directory (,directory)
                    (let (,@(when posix `((,old (posix-working-directory))))
                          ,@(when lisp `((*default-pathname-defaults* (parse-namestring ,directory)))))
                      ,@(if posix
                            `((set-posix-working-directory ,directory)
                              (unwind-protect (progn ,@body)
                                (set-posix-working-directory ,old)))
                            body))))
             (if ,existsp
                 ,(ecase if-exists
                         (:continue `(invoke-within-directory ,directory-form))
                         (:error `(error 'pathname-busy :pathname ,directory-form)))
                 ,(ecase if-does-not-exist
                         (:create `(progn
                                     (ensure-directories-exist ,directory-form)
                                     (invoke-within-directory ,directory-form)))
                         (:error `(error 'pathname-not-present :pathname ,directory-form))))))))))

(defun invoke-maybe-within-directory (fn &optional directory)
  "Invoke FN, possibly, when DIRECTORY is non-NIL, within context
established by WITHIN-DIRECTORY."
  (if directory
      (within-directory (directory)
        (funcall fn))
      (funcall fn)))

(defmacro maybe-within-directory (directory &body body)
  "Execute BODY, possibly, when DIRECTORY is non-NIL, within context
established by WITHIN-DIRECTORY."
  `(invoke-maybe-within-directory (lambda () ,@body) ,directory))

(defun file-as-vector (filename &rest rest &key (element-type '(unsigned-byte 8)) position &allow-other-keys)
  "Return contents of FILENAME, starting from POSITION, as simple vector
with ELEMENT-TYPE, defaulting to (UNSIGNED-BYTE 8)."
  (with-open-stream (s (apply #'open filename :element-type element-type (remove-from-plist rest :element-type :position)))
    (stream-as-vector s (file-length s) :element-type element-type :position position)))

(defun file-as-string (filename &rest rest &key (element-type 'character) position &allow-other-keys)
  "Return contents of FILENAME, starting from POSITION, as simple vector
with ELEMENT-TYPE, defaulting to CHARACTER."
  (with-open-stream (s (apply #'open filename :element-type element-type (remove-from-plist rest :element-type :position)))
    (stream-as-vector s (file-length s) :element-type element-type :position position)))

(defun file-line (filename &rest rest &key (element-type 'character) position &allow-other-keys)
  "Return the first line of FILENAME, starting from POSITION, as simple vector
with ELEMENT-TYPE, defaulting to CHARACTER."
  (with-open-stream (s (apply #'open filename :element-type element-type (remove-from-plist rest :element-type :position)))
    (when position
      (file-position s position))
    (read-line s nil nil)))

(defmacro with-output-to-file ((stream filespec &rest options &key (if-does-not-exist :create) (if-exists :supersede) &allow-other-keys) &body body)
  "Like WITH-OPEN-FILE, but with defaults conveniently set for file creation/overwriting."
  `(with-open-file (,stream ,filespec :direction :output :if-does-not-exist ,if-does-not-exist :if-exists ,if-exists
                            ,@(remove-from-plist options :direction :if-does-not-exist :if-exists))
     ,@body))

(defun rename-to-directory (pathname target-directory)
  "Unfortunately, RENAME-FILE is underwhelming, when moving directories."
  (if (pathname-name pathname)
      (rename-file (namestring pathname) (namestring (make-pathname :directory (pathname-directory target-directory) :name (pathname-name pathname))))
      ;; We rely on that RENAME-FILE works on directories.  Known to be true on SBCL/linux and CCL/linux.
      ;; In fact, this is weakly supported by CLHS:
      ;; "It is an error [...] for filespec to contain a nil component where the file system does not permit a nil component..."
      (rename-file (namestring pathname) (namestring (make-pathname :directory (append (pathname-directory target-directory) (list (lastcar (pathname-directory pathname)))))))))

(defun remove-file (p)
  #+sbcl (sb-posix:unlink p)
  #-sbcl (error "~@<Not implemented: REMOVE-FILE.~:@>"))

(defun symlink-to-p (symlink target)
  "See if SYMLINK does point at TARGET."
  (if-let ((destination (file-exists-p symlink)))
    (pathname-match-p destination target)))

(defun make-symlink (symlink target)
  #+(and sbcl (not win32)) (sb-posix:symlink target symlink)
  #+(or (not sbcl) win32) (error "~@<Not implemented: MAKE-SYMLINK.~:@>"))

(defun symlink-target-file (symlink)
  "See if the file SYMLINK points at exists, and return that."
  (when-let ((destination (file-exists-p symlink)))
    (when (not (pathname-match-p destination symlink))
      destination)))

(defun ensure-symlink (symlink target)
  "Ensure that file at SYMLINK is a symbolic link pointing to TARGET."
  (unless (symlink-to-p symlink target)
    (when (file-exists-p symlink)
      (remove-file symlink))
    (make-symlink symlink target)
    t))

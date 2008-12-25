(in-package :pergamum)


(defun subfile (directory-pathname sub &rest make-pathname-keywords)
  "Return a file pathname with name SUB in DIRECTORY-PATHNAME.
   MAKE-PATHNAME-KEYWORDS are passed to MAKE-PATHNAME."
  (merge-pathnames (apply #'make-pathname :directory `(:relative ,@(butlast sub)) :name (lastcar sub) make-pathname-keywords) directory-pathname))

(defun subfile* (directory-pathname &rest sub)
  "Return a file pathname with name SUB in DIRECTORY-PATHNAME."
  (merge-pathnames (make-pathname :directory `(:relative ,@(butlast sub)) :name (lastcar sub)) directory-pathname))

(defun subdirectory (directory-pathname sub &rest make-pathname-keywords)
  "Return a subdirectory pathname with name SUB in DIRECTORY-PATHNAME.
   MAKE-PATHNAME-KEYWORDS are passed to MAKE-PATHNAME."
  (merge-pathnames (apply #'make-pathname :directory `(:relative ,@sub) make-pathname-keywords) directory-pathname))

(defun subdirectory* (directory-pathname &rest sub)
  "Return a subdirectory pathname with name SUB in DIRECTORY-PATHNAME."
  (merge-pathnames (make-pathname :directory `(:relative ,@sub)) directory-pathname))

(defun flatten-path-list (path &optional absolute)
  "Transform a list of strings in PATH into a string constituting of 
   individual strings interspersed with slashes.
   A leading slash is prepended when ABSOLUTE is non-nil."
  (apply #'concatenate 'simple-base-string
         (xform (not absolute) #'rest (mapcan (curry #'list "/") path))))

(defun fuse-downcased-string-path-list (path &optional absolute)
  "Transform a list of strings in PATH into a string constituting of 
   individual strings interspersed with slashes.
   A leading slash is prepended when ABSOLUTE is non-nil."
  (apply #'concatenate 'simple-base-string
         (xform (not absolute) #'rest (mapcan (compose (curry #'list "/") #'string-downcase #'string) path))))

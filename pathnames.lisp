(in-package :pergamum)


(defun subfile (directory-pathname sub)
  "Return a file pathname with name SUB in DIRECTORY-PATHNAME."
  (merge-pathnames (make-pathname :name sub) directory-pathname))

(defun subdirectory (directory-pathname sub)
  "Return a subdirectory pathname with name SUB in DIRECTORY-PATHNAME."
  (merge-pathnames (make-pathname :directory `(:relative ,sub)) directory-pathname))

(defun flatten-path-list (path &optional absolute)
  "Transform a list of strings in PATH into a string constituting of 
   individual strings interspersed with slashes.
   A leading slash is prepended when ABSOLUTE is non-nil."
  (apply #'concatenate 'simple-base-string
         (xform (not absolute) #'rest (mappend (curry #'list "/") path))))

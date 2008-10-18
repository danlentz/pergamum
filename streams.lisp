(in-package :pergamum)

(defun stream-as-vector (stream &optional (size (file-length stream)) &key (element-type '(unsigned-byte 8)))
  (lret ((vector (make-array size :element-type element-type)))
    (read-sequence vector stream :end size)))

(defun file-as-vector (filename &rest rest &key (element-type '(unsigned-byte 8)) &allow-other-keys)
  (with-open-stream (s (apply #'open filename :element-type element-type rest))
    (stream-as-vector s (file-length s) :element-type element-type)))

(defmacro with-output-to-file ((stream filespec &rest options &key (if-does-not-exist :create) (if-exists :supersede)) &body body)
  "Like WITH-OPEN-FILE, but with defaults conveniently set for file creation/overwriting."
  `(with-open-file (,stream ,filespec :direction :output :if-does-not-exist ,if-does-not-exist :if-exists ,if-exists
                            ,@(remove-from-plist options :direction :if-does-not-exist :if-exists))
     ,@body))

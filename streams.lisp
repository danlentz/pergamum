(in-package :pergamum)

(defun stream-as-vector (stream &optional (size (file-length stream)) &key (element-type '(unsigned-byte 8)))
  (lret ((vector (make-array size :element-type element-type)))
    (read-sequence vector stream :end size)))

(defun file-as-vector (filename &rest rest &key (element-type '(unsigned-byte 8)) &allow-other-keys)
  (with-open-stream (s (apply #'open filename :element-type element-type rest))
    (stream-as-vector s (file-length s) :element-type element-type)))

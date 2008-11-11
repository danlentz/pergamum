;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PERGAMUM; Base: 10 -*-
;;;

(in-package :pergamum)

(defun stream-as-vector (stream &optional (size (file-length stream)) &key (element-type '(unsigned-byte 8)))
  (lret ((vector (make-array size :element-type element-type)))
    (read-sequence vector stream :end size)))

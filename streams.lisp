;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PERGAMUM; Base: 10 -*-
;;;

(in-package :pergamum)

(defun stream-as-vector (stream &optional (size (file-length stream)) &key (element-type '(unsigned-byte 8)))
  (lret ((vector (make-array size :element-type element-type)))
    (read-sequence vector stream)))

(defun all-stream-forms (stream &aux (eof-marker (gensym "EOF")) (*read-eval* nil))
  "Read forms from STREAM, until its end."
  (iter (for form = (read stream nil eof-marker))
        (until (eq form eof-marker))
        (collect form)))

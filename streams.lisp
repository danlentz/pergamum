;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PERGAMUM; Base: 10 -*-
;;;

(in-package :pergamum)

(defun stream-as-vector (stream &optional (size (file-length stream)) &key (element-type '(unsigned-byte 8)) position)
  "Return contents of STREAM, starting from POSITION, as simple vector
with ELEMENT-TYPE defaulting to (UNSIGNED-BYTE 8)."
  (lret ((vector (make-array size :element-type element-type)))
    (when position
      (unless (file-position stream position)
        (error "~@<Failed to seek to position ~S of stream ~S.~:@>" position stream)))
    (read-sequence vector stream)))

(defun stream-as-string (stream &optional (size (file-length stream)) &key (element-type 'character) position)
  "Return contents of STREAM, starting from POSITION, as simple vector
with ELEMENT-TYPE defaulting to CHARACTER."
  (stream-as-vector stream size :element-type element-type :position position))

(defun all-stream-forms (stream &aux (eof-marker (gensym "EOF")) (*read-eval* nil))
  "Read forms from STREAM, until its end."
  (iter (for form = (read stream nil eof-marker))
        (until (eq form eof-marker))
        (collect form)))

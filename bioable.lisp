;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PERGAMUM; Base: 10 -*-
;;;

(in-package :pergamum)

(defclass bioable ()
  ((range :accessor bioable-range :type extent-spec :initarg :range)))

(define-print-object-method ((o bioable) range)
    "~@<#<BIOABLE~; range: ~/pergamum:print-extent-spec/~;>~:@>" range)

(defgeneric read-block (bioable base vector &optional start end)
  (:documentation
   "Read bytes from BIOABLE at BASE into an extent of VECTOR, marked by
START, defaulting to 0, and END, defaulting to NIL and interpreted as
end of the vector.")
  (:method :around ((o bioable) base vector &optional (start 0) end)
    (call-next-method o base vector start (or end (length vector)))))

(defgeneric write-block (bioable base vector &optional start end)
  (:documentation
   "Write bytes from an extent of VECTOR into BIOABLE at BASE. The extent
of vector is marked by START, defaulting to 0, and END, defaulting to NIL
and interpreted as end of the vector.")
  (:method :around ((o bioable) base vector &optional (start 0) end)
    (call-next-method o base vector start (or end (length vector)))))

(defgeneric read-blocks (bioable extent-specs)
  (:method ((o bioable) (extent-specs list))
    (iter (for spec in extent-specs)
          (for iovec = (make-array (cdr spec) :element-type '(unsigned-byte 8)))
          (read-block o (car spec) iovec)
          (collect iovec))))

(defgeneric u8-extent (bioable extent-spec)
  (:method ((o bioable) (extent extent))
    (u8-extent o (extent-spec extent)))
  (:method ((o bioable) (spec cons))
    (make-extent 'extent (car spec) (lret ((iovec (make-array (cdr spec) :element-type '(unsigned-byte 8))))
                                      (read-block o (car spec) iovec)))))
(defgeneric write-u8-extent (bioable extent)
  (:method ((o bioable) (extent extent))
    (write-block o (extent-base extent) (extent-data extent))))
(defsetf u8-extent write-u8-extent)

(defgeneric u8-extents (bioable extent-specs)
  (:method ((o bioable) extent-specs)
    (iter (for exspec in extent-specs)
          (collect (u8-extent o exspec)))))

(defgeneric write-u8-extents (bioable extents &key preserve-around before-fn stream)
  (:documentation
   "Write a list of EXTENTS into BIOABLE.
When PRESERVE-AROUNDS is non-NIL, specific methods attempt, if applicable, 
to preserve the data surrounding the written BIOABLE's regions.
The generic method specifically does not try to do that.
BEFORE-FN, when non-NIL, is called before each extent write
with two arguments: the output STREAM and extent.")
  (:method ((o bioable) (extents list) &key preserve-around before-fn (stream t))
    (declare (ignore preserve-arounds))
    (dolist (e extents)
      (funcall (or before-fn #'values) e stream)
      (write-block o (extent-base e) (extent-data e)))
    extents))

(defsetf u8-extents write-u8-extents)

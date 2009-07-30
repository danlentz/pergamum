;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PERGAMUM; Base: 10 -*-
;;;

(in-package :pergamum)

(defclass extentable ()
  ((range :accessor extentable-range :type extent-spec :initarg :range)))

(defmethod print-object ((extentable extentable) stream)
  (format stream "~@<#<EXTENTABLE~; range: ~/pergamum:print-extent-spec/~;>~:@>"
          (slot-value extentable 'range)))

(defclass subextent ()
  ((parent :accessor subextent-parent :type extentable :initarg :parent)
   (extent :accessor subextent-extent :type cons :initarg :extent)))

(defmethod print-object ((subextent subextent) stream)
  (format stream "~@<#<SUBEXTENT~; extent: ~S  parent: ~S~;>~:@>"
          (slot-value subextent 'extent) (slot-value subextent 'parent)))

(defun subextent-absolutize (subex val)
  (+ val (extent-spec-base (subextent-extent subex))))

(defun subextent-relativize (subex val)
  (- val (extent-spec-base (subextent-extent subex))))

(defgeneric u8-extent (extentable extent-spec))
(defgeneric set-u8-extent (extentable extent-spec extent))
(defsetf u8-extent set-u8-extent)

(defun (setf extentable-u8-vector) (vector extentable base)
  (set-u8-extent extentable (cons base (length vector)) (make-extent 'extent base vector)))

(defgeneric write-u8-extents (extentable extents &optional preserve-around)
  (:documentation
   "")
  #+nil
  (:method ((o extentable) (extents list))
    (dolist (e extents)
      (set-u8-extent o (extent (extent-base e) (extent-length e)) e))))

(defgeneric u8-extents (extentable extent-specs)
  (:method ((o extentable) extent-specs)
    (iter (for exspec in extent-specs)
          (collect (u8-extent o exspec)))))

(defsetf u8-extents write-u8-extents)

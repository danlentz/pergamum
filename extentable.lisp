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
  (set-u8-extent extentable (cons base (length vector)) (make-extent 'extent :base base :vector vector)))

(defun u8-extent-list (extentable extent-list-spec)
  (make-instance 'u8-extent-list
                 :extents (mapcar (curry #'u8-extent extentable) extent-list-spec)))

(defun set-u8-extent-list (extentable extent-list-spec extent-list)
  (declare (u8-extent-list extent-list))
  (unless (extent-list-matches-spec-p extent-list-spec extent-list)
    (error 'extent-list-spec-mismatch :extent-list extent-list :spec extent-list-spec))
  (mapc (curry #'set-u8-extent extentable) extent-list-spec (extent-list-extents extent-list))
  extent-list)

(defsetf u8-extent-list set-u8-extent-list)
(in-package :pergamum)

(defclass extentable ()
  ((range :accessor extentable-range :type extent-spec :initarg :range)))

(defmethod print-object ((extentable extentable) stream)
  (format stream "~@<#<EXTENTABLE~; range: ~/pergamum:print-extent-spec/~;>~:@>"
          (slot-value extentable 'range)))

(defclass subextent ()
  ((parent :accessor subextent-parent :type extentable :initarg :parent)
   (base :accessor subextent-base :type (integer 0) :initarg :base)))

(defmethod print-object ((subextent subextent) stream)
  (format stream "~@<#<SUBEXTENT~; base: ~S  parent: ~S~;>~:@>"
          (slot-value subextent 'base) (slot-value subextent 'parent)))

(defgeneric u8-extent (extentable extent-spec))
(defgeneric set-u8-extent (extentable extent-spec extent))
(defsetf u8-extent set-u8-extent)

(defun (setf extentable-u8-vector) (vector extentable base)
  (set-u8-extent extentable (cons base (length vector)) (cons base vector)))

(defun u8-extent-list (extentable extent-list-spec)
  (make-instance 'u8-extent-list
                 :extents (mapcar (curry #'u8-extent extentable) extent-list-spec)))

(defun set-u8-extent-list (extentable extent-list-spec extent-list)
  (declare (u8-extent-list extent-list))
  (unless (extent-list-matches-spec-p extent-list-spec extent-list)
    (error 'extent-list-spec-mismatch :extent-list extent-list :spec extent-list-spec))
  (mapcar (curry #'set-u8-extent extentable) extent-list-spec (extent-list-extents extent-list))
  extent-list)

(defsetf u8-extent-list set-u8-extent-list)
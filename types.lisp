;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PERGAMUM; Base: 10 -*-
;;;

(in-package :pergamum)

(defun coerce-to-sequence (o)
  "Given an Object, return the corresponding sequence."
  (etypecase o
    (sequence o)
    (extent (extent-data o))))

(defun remove-if-not-subtype-of (type types)
  "Given a list of TYPES, return only those which are subtype to TYPE."
  (remove-if-not (rcurry #'subtypep type) types))

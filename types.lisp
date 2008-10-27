;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PERGAMUM; Base: 10 -*-
;;;

(in-package :pergamum)

(defun coerce-to-sequence (o)
  (etypecase o
    (vector o)
    (extent (extent-data o))))

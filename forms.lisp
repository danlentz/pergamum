;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PERGAMUM; Base: 10 -*-
;;;

(in-package :pergamum)

(defun quoted-p (form)
  (and (consp form) (eq (car form) 'quote)))

(defun quoted-form (quoted)
  (second quoted))
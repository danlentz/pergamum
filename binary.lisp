;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PERGAMUM; Base: 10 -*-
;;;

(in-package :pergamum)

(define-modify-macro logandf (&rest rest) logand)
(define-modify-macro logiorf (&rest rest) logior)
(define-modify-macro logandcf (param) logandc2)
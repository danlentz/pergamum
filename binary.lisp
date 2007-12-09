(in-package :pergamum)

(define-modify-macro logandf (&rest rest) logand)
(define-modify-macro logiorf (&rest rest) logior)
(define-modify-macro logandcf (param) logandc2)
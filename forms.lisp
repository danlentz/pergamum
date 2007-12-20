(in-package :pergamum)

(defun quoted-p (form)
  (and (consp form) (eq (car form) 'quote)))

(defun quoted-form (quoted)
  (second quoted))
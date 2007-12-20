(in-package :pergamum)

(defun quoted-p (form)
  (and (consp form) (eq (car form) 'quote)))

(defun quoted-forms (quoted)
  (cdr quoted))
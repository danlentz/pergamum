(in-package :pergamum)

(defun make-condition-raiser (type &rest params)
  (lambda (&rest rest)
    (declare (ignore rest))
    (signal type params)))

(defun make-error-raiser (type &rest params)
  (lambda (&rest rest)
    (declare (ignore rest))
    (error type params)))
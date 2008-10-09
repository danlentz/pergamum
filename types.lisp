(in-package :pergamum)

(defun coerce-to-sequence (o)
  (etypecase o
    (vector o)
    (extent (extent-data o))))

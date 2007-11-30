(in-package :pergamum)

(defun hash-table-next (h)
  (with-hash-table-iterator (f h)
    (f)))
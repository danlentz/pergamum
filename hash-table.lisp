(in-package :pergamum)

(defun hash-table-next (h)
  (with-hash-table-iterator (f h)
    (f)))

(defun hash-table-key-present-p (h k)
  (nth 1 (gethash k h)))
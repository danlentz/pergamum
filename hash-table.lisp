(in-package :pergamum)

(defun hash-table-next (h)
  (with-hash-table-iterator (f h)
    (f)))

(defun hash-table-pop (h)
  (multiple-value-bind (has-p key value) (hash-table-next h)
    (when has-p
      (remhash key h)
      (values has-p key value))))

(defmacro hash-table-itearate ((key value) hash-table &body body)
  (once-only (hash-table)
    (with-gensyms (value-p)
      `(iter (for (values ,value-p ,key ,value) = (hash-table-pop ,hash-table))
             (while ,value-p)
             ,@body))))

(defun hash-table-key-present-p (h k)
  (nth 1 (gethash k h)))
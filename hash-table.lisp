(in-package :pergamum)

(defun hash-table-next (hash-table)
  "Return a key/value pair from the HASH-TABLE, in a present-p/key/value value triplet."
  (with-hash-table-iterator (f hash-table)
    (f)))

(defun hash-table-pop (hash-table)
  "Return a key/value pair from the HASH-TABLE, in a present-p/key/value value triplet,
   while removing that association from HASH-TABLE."
  (multiple-value-bind (has-p key value) (hash-table-next hash-table)
    (when has-p
      (remhash key hash-table)
      (values has-p key value))))

(defmacro hash-table-itearate ((key value) hash-table &body body)
  "Destructively iterate over HASH-TABLE associations, by picking a key/value pair,
   removing that association from the hash-table, and binding KEY and VALUE for
   the iteration body on each iteration step."
  (once-only (hash-table)
    (with-gensyms (value-p)
      `(iter (for (values ,value-p ,key ,value) = (hash-table-pop ,hash-table))
             (while ,value-p)
             ,@body))))

(defun make-hash-table-injector (hash-table)
  "Given a HASH-TABLE, return an injector function accepting KEY and VALUE."
  (lambda (k v)
    (setf (gethash k hash-table) v)))

(defun hash-table-key-present-p (hash-table key)
  "Given a HASH-TABLE and a KEY, return an injector function accepting KEY and VALUE."
  (nth 1 (gethash key hash-table)))

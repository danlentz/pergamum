(in-package :pergamum)

(defparameter *marred* nil)

(defun mar (&rest syms)
  (setf *marred* (remove-duplicates (nunion *marred* syms))))

(defun export-unmarred (pred &optional (package *package*) (marred *marred*))
  (let (syms)
    (do-symbols (sym package)
      (when (and (funcall pred sym) (eq (symbol-package sym) package))
        (push sym syms)))
    (export (set-difference syms marred) package)))

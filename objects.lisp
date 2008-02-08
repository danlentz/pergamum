(in-package :pergamum)

(defun copy-slots (from to slots)
  (dolist (slot slots)
    (setf (slot-value to slot) (slot-value from slot))))

(define-compiler-macro copy-slots (&whole whole from to slots)
  (if (quoted-p slots)
      (once-only (from to) 
        `(setf ,@(iter (for slot in (quoted-form slots))
                       (nconcing `((slot-value ,to ',slot) (slot-value ,from ',slot))))))
      whole))

(defun slot-value* (o slot-name &optional (default :unbound-slot))
  (if (slot-boundp o slot-name)
      (slot-value o slot-name)
      default))
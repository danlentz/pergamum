(in-package :pergamum)

(defun make-queue ()
  "Make a PAIP queue."
  (lret ((q (cons nil nil)))
    (setf (car q) q)))

(defun enqueue (elt q)
  "En-Norvig-queue."
  (setf (car q)
        (setf (cdar q) (cons elt nil))))

(defun dequeue (q)
  "De-Norvig-queue."
  (prog1 (pop (cdr q))
    (if (null (cdr q)) (setf (car q) q))))

(defun queue-contents (q)
  "The contents of a PAIP-queue."
  (cdr q))

(defun queue-empty-p (q)
  "Test the PAIP-queue for emptiness."
  (null (cdr q)))

(defun mapqueue (fn &rest lists)
  "Like MAPCAR (modulo multiple parameter lists), but returns a Norvig-queue."
  (lret ((q (make-queue)))
    (iter (for tails on lists)
          (while (every #'consp tails))
          (enqueue (apply fn (mapcar #'car tails)) q))))
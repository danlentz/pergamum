;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PERGAMUM; Base: 10 -*-
;;;

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

(defun mapcons (fn initial &rest lists)
  "Like MAPCAR, but specifies the cdr of the last cons cell."
  (let ((q (make-queue)))
    (apply #'mapqueue fn lists)
    (setf (cdar q) initial)
    (queue-contents q)))

(defun unzip (fn sequence &key (key #'identity))
  (let (ret-yes ret-no)
    (iter (for elt in sequence)
          (if (funcall fn (funcall key elt))
              (collect elt into yes)
              (collect elt into no))
          (finally (setf ret-yes yes ret-no no)))
    (values ret-yes ret-no)))

(defun diff-lists (list1 list2 &key (skip-mismatches 0))
  "Recursively look for differences between LIST1 and LIST2.
   When such a difference is found, its formpath is returned as first value,
   while the differing leaves are returned as second and third values."
  (declare (special skip-mismatches))
  (iter (for f1 in list1)
        (for f2 in list2)
        (collect (car (ensure-cons f1)) into path)
        (unless (or (equal f1 f2) (< 0 (decf skip-mismatches)))
          (multiple-value-bind (ret diff1 diff2) (when (eq (car (ensure-cons f1)) (car (ensure-cons f2)))
                                                   (diff-lists f1 f2))
            (return (values (list (butlast path) ret)
                            (if ret diff1 f1)
                            (if ret diff2 f2)))))))

;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PERGAMUM; Base: 10 -*-
;;;

(in-package :pergamum)


(defun excess (x measure)
  "When X exceeds MEASURE, return their difference, otherwise return NIL."
  (declare (integer x measure))
  (when (> x measure)
    (- x measure)))

(defun shortage (x measure)
  "When MEASURE exceeds X, return their difference, otherwise return NIL."
  (declare (integer x measure))
  (when (> measure x)
    (- measure x)))

(defun invoke-with-measure (value measure fn)
  (cond ((> value measure) (funcall fn nil               value (- value measure)))
        ((< value measure) (funcall fn (- measure value) value nil))
        (t                 (funcall fn nil               value nil))))

(defmacro with-measure ((shortage x excess) (value measure) &body body)
  `(invoke-with-measure ,value ,measure (lambda (,shortage ,x ,excess) ,@body)))

(defun >< (y x1 x2)
  (and (> y x1) (< y x2)))

(defun >=< (y x1 x2)
  (and (>= y x1) (<= y x2)))

(defun >=/< (y x1 x2)
  (and (>= y x1) (< y x2)))

(defun >/=< (y x1 x2)
  (and (> y x1) (<= y x2)))

(defun power-of-2-p (x)
  (zerop (logand x (1- x))))

(defun ilog2-ceiling (x)
  (ash 1 (integer-length x)))

(defun ilog2-cover (x)
  (if (power-of-2-p x)
      x
      (ilog2-ceiling x)))

(defun bcd-integer-length (digits)
  "Return the maximum number of bits needed to store the
   result of decoding of a DIGITS long BCD integer."
  (declare (type (integer 0) digits))
  (nth-value 0 (ceiling (log (1- (expt 10 digits)) 2) 1)))

(defun decode-bcd (digits bcd &aux (decoded (ldb (byte 4 0) bcd)))
  "Decode a DIGITS-long BCD."
  (declare (type (integer 0) bcd decoded) (fixnum digits))
  (iter (repeat digits)
        (for position from 4 by 4)
        (for base initially 10 then (* base 10))
        (incf decoded (* base (ldb (byte 4 position) bcd)))
        (finally (return decoded))))

(defun bisect (test n &optional (base 0))
  "Find, using bisection, the largest positive integer below N and above,
   or equal to BASE, which satisfies TEST of one argument.
   Assumes monotonous truth value of TEST."
  (labels ((traverse (x fallback attack)
             (if (funcall test x)
                 (cond ((zerop attack) x)
                       ((= attack 1)   (if (funcall test (1+ x)) (1+ x) x))
                       (t              (rebalance x (1+ attack))))
                 (cond ((= fallback 1) (when (funcall test (1- x)) (1- x)))
                       (t              (rebalance (- x fallback) fallback)))))
           (rebalance (origin span &aux (middle (ash span -1)))
             (traverse (+ origin middle) middle (- span middle 1))))
    (rebalance base (- n base))))

(defun split-number (x split-list &optional (base 10))
  "Split the integer X to a list of its BASE-imal digit constituents, with split points determined by the SPLIT-LIST of positions."
  (declare (type integer x) (type (integer 2) base))
  (nconc (iter (for power in split-list)
               (let* ((expt (expt base power))
                      (coll (floor x expt)))
                 (collect coll)
                 (decf x (* expt coll))))
         (list x)))

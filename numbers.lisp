;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PERGAMUM; Base: 10 -*-
;;;

(in-package :pergamum)

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

(defmacro with-alignment ((aligned &optional left right mask) alignment value &body body)
  "Execute BODY with ALIGNED, LEFT, RIGHT and MASK bound to various
products of applying ALIGNMENT to VALUE.
ALIGNED denotes the floor alignment, LEFT denotes the difference
between VALUE and ALIGNED and RIGHT denotes the difference between
VALUE and the sum of ALIGNED and ALIGNMENT."
  (let ((once-value (gensym))
        (mask (or mask (gensym))))
    `(let* ((,once-value ,value)
            (,mask (1- ,alignment))
            ,@(when aligned `((,aligned (logandc1 ,mask ,once-value))))
            ,@(when left `((,left (logand ,mask ,once-value))))
            ,@(when right `((,right (logand ,mask (- ,mask -1 ,(if left left `(logand ,mask ,once-value))))))))
       ,@body)))

(defun map-alignment-head (left right fn)
  (let ((granule-start left)
        (inner-part right)
        (inner-start 0))
    (iter (for g from granule-start)
          (for i from inner-start)
          (repeat inner-part)
          (funcall fn g i))))

(defun map-alignment-tail (length left fn)
  (let* ((granule-start 0)
         (inner-part left)
         (inner-start (- length inner-part)))
    (iter (for g from granule-start)
          (for i from inner-start)
          (repeat inner-part)
          (funcall fn g i))))

(defun operate-on-extremity (length rightp left right fn)
  "Given the split of an alignment granule to LEFT and RIGHT and an extent
LENGTH, map FN over the granule-local and absolute sets of offsets within
intersection of that granule and either the leftmost or rightmost 
corresponding part of the extent, depending on whetner RIGHTP is non-NIL.
FN must accept two arguments: the intra-granule offset, and extent offset."
  (let* ((inner-part (if rightp right left))
         (inner-boundary-offset (if rightp (1- inner-part) (- length inner-part)))
         (granule-start (if rightp (+ left right -1) 0))
         (step (if rightp -1 1)))
    (iter (for g from granule-start by step)
          (for i from inner-boundary-offset by step)
          (repeat inner-part)
          (funcall fn g i))))

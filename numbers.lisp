(in-package :pergamum)

(defun power-of-2-p (x)
  (zerop (logand x (1- x))))

(defun ilog2-ceiling (x)
  (ash 1 (integer-length x)))

(defun ilog2-cover (x)
  (if (power-of-2-p x)
      x
      (ilog2-ceiling x)))

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
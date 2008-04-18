(in-package :pergamum)

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
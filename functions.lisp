(in-package :pergamum)

(defun bukkake-combinator (&rest functions)
  "Return a function accepting an indefinite amount of values, 
   applying all FUNCTIONS to them in turn, returning the value
   of last application.
   Name courtesy of Andy Hefner."
  (lambda (&rest params)
    (mapc (rcurry #'apply params) (butlast functions))
    (apply (lastcar functions) params)))

(defun bukkake-combinator-1 (&rest functions)
  "Return a function accepting one value,  applying all FUNCTIONS
   to it in turn, returning the value of last application.
   Name courtesy of Andy Hefner."
  (lambda (param)
    (mapc (rcurry #'funcall param) (butlast functions))
    (funcall (lastcar functions) param)))

(defun maybe (function arg)
  "Given FUNCTION, return the result of applying ARG to it, when the latter is non-NIL."
  (when arg
    (funcall function arg)))

(defun maybecall (bool function &rest args)
  "Given FUNCTION, return the result of applying ARGS to it, when BOOL is non-NIL."
  (when bool
    (apply function args)))

(defun iterate-until (pred function &rest initial-args)
  "Given an INITIAL parameter value and a FUNCTION, iteratily apply the latter to the parameter, getting the new parameter, returning the last non-NIL one."
  (iter (with params = initial-args)
    (for result = (multiple-value-list (apply function params)))
    (until (apply pred result))
    (setf params result)
    (finally (return params))))

(defun collect-until (pred function &rest initial-args)
  "Given an INITIAL parameter value and a FUNCTION, iteratively apply the latter to the parameter, getting the new parameter, until it becomes NIL, collecting all non-NIL result parameters."
  (iter (with params = initial-args)
    (for result = (multiple-value-list (apply function params)))
    (until (apply pred result))
    (collect result)
    (setf params result)))

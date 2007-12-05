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
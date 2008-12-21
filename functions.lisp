;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PERGAMUM; Base: 10 -*-
;;;

(in-package :pergamum)

(defun latch-fn (fn)
  "Produce a function which applies FN to its parameters."
  (lambda (&rest args)
    (apply fn args)))

(defun latch-args (&rest args)
  "Produce a function which applies its first parameter to ARGS."
  (lambda (fn)
    (apply fn args)))

(defun bukkake-combine (&rest functions)
  "Return a function accepting an indefinite amount of values, 
   applying all FUNCTIONS to them in turn, returning the value
   of last application.
   Name courtesy of Andy Hefner."
  (lambda (&rest params)
    (mapc (rcurry #'apply params) (butlast functions))
    (apply (lastcar functions) params)))

(defun bukkake-combine-1 (&rest functions)
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
  "Given FUNCTION, return the result of applying it to ARGS, when BOOL is non-NIL."
  (when bool
    (apply function args)))

(defun xform (bool function &rest args)
  "Given FUNCTION, either return the result of applying it to ARGS, when BOOL is non-NIL, or return them as multiple values."
  (if bool
      (apply function args)
      (values-list args)))

(defun xform-if (predicate function &rest args)
  "Given FUNCTION, either return the result of applying it to ARGS, when PREDICATE applied to them yields non-NIL, or return them as multiple values."
  (if (apply predicate args)
      (apply function args)
      (values-list args)))

(defun xform-if-not (predicate function &rest args)
  "Given FUNCTION, either return the result of applying it to ARGS, when PREDICATE applied to them yields NIL, or return them as multiple values."
  (if (apply predicate args)
      (values-list args)
      (apply function args)))

(define-compiler-macro xform (&whole form bool function &rest args)
  (if (endp (rest args))
      `(if ,bool
           (funcall ,function ,(first args))
           ,(first args))
      form))

(define-compiler-macro xform-if (&whole form predicate function &rest args)
  (if (endp (rest args))
      `(if (funcall ,predicate ,(first args))
           (funcall ,function ,(first args))
           ,(first args))
      form))

(define-compiler-macro xform-if-not (&whole form predicate function &rest args)
  (if (endp (rest args))
      `(if (funcall ,predicate ,(first args))
           ,(first args)
           (funcall ,function ,(first args)))
      form))

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

(defun or-p (a b)
  (or a b))

(defun and-p (a b)
  (and a b))

;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PERGAMUM; Base: 10 -*-
;;;

(in-package :pergamum)

(defun feq (val)
  "An EQ currier."
  (lambda (x) (eq x val)))

(defun feql (val)
  "An EQL currier."
  (lambda (x) (eql x val)))

(defun fequal (val)
  "An EQUAL currier."
  (lambda (x) (equal x val)))

(defun fequalp (val)
  "An EQUALP currier."
  (lambda (x) (equalp x val)))

(defun f= (val)
  "A = currier."
  (lambda (x) (= x val)))

(defun latch (&rest args)
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

(defun apply/find-if (pred fn &rest args)
  "Return the first member of a set computed by application of FN to ARGS,
   which also satisfies PRED. The second value indicates whether the set
   returned by FN was non-empty."
  (declare (type (function (*) list) fn))
  (let ((set (apply fn args)))
    (values (find-if pred set) (not (null set)))))

(define-compiler-macro apply/find-if (&whole whole pred fn &rest args)
  (if (null (cdr args))
      (with-gensyms (set)
        (once-only (pred fn)
         `(let ((,set (funcall ,fn ,(car args))))
            (values (find-if ,pred ,set) (not (null ,set))))))
      whole))

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

#+sbcl
(defun function-insn-vector (name)
  (sb-sys:without-gcing
    (let* ((function (fdefinition name))
           (object (sb-disassem::fun-code function))
           (vector-sap (sb-kernel:code-instructions object))
           (vector-length (sb-disassem::code-inst-area-length object)))
      (let ((final-vec (make-array vector-length :element-type '(unsigned-byte 8))))
        (iter (for i below vector-length)
              (setf (aref final-vec i) (sb-vm::sap-ref-8 vector-sap i)))
        final-vec))))
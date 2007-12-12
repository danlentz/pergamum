(in-package :pergamum)

(defun make-condition-raiser (type &rest params)
  (lambda (&rest rest)
    (declare (ignore rest))
    (signal type params)))

(defun make-error-raiser (type &rest params)
  (lambda (&rest rest)
    (declare (ignore rest))
    (error type params)))

(defmacro condition-bind-default ((&rest bindings) &body body)
  "Establish default bindings in the Zetalisp style, as described in the
   2001 Kent Pitman's paper `Condition Handling in the Lisp Language Family.'"
  (with-gensyms (cond)
    `(handler-bind (,@(iter (for (type handler) in bindings)
                            (collect `(,type (lambda (,cond)
                                               (signal ,cond)
                                               (funcall ,handler ,cond))))))
       ,@body)))
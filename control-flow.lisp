;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PERGAMUM; Base: 10 -*-
;;;

(in-package :pergamum)


(defmacro if-lret (bindings &body (then-form &optional else-form))
  "Creates new variable bindings, and conditionally executes either
THEN-FORM or ELSE-FORM. ELSE-FORM defaults to NIL.

BINDINGS must be either single binding of the form:

 (variable initial-form)

or a list of bindings of the form:

 ((variable-1 initial-form-1)
  (variable-2 initial-form-2)
  ...
  (variable-n initial-form-n))

All initial-forms are executed sequentially in the specified order. Then all
the variables are bound to the corresponding values.

If all variables were bound to true values, the THEN-FORM is executed with the
bindings in effect, and the value of the last bound variable is returned, 
otherwise the ELSE-FORM is executed with the bindings in effect."
  (let* ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                           (list bindings)
                           bindings))
         (variables (mapcar #'car binding-list)))
    `(let ,binding-list
       (if (and ,@variables)
           (progn ,then-form ,(car (lastcar binding-list)))
           ,else-form))))

(defmacro if-lret* (bindings then-form &optional else-form)
  "Creates new variable bindings, and conditionally executes either THEN-FORM
or ELSE-FORM. ELSE-FORM defaults to NIL.

BINDINGS must be a list of bindings of the form:

 ((variable-1 initial-form-1)
  (variable-2 initial-form-2)
  ...
  (variable-n initial-form-n))

Each initial-form is executed in turn, and the variable bound to the
corresponding value. Initial-form expressions can refer to variables
previously bound by the IF-LET*.

If all variables are true after the bindings are complete, the THEN-FORM is
executed with the bindings in effect, and the value of the last bound 
variable is returned, otherwise the ELSE-FORM is executed with the bindings
NOT in effect."
  (with-gensyms (%if-let* block else-tag)
    `(macrolet ((,%if-let* (rec-bindings)
                  (let* ((,%if-let* ',%if-let*)
                         (,block ',block)
                         (,else-tag ',else-tag))
                    (destructuring-bind (first-binding &rest rest) rec-bindings
                      `(let (,first-binding)
                         (if ,(car first-binding)
                             ,(if (endp rest)
                                  `(return-from ,,block (progn ,',then-form ,(car first-binding)))
                                  `(,,%if-let* ,rest))
                             (go ,,else-tag)))))))
       (block ,block
         (tagbody
            (,%if-let* ,bindings)
            ,else-tag
            (return-from ,block ,else-form))))))
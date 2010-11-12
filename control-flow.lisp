;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PERGAMUM; Base: 10 -*-
;;;

(in-package :pergamum)


(defmacro when-lret (bindings &body forms)
    "Creates new variable bindings, and conditionally executes FORMS.

BINDINGS must be either single binding of the form:

 (variable initial-form)

or a list of bindings of the form:

 ((variable-1 initial-form-1)
  (variable-2 initial-form-2)
  ...
  (variable-n initial-form-n))

All initial-forms are executed sequentially in the specified order. Then all
the variables are bound to the corresponding values.

If all variables were bound to true values, then FORMS are executed as an
implicit PROGN."
  (let* ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                           (list bindings)
                           bindings))
         (variables (mapcar #'car binding-list)))
    `(let ,binding-list
       (when (and ,@variables)
         ,@forms
         ,(car (lastcar binding-list))))))

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

(defmacro cond-let (&body clauses)
  "Like COND, but each condition is a LET-like binding subform, instead.

 (cond-let
   ((variable-1 condition-form-1)
    clause-body-forms...)
   ((variable-2 condition-form-2)
    clause-body-forms...)
   ...
   (t
    clause-body-forms...))

Each condition form is executed in turn, until one returns non-NIL,
at which point the corresponding clause's body forms are executed
with the variable bound to the value returned."
  (when clauses
    (destructuring-bind (binding &body body) (first clauses)
      (if (endp (cdr clauses))
          (if (eq t binding)
              (ensure-form body)
              `(when-let* (,binding) ,@body))
          `(if-let ,binding
                   ,(ensure-form body)
                   (cond-let ,@(rest clauses)))))))

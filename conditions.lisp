;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PERGAMUM; Base: 10 -*-
;;;

(in-package :pergamum)

(defun make-condition-raiser (type &rest params)
  (lambda (&rest rest)
    (declare (ignore rest))
    (signal type params)))

(defun make-error-raiser (type &rest params)
  (lambda (&rest rest)
    (declare (ignore rest))
    (error type params)))

(defmacro with-retry-restart ((restart-name condition-binding &body restart-body) &body body)
  `(loop (restart-case (return (progn ,@body))
           (,restart-name (,@condition-binding)
            ,@restart-body))))

(defmacro with-ignore-restart ((restart-name condition-binding &body restart-body) &body body)
  (with-gensyms (block)
   `(block ,block
      (restart-case (progn ,@body)
        (,restart-name (,@condition-binding)
          ,@restart-body
          (return-from ,block))))))

(defun make-fixed-restarter (restart-name &rest params)
  (lambda (cond)
    (with-condition-restarts cond (list (find-restart restart-name))
      (apply #'invoke-restart restart-name cond params))))

(defmacro with-condition-restart-binding ((&rest condition-restart-specs) &body body)
  `(handler-bind (,@(iter (for (condition restart . params) in condition-restart-specs)
                          (collect `(,condition (make-fixed-restarter ',restart ,@params)))))
     ,@body))

(defmacro with-condition-printing ((stream type) &body body)
  (with-gensyms (cond)
    `(handler-case (progn ,@body)
       (,type (,cond)
	 (format ,stream "~@<~A~:@>" ,cond)))))

(defmacro with-condition-collection ((type) &body body)
  "Broken."
  (with-gensyms (list cond)
    `(let (,list)
       (handler-case (progn ,@body)
         (,type (,cond)
           (push ,cond ,list)))
       (nreverse ,list))))

(defmacro returning-conditions (type &body body)
  "Catch and return conditions SUBTYPEP to TYPE, during BODY execution."
  `(handler-case (progn ,@body) (,type (cond) cond)))

(defmacro condition-bind-default ((&rest bindings) &body body)
  "Establish default bindings in the Zetalisp style, as described in the
   2001 Kent Pitman's paper `Condition Handling in the Lisp Language Family.'"
  (with-gensyms (cond)
    `(handler-bind (,@(iter (for (type handler) in bindings)
                            (collect `(,type (lambda (,cond)
                                               (signal ,cond)
                                               (funcall ,handler ,cond))))))
       ,@body)))

(defun simple-condition-reporter (condition stream)
  "The should-have-been-defined report function for simple conditions."
  (declare (stream stream) (simple-condition condition))
  (apply #'format stream (simple-condition-format-control condition)
         (simple-condition-format-arguments condition)))

(define-condition redefinition ()
  ())

(define-condition simple-redefinition (redefinition simple-warning)
  ())

(define-condition bad-redefinition (redefinition simple-error)
  ())

(defun warn-redefinition (format-control &rest arguments)
  (warn 'simple-redefinition :format-control format-control :format-arguments arguments))

(defun bad-redefinition (format-control &rest arguments)
  (error 'bad-redefinition :format-control format-control :format-arguments arguments))
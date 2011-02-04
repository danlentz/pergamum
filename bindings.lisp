;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PERGAMUM; Base: 10 -*-
;;;

(in-package :pergamum)

(defmacro define-binder (name variable &key invoker maybep (fixed-value nil fixed-value-p) (documentation t))
  (flet ((proper-sym (format-control &rest args)
           (apply #'format-symbol (symbol-package variable) format-control args))
         (docstring (what)
           (when documentation
             (list (format nil "~A with ~A ~:[~;possibly ~]bound to VALUE." what variable maybep)))))
    (let ((body  (proper-sym "BODY"))
          (pred (proper-sym "PRED"))
          (value (proper-sym "VALUE"))
          (fn (proper-sym "FN"))
          (invoker (or invoker (proper-sym "INVOKE~:[~;-MAYBE~]-~A" maybep name))))
      `(progn
         (defun ,invoker (,@(when maybep `(,pred)) ,value ,fn)
           ,@(docstring "Call FN")
           ,(if maybep
                `(if ,pred
                     (let ((,variable ,value))
                       (declare (special ,variable))
                       (funcall ,fn))
                     (funcall ,fn))
                `(let ((,variable ,value))
                   (declare (special ,variable))
                   (funcall ,fn))))
         (defmacro ,name (,@(if (and maybep (not fixed-value-p))
                                `((,pred ,value))
                                `(,@(when maybep `(,pred)) ,@(unless fixed-value-p `(,value))))
                          &body ,body)
           ,@(docstring "Execute BODY")
           `(,',invoker ,,@(when maybep `(,pred))
                        ,,(if fixed-value-p
                              (list 'quote fixed-value)
                              value)
                        (lambda () ,@,body)))))))

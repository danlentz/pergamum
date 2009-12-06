;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PERGAMUM; Base: 10 -*-
;;;

(in-package :pergamum)


(defmacro define-execute-with-bound-variable (variable &body options)
  (let* ((package (symbol-package variable))
         (invoker-name (format-symbol package "EXECUTE-WITH-~A-BOUND" variable)))
    (multiple-value-bind (bindings unrecognised) (unzip (compose (curry #'eq :binding) #'car) options)
      (when unrecognised
        (error "~@<In ~A: unrecognised options: ~A.~:@>"
               'define-execute-with-maybe-bound-variable (mapcar #'car unrecognised)))
      `(progn
         (defun ,invoker-name (maybe value fn)
           ,(format nil "Execute FN with ~A possibly bound to VALUE, depending on whether MAYBE is non-NIL." variable)
           (if maybe
               (let ((,variable value))
                 (funcall fn))
               (funcall fn)))
         ,@(iter (for binding in bindings)
                 (destructuring-bind (name form &key (define-with-macro t) define-with-maybe-macro documentation) (rest binding)
                   (when define-with-macro
                     (collect `(defmacro ,(format-symbol package "WITH-~A" name) (() &body body)
                                 ,@(when documentation `(,documentation))
                                 `(,',invoker-name t ,',form (lambda () ,@body)))))
                   (when define-with-maybe-macro
                     (collect `(defmacro ,(format-symbol package "WITH-MAYBE-~A" name) ((maybe) &body body)
                                 ,@(when documentation `(,documentation))
                                 `(,',invoker-name ,maybe ,',form (lambda () ,@body)))))))))))

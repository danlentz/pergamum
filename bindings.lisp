;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PERGAMUM; Base: 10 -*-
;;;

(in-package :pergamum)


(defmacro define-execute-with-special (name)
  (let ((execute-with-fn-name (format-symbol (symbol-package name) "EXECUTE-WITH-~A" name))
        (with-macro-name (format-symbol (symbol-package name) "WITH-~A" name))
        (special-var-name (format-symbol (symbol-package name) "*~A*" name)))
    `(progn
       (defun ,execute-with-fn-name (,name fn)
         ,(format nil "Call FN within dynamic context where ~A is bound to ~A.
FN must be a function of no arguments." special-var-name name)
         (let ((,special-var-name ,name))
           (declare (special ,special-var-name))
           (funcall fn)))

       (defmacro ,with-macro-name (,name &body body)
         ,(format nil "Execute BODY within dynamic context where ~A is bound to
the result of evaluation of ~A." special-var-name name)
         
         `(,',execute-with-fn-name ,,name (lambda () ,@body))))))

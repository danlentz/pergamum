;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PERGAMUM; Base: 10 -*-
;;;

(in-package :pergamum)


(defmacro define-protocol-class (name provided-superclasses slots &rest options)
  `(progn
     (defclass ,name (,@provided-superclasses)
       (,@slots)
       ,@options)
     (let ((the-class (find-class ',name)))
       (defmethod initialize-instance :after ((o ,name) &key &allow-other-keys)
         (when (eq (class-of o) the-class)
           (error 'protocol-class-instantiation :class (class-of o)))))))
;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PERGAMUM; Base: 10 -*-
;;;

(in-package :pergamum)

(defstruct unreadable-object)

(defmethod print-object ((o unreadable-object) s)
  (print-unreadable-object (o s)))

(defun array-reader (stream &optional sharp char)
  (declare (ignore sharp char))
  (let ((list (read stream)))
    `(make-array ,(length list)
                 :initial-contents (list ,@list))))

(defun enable-array-reader ()
  "Enable #A(a b c) syntax for element-evaluated constant arrays."
  (set-dispatch-macro-character #\# #\A 'array-reader))

(defun curry-reader (stream char)
  (declare (ignorable char))
  (let ((contents (read-delimited-list #\] stream t)))
    `(curry #',(car contents) ,@(cdr contents))))

(defun enable-curry-reader ()
  "Enable [fn a b c] syntax for currying."
  (set-macro-character  #\[ #'curry-reader t)
  (set-syntax-from-char #\] #\)))

(defun compose-reader (stream char)
  (declare (ignorable char))
  `(compose ,@(read-delimited-list #\} stream t)))

(defun enable-compose-reader ()
  "Enable {#'a #'b #'c} syntax for function composition."
  (set-macro-character  #\{ #'compose-reader t)
  (set-syntax-from-char #\} #\)))

(defun invoke-with-safe-reader-context (fn)
  (let ((*read-eval* nil)
        (*readtable* (copy-readtable)))
    (set-dispatch-macro-character #\# #\. (lambda (stream &optional char sharp)
                                            (declare (ignore char sharp))
                                            (let ((*read-suppress* t))
                                              (read stream nil nil t))))
    (funcall fn)))

(defmacro with-safe-reader-context (() &body body)
  `(invoke-with-safe-reader-context (lambda () ,@body)))

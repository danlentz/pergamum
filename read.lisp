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

(defun read-ignoring-missing-packages (stream &optional (eof-error-p t) eof-value recursive-p)
  "Like READ, but do not signal an error when the expression to be read contains
symbols in missing packages, at the cost of mutilation of such forms.
Thanks to Pascal Bourgignon and Tobias Rittweiler, without whom this function
would not exist."
  (let ((*readtable* (copy-readtable)))
    (set-syntax-from-char #\: #\')
    (multiple-value-bind (result offset) (read stream eof-error-p eof-value recursive-p)
      (if (eql #\: (peek-char nil stream eof-error-p eof-value recursive-p))
          (progn
            (read-char stream eof-error-p eof-value recursive-p)
            (read stream eof-error-p eof-value recursive-p))
          (values result offset)))))
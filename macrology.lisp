;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PERGAMUM; Base: 10 -*-
;;;

(in-package :pergamum)


(defmacro with-ignored-names ((pastable-ignore-form-var &rest specs) &body body)
  "Each SPEC must be either a NAME, or a (NAME THING), with plain
NAME producing a plain, unprefixed gensym in the expansion.

Evaluates FORMS with ignored names rebound to gensyms, and tracks the
ignored names, so that PASTABLE-IGNORE-FORM-VAR contains a pastable list,
which contains an ignore declaration in the case there were any symbols
to ignore.

Example:
  (defmacro ignorably-let (((var value-form)) &body body)
    (with-ignored-names (ignores var)
      `(let ((,var ,value-form))
         ,@ignores
         ,@body)))
  (ignorably-let ((_ 123))
    (foo)) =>
  (let ((#:g42 123))
    (declare (ignore #:g42))
    (foo))"
  (let ((final-names (make-gensym-list (length specs) "FINAL-NAMES"))
        (ignore-tracker (gensym "IGNORE-TRACKER"))
        (ignore-name-p (gensym "IGNORE-CRITERIA"))
        (symbol-selector-name (gensym "SYMBOL-SELECTOR"))
        (names-and-things (mapcar (lambda (spec)
                                    (etypecase spec
                                      (list
                                       (destructuring-bind (name thing) spec
                                         (list name thing)))
                                      (symbol
                                       (list spec nil))))
                                  specs)))
    (multiple-value-bind (names things) (apply #'values (apply #'mapcar #'list names-and-things))
      `(labels ((,ignore-name-p (x) (string= (symbol-name x) "_"))
                (,symbol-selector-name (x &optional (thing "G"))
                  (if (,ignore-name-p x)
                      (gensym thing)
                      x)))
         (let* (,@(mapcar (lambda (g n tn) (list g `(,symbol-selector-name ,n ,@(when tn (list tn)))))
                          final-names names things)
                (,pastable-ignore-form-var (let ((,ignore-tracker (remove nil (mapcar (lambda (n f)
                                                                                        (when (,ignore-name-p n) f))
                                                                                      (list ,@names)
                                                                                      (list ,@final-names)))))
                                             (when ,ignore-tracker
                                               `((declare (ignore ,@,ignore-tracker)))))))
           (let (,@(mapcar (lambda (n g) (list n g))
                           names final-names))
             ,@body))))))

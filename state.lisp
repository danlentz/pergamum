;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PERGAMUM; Base: 10 -*-
;;;

(in-package :pergamum)

(defmacro define-variable-set (name-and-options &body var-clauses)
  "Define a named set of global variables, with associated docstrings,
default values and an initialiser function INITIALIZE-NAME, assigning
these values to the variables.

NAME-AND-OPTIONS specifies a name for the variable set, and a set of
options.  NAME-AND-OPTIONS can be either a symbol, specifying just the
name, or a list, where the first element must be a symbol specifying
the name, and the rest must be a property list of options.

VAR-CLAUSES is a list of clauses, each being either a variable name,
in which case there's no associated documentation and the default
value being NIL, or a three-element list, specifying the name, the
form specifying the default value and the docstring.

The default values are only assigned through a call to the initialiser
function, unless APPLY-DEFAULTS option is provided."
  (destructuring-bind (name &key apply-defaults) (ensure-cons name-and-options)
    (let* ((normalised-clauses (mapcar #'ensure-list var-clauses))
           (names              (mapcar #'first  normalised-clauses))
           (default-forms      (mapcar #'second normalised-clauses))
           (documentations     (mapcar #'third  normalised-clauses)))
      `(progn
         ,@(iter (for name in names)
                 (for form in default-forms)
                 (for doc  in documentations)
                 (unless (or (null doc) (stringp doc))
                   (error "~@<In DEFINE-GLOBAL-VARIABLE-SET: ~
                              third clause parameter must be a string, whenever specified.~:@>"))
                 (collect `(defvar ,name ,(when apply-defaults form) ,doc)))
         (defun ,(format-symbol (symbol-package name) "INITIALIZE-~A" name) ()
           ,@(iter (for name in names)
                   (for form in default-forms)
                   (collect `(setf ,name ,form))))))))
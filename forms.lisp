;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PERGAMUM; Base: 10 -*-
;;;

(in-package :pergamum)

(defun quoted-p (form)
  (and (consp form) (eq (car form) 'quote)))

(defun quoted-form (quoted)
  (second quoted))

(defun quote-when (c form)
  (if c (list 'quote form) form))

(defun quote-if-non-self-evaluating (form)
  (if (or (typep form 'cons) (typep form 'symbol)) (list 'quote form) form))

(defun ensure-form (form-or-forms)
  "Given FORM-OR-FORMS, which must be either an atom or a list of forms 
individually suitable for evaluation, return a single form suitable for 
evaluation;  that is, return it unmodified in the atom case, and either
return its first element, or prepend a PROGN symbol in the list case."
  (cond ((atom form-or-forms)       form-or-forms)
        ((endp (cdr form-or-forms)) (car form-or-forms))
        (t                          (cons 'progn form-or-forms))))

(defmacro op-parameter-destructurer ((op params) form &body body)
  (once-only (form)
    `(let (,@(when op `((,op (if (consp ,form) (first ,form) ,form))))
	   ,@(when params `((,params (when (consp ,form) (rest ,form))))))
       ,@body)))

(defun ensure-destructurisation (spec form)
  (declare (list spec))
  (cond ((atom form) `(,@(iter (for i below (length spec)) (collect `(nth ,i ,form)))))
        ((= (length form) (length spec)) form)
        (t (error "form ~S doesn't match destructurisation ~S" form spec))))

(defun emit-declaration (type symbols)
  (when symbols (list (list* type symbols))))

(defun emit-declarations (&key ignore special)
  (when (or ignore special)
    (append (emit-declaration 'ignore ignore)
            (emit-declaration 'special special))))

(defun emit-binding-form-body (body &key declarations)
  (prepend (list* 'declare declarations) body :key #'rest))

(defun emit-let (bindings body &key declarations)
  (declare (list bindings))
  (if bindings
      (append `(let ,bindings)
	      (emit-binding-form-body body :declarations declarations))
      `(progn ,@body)))

(defun emit-lambda-body (body &key documentation declarations)
  (prepend documentation (emit-binding-form-body body :declarations declarations)))

(defun emit-lambda (list body &key documentation declarations)
  (append `(lambda ,list)
	  (emit-lambda-body body :documentation documentation :declarations declarations)))

(defun emit-named-lambda (name list body &key documentation declarations)
  `(labels ((,name ,list
	      ,@(emit-lambda-body body :documentation documentation :declarations declarations))) #',name))

(defmacro with-named-lambda-emission ((name lambda-list &key documentation declarations) &body body)
  `(emit-named-lambda ,name ,lambda-list (list ,@body) :documentation ,documentation :declarations ,declarations))

(defun emit-defun (name list body &key documentation declarations)
  `(defun ,name ,list
     ,@(emit-lambda-body body :documentation documentation :declarations declarations)))

(defmacro with-defun-emission ((name lambda-list &key documentation declarations) &body body)
  `(emit-defun ,name ,lambda-list (list ,@body) :documentation ,documentation :declarations ,declarations))

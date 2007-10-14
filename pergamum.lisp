(defpackage pergamum
  (:use :common-lisp :alexandria)
  (:export
   #:op-parameter-destructurer
   #:define-evaluation-domain
   ))

(in-package :pergamum)

(defmacro op-parameter-destructurer ((op &rest params) form &body body)
  `(let ((,op ,(if (consp form) (first form) form))
	 (,params ,@(unless (consp form) `(rest ,form))))
     ,@body))

;; This macro belongs to the wider world.
(defmacro define-evaluation-domain (domain-name (&rest evaluation-result-vars))
  (with-gensyms (table-name name lambda-list type op params body)
    `(progn
       (defvar ,table-name (make-hash-table))
       
       (defmacro ,(format-symbol (symbol-package domain-name) "DEFINE-~S" domain-name) (,name ,lambda-list &body ,body)
	 `(setf (gethash ',,domain-name ,,table-name)
		(lambda ,,lambda-list ,@,body)))

       (defmacro ,(format-symbol (symbol-package domain-name) "APPLY-BIND-~S" domain-name) ((,@evaluation-result-vars) ,type &body ,body)
	 "Given a parser op invocation, deliver the appropriately specialised op 'guts'."
	 (op-parameter-destructurer (,op ,params) ,type
	   `(multiple-value-bind (,,@evaluation-result-vars) (apply (gethash ,,op ,,table-name) ,,params)
	      ,@,body)))

       (defmacro ,(format-symbol (symbol-package domain-name) "FUNCALL-BIND-~S" domain-name) ((,@evaluation-result-vars) ,type &body ,body)
	 "Given a parser op invocation, deliver the appropriately specialised op 'guts'."
	 (op-parameter-destructurer (,op ,params) ,type
	   `(multiple-value-bind (,,@evaluation-result-vars) (funcall (gethash ,,op ,,table-name) ,@,params)
	      ,@,body))))))

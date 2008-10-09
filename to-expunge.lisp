(in-package :pergamum)

(defmacro define-evaluation-domain (domain-name)
  (let* ((target-package (symbol-package domain-name))
         (table-name (format-symbol target-package "*~A-EVALUATIONS*" domain-name))
         (macro-p-table-name (format-symbol target-package "*~A-MACRO-P*" domain-name)))
    (when (boundp table-name)
      (warn "redefining ~S in DEFINE-EVALUATION-DOMAIN" domain-name))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel)
	 (defparameter ,table-name (make-hash-table :test #'equal))
	 (defparameter ,macro-p-table-name (make-hash-table :test #'equal)))
       
       (defun ,(format-symbol target-package "APPLY-~A" domain-name) (query-name form)
	 (op-parameter-destructurer (op params) form
	   (let ((evaluator (gethash (list op query-name) ,table-name)))
	     (unless evaluator
	       (error "no function evaluator ~S defined for op ~S, within evaluation domain ~S" query-name op ',domain-name))
	     (apply evaluator params))))
       
       (defmacro ,(format-symbol target-package "EVAL-~A" domain-name) (query-name form)
	 (op-parameter-destructurer (op params) form
	   (let ((evaluator (gethash (list op query-name) ,table-name))
		 (macro-p (gethash (list op query-name) ,macro-p-table-name)))
	     (unless evaluator
	       (error "no evaluator ~S defined for op ~S, within evaluation domain ~S" query-name op ',domain-name))
	     (if macro-p
		 (apply evaluator params)
		 `(funcall (load-time-value (gethash (list ',op ',query-name) ,',table-name)) ,@params))))))))

(defun define-evaluations (domain-name macro-p op lambda-list evaluations)
  (let* ((target-package (symbol-package domain-name))
	 (table-name (format-symbol target-package "*~A-EVALUATIONS*" domain-name))
	 (macro-p-table-name (format-symbol target-package "*~A-MACRO-P*" domain-name))
	 (lambda-binds (lambda-list-binds lambda-list)))
    (unless (boundp table-name)
      (error "undefined evaluation domain ~S" domain-name))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf ,@(iter (for (query-name interested-by-list . body) in evaluations)
		     (unless (every (rcurry #'member lambda-binds) interested-by-list)
		       (error "the interested-by binding specification ~S is not a subset of the main binding list ~S."
			      interested-by-list lambda-list))
		     (appending
		      `((gethash `(,',op ,',query-name) ,macro-p-table-name) ,macro-p
			(gethash `(,',op ,',query-name) ,table-name)
			,(emit-named-lambda (format-symbol target-package "~A-~A-~A" op domain-name query-name) lambda-list body
					    :declarations (emit-declarations :ignore (set-difference lambda-binds interested-by-list))))))))))

(defmacro define-function-evaluations (domain-name op lambda-list &body evaluations)
  (define-evaluations domain-name nil op lambda-list evaluations))

(defmacro define-macro-evaluations (domain-name op lambda-list &body evaluations)
  (define-evaluations domain-name t op lambda-list evaluations))

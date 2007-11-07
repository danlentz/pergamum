(in-package :pergamum)

(defun quote-when (c form)
  (if c (list 'quote form) form))

(defmacro op-parameter-destructurer ((op params) form &body body)
  (once-only (form)
    `(let ((,op (if (consp ,form) (first ,form) ,form))
	   (,params (when (consp ,form) (rest ,form))))
       ,@body)))

(defun lambda-xform (fn spec form &optional acc (mode '&mandatory) key)
  "Perform FN on the input elements combined with the corresponding
   element of the lambda-list encoded SPEC."
  (flet ((yield (acc selt felt)
	   (cons (funcall fn selt felt) acc)))
    (cond ((and (null spec) (null form))
	   (nreverse acc))
	  ((null spec)
	   (error "Form has unspecified leftover: ~S ~S." form spec))
	  (t
	   (destructuring-bind (elt &rest rest) spec
	     (if (member elt '(&optional &key &rest &body))
		 (let ((rest (if (eq elt '&key)
				 (loop :for key-val :in rest :nconc
				    (op-parameter-destructurer (k v) key-val
				      (list (intern (string k) :keyword) (car v))))
				 rest)))
		   (ecase mode
		     ((&mandatory &optional) (lambda-xform fn rest form acc elt))
		     (&key (case elt
			     ((&optional &key &rest)
			      (error "Misplaced ~S in lambda list." elt))
			  (t (lambda-xform fn rest form acc elt))))
		     ((&rest &body) (case elt
				      ((&key &optional)
				       (error "Misplaced ~S in lambda list." elt))
				      (t (lambda-xform fn rest form acc elt))))))
		 (ecase mode
		   (&mandatory
		    (unless form
		      (error "Missing mandatory argument."))
		    (lambda-xform fn rest (cdr form) (yield acc elt (car form)) mode))
		   (&optional
		    (if form
			(lambda-xform fn rest (cdr form) (yield acc elt (car form)) mode)
			(nreverse acc)))
		   (&key
		    (if key
			(if (null form)
			    (error "Missing argument for key ~S." key)
			    (lambda-xform fn (remove-from-plist spec key) (cdr form)
					  (yield (cons key acc)
						 (getf spec key) (car form)) '&key))
			(if form
			    (lambda-xform fn spec (cdr form) acc '&key (car form))
			    (nreverse acc))))
		   ((&body &rest)
		    (unless (= (length spec) 1)
		      (error "Bad or missing spec for the rest."))
		    (nreverse (nconc (nreverse (mapcar (curry (the function fn) elt) form)) acc))))))))))

(defun mklist (x)
  "Ensure that X is a list."
  (if (listp x) x (list x)))

(defun lambda-list-binds (list)
  "Yield a list of symbols bound by a well-formed lambda LIST."
  (iter (for elt in list)
	(cond ((consp elt)
	       (collect (car elt)))
	      ((not (member elt lambda-list-keywords))
	       (collect elt)))))

(defun emit-ignore-declaration (symbols)
  (when symbols (list* 'ignore symbols)))

(defun emit-declarations (&key ignore)
  (when (or ignore)
    (list* 'declare (append (list (emit-ignore-declaration ignore))))))

(defun emit-binding-form-body (body &key declarations)
  (append (list declarations) body))

(defun destructure-binding-form-body (body &optional declarations)
  (if (and (consp body) (consp (car body)) (eq (caar body) 'declare))
      (destructure-binding-form-body (cdr body) (append declarations (list (car body))))
      (values body declarations)))

(defun emit-lambda-body (body &key documentation declarations)
  (append (mklist documentation)
	  (emit-binding-form-body body :declarations declarations)))

(defun emit-lambda (list body &key documentation declarations)
  (append `(lambda ,list)
	  (emit-lambda-body body :documentation documentation :declarations declarations)))

(defun emit-named-lambda (name list body &key documentation declarations)
  `(labels ((,name ,list
	      ,@(emit-lambda-body body :documentation documentation :declarations declarations))) #',name))

(defmacro lambda-list-satisfied-p (list value)
  "Checks whether the VALUE is destructurable by lambda LIST.
   NOTE: evaluation of VALUE might raise an unrelated error,
   potentially yielding a false negative."
  `(ignore-errors
     (destructuring-bind ,list ,value ,@(emit-lambda-body '(t) :declarations (emit-declarations :ignore (lambda-list-binds list))))))

(defmacro define-evaluation-domain (domain-name)
  (let ((table-name (format-symbol (symbol-package domain-name) "*~A-EVALUATIONS*" domain-name))
	(macro-p-table-name (format-symbol (symbol-package domain-name) "*~A-MACRO-P*" domain-name)))
    (when (boundp table-name)
      (warn "redefining ~S in DEFINE-EVALUATION-DOMAIN" domain-name))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel)
	 (defparameter ,table-name (make-hash-table :test #'equal))
	 (defparameter ,macro-p-table-name (make-hash-table :test #'equal)))
       
       (defun ,(format-symbol (symbol-package domain-name) "APPLY-~S" domain-name) (query-name form)
	 (op-parameter-destructurer (op params) form
	   (let ((evaluator (gethash (list op query-name) ,table-name)))
	     (unless evaluator
	       (error "no function evaluator ~S defined for op ~S, within evaluation domain ~S" query-name op ',domain-name))
	     (apply evaluator params))))
       
       (defmacro ,(format-symbol (symbol-package domain-name) "EVAL-~S" domain-name) (query-name form)
	 (op-parameter-destructurer (op params) form
	   (let ((evaluator (gethash (list op query-name) ,table-name))
		 (macro-p (gethash (list op query-name) ,macro-p-table-name)))
	     (unless evaluator
	       (error "no evaluator ~S defined for op ~S, within evaluation domain ~S" query-name op ',domain-name))
	     (if macro-p
		 (apply evaluator params)
		 `(funcall (gethash (list ',op ',query-name) ,',table-name) ,@params))))))))

(defun define-evaluations (domain-name macro-p op lambda-list evaluations)
  (let* ((target-package (symbol-package domain-name))
	 (table-name (format-symbol target-package "*~A-EVALUATIONS*" domain-name))
	 (macro-p-table-name (format-symbol target-package "*~A-MACRO-P*" domain-name))
	 (lambda-binds (lambda-list-binds lambda-list)))
    (unless (boundp table-name)
      (error "undefined evaluation domain ~S." domain-name))
    `(eval-when (:compile-toplevel :load-toplevel)
       (setf ,@(iter (for (query-name interested-by-list . body) in evaluations)
		     (unless (every (rcurry #'member lambda-binds) interested-by-list)
		       (error "the interested-by binding specification ~S is not a subset of the main binding list ~S."
			      interested-by-list lambda-list))
		     (appending
		      `((gethash `(,',op ,',query-name) ,macro-p-table-name) ,macro-p
			(gethash `(,',op ,',query-name) ,table-name)
			,(emit-named-lambda (format-symbol target-package "~S-~S-~S" op domain-name query-name) lambda-list body
					    :declarations (emit-declarations :ignore (set-difference lambda-binds interested-by-list))))))))))

(defmacro define-function-evaluations (domain-name op lambda-list &rest evaluations)
  (define-evaluations domain-name nil op lambda-list evaluations))

(defmacro define-macro-evaluations (domain-name op lambda-list &rest evaluations)
  (define-evaluations domain-name t op lambda-list evaluations))

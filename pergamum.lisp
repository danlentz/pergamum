(in-package :pergamum)

(defun quote-when (c form)
  (if c (list 'quote form) form))

(defmacro op-parameter-destructurer ((op params) form &body body)
  (once-only (form)
    `(let ((,op (if (consp ,form) (first ,form) ,form))
	   (,params (when (consp ,form) (rest ,form))))
       ,@body)))

(defun map-lambda-list (fn spec form &optional acc (mode '&mandatory))
  "Map FN on the lambda list application with the corresponding
   elements of the lambda list SPEC."
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
		     ((&mandatory &optional) (map-lambda-list fn rest form acc elt))
		     (&key (case elt
			     ((&optional &key &rest)
			      (error "Misplaced ~S in lambda list." elt))
			  (t (map-lambda-list fn rest form acc elt))))
		     ((&rest &body) (case elt
				      ((&key &optional)
				       (error "Misplaced ~S in lambda list." elt))
				      (t (map-lambda-list fn rest form acc elt))))))
		 (ecase mode
		   (&mandatory
		    (unless form
		      (error "Missing mandatory argument."))
		    (map-lambda-list fn rest (cdr form) (yield acc elt (car form)) mode))
		   (&optional
		    (if form
			(map-lambda-list fn rest (cdr form) (yield acc elt (car form)) mode)
			(nreverse acc)))
		   (&key
                    (destructuring-bind (&optional key val &rest rest) form
                      (unless key
                        (return-from map-lambda-list (nreverse acc)))
                      (unless val
                        (error "odd number of &KEY arguments"))
                      (multiple-value-bind (property-name value list) (get-properties spec (list key))
                        (declare (ignore property-name ))
                        (unless list
                          (error "unknown &KEY argument: ~S" key))
                        (map-lambda-list fn (remove-from-plist spec key) rest
                                         (yield (cons key acc) value val) '&key))))
		   ((&body &rest)
		    (unless (= (length spec) 1)
		      (error "Bad or missing spec for the rest."))
		    (nreverse (nconc (nreverse (mapcar (curry (the function fn) elt) form)) acc))))))))))

(defmacro order (fnspec &rest order)
  "Given a function specification (either FNSYM or (FNSYM NPARAMS)),
   and a list of positional parameter specifiers, produce a function
   calling the function FNSYM, with received parameters either ignored
   (if their corresponding positional parameter specifier is omitted),
   or passed in the order specified by the aforementioned parameter
   specifiers. Example: (funcall (order > 1 0) 1 2) => NIL."
  (let* ((long-form-p (consp fnspec))
	 (length (if long-form-p (second fnspec) (length order)))
	 (fnsym (if long-form-p (first fnspec) fnspec))
	 (specs (loop :for i :from 0 :below length
		   :collect (cons (not (null (find i order))) (gensym)))))
    `(lambda ,(mapcar #'cdr specs)
       ,@(if-let ((ignore-list (xform/filter-if #'cdr (compose #'null #'car) specs)))
		 `((declare (ignore ,ignore-list))))
       (,fnsym ,@(mapcar (compose (the function #'cdr) (the function (rcurry #'nth specs))) order)))))

(defun lambda-list-application-types-match-p (typespec list)
  (every (complement #'null) (map-lambda-list (order typep 1 0) typespec list)))

(defun mklist (x)
  "Ensure that X is a list."
  (if (listp x) x (list x)))

(defun ensure-destructurisation (spec form)
  (declare (list spec))
  (cond ((atom form) `(,@(iter (for i below (length spec)) (collect `(nth ,i ,form)))))
        ((= (length form) (length spec)) form)
        (t (error "form ~S doesn't match destructurisation ~S" form spec))))

(defun lambda-list-binds (list)
  "Yield a list of symbols bound by a well-formed lambda LIST."
  (iter (for elt in list)
	(cond ((consp elt)
	       (collect (car elt)))
	      ((not (member elt lambda-list-keywords))
	       (collect elt)))))

(defun emit-declaration (type symbols)
  (when symbols (list (list* type symbols))))

(defun emit-declarations (&key ignore special)
  (when (or ignore special)
    (list* 'declare (append (emit-declaration 'ignore ignore)
			    (emit-declaration 'special special)))))

(defun emit-binding-form-body (body &key declarations)
  (append (when declarations (list declarations)) body))

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

(defmacro with-named-lambda-emission (name lambda-list &body body)
  `(emit-named-lambda ,name ,lambda-list (list ,@body)))

(defmacro lambda-list-satisfied-p (list value)
  "Checks whether the VALUE is destructurable by lambda LIST.
   NOTE: evaluation of VALUE might raise an unrelated error,
   potentially yielding a false negative."
  `(ignore-errors
     (destructuring-bind ,list ,value ,@(emit-lambda-body '(t) :declarations (emit-declarations :ignore (lambda-list-binds list))))))

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
      (error "undefined evaluation domain ~S" domain-name table-name))
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

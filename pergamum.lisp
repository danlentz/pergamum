(defpackage pergamum
  (:use :common-lisp :alexandria)
  (:export
   #:quote-when
   #:lambda-xform
   #:op-parameter-destructurer
   #:define-lambda-parser
   ))

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
;;   (format t "matching ~S to ~S, so far got acc ~S~%" spec form acc)
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

;; This macro belongs to the wider world.
(defmacro define-lambda-parser (parser-name (&rest evaluation-result-vars))
  (with-gensyms (name lambda-list)
    (let ((table-name (format-symbol (symbol-package parser-name) "*~S-PARSER*" parser-name)))
      `(progn
	 (defparameter ,table-name (make-hash-table))
	 
	 (defmacro ,(format-symbol (symbol-package parser-name) "DEFINE-~S" parser-name) (,name ,lambda-list &body body)
	   `(setf (gethash ',,name ,',table-name)
		  #'(lambda ,,lambda-list ,@body)))
	 
	 (defmacro ,(format-symbol (symbol-package parser-name) "APPLY-BIND-~S" parser-name) ((,@evaluation-result-vars) type &body body)
	   "Apply a lambda list parser to an opaque value."
	   (with-gensyms (op paramz lambda-parser)
	     `(op-parameter-destructurer (,op ,paramz) ,type
		(let ((,lambda-parser (gethash ,op ,',table-name)))
		  (unless ,lambda-parser
		    (error "Unknown lambda parser ~A." ,lambda-parser))
		  (multiple-value-bind (,,@evaluation-result-vars) (apply ,lambda-parser ,paramz)
		    ,@body)))))
	 
	 (defmacro ,(format-symbol (symbol-package parser-name) "FUNCALL-BIND-~S" parser-name) ((,@evaluation-result-vars) type &body body)
	   "Apply a lambda list parser to a literally comprehensible form."
	   (with-gensyms (lambda-parser)
	     (op-parameter-destructurer (op paramy) type
	       `(let ((,lambda-parser (gethash ,op ,',table-name)))
		  (unless ,lambda-parser
		    (error "Unknown lambda parser ~A." ,lambda-parser))
		  (multiple-value-bind (,,@evaluation-result-vars) (funcall ,lambda-parser ,@paramy)
		    ,@body)))))))))
  
(in-package :pergamum)

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
	   (destructuring-bind (spec-elt &rest rest) spec
	     (if (member spec-elt '(&optional &key &rest &body))
		 (let ((rest (if (eq spec-elt '&key)
				 (loop :for key-val :in rest :nconc
				    (op-parameter-destructurer (k v) key-val
                                      (when v
                                        (list (list k (intern (string k) :keyword)) (car v)))))
				 rest)))
		   (ecase mode
		     ((&mandatory &optional) (map-lambda-list fn rest form acc spec-elt))
		     (&key (case spec-elt
			     ((&optional &key &rest)
			      (error "Misplaced ~S in lambda list." spec-elt))
                             (t (map-lambda-list fn rest form acc spec-elt))))
		     ((&rest &body) (case spec-elt
				      ((&key &optional)
				       (error "Misplaced ~S in lambda list." spec-elt))
				      (t (map-lambda-list fn rest form acc spec-elt))))))
		 (ecase mode
		   (&mandatory
		    (unless form
		      (error "Missing mandatory argument."))
		    (map-lambda-list fn rest (cdr form) (yield acc spec-elt (car form)) mode))
		   (&optional
                    (op-parameter-destructurer (name default-value) spec-elt
                      (if form
                          (map-lambda-list fn rest (cdr form) (yield acc name (car form)) mode)
                          (nreverse (yield acc name (car default-value))))))
		   (&key
                    (destructuring-bind (&optional key actual-value &rest form-rest) form
                      (unless key
                        (return-from map-lambda-list (nreverse acc)))
                      (unless actual-value
                        (error "odd number of &KEY arguments"))
                      (destructuring-bind ((keysym keyword) default-value &rest spec-rest)
                          (or (member-if (lambda (x) (and (consp x) (eq (second x) key))) spec) (list (list nil nil) nil))
                        (declare (ignore keyword default-value spec-rest))
                        (unless keysym
                          (error "unknown &KEY argument: ~S" key))
                        (map-lambda-list fn (remove-from-plist spec key) form-rest
                                         (yield (cons key acc) keysym actual-value) '&key))))
		   ((&body &rest)
		    (unless (= (length spec) 1)
		      (error "Bad or missing spec for the rest."))
		    (nreverse (nconc (nreverse (mapcar (curry (the function fn) spec-elt) form)) acc))))))))))

(defun lambda-list-application-types-match-p (typespec list)
  (every (complement #'null) (map-lambda-list (order typep 1 0) typespec list)))

(defun lambda-list-binds (list)
  "Yield a list of symbols bound by a well-formed lambda LIST."
  (iter (for elt in list)
	(cond ((consp elt)
	       (collect (car elt)))
	      ((not (member elt lambda-list-keywords))
	       (collect elt)))))

(defun lambda-list-1 (lambda-list-spec lambda-list name)
  "Retrieve from LAMBDA-LIST the value of parameter with NAME, according to LAMBDA-LIST-SPEC."
  (unless (find name (lambda-list-binds lambda-list-spec))
    (error "~S not bound by lambda list spec ~S" name lambda-list-spec))
  (map-lambda-list (lambda (selt felt) (when (eq selt name) (return-from lambda-list-1 felt))) lambda-list-spec lambda-list)
  nil)
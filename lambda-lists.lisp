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

(defun lambda-list-application-types-match-p (typespec list)
  (every (complement #'null) (map-lambda-list (order typep 1 0) typespec list)))

(defun lambda-list-binds (list)
  "Yield a list of symbols bound by a well-formed lambda LIST."
  (iter (for elt in list)
	(cond ((consp elt)
	       (collect (car elt)))
	      ((not (member elt lambda-list-keywords))
	       (collect elt)))))

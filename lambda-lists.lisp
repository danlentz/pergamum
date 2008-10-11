(in-package :pergamum)

(defun map-lambda-list (fn spec form &key insert-keywords)
  "Map FN on the lambda list application with the corresponding
   elements of the lambda list SPEC.

   FN must be a function of four arguments: variable lambda type, variable name, default value and actual value."
  (labels ((yield (acc type var default actual)
             (cons (funcall fn type var default actual) acc))
           (rec (spec form acc mode)
             (cond ((and (null spec) (null form))
                    (nreverse acc))
                   ((null spec)
                    (case mode
                      (&key (if (evenp (length form))
                                (error "unknown &KEY argument: ~S" (car form))
                                (error "odd number of &KEY arguments")))
                      (t (error "~@<excess arguments in form: ~S, spec: ~S~@:>" form spec))))
                   (t
                    (destructuring-bind (spec-elt &rest spec-rest) spec
                      (if (member spec-elt '(&optional &key &rest &body))
                          (let ((spec-rest (if (eq spec-elt '&key)
                                               (mapcar (fif-1 #'consp #'identity (rcurry #'list nil)) spec-rest)
                                               spec-rest)))
                            (when (or (and (member mode '(&key)) (member spec-elt '(&optional &key &rest &body)))
                                      (and (member mode '(&rest &body)) (member spec-elt '(&optional &rest &key &body))))
                              ;; &rest -> &key transitions are disallowed, as we have interesting semantics for their handling
                              (error "~@<Misplaced ~S in lambda list~@:>" spec-elt))
                            (rec spec-rest form acc spec-elt))
                          (ecase mode
                            (&mandatory
                             (unless form
                               (error "Missing mandatory argument."))
                             (rec spec-rest (cdr form) (yield acc mode spec-elt (car form) (car form)) mode))
                            (&optional
                             (op-parameter-destructurer (name default-value) spec-elt
                               (rec spec-rest (cdr form) 
                                    (yield acc mode name default-value (if form (car form) (car default-value))) mode)))
                            (&key
                             (destructuring-bind (keysym default-value) spec-elt
                               (let ((keyword (intern (symbol-name keysym) :keyword)))
                                 (multiple-value-bind (got-p actual-value tail) (get-properties form `(,keyword))
                                   (declare (ignore tail))
                                   (let ((winning-value (if got-p actual-value default-value)))
                                     (rec spec-rest (remove-from-plist form keyword)
                                          (yield (if insert-keywords (cons keyword acc) acc) mode keysym default-value winning-value) '&key))))))
                            ((&body &rest)
                             (unless (= (length spec) 1)
                               (error "Bad or missing spec for &REST."))
                             (nreverse (nconc (nreverse (mapcar (curry (the function fn) mode spec-elt spec-elt) form)) acc))))))))))
    (rec spec form nil '&mandatory)))

(defun map-lambda-list-defaults-actuals (fn lambda-list application &key insert-keywords)
  (map-lambda-list (order-funcalling (fn 4) 2 3) lambda-list application :insert-keywords insert-keywords))

(defun map-lambda-list-bindings-actuals (fn lambda-list application &key insert-keywords)
  (map-lambda-list (order-funcalling (fn 4) 1 3) lambda-list application :insert-keywords insert-keywords))

(defun lambda-list-binds (list)
  "Yield a list of symbols bound by a well-formed lambda LIST."
  (iter (for elt in list)
	(cond ((consp elt)
	       (collect (car elt)))
	      ((not (member elt lambda-list-keywords))
	       (collect elt)))))

(defun lambda-list-1 (lambda-list application-form name)
  "Retrieve from LAMBDA-LIST the value of parameter with NAME, according to LAMBDA-LIST-SPEC."
  (unless (find name (lambda-list-binds lambda-list))
    (error "~S not bound by lambda list spec ~S" name lambda-list))
  (map-lambda-list-bindings-actuals (lambda (var actual) (when (eq var name) (return-from lambda-list-1 actual))) lambda-list application-form)
  nil)
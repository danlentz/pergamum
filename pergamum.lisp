(in-package :pergamum)

(defmacro nand (&rest rest)
  `(not (and ,@rest)))

(defmacro nor (&rest rest)
  `(not (or ,@rest)))

(define-modify-macro orf (&rest params) or)
(define-modify-macro andf (&rest params) and)
(define-modify-macro xorf (&rest params) xor)
(define-modify-macro notf () not)

(defun quote-when (c form)
  (if c (list 'quote form) form))

(defmacro op-parameter-destructurer ((op params) form &body body)
  (once-only (form)
    `(let (,@(when op `((,op (if (consp ,form) (first ,form) ,form))))
	   ,@(when params `((,params (when (consp ,form) (rest ,form))))))
       ,@body)))

(defmacro order (fnspec &rest order)
  "Given a function specification (either FNSYM or (FNSYM NPARAMS)),
   and a list of positional parameter specifiers, produce a function
   calling the function FNSYM, with received parameters either ignored
   (if their corresponding positional parameter specifier is omitted),
   or passed in the order specified by the aforementioned parameter
   specifiers. Example: (funcall (order > 1 0) 1 2) => NIL."
  (op-parameter-destructurer (fnsym length) fnspec
    (let* ((length (or (car length) (length order)))
           (specs (iter (for i below length)
                        (collect (cons (not (null (find i order))) (gensym))))))
      `(lambda ,(mapcar #'cdr specs)
         ,@(if-let ((ignore-list (xform/filter-if #'cdr (compose #'null #'car) specs)))
                   `((declare (ignore ,ignore-list))))
         (,fnsym ,@(mapcar (compose (the function #'cdr) (the function (rcurry #'nth specs))) order))))))

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

(defun prepend (something list &key (test (complement #'null)) (key #'identity))
  (if (funcall test (funcall key something))
      (list* something list)
      list))

(defun emit-binding-form-body (body &key declarations)
  (prepend (list* 'declare declarations) body :key #'rest))

(defun destructure-binding-form-body (body &optional declarations)
  (if (and (consp body) (consp (car body)) (eq (caar body) 'declare))
      (destructure-binding-form-body (cdr body) (append declarations (cdar body)))
      (values declarations body)))

(defun destructure-def-body (body)
  (destructuring-bind (documentation body) (if (stringp (first body))
                                               (list (first body) (rest body))
                                               (list nil body))
    (multiple-value-bind (declarations body) (destructure-binding-form-body body)
      (values documentation declarations body))))

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

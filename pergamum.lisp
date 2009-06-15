;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PERGAMUM; Base: 10 -*-
;;;

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

(defun quote-if-non-self-evaluating (form)
  (if (or (typep form 'cons) (typep form 'symbol)) (list 'quote form) form))

(defmacro op-parameter-destructurer ((op params) form &body body)
  (once-only (form)
    `(let (,@(when op `((,op (if (consp ,form) (first ,form) ,form))))
	   ,@(when params `((,params (when (consp ,form) (rest ,form))))))
       ,@body)))

(labels ((emit-order (need-funcall-p fnspec order)
           (op-parameter-destructurer (fnsym length) fnspec
             (let* ((length (or (car length) (length order)))
                    (specs (iter (for i below length)
                                 (collect (cons (find i order) (gensym))))))
               `(lambda ,(mapcar #'cdr specs)
                  ,@(if-let ((ignore-list (map-remove-if-not #'cdr (compose #'null #'car) specs)))
                            `((declare (ignore ,@ignore-list))))
                  (,@(when need-funcall-p '(funcall)) ,fnsym ,@(mapcar (compose (the function #'cdr) (the function (rcurry #'nth specs))) order)))))))
 (defmacro order (fnspec &rest order)
   "Given a function specification (either FNSYM or (FNSYM NPARAMS)),
   and a list of positional parameter specifiers, produce a function
   calling the function FNSYM, with received parameters either ignored
   (if their corresponding positional parameter specifier is omitted),
   or passed in the order specified by the aforementioned parameter
   specifiers. Example: (funcall (order > 1 0) 1 2) => NIL."
   (emit-order nil fnspec order))

 (defmacro order-funcalling (fnspec &rest order)
   "Given a function specification (either FNSYM or (FNSYM NPARAMS)),
   and a list of positional parameter specifiers, produce a function
   FUNCALLing FNSYM, with received parameters either ignored
   (if their corresponding positional parameter specifier is omitted),
   or passed in the order specified by the aforementioned parameter
   specifiers. Example: (funcall (order > 1 0) 1 2) => NIL."
   (emit-order t fnspec order)))

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

(defun destructure-binding-form-body (body)
  (labels ((do-destructure (body declarations)
             (if (and (consp body) (consp (car body)) (eq (caar body) 'declare))
                 (do-destructure (cdr body) (append declarations (cdar body)))
                 (values declarations body))))
    (do-destructure body nil)))

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

(defmacro measuring-time-lapse-1 ((time-var) measured-form &body body)
  "First, execute MEASURED-FORM, then execute BODY with TIME-VAR bound to
   amount of seconds it took to execute MEASURED-FORM.

   The return value is that of the MEASURED-FORM."
  (with-gensyms (start-time)
    `(let ((,start-time (get-internal-real-time)))
       (prog1 ,measured-form
         (let* ((,time-var (coerce (/ (- (get-internal-real-time) ,start-time) internal-time-units-per-second) 'float)))
           ,@body)))))

(defmacro measuring-time-lapse ((time-var) measured-form &body body)
  "First, execute MEASURED-FORM, then execute BODY with TIME-VAR bound to
   amount of seconds it took to execute MEASURED-FORM.

   The return value is that of the last form in BODY."
  (with-gensyms (start-time)
    `(let ((,start-time (get-internal-real-time)))
       ,measured-form
       (let* ((,time-var (coerce (/ (- (get-internal-real-time) ,start-time) internal-time-units-per-second) 'float)))
         ,@body))))

(defmacro measuring-performance ((unit-var units &key (rounds 8) (strategy 'minimize) (scale :unit) (type 'float)) &body body)
  "Try measuring performance of BODY's execution, with UNITS numerically representing the total amount
   of work, by splitting that work by executing FORM ROUNDS times with UNIT-VAR bound to a proportionally
   small amount and measuring time of each execution.
   STRATEGY is then applied to select the execution time from candidates, and that is used
   to calculate unit-per-second performance, as a numeric value scaled with regard to SCALE, and
   coerced to TYPE, which must be a subtype of RATIONAL.

   STRATEGY can be either MINIMIZE or MAXIMIZE.
   SCALE can be either :UNIT, :K, :KI, :M or :MI, for scale of 1, 1000, 1024, 1000000 and 1048576,
   correspondingly, or a numeric constant."
  (with-gensyms (seconds)
    `(let ((,unit-var (coerce (ceiling ,units ,rounds) 'integer)))
       (coerce
        (round ,unit-var
               (* (iter (repeat ,rounds)
                        (,strategy (measuring-time-lapse (,seconds) (progn ,@body)
                                     ,seconds)))
                  ,(case scale
                         (:unit 1)
                         (:k 1000)
                         (:ki 1024)
                         (:m 1000000)
                         (:mi 1048576)
                         (t (if (rationalp scale) scale (error "~@<Bad scale: ~S~:@>" scale))))))
        ,type))))
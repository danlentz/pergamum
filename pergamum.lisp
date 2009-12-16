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

(defun vector-= (a1 a2 &optional (start 0) end)
  "Determine whether subsequences of vectors A1 and A2 test positively,
according to #'=, elementwise, in the range determined by START and END.
When END is NIL, it is interpreted as the minimum of lengths of A1 and A2."
  (iter (for i from start below (or end (min (length a1) (length a2))))
        (always (= (aref a1 i) (aref a2 i)))))

(defmacro with-measured-time-lapse ((time-var) measured-form &body body)
  "First, execute MEASURED-FORM, then execute BODY with TIME-VAR bound to
amount of seconds it took to execute MEASURED-FORM.
The return value is that of the MEASURED-FORM."
  (with-gensyms (start-time)
    `(let ((,start-time (get-internal-real-time)))
       (prog1 ,measured-form
         (let* ((,time-var (coerce (/ (- (get-internal-real-time) ,start-time) internal-time-units-per-second) 'float)))
           ,@body)))))

(defmacro with-time-lapse-measure ((time-var) measured-form &body body)
  "First, execute MEASURED-FORM, then execute BODY with TIME-VAR bound to
amount of seconds it took to execute MEASURED-FORM.
The return value is that of the last form in BODY."
  (with-gensyms (start-time)
    `(let ((,start-time (get-internal-real-time)))
       ,measured-form
       (let ((,time-var (float (/ (- (get-internal-real-time) ,start-time) internal-time-units-per-second))))
         ,@body))))

(defmacro with-measured-performance ((unit-var units &key (rounds 8) (strategy 'minimize) (scale :unit) (type 'float)) &body body)
  "Try measuring performance of BODY's execution, with UNITS numerically representing the total amount
of work, by splitting that work by executing FORM ROUNDS times with UNIT-VAR bound to a proportionally
small amount and measuring time of each execution.
STRATEGY is then applied to select the execution time from candidates, and that is used
to calculate unit-per-second performance, as a numeric value scaled with regard to SCALE, and
coerced to TYPE, which must be a subtype of RATIONAL.

STRATEGY can be either MINIMIZE or MAXIMIZE.
SCALE can be either :UNIT, :K, :KI, :M or :MI, for scale of 1, 1000, 1024, 1000000 and 1048576,
correspondingly, or a numeric constant."
  (declare (type (member minimize maximize) strategy))
  (with-gensyms (seconds)
    `(let ((,unit-var (coerce (ceiling ,units ,rounds) 'integer)))
       (coerce
        (round ,unit-var
               (* (iter (repeat ,rounds)
                        (,strategy (with-time-lapse-measure (,seconds) (progn ,@body)
                                     ,seconds)))
                  ,(case scale
                         (:unit 1)
                         (:k 1000)
                         (:ki 1024)
                         (:m 1000000)
                         (:mi 1048576)
                         (t (if (rationalp scale) scale (error "~@<Bad scale: ~S~:@>" scale))))))
        ,type))))
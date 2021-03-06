;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PERGAMUM; Base: 10 -*-
;;;

(in-package :pergamum)

(defvar *debug-condition* nil
  "The condition the debugger was invoked with.")

(defun dsignal (type &rest params)
  "Raise a condition of TYPE parametrised by PARAMS within a dynamic 
   context augmented by binding *DEBUG-CONDITION* to that condition."
  (let ((*debug-condition* (apply #'make-condition type params)))
    (declare (special *debug-condition*))
    (signal *debug-condition*)))

(defun derror (type &rest params)
  "Raise a condition of TYPE parametrised by PARAMS within a dynamic 
   context augmented by binding *DEBUG-CONDITION* to that condition.

   If the condition is not handled the debugger is entered by passing
   the condition object to INVOKE-DEBUGGER."
  (let ((*debug-condition* (apply #'make-condition type params)))
    (declare (special *debug-condition*))
    (signal *debug-condition*)
    (invoke-debugger *debug-condition*)))

(defun invoke-with-condition-recourses (condition fn common-recourse recourses)
  (let (attempted)
    (flet ((try ()
             (setf attempted t)
             (funcall fn)))
      (let* ((attempt-fn #'try)
             (recourses (copy-list recourses)))
        (tagbody retry-tag
           (handler-case (return-from invoke-with-condition-recourses
                           (let ((values (multiple-value-list (funcall attempt-fn))))
                             (if attempted
                                 (values values)
                                 (try))))
             (condition-type (condvar)
               (setf attempted nil)
               (flet ((call-next-recourse-and-retry ()
                        (when-let ((recourse (pop recourses)))
                          (setf attempt-fn (curry (cdr recourses) condvar))
                          (go retry-tag))))
                 (cond (common-recourse
                        (funcall common-recourse condvar))
                       (t
                        (call-next-recourse-and-retry)
                        (error condvar)))))))))))

(defmacro with-condition-recourses (condition form &body clauses)
  "Execute FORM in a dynamic environment where a number of recourses
for CONDITION is available, which determine reactions to it, and
provide pretext for re-execution of FORM, with a reduced set of
available recourses.  When FORM executes without raising a condition,
it supplies the return value.  Whenever CONDITION arises, the control
is transferred to the common recourse, which can be specified in the
following form:
   (:common ([var] [recourse-name]) declaration* form*)
where VAR is optionally bound to the condition for which a recourse is
sought and RECOURSE-NAME is optionally bound to the name of the
activated recourse.  FORM*s are executed in the scope where
CALL-NEXT-RECOURSE-AND-RETRY is lexically bound to a function of no
arguments, which, depending on whether there are any remaining
recourses, either invokes one, and retries execution of FORM, or
returns NIL.  If no forms in the COMMON clause transfer control, the
last form provides the return value of the whole
WITH-CONDITION-RECOURSES form.  The default action of the common
recourse, when the COMMON clause is not provided, is to invoke
CALL-NEXT-RECOURSE-AND-RETRY, and, if it returns, resignal the
condition using ERROR.  Recourses are specified by RECOURSE clauses,
in the following form:
   (<recourse-name> ([var]) declaration* form*)
where VAR is optionally bound to the condition for which a recourse is
sought."
  (with-symbols-packaged-in (try call-next-recourse-and-retry) *package*
    (let* ((common-clause (or (cdr (assoc :common clauses))
                              `((condition) (,call-next-recourse-and-retry) (error condition))))
           (recourse-clauses (remove :common clauses :key #'car)))
      (destructuring-bind ((&optional (condition-var (gensym)) (recourse-name-var (gensym))) &body common-clause-body) common-clause
        (flet ((emit-1lam (maybe-arg body &aux (unmaybe-arg (gensym)))
                 `(lambda (,(or maybe-arg unmaybe-arg))
                    ,@(unless maybe-arg `((declare (ignore ,unmaybe-arg))))
                    ,@body)))
          (with-gensyms (block attempted attempt-fn recourses retry-tag)
            `(block ,block
               (let (,attempted)
                 (flet ((,try ()
                          (setf ,attempted t)
                          ,form))
                   (let ((,attempt-fn #',try))
                     (let ((,recourses (list ,@(iter (for (name binding . body) in recourse-clauses)
                                                     (collect `(cons ',name ,(emit-1lam (car binding) body)))))))
                       (tagbody ,retry-tag
                          (handler-case (return-from ,block (let ((values (multiple-value-list (funcall ,attempt-fn))))
                                                              (if ,attempted
                                                                  (values values)
                                                                  (,try))))
                            (,condition (,condition-var)
                              (setf ,attempted nil)
                              (flet ((,call-next-recourse-and-retry ()
                                       (when-let ((recourse (pop ,recourses)))
                                         (setf ,attempt-fn (curry (cdr recourse) ,condition-var))
                                         (go ,retry-tag))))
                                (let ((,recourse-name-var (caar ,recourses)))
                                  (declare (ignorable ,recourse-name-var))
                                  ,@(butlast common-clause-body)
                                  (return-from ,block ,(car (last common-clause-body)))))))))))))))))))

(defun signalling (type &rest params)
  (lambda (&rest rest)
    (declare (ignore rest))
    (signal type params)))

(defun erring (type &rest params)
  (lambda (&rest rest)
    (declare (ignore rest))
    (error type params)))

(defmacro with-breaking-on-conditions ((type) &body body)
  "Execute BODY within dynamic extent, where unhandled conditions of
TYPE are intercepted and BREAK-ed upon."
  (with-gensyms (cond)
    `(handler-case (progn ,@body)
       (,type (,cond)
         (break "Caught: ~A." ,cond)))))

(defmacro with-retry-restarts ((&rest restart-clauses) &body body)
  `(loop (restart-case (return (progn ,@body))
           ,@(iter (for clause in restart-clauses)
                   (destructuring-bind (restart-name condition-binding &body restart-body) clause
                     (collect `(,restart-name (,@condition-binding)
                                              ,@restart-body)))))))

(defmacro with-ignore-restart ((restart-name condition-binding &body restart-body) &body body)
  (with-gensyms (block)
   `(block ,block
      (restart-case (progn ,@body)
        (,restart-name (,@condition-binding)
          ,@restart-body
          (return-from ,block))))))

(defun make-fixed-restarter (restart-name &rest params)
  (lambda (cond)
    (with-condition-restarts cond (list (find-restart restart-name))
      (apply #'invoke-restart restart-name cond params))))

(defmacro with-condition-restart-binding ((&rest condition-restart-specs) &body body)
  `(handler-bind (,@(iter (for (condition restart . params) in condition-restart-specs)
                          (collect `(,condition (make-fixed-restarter ',restart ,@params)))))
     ,@body))

(defmacro with-maybe-just-printing-conditions ((stream type &optional (format "~@<;; ~@;~A~:@>~%")) just-print-p &body body)
  "Execute BODY with conditions of TYPE being handled by printing them
into STREAM, iff JUST-PRINT-P is non-NIL."
  (with-gensyms (cond)
    `(flet ((body () ,@body))
       (if ,just-print-p
           (handler-case (funcall #'body)
             (,type (,cond)
               (format ,stream ,format ,cond)))
           (funcall #'body)))))

(defmacro with-condition-printing ((stream type) &body body)
  "Execute BODY with conditions of TYPE being handled by printing them."
  (with-gensyms (cond)
    `(handler-case (progn ,@body)
       (,type (,cond)
	 (format ,stream "~@<~A~:@>" ,cond)))))

(defmacro with-error-resignaling ((&rest clauses) &body body)
  "Execute BODY within dynamic extent, where CLAUSE establish
handlers for certain condition types, which resignal those conditions
as conditions of a different type.

Each clause must be of a following form:

 (TYPE ((&optional COND) &rest AS))

Such a clause would establish a handler for conditions of TYPE, which
would then be resignalled by splicing the AS argument list to an ERROR
form, with the latter evaluated within a lexical context, where COND
would be optionally bound to the condition being handled."
  `(handler-bind (,@(mapcar (lambda (clause)
                              (destructuring-bind (type ((&optional cond) &rest as)) clause
                                (let ((binding (or cond (gensym "IGNORED"))))
                                  `(,type (lambda (,binding)
                                            ,@(when (null cond) `((declare (ignore ,binding))))
                                            (error ',(first as) ,@(rest as)))))))
                            clauses))
     ,@body))

(defmacro with-collected-conditions ((type &optional backtrace backtrace-as-list) &body body)
  "Execute BODY with conditions of TYPE handled by returning the condition
object as primary value, and optionally, the backtrace as the secondary value.
When no condition of TYPE arises, return NIL as primary and secondary values,
shifting normal return values produced by BODY by two."
  (with-gensyms (block)
    `(block ,block
       (handler-bind ((,type (lambda (c)
                               (return-from ,block
                                 (values c ,(when backtrace
                                                  `(when ,backtrace
                                                     (if ,backtrace-as-list
                                                         (backtrace-as-list most-positive-fixnum)
                                                         (with-output-to-string (string-stream)
                                                           (backtrace most-positive-fixnum string-stream))))))))))
         (multiple-value-call #'values nil nil (progn ,@body))))))

(defun invoke-with-recorded-status (backtracep backtrace-as-list-p fn)
  (declare (boolean backtracep) (function fn))
  (multiple-value-bind (condition backtrace return-value)
      (with-collected-conditions (serious-condition backtracep backtrace-as-list-p)
        (funcall fn))
    (list* :return-value return-value
           (when condition (list* :condition condition
                                  (when backtrace
                                    `(:backtrace ,backtrace)))))))

(defun invoke-with-maybe-prepended-output (maybe fn)
  (declare (boolean maybe) (function fn))
  (if maybe
      (let ((output (make-string-output-stream)))
        (destructuring-bind (&rest status &key &allow-other-keys) (let ((*standard-output* output)
                                                                        (*error-output* output))
                                                                    (funcall fn))
          (list* :output (string-right-trim '(#\Newline) (get-output-stream-string output)) status)))
      (funcall fn)))

(defmacro with-recorded-status ((&key record-backtrace backtrace-as-list record-output) &body body)
  "Execute BODY, while recording its return value and its various effects,
returning them in a property list, with following properties:

 :RETURN-VALUE - the value returned by BODY, unless a condition of type ERROR
                 interrupts its execution, in which case NIL is returned;
 :OUTPUT       - the aggregate output to the *STANDARD-OUTPUT* and *ERROR-OUTPUT*
                 streams, captured when RECORD-OUTPUT evaluates to non-NIL;
 :CONDITION    - the condition of type ERROR, if any such arises during execution
                 of BODY;
 :BACKTRACE    - the backtrace at the point of occurence of the error, if both
                 the error occured, and RECORD-BACKTRACE evaluates to non-NIL."
  `(invoke-with-maybe-prepended-output
    ,record-output
    (lambda ()
      (invoke-with-recorded-status ,record-backtrace ,backtrace-as-list
                                   (lambda () ,@body)))))

(defmacro condition-bind-default ((&rest bindings) &body body)
  "Establish default bindings in the Zetalisp style, as described in the
   2001 Kent Pitman's paper `Condition Handling in the Lisp Language Family.'"
  (with-gensyms (cond)
    `(handler-bind (,@(iter (for (type handler) in bindings)
                            (collect `(,type (lambda (,cond)
                                               (signal ,cond)
                                               (funcall ,handler ,cond))))))
       ,@body)))

(defmacro define-reported-condition (name superclasses slots &rest options)
  "Like DEFINE-CONDITION, but the :REPORT option is handled differently.

   Its format is as follows:
   (:report (&rest slots) format-control &rest format-arguments)
   The reporting is established by a call to FORMAT, with SLOTS bound
   to the slots of the reported condition."
  (if-let ((report-option (rest (assoc :report options))))
          (destructuring-bind (bindings format-control &rest format-arguments) report-option
            (with-gensyms (condition stream)
              `(define-condition ,name ,superclasses ,slots
                 (:report (lambda (,condition ,stream)
                            (with-slots ,bindings ,condition
                              (format ,stream ,format-control ,@format-arguments)))))))
          (error "~@<Error while parsing arguments to DEFMACRO DEFINE-REPORTED-CONDITION: an :REPORT option is mandatory.~:@>")))

(defun report-simple-condition (condition stream)
  (apply #'format stream (simple-condition-format-control condition) (simple-condition-format-arguments condition)))

(defmacro define-simple-error (base-type &key object-initarg)
  "Define a simple error subclassing from BASE-TYPE and a corresponding
function, analogous to ERROR, but also optionally taking the object 
against which to err, and passing it to ERROR via the OBJECT-INITARG
keyword. The name of the simple error is constructed by prepending
'SIMPLE-' to BASE-TYPE.
Whether or not the error signaller will require and pass the
object is specified by OBJECT-INITARG being non-NIL."
  (let ((type (format-symbol t "SIMPLE-~A" base-type)))
    `(progn
       (define-condition ,type (,base-type simple-error)
         ()
         (:report report-simple-condition))
       (defun ,base-type (,@(when object-initarg `(o)) format-control &rest format-arguments)
         (error ',type ,@(when object-initarg `(,object-initarg o)) :format-control format-control :format-arguments format-arguments)))))

(defun simple-condition-reporter (condition stream)
  "The should-have-been-defined report function for simple conditions."
  (declare (stream stream) (simple-condition condition))
  (apply #'format stream (simple-condition-format-control condition)
         (simple-condition-format-arguments condition)))

;;;
;;; *BREAK-ON-SIGNALS* control
;;;
(defun call-with-maybe-break-on-signals-set (bool bos-value fn)
  (if bool
      (let ((*break-on-signals* bos-value))
        (funcall fn))
      (funcall fn)))

(defmacro with-maybe-set-break-on-signals ((bool &optional (bos-value ''error)) &body body)
  `(call-with-maybe-break-on-signals-set ,bool ,bos-value (lambda () ,@body)))

;;;
;;; Canned conditions
;;;
(define-condition redefinition ()
  ())

(define-condition simple-redefinition (redefinition simple-warning)
  ())

(define-condition bad-redefinition (redefinition simple-error)
  ())

(define-reported-condition missing-implementation (program-error)
  ((missing-designator :accessor missing-implementation-designator :initarg :designator))
  (:report (missing-designator)
           "~@<~A not implemented.~:@>" missing-designator)
  (:default-initargs
      :designator "Function"))

(defun warn-redefinition (format-control &rest arguments)
  (warn 'simple-redefinition :format-control format-control :format-arguments arguments))

(defun bad-redefinition (format-control &rest arguments)
  (error 'bad-redefinition :format-control format-control :format-arguments arguments))

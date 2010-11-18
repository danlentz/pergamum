;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PERGAMUM; Base: 10 -*-
;;;

(in-package :pergamum)


;;;;
;;;; IDENTITY
;;;;
(defun object-identity (x)
  #-(or sbcl)
  (not-implemented)
  #+sbcl
  (sb-kernel:get-lisp-obj-address x))

;;;;
;;;; NAMED-OBJECT & NAMED-STRUCTURE
;;;;
(defclass named-object ()
  ((name :accessor object-name :initarg :name)))

(defstruct named-structure
  name)

(defmethod object-name ((o named-structure))
  (named-structure-name o))

(defmethod (setf object-name) (value (o named-structure))
  (setf (named-structure-name o) value))

;;;;
;;;; COPY-SLOTS
;;;;
(defun copy-slots (from to slots)
  (dolist (slot slots)
    (setf (slot-value to slot) (slot-value from slot))))

(define-compiler-macro copy-slots (&whole whole from to slots)
  (if (quoted-p slots)
      (once-only (from to) 
        `(setf ,@(iter (for slot in (quoted-form slots))
                       (nconcing `((slot-value ,to ',slot) (slot-value ,from ',slot))))))
      whole))

;;;;
;;;; SLOT-VALUE*
;;;;
(defun slot-value* (object slot-name &optional (default :unbound-slot))
  "Return the value of slot named SLOT-NAME in OBJECT, when it is bound;
otherwise return DEFAULT, which defaults to :UNBOUND-SLOT."
  (cond ((not (slot-exists-p object slot-name))
         (format nil "#<MISSING-SLOT-~S>" slot-name))
        ((slot-boundp object slot-name)
         (slot-value object slot-name))
        (t default)))

(defmacro define-print-object-method (((object object-type &key (unreadable t) (type t) identity (unbound-slot-value :unbound-slot)) &rest slots) format-control
                                      &rest format-arguments)
  "Define a PRINT-OBJECT method for objects with OBJECT-TYPE.
The definition is a call to FORMAT, with FORMAT-CONTROL and 
FORMAT-ARGUMENTS, with SLOTS bound to slots of OBJECT, defaulting
when the respective slots are unbound, to :UNBOUND-SLOT.
When UNREADABLE is non-NIL, the call to FORMAT is wrapped into
PRINT-UNREADABLE-OBJECT, with TYPE and IDENTITY passed to it."
  (with-gensyms (stream)
    `(defmethod print-object ((,object ,object-type) ,stream)
       (symbol-macrolet ,(iter (for slot in slots)
                               (collect `(,slot (slot-value* ,object ',slot ,unbound-slot-value))))
         ,(if unreadable
              `(print-unreadable-object (,object ,stream :type ,type :identity ,identity)
                 (format ,stream ,format-control ,@format-arguments))
              `(format ,stream ,format-control ,@format-arguments))))))

(define-method-combination primary-method-not-required ()
  ((around (:around))
   (before (:before))
   (primary ())
   (after (:after)))
  :documentation "A variant of standard method combination not requiring the existence of an applicable primary method, in the presence of an :AROUND method."
  (flet ((call-methods (methods)
           (mapcar #'(lambda (method)
                       `(call-method ,method))
                   methods)))
    (let ((form (if (or before after (rest primary))
                    `(multiple-value-prog1
                         (progn ,@(call-methods before)
                                (call-method ,(first primary)
                                             ,(rest primary)))
                       ,@(call-methods (reverse after)))
                    `(call-method ,(first primary)))))
      (if around
          `(call-method ,(first around)
                        (,@(rest around)
                           (make-method ,form)))
          form))))

(define-method-combination most-specific-last ()
  ((around (:around) :order :most-specific-last)
   (before (:before))
   (primary () :required t :order :most-specific-last)
   (after (:after)))
  (flet ((call-methods (methods)
           (mapcar #'(lambda (method)
                       `(call-method ,method))
                   methods)))
    (let ((form (if (or before after (rest primary))
                    `(multiple-value-prog1
                         (progn ,@(call-methods before)
                                (call-method ,(first primary)
                                             ,(rest primary)))
                       ,@(call-methods (reverse after)))
                    `(call-method ,(first primary)))))
      (if around
          `(call-method ,(first around)
                        (,@(rest around)
                           (make-method ,form)))
          form))))

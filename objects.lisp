;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PERGAMUM; Base: 10 -*-
;;;

(in-package :pergamum)

(defun copy-slots (from to slots)
  (dolist (slot slots)
    (setf (slot-value to slot) (slot-value from slot))))

(define-compiler-macro copy-slots (&whole whole from to slots)
  (if (quoted-p slots)
      (once-only (from to) 
        `(setf ,@(iter (for slot in (quoted-form slots))
                       (nconcing `((slot-value ,to ',slot) (slot-value ,from ',slot))))))
      whole))

(defun slot-value* (object slot-name &optional (default :unbound-slot))
  "Return the value of slot named SLOT-NAME in OBJECT, when it is bound;
   otherwise return DEFAULT, which defaults to :UNBOUND-SLOT."
  (if (slot-boundp object slot-name)
      (slot-value object slot-name)
      default))

(defmacro define-print-object-method (((object type &key (unbound-slot-value :unbound-slot)) &rest slots) format-control &rest format-arguments)
  "Define a PRINT-OBJECT method for objects with TYPE.

   The definition is a call to FORMAT, with FORMAT-CONTROL and 
   FORMAT-ARGUMENTS, with SLOTS bound to slots of OBJECT, defaulting
   when the respective slots are unbound, to :UNBOUND-SLOT."
  (with-gensyms (stream)
    `(defmethod print-object ((,object ,type) ,stream)
       (symbol-macrolet ,(iter (for slot in slots)
                               (collect `(,slot (slot-value* ,object ',slot ,unbound-slot-value))))
         (format ,stream ,format-control ,@format-arguments)))))

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

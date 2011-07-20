;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PERGAMUM; Base: 10 -*-
;;;

(in-package :pergamum)


(defmacro define-protocol-class (name provided-superclasses slots &rest options)
  `(progn
     (defclass ,name (,@provided-superclasses)
       (,@slots)
       ,@options)
     (let ((the-class (find-class ',name)))
       (defmethod initialize-instance :after ((o ,name) &key &allow-other-keys)
         (when (eq (class-of o) the-class)
           (error 'protocol-class-instantiation :class (class-of o)))))))

(defmacro defclass-structly (name lambda-list &body slots)
  "TODO:
- support defstruct constructor lambda lists
- support read-only option"
  (let* ((keyword-posn (position-if #'keywordp lambda-list))
         (superclasses (subseq lambda-list 0 keyword-posn))
         (ds-options (when keyword-posn
                       (nthcdr keyword-posn lambda-list))))
    (assert (evenp (length ds-options)))
    (destructuring-bind (&rest keys &key as-struct (readers (not as-struct)) writers no-constructor (slot-prefix "") (slot-suffix "") &allow-other-keys) ds-options
      (if as-struct
          (progn
            (when (or readers writers no-constructor)
              (error "~@<In DEFCLASS-STRUCTLY ~A: :AS-STRUCT option is incompatible with :READER, :WRITER and :NO-CONSTRUCTOR options.~:@>" name))
            (when (> (length superclasses) 1)
              (error "~@<In DEFCLASS-STRUCTLY ~A: :AS-STRUCT option is incompatible with multiple inheritance.~:@>" name))
            `(defstruct (,name ,@(remove-from-plist :as-struct :conc-name) :conc-name slot-prefix)
               ,@slots))
          (let (defaults)
            (when-let ((unknown-options (remove-from-plist keys :as-struct :readers :writers :no-constructor :slot-prefix :slot-suffix)))
              (error "~@<In DEFCLASS-STRUCTLY ~A: :AS-STRUCT option does not support following options:~{ ~A~}.~:@>" name unknown-options))
            `(progn
               (defclass ,name ,superclasses
                 ,(iter (for slot-spec in slots)
                        (destructuring-bind (slot-name &optional default &key type) (ensure-list slot-spec)
                          (let ((initarg (make-keyword (string slot-name))))
                            (collect `(,slot-name :initarg ,initarg
                                                  ,@(when type
                                                          `(:type ,type))
                                                  ,@(when (or readers writers)
                                                          `(,(cond ((and readers writers) :accessor)
                                                                   (readers               :reader)
                                                                   (writers               :writer))
                                                             ,(intern (strconcat* slot-prefix (string slot-name) slot-suffix)
                                                                      (symbol-package name))))))
                            (when default
                              (push initarg defaults)
                              (push default defaults)))))
                 ,@(when defaults `((:default-initargs ,@(nreverse defaults)))))
               ,@(unless no-constructor
                         `((defun ,(format-symbol (symbol-package name) "MAKE-~A" name) (,@(mapcar (compose #'first #'ensure-list) slots))
                             (make-instance ',name ,@(iter (for (slot-name . nil) in slots)
                                                           (collect (make-keyword (string slot-name)))
                                                           (collect slot-name))))))))))))
;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PERGAMUM; Base: 10 -*-
;;;

(in-package :pergamum)

(defun hash-table-next (hash-table)
  "Return a key/value pair from the HASH-TABLE, in a present-p/key/value value triplet."
  (with-hash-table-iterator (f hash-table)
    (f)))

(defun hash-table-pop (hash-table)
  "Return a key/value pair from the HASH-TABLE, in a present-p/key/value value triplet,
   while removing that association from HASH-TABLE."
  (multiple-value-bind (has-p key value) (hash-table-next hash-table)
    (when has-p
      (remhash key hash-table)
      (values has-p key value))))

(defmacro hash-table-itearate ((key value) hash-table &body body)
  "Destructively iterate over HASH-TABLE associations, by picking a key/value pair,
   removing that association from the hash-table, and binding KEY and VALUE for
   the iteration body on each iteration step."
  (once-only (hash-table)
    (with-gensyms (value-p)
      `(iter (for (values ,value-p ,key ,value) = (hash-table-pop ,hash-table))
             (while ,value-p)
             ,@body))))

(defun xform-hash-table (fn hash-table)
  "Create a copy of HASH-TABLE, with values transformed by FN."
  (lret ((ret (copy-hash-table hash-table)))
    (iter (for (key val) in-hashtable hash-table)
          (setf (gethash key ret) (funcall fn val)))))

(defun make-hash-table-injector (hash-table)
  "Given a HASH-TABLE, return an injector function accepting KEY and VALUE."
  (lambda (k v)
    (setf (gethash k hash-table) v)))

(defun hash-table-key-present-p (hash-table key)
  "Given a HASH-TABLE and a KEY, return an injector function accepting KEY and VALUE."
  (nth 1 (gethash key hash-table)))

(defmacro apply-key (key element)
  `(if ,key
       (funcall ,key ,element)
       ,element))

(defmacro satisfies-the-test (elt)
  (with-gensyms (key-tmp)
    `(let ((,key-tmp (apply-key key ,elt)))
       (cond (testp (funcall test ,key-tmp))
             (notp (not (funcall test-not ,key-tmp)))
             (t t)))))

(declaim (inline maphash*))
(defun maphash* (function table &key key (test nil testp) (test-not nil notp))
  "Like MAPHASH, but collects the return values of FUNCTION."
  (when (and testp notp)
    (error ":TEST and :TEST-NOT were both supplied."))
  (iter (for (k v) in-hashtable table)
        (when (satisfies-the-test v)
          (collect (funcall function k v)))))

(declaim (inline maphash-keys))
(defun maphash-keys (function table &key key (test nil testp) (test-not nil notp))
  "Like MAPHASH, but calls FUNCTION with each key in the hash table TABLE."
  (when (and testp notp)
    (error ":TEST and :TEST-NOT were both supplied."))
  (iter (for (k v) in-hashtable table)
        (when (satisfies-the-test v)
          (collect (funcall function k)))))

(declaim (notinline maphash-values))
(defun maphash-values (function table &key key (test nil testp) (test-not nil notp))
  "Like MAPHASH, but calls FUNCTION with each value in the hash table TABLE."
  (when (and testp notp)
    (error ":TEST and :TEST-NOT were both supplied."))
  (iter (for (nil v) in-hashtable table)
        (when (satisfies-the-test v)
          (collect (funcall function v)))))

(define-condition container-condition ()
  ((container :initarg :container)))

(define-condition container-missing-cell-error (cell-error container-condition)
  ((type :initarg :type))
  (:report (lambda (condition stream)
             (format stream "~@<~A ~S not defined in ~S~:@>" (slot-value condition 'type) (cell-error-name condition) (slot-value condition 'container)))))

(defmacro define-subcontainer (accessor-name &key (type accessor-name) compound-name-p container-transform container-slot name-transform-fn
                               spread-compound-name-p remover coercer iterator mapper if-does-not-exist (if-exists :warn)
                               (description (string-downcase (string type))) type-allow-nil-p (key-type 'symbol) iterator-bind-key)
  "Define a namespace accessible via the first parameter of the accessors.

   Access to the container can optionally be routed via CONTAINER-TRANSFORM, 
   with the name also being optionally transformable via NAME-TRANSFORM-FN.
   As a variation on this, CONTAINER-SLOT specifies that the container storage
   is to be accessed via (SLOT-VALUE <CONTAINER> CONTAINER-SLOT).

   Compound (i.e. of type CONS) names can be used by providing 
   the COMPOUND-NAME-P key. Spread compound name specification
   (i.e. access like '(accessor 'name-component-1 'name-component-2 ...)' 
   instead of '(accessor '(name-component-1 name-component-2 ...))
   can be achieved by specifying the SPREAD-COMPOUND-NAME key. 
   REMOVER, COERCER, ITERATOR and MAPPER define optional functions/macros to
   coerce names/objects to objects, remove, iterate and map over namespace 
   entries.

   IF-DOES-NOT-EXIST, when specified, seals the runtime behaviour of the
   accessor to the value of the key.
   Note that spread compound name reader methods do not allow runtime
   customisation of the IF-DOES-NOT-EXIST accessor action, and relies on the
   compile-time value of IF-DOES-NOT-EXIST passed to DEFINE-SUBCONTAINER,
   absent which it defaults to :ERROR. 

   Defined keywords:
    :TYPE - type of objects stored through defined accessor, 
            defaults to ACCESSOR-NAME
    :KEY-TYPE - type of keys, defaults to SYMBOL. Only makes a difference
                in the coercer type dispatch
    :COMPOUND-NAME-P, :CONTAINER-TRANSFORM, :NAME-TRANSFORM-FN, 
    :SPREAD-COMPOUND-NAME-P - see above
    :IF-EXISTS - one of :ERROR, :WARN or :CONTINUE, defaults to :WARN
    :COERCER - whether to define a coercer function, called COERCE-TO-TYPE
    :REMOVER - whether to define a function for removal of namespace entries.
              The name chose is the value of the keyword argument, unless
              it is T, in which case it is REMOVE-TYPE
    :ITERATOR - whether to define an iterator DO-... macro
              The name chosen is the value of the keyword argument, unless
              it is T, in which case it is DO-CONTAINER-TRANSFORM.
    :MAPPER - whether (and how) to define a mapper function.
              The name chosen is the value of the keyword argument, unless
              it is T, in which case it is MAP-CONTAINER-TRANSFORM.
    :DESCRIPTION - specify the printed description of the stored semantic 
              objects.
    :TYPE-ALLOW-NIL-P - Whether to allow store of NIL values.
    :ITERATOR-BIND-KEY - whether to bind the key in the defined iterator

   Typical usages include:
     - variable namespace holder, namespace accessed directly:
       namespace-accessor-name
     - variable namespace holder, namespaces accessed via selector:
       namespace-accessor-name :container-transform SELECTOR"
  (declare (type (member :continue :warn :error) if-exists)
           (type (member :continue :error nil) if-does-not-exist)
           (type (or null string) description))
  (let* ((container-form (cond (container-transform `(,container-transform container))
                               (container-slot `(slot-value container ',container-slot))
                               (t 'container)))
         (hash-key-form (cond ((null name-transform-fn) 'name)
                              ((null compound-name-p) `(,name-transform-fn name))
                              (t `(mapcar #',name-transform-fn name)))))
    `(progn
       ,@(if spread-compound-name-p
             `((defun ,accessor-name (container &rest name)
                 (multiple-value-bind (value presentp) (gethash ,hash-key-form ,container-form)
                   (if presentp
                       value
                       ,(ecase (or if-does-not-exist :error)
                               (:error `(error 'container-missing-cell-error :type ,description :name name :container container))
                               (:continue nil))))))
             `((defun ,accessor-name (container name ,@(unless if-does-not-exist '(&key (if-does-not-exist :error))))
                 (multiple-value-bind (value presentp) (gethash ,hash-key-form ,container-form)
                   (if presentp
                       value
                       ,(ecase if-does-not-exist
                               (:error `(error 'container-missing-cell-error :type ,description :name name :container container))
                               (:continue nil)
                               ((nil)
                                `(ecase if-does-not-exist
                                   (:error (error 'container-missing-cell-error :type ,description :name name :container container))
                                   (:continue nil)))))))))
       (defun (setf ,accessor-name) (val container ,@(when spread-compound-name-p '(&rest)) name)
                 (declare (type ,(if type-allow-nil-p `(or null ,type) type) val))
                 ,@(unless (eq if-exists :continue)
                           `((when (,accessor-name container name :if-does-not-exist :continue)
                               (,(ecase if-exists (:warn 'warn-redefinition) (:error 'bad-redefinition)) "~@<redefining ~A ~S in ~A~:@>" ,description name 'define-sub-container))))
                 (setf (gethash ,hash-key-form ,container-form) val))
       ,@(when-let ((remover remover)
                    (name (if (eq remover t)
                              (format-symbol (symbol-package accessor-name) "REMOVE-~A" type)
                              remover)))
                   `((defun ,name (container ,@(when spread-compound-name-p '(&rest)) name)
                       (remhash ,hash-key-form ,container-form))))
       ,@(when coercer
               `((defun ,(format-symbol (symbol-package accessor-name) "COERCE-TO-~A" type) (container ,@(when spread-compound-name-p '(&rest)) spec)
                   (declare (type (or ,type symbol) spec))
                   (etypecase spec
                     (,type spec)
                     (,key-type ,(if spread-compound-name-p `(apply (function ,accessor-name) container spec) `(,accessor-name container spec)))))))
       ,@(when iterator
               `((defmacro ,(cond ((and iterator (not (eq iterator t))) iterator)
                                  (container-transform (format-symbol (symbol-package accessor-name) "DO-~A" container-transform))
                                  (t (error "~@<It is not known to me, how to name the iterator: neither :ITERATOR, nor :CONTAINER-TRANSFORM were provided.~:@>")))
                     ((,@(when iterator-bind-key '(key)) var container) &body body)
                   ;; IQ test: do you understand ,',? I don't.
                   `(iter (for (,,(if iterator-bind-key 'key nil) ,var) in-hashtable (,',container-transform ,container))
                          ,@body))))
       ,@(when mapper
               `((defun ,(format-symbol (symbol-package accessor-name) "MAP-~A" container-transform) (fn container &rest parameters)
                   (apply #'maphash-values fn ,container-form parameters)))))))

(defmacro define-root-container (container accessor-name &key (type accessor-name) compound-name-p container-transform container-slot name-transform-fn
                                 spread-compound-name-p remover coercer iterator mapper if-does-not-exist (if-exists :warn)
                                 (description (string-downcase (string type))) type-allow-nil-p (key-type 'symbol) iterator-bind-key)
  "Define a namespace stored in CONTAINER.

   Access to the container can optionally be routed via CONTAINER-TRANSFORM, 
   with the name also being optionally transformable via NAME-TRANSFORM-FN.
   As a variation on this, CONTAINER-SLOT specifies that the container storage
   is to be accessed via (SLOT-VALUE <CONTAINER> CONTAINER-SLOT).

   Compound (i.e. of type CONS) names can be used by providing 
   the COMPOUND-NAME-P key. Spread compound name specification
   (i.e. access like '(accessor 'name-component-1 'name-component-2 ...)' 
   instead of '(accessor '(name-component-1 name-component-2 ...))
   can be achieved by specifying the SPREAD-COMPOUND-NAME key. 
   REMOVER, COERCER, ITERATOR and MAPPER define optional functions/macros to
   coerce names/objects to objects, remove, iterate and map over namespace 
   entries.

   IF-DOES-NOT-EXIST, when specified, seals the runtime behaviour of the
   accessor to the value of the key.
   Note that spread compound name reader methods do not allow runtime
   customisation of the IF-DOES-NOT-EXIST accessor action, and relies on the
   compile-time value of IF-DOES-NOT-EXIST passed to DEFINE-SUBCONTAINER,
   absent which it defaults to :ERROR. 

   Defined keywords:
    :TYPE - type of objects stored through defined accessor, 
            defaults to ACCESSOR-NAME
    :KEY-TYPE - type of keys, defaults to SYMBOL. Only makes a difference
                in the coercer type dispatch
    :COMPOUND-NAME-P, :CONTAINER-TRANSFORM, :NAME-TRANSFORM-FN, 
    :SPREAD-COMPOUND-NAME-P - see above
    :IF-EXISTS - one of :ERROR, :WARN or :CONTINUE, defaults to :WARN
    :IF-SPREAD-COMPOUND-DOES-NOT-EXIST - one of :ERROR or :CONTINUE,
                                         defaults to :ERROR
    :COERCER - whether to define a coercer function, called COERCE-TO-TYPE
    :REMOVER - whether to define a function for removal of namespace entries.
              The name chose is the value of the keyword argument, unless
              it is T, in which case it is REMOVE-TYPE
    :ITERATOR - whether to define an iterator DO-... macro
              The name chosen is the value of the keyword argument, unless
              it is T, in which case it is DO-CONTAINER-TRANSFORM.
    :MAPPER - whether (and how) to define a mapper function.
              The name chosen is the value of the keyword argument, unless
              it is T, in which case it is MAP-CONTAINER-TRANSFORM.
    :DESCRIPTION - specify the printed description of the stored semantic 
              objects.
    :TYPE-ALLOW-NIL-P - Whether to allow store of NIL values.
    :ITERATOR-BIND-KEY - whether to bind the key in the defined iterator

   Typical usages include:
     - global namespace holder, namespace accessed directly:
       *NAMESPACE-HOLDER* namespace-accessor-name
     - global namespace holder, namespaces accessed via selector:
       *NAMESPACE-HOLDER* namespace-accessor-name :container-transform SELECTOR"
  (declare (type (member :continue :warn :error) if-exists)
           (type (member :continue :error nil) if-does-not-exist)
           (type (or null string) description))
  (let* ((container-form (cond (container-transform `(,container-transform ,container))
                               (container-slot `(slot-value ,container ',container-slot))
                               (t container)))
         (hash-key-form (cond ((null name-transform-fn) 'name)
                              ((null compound-name-p) `(,name-transform-fn name))
                              (t `(mapcar #',name-transform-fn name)))))
    `(progn
       ,@(if spread-compound-name-p
             `((defun ,accessor-name (&rest name)
                 (multiple-value-bind (value presentp) (gethash ,hash-key-form ,container-form)
                   (if presentp
                       value
                       ,(ecase (or if-does-not-exist :error)
                               (:error `(error 'container-missing-cell-error :type ,description :name name :container ,container))
                               (:continue nil))))))
             `((defun ,accessor-name (name &key (if-does-not-exist :error))
                 (multiple-value-bind (value presentp) (gethash ,hash-key-form ,container-form)
                   (if presentp
                       value
                       ,(ecase if-does-not-exist
                               (:error `(error 'container-missing-cell-error :type ,description :name name :container ,container))
                               (:continue nil)
                               ((nil)
                                `(ecase if-does-not-exist
                                   (:error (error 'container-missing-cell-error :type ,description :name name :container ,container))
                                   (:continue nil)))))))))
       (defun (setf ,accessor-name) (val ,@(when spread-compound-name-p '(&rest)) name)
                 (declare (type ,(if type-allow-nil-p `(or null ,type) type) val))
                 ,@(unless (eq if-exists :continue)
                           `((when (,accessor-name name :if-does-not-exist :continue)
                               (,(ecase if-exists (:warn 'warn-redefinition) (:error 'bad-redefinition)) "~@<redefining ~A ~S in ~A~:@>" ,description name 'define-root-container))))
                 (setf (gethash ,hash-key-form ,container-form) val))
       ,@(when-let ((remover remover)
                    (name (if (eq remover t)
                              (format-symbol (symbol-package accessor-name) "REMOVE-~A" type)
                              remover)))
                   (if spread-compound-name-p
                       `((defun ,name (rest name)
                           (remhash ,hash-key-form ,container-form)))
                       `((defun ,name (name)
                           (remhash ,hash-key-form ,container-form)))))
       ,@(when coercer
               `((defun ,(format-symbol (symbol-package accessor-name) "COERCE-TO-~A" type) (spec)
                   (declare (type (or ,type symbol) spec))
                   (etypecase spec
                     (,type spec)
                     (,key-type ,(if spread-compound-name-p `(apply (function ,accessor-name) spec) `(,accessor-name spec)))))))
       ,@(when iterator
               `((defmacro ,(cond ((and iterator (not (eq iterator t))) iterator)
                                  (container-transform (format-symbol (symbol-package accessor-name) "DO-~A" container-transform))
                                  (t (error "~@<It is not known to me, how to name the iterator: neither :ITERATOR, nor :CONTAINER-TRANSFORM provided.~:@>")))
                     ((,@(when iterator-bind-key '(key)) var) &body body)
                   ;; IQ test: do you understand ,',? I don't.
                   `(iter (for (,,(if iterator-bind-key 'key nil) ,var) in-hashtable ,',container-form)
                          ,@body))))
       ,@(when mapper
               `((defun ,(if container-transform
                             (format-symbol (symbol-package accessor-name) "MAP-~A" container-transform)
                             mapper) (fn &rest parameters)
                   (apply #'maphash-values fn ,container-form parameters)))))))

(defun copy-hash-table-empty (table &key key test size rehash-size rehash-threshold)
  "Returns an empty copy of hash TABLE. The copy has the same properties
   as the original, unless overridden by the keyword arguments."
  (setf key (or key 'identity))
  (setf test (or test (hash-table-test table)))
  (setf size (or size (hash-table-size table)))
  (setf rehash-size (or rehash-size (hash-table-rehash-size table)))
  (setf rehash-threshold (or rehash-threshold (hash-table-rehash-threshold table)))
  (make-hash-table :test test :size size
                   :rehash-size rehash-size
                   :rehash-threshold rehash-threshold))

(defmacro with-empty-hash-containers ((&rest containers) &body body)
  "Evaluate BODY with CONTAINERS rebound to empty ones."
  `(let ,(iter (for container in containers)
               (collect `(,container (copy-hash-table-empty ,container))))
     ,@body))
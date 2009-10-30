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

(defun puthash-unique (key hash-table value)
  "Only store VALUE under KEY in HASH-TABLE when there's nothing
occupying KEY. The second return value indicates success."
  (declare (type hash-table hash-table))
  (multiple-value-bind (key present-p) (gethash key hash-table)
    (unless present-p
      (values (setf (gethash key hash-table) value)))))

(defsetf gethash-unique puthash-unique)

(define-condition container-condition ()
  ((container :initarg :container)))

(define-condition container-missing-cell-error (cell-error container-condition)
  ((type :initarg :type))
  (:report (lambda (condition stream)
             (format stream "~@<~A ~S not defined in ~S~:@>" (slot-value condition 'type) (cell-error-name condition) (slot-value condition 'container)))))

(defun emit-getter (globalp rootp accessor-name container container-form hash-key-form spread-compound-name-p if-does-not-exist description)
  `((,@(when globalp '(defun)) ,accessor-name (,@(unless rootp '(container)) ,@(if spread-compound-name-p
                                                                                   '(&rest name)
                                                                                   `(name ,@(unless if-does-not-exist '(&key (if-does-not-exist :error))))))
       (multiple-value-bind (value presentp) (gethash ,hash-key-form ,container-form)
         (if presentp
             value
             ,(ecase (or if-does-not-exist (when spread-compound-name-p :error))
                     (:error `(error 'container-missing-cell-error :type ,description :name name :container ,(if rootp container 'container)))
                     (:continue nil)
                     ((nil)
                      (if spread-compound-name-p
                          (error "~@<In EMIT-GETTER: NIL was specified as spread-compound IF-DOES-NOT-EXIST keyword.~:@>")
                          `(ecase if-does-not-exist
                             (:error (error 'container-missing-cell-error :type ,description :name name :container ,(if rootp container 'container)))
                             (:continue nil))))))))))

(defun emit-setter (globalp rootp def-name accessor-name container-form hash-key-form spread-compound-name-p if-exists type type-allow-nil-p description)
  `((,@(when globalp '(defun)) (setf ,accessor-name) (val ,@(unless rootp '(container)) ,@(when spread-compound-name-p '(&rest)) name)
       (declare (type ,(if type-allow-nil-p `(or null ,type) type) val))
       ,@(unless (eq if-exists :continue)
                 `((when (nth-value 1 (gethash ,hash-key-form ,container-form))
                     ,(ecase if-exists
                             (:return-nil `(return-from ,accessor-name))
                             (:warn `(warn-redefinition "~@<redefining ~A ~S in ~A~:@>" ,description name ',def-name)) 
                             (:error `(bad-redefinition "~@<redefining ~A ~S in ~A~:@>" ,description name ',def-name))))))
       (values (setf (gethash ,hash-key-form ,container-form) val)
               t))))

(defun emit-remover (globalp rootp remover accessor-name container-form hash-key-form spread-compound-name-p type)
  (let ((name (if (eq remover t)
                  (format-symbol (symbol-package accessor-name) "REMOVE-~A" type)
                  remover)))
    `((,@(when globalp '(defun)) ,name (,@(unless rootp '(container)) ,@(when spread-compound-name-p '(&rest)) name)
         (remhash ,hash-key-form ,container-form)))))

(defun emit-coercer (globalp rootp accessor-name spread-compound-name-p type key-type)
  `((,@(when globalp '(defun))
       ,(format-symbol (symbol-package accessor-name) "COERCE-TO-~A" type)
       (,@(unless rootp '(container)) ,@(when spread-compound-name-p '(&rest)) spec)
       (declare (type (or ,type symbol) spec))
       (etypecase spec
         (,type spec)
         (,key-type ,(if spread-compound-name-p `(apply (function ,accessor-name) ,@(unless rootp '(container)) spec) `(,accessor-name ,@(unless rootp '(container)) spec)))))))

(defun emit-iterator (globalp iterator accessor-name container-transform container-slot iterator-bind-key)
  `((,@(when globalp '(defmacro)) ,(cond ((and iterator (not (eq iterator t))) iterator)
                                         (container-transform (format-symbol (symbol-package accessor-name) "DO-~A" container-transform))
                                         (t (error "~@<It is not known to me, how to name the iterator: neither :ITERATOR, nor :CONTAINER-TRANSFORM were provided.~:@>")))
       ((,@(when iterator-bind-key '(key)) var container) &body body)
       ;; IQ test: do you understand ,',? I don't. ; ;
       `(iter (for (,,(if iterator-bind-key 'key nil) ,var) in-hashtable ,,(cond (container-transform ``(,container-transform ,container))
                                                                                 (container-slot ``(slot-value ,container ,'',container-slot))
                                                                                 (t 'container)))
              ,@body))))

(defun emit-mapper (globalp rootp mapper accessor-name container-form container-transform)
  `((,@(when globalp '(defun)) ,(if container-transform
                                    (format-symbol (symbol-package accessor-name) "MAP-~A" container-transform)
                                    mapper)
       (fn ,@(when rootp '(container)) &rest parameters)
       (apply #'maphash-values fn ,container-form parameters))))

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
 :IF-EXISTS - one of :ERROR, :WARN, :RETURN-NIL or :CONTINUE, defaults to :WARN
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
  (declare (type (member :continue :return-nil :warn :error) if-exists)
           (type (member :continue :error nil) if-does-not-exist)
           (type (or null string) description))
  (let* ((container-form (cond (container-transform `(,container-transform container))
                               (container-slot `(slot-value container ',container-slot))
                               (t 'container)))
         (hash-key-form (cond ((null name-transform-fn) 'name)
                              ((null compound-name-p) `(,name-transform-fn name))
                              (t `(mapcar #',name-transform-fn name)))))
   `(progn
      ,@(emit-getter t nil accessor-name nil container-form hash-key-form spread-compound-name-p if-does-not-exist description)
      ,@(emit-setter t nil 'define-subcontainer accessor-name container-form hash-key-form spread-compound-name-p if-exists type type-allow-nil-p description)
      ,@(when remover
              (emit-remover t nil remover accessor-name container-form hash-key-form spread-compound-name-p type))
      ,@(when coercer
              (emit-coercer t nil accessor-name spread-compound-name-p type key-type))
      ,@(when iterator
              (emit-iterator t iterator accessor-name container-transform container-slot iterator-bind-key))
      ,@(when mapper
              (emit-mapper t nil mapper accessor-name container-form container-transform)))))

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
 :IF-EXISTS - one of :ERROR, :WARN, :RETURN-NIL or :CONTINUE, defaults to :WARN
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
  (declare (type (member :continue :return-nil :warn :error) if-exists)
           (type (member :continue :error nil) if-does-not-exist)
           (type (or null string) description))
  (let* ((container-form (cond (container-transform `(,container-transform ,container))
                               (container-slot `(slot-value ,container ',container-slot))
                               (t container)))
         (hash-key-form (cond ((null name-transform-fn) 'name)
                              ((null compound-name-p) `(,name-transform-fn name))
                              (t `(mapcar #',name-transform-fn name)))))
    `(progn
       ,@(emit-getter t t accessor-name container container-form hash-key-form spread-compound-name-p if-does-not-exist description)
       ,@(emit-setter t t 'define-root-container accessor-name container-form hash-key-form spread-compound-name-p if-exists type type-allow-nil-p description)
       ,@(when remover
               (emit-remover t t remover accessor-name container-form hash-key-form spread-compound-name-p type))
       ,@(when coercer
               (emit-coercer t t accessor-name spread-compound-name-p type key-type))
       ,@(when iterator
               `((defmacro ,(cond ((and iterator (not (eq iterator t))) iterator)
                                  (container-transform (format-symbol (symbol-package accessor-name) "DO-~A" container-transform))
                                  (t (error "~@<It is not known to me, how to name the iterator: neither :ITERATOR, nor :CONTAINER-TRANSFORM provided.~:@>")))
                     ((,@(when iterator-bind-key '(key)) var) &body body)
                   ;; IQ test: do you understand ,',? I don't.
                   `(iter (for (,,(if iterator-bind-key 'key nil) ,var) in-hashtable ,,(cond (container-transform ``(,container-transform container))
                                                                                             (container-slot ``(slot-value container ,'',container-slot))
                                                                                             (t `',container)))
                          ,@body))))
       ,@(when mapper
              (emit-mapper t t mapper accessor-name container-form container-transform)))))

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
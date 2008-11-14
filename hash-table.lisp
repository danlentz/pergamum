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

(declaim (inline maphash-values))
(defun maphash-values (function table &key key (test nil testp) (test-not nil notp))
  "Like MAPHASH, but calls FUNCTION with each value in the hash table TABLE."
  (when (and testp notp)
    (error ":TEST and :TEST-NOT were both supplied."))
  (iter (for (nil v) in-hashtable table)
        (when (satisfies-the-test v)
          (collect (funcall function v)))))

(defmacro define-container-hash-accessor (container-name accessor-name &key (type accessor-name) compound-name-p container-transform name-transform-fn parametrize-container
                                          spread-compound-name-p (spread-compound-name-does-not-exist-behavior :error) coercer mapper (when-exists :warn))
  "Define a namespace, either stored in CONTAINER-NAME, or accessible via the first parameter of the accessors, when PARAMETRIZE-CONTAINER is specified.

   Access to the container can optionally be routed via CONTAINER-TRANSFORM, with the name also being optionally transformable via NAME-TRANSFORM-FN.

   Compound (i.e. of type CONS) names can be used by providing the COMPOUND-NAME-P key. Spread compound name specification
   (i.e. access like '(accessor 'name-component-1 'name-component-2 ...)' instead of '(accessor '(name-component-1 name-component-2 ...))
   can be achieved by specifying the SPREAD-COMPOUND-NAME key. COERCER and MAPPER define optional coercer and mapper.
   
   Typical usages include:
     - global namespace holder, namespace accessed directly:
       *NAMESPACE-HOLDER* namespace-accessor-name
     - global namespace holder, namespaces accessed via selector:
       *NAMESPACE-HOLDER* namespace-accessor-name :container-transform SELECTOR
     - variable namespace holder, namespace accessed directly:
       IGNORED namespace-accessor-name :parametrize-container t
     - variable namespace holder, namespaces accessed via selector:
       IGNORED namespace-accessor-name :container-transform SELECTOR :parametrize-container t"
  (declare (type (member :continue :warn :error) when-exists))
  (let* ((container (if parametrize-container 'container container-name))
         (container-form (if container-transform `(,container-transform ,container) container))
         (hash-key-form (cond ((null name-transform-fn) 'name)
                              ((null compound-name-p) `(,name-transform-fn name))
                              (t `(mapcar #',name-transform-fn name)))))
    (when (and parametrize-container coercer)
      (error "~@<cannot define coercers for accessors with parametrized containers in DEFINE-CONTAINER-ACCESSOR~:@>"))
    `(progn
       ,@(if spread-compound-name-p
             `((defun ,accessor-name (,@(when parametrize-container `(,container)) &rest name)
                 (or (gethash ,hash-key-form ,container-form)
                     ,@(when (eq spread-compound-name-does-not-exist-behavior :error)
                             `((error "~@<~A ~S not defined in ~S~:@>" ,(string-downcase (string type)) name ,container))))))
             `((defun ,accessor-name (,@(when parametrize-container `(,container)) name &key (if-does-not-exist :error))
                 (or (gethash ,hash-key-form ,container-form)
                     (when (eq if-does-not-exist :error)
                       (error "~@<~A ~S not defined in ~S~:@>" ,(string-downcase (string type)) name ,container))))))
       ,@(if spread-compound-name-p
             `((defun (setf ,accessor-name) (val ,@(when parametrize-container `(,container)) &rest name)
                 (declare (type ,type val))
                 ,@(unless (eq when-exists :continue)
                           `((when (,accessor-name name :if-does-not-exist :continue)
                               (,(ecase when-exists (:warn 'warn) (:error 'error)) "~@<redefining ~A ~S~:@>" ,(string-downcase (string type)) name))))
                 (setf (gethash ,hash-key-form ,container-form) val)))
             `((defun (setf ,accessor-name) (val ,@(when parametrize-container `(,container)) name)
                 (declare (type ,type val))
                 ,@(unless (eq when-exists :continue)
                           `((when (,accessor-name name :if-does-not-exist :continue)
                               (,(ecase when-exists (:warn 'warn) (:error 'error)) "~@<redefining ~A ~S~:@>" ,(string-downcase (string type)) name))))
                 (setf (gethash ,hash-key-form ,container-form) val))))
       ,@(when coercer
           `((defun ,(format-symbol (symbol-package accessor-name) "COERCE-TO-~A" type) (spec)
               (declare (type (or ,type symbol) spec))
               (etypecase spec
                 (,type spec)
                 (symbol (,accessor-name spec))))))
       ,@(when mapper
           `((defun ,(format-symbol (symbol-package accessor-name) "MAP-~A" (or container-transform mapper)) (fn ,@(when parametrize-container `(,container)) &rest parameters)
               (apply #'maphash-values fn ,container-form parameters)))))))

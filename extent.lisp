;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PERGAMUM; Base: 10 -*-
;;;

(in-package :pergamum)

(defclass baseless-extent ()
  ((data :accessor extent-data :type vector :initarg :data)))

(defclass extent (baseless-extent)
  ((base :accessor extent-base :type integer :initarg :base)))

(defmethod print-object ((o extent) stream)
  (print-unreadable-object (o stream)
    (format stream "EXTENT ~X:~X" (extent-base o) (extent-end o))))

(defun make-extent (type base vector &rest keys &key (element-type (array-element-type vector) element-type-p) &allow-other-keys)
  (declare (type integer base) (type vector vector))
  (apply #'make-instance type
         :base base
         :data (if (and element-type-p (subtypep (array-element-type vector) element-type))
                   vector
                   (make-array (length vector) :element-type element-type :initial-contents vector))
         (remove-from-plist keys :element-type :base :data)))

(defun extent-length (extent)
  (declare (type baseless-extent extent))
  (array-dimension (extent-data extent) 0))

(defun extent-end (extent)
  (declare (type extent extent))
  (+ (extent-base extent) (extent-length extent)))

(defun point-extent-base-p (extent p)
  (declare (type integer p) (type extent extent))
  (= p (extent-base extent)))

(defun point-in-extent-p (extent p)
  (declare (type integer p) (type extent extent))
  (and (>= p (extent-base extent)) (< p (+ (extent-base extent) (extent-length extent)))))

(defun extents-intersect-p (x y)
  (declare (type extent x y))
  (and (plusp (extent-length x)) (plusp (extent-length y))
       (or (point-in-extent-p x (extent-base y)) (point-in-extent-p x (+ (extent-base y) (extent-length y) -1)))))

(defun split-extent (extent offset &optional ignore-head ignore-tail)
  (values (unless ignore-head (make-extent 'extent (extent-base extent) (subseq (extent-data extent) 0 offset)))
          (unless ignore-tail (make-extent 'extent (+ (extent-base extent) offset) (subseq (extent-data extent) offset)))))

(defmacro with-split-extent ((head tail) extent offset &body body)
  (let ((headvar (or head (gensym)))
        (tailvar (or tail (gensym))))
    `(multiple-value-bind (,headvar ,tailvar) (split-extent ,extent ,offset ,(null head) ,(null tail))
       ,@(unless (and head tail) `((declare (ignore ,@(unless head `(,headvar)) ,@(unless tail `(,tailvar))))))
       ,@body)))

(defun rebase-extent (fn extent)
  "Call FN on EXTENT's address, combining the result with its data for
making of a new extent."
  (declare (type (function (integer) integer) fn) (type extent extent))
  (make-extent (type-of extent) (funcall fn (extent-base extent)) (extent-data extent)))

(defun extent-data-equalp (e1 e2)
  "Return T if data vectors of E1 and E2 match according to EQUALP."
  (declare (type extent e1 e2))
  (equalp (extent-data e1) (extent-data e2)))

(defun extent-equalp (e1 e2)
  "Return T if data vectors of E1 and E2 match according to EQUALP,
and their base addresses match as well."
  (declare (type extent e1 e2))
  (and (extent-data-equalp e1 e2)
       (= (extent-base e1) (extent-base e2))))

;;;
;;; Extent specs.
;;;
(defun extent-spec (extent)
  (declare (type extent extent))
  (cons (extent-base extent) (length (extent-data extent))))
 
(deftype extent-spec () `(cons integer (integer 0)))

(defun extent (base length)
  (cons base length))

(defun print-extent-spec (stream spec colon at-sign)
  (declare (ignore colon at-sign))
  (pprint-logical-block (stream spec)
    (format stream "(~X:~X)" (car spec) (+ (car spec) (cdr spec)))))

(defun print-extent (stream extent &optional (endianness :little-endian))
  (print-u8-sequence stream (extent-data extent) :address (extent-base extent) :endianness endianness))

(defgeneric serialize-extent (stream extent)
  (:method (stream (o extent))
    (print (cons (extent-base o) (extent-data o)) stream)))

(defun extent-spec-base (spec)
  (car spec))

(defun extent-spec-length (spec)
  (cdr spec))

(defun extent-spec-end (spec)
  (+ (car spec) (cdr spec)))

(defun rebase-extent-spec (fn extent-spec)
  "Call FN on EXTENT-SPEC's address and combine the result with 
its length to create a new extent spec."
  (declare (type (function (integer) integer) fn) (type cons extent-spec))
  (cons (funcall fn (car extent-spec)) (cdr extent-spec)))

;;;
;;; Subextents.
;;;
(defclass subextent ()
  ((parent :accessor subextent-parent :type bioable :initarg :parent)
   (extent :accessor subextent-extent :type cons :initarg :extent)))

(defmethod print-object ((subextent subextent) stream)
  (format stream "~@<#<SUBEXTENT~; extent: ~S  parent: ~S~;>~:@>"
          (slot-value subextent 'extent) (slot-value subextent 'parent)))

(defun subextent-absolutize (subex val)
  (+ val (extent-spec-base (subextent-extent subex))))

(defun subextent-relativize (subex val)
  (- val (extent-spec-base (subextent-extent subex))))

;;;
;;; Utilities. Crap ones, admittedly.
;;;
(defmacro do-extent-spec-aligned-blocks (alignment (addr len spec) &body body)
  "Execute body with ADDR being set to all successive beginnings of ALIGNMENT-aligned blocks covering the extent specified by SPEC."
  (once-only (alignment spec)
    `(iter (for ,addr from (align-down ,alignment (extent-spec-base ,spec)) below (extent-spec-end ,spec) by ,alignment)
           (for ,len = (min ,alignment (- (extent-spec-end ,spec) ,addr)))
           ,@body)))

(defmacro with-aligned-extent-spec-pieces (alignment (prehead head body tail &optional posttail) extent-spec &body innards)
  "Bind the HEAD, BODY and TAIL pieces of EXTENT-SPEC, with possible destructurisation, as mandated by aligning it by ALIGNMENT (evaluated)."
  (let ((d-prehead (consp prehead)) (d-head (consp head)) (d-body (consp body)) (d-tail (consp tail)) (d-posttail (consp posttail)))
    (with-optional-subform-captures (((prehead-base prehead-length) (car cadr) d-prehead prehead)
                                     ((head-base head-length) (car cadr) d-head head)
                                     ((body-base body-length) (car cadr) d-body body)
                                     ((tail-base tail-length) (car cadr) d-tail tail)
                                     ((posttail-base posttail-length) (car cadr) d-posttail posttail))
      (with-gensyms (base length alignment-mask)
        (once-only (alignment extent-spec)
          `(let* ((,alignment-mask (1- ,alignment))
                  (,base (car ,extent-spec))
                  (,length (cdr ,extent-spec))
                  (,prehead-base (logandc1 ,alignment-mask ,base))
                  (,prehead-length (logand ,alignment-mask ,base))
                  (,head-base ,base)
                  (,head-length (min ,length (logand ,alignment-mask (- ,alignment ,prehead-length))))
                  (,body-base (+ ,head-base ,head-length))
                  (,body-length (logandc1 ,alignment-mask (- ,length ,head-length)))
                  (,tail-base (+ ,body-base ,body-length))
                  (,tail-length (- ,length ,head-length ,body-length))
                  ,@(when posttail `((,posttail-base (+ ,tail-base ,tail-length))))
                  ,@(when posttail `((,posttail-length (- ,alignment (logand ,alignment-mask (+ ,tail-base ,tail-length))))))
                  ,@(unless d-prehead `((,prehead (cons ,prehead-base ,prehead-length))))
                  ,@(unless d-head `((,head (cons ,base ,head-length))))
                  ,@(unless d-body `((,body (cons ,body-base ,body-length))))
                  ,@(unless d-tail `((,tail (cons ,tail-base ,tail-length))))
                  ,@(unless (or (null posttail) d-posttail) `((,posttail (cons ,posttail-base ,posttail-length)))))
             (declare (ignorable ,prehead-base ,prehead-length ,head-base ,head-length ,body-base ,body-length ,tail-base ,tail-length ,@(when posttail `(,posttail-base ,posttail-length))))
             ,@innards))))))

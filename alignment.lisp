;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PERGAMUM; Base: 10 -*-
;;;

(in-package :pergamum)

(define-condition alignment-condition ()
  ((alignment-requirement :accessor condition-alignment-requirement :initarg :alignment-requirement)))

(define-condition address-misalignment (alignment-condition)
  ((address :accessor condition-address :initarg :address))
  (:report (lambda (condition stream)
             (format stream "operation requires the address to be aligned by ~S, which is violated by the address value of ~S"
                     (condition-alignment-requirement condition) (condition-address condition)))))

(define-condition size-misalignment (alignment-condition)
  ((size :accessor condition-size :initarg :size))
  (:report (lambda (condition stream)
             (format stream "operation requires the size to be aligned by ~S, which is violated by the size value of ~S"
                     (condition-alignment-requirement condition) (condition-size condition)))))

(defun aligned-p (alignment value)
  (and (integerp value) (zerop (logand value (1- alignment)))))

(defun aligned-by-4-p (value)
  (aligned-p 4 value))

(defun aligned-by-16-p (value)
  (aligned-p 16 value))

(deftype aligned-4 () `(satisfies aligned-by-4-p))

(deftype aligned-16 () `(satisfies aligned-by-16-p))

(defun align-down (alignment value)
  (logandc1 (1- alignment) value))

(defun align-up (alignment value)
  (let ((tail (logand (1- alignment) value)))
    (+ (align-down alignment value)
       (if (zerop tail) 0 alignment))))

(defun check-size-alignment (req value)
  (unless (aligned-p req value)
    (error 'size-misalignment :alignment-requirement req :size value)))

(defun check-address-alignment (req value)
  (unless (aligned-p req value)
    (error 'address-misalignment :alignment-requirement req :address value)))

(defmacro with-alignment ((aligned &optional left right mask) alignment value &body body)
  "Execute BODY with ALIGNED, LEFT, RIGHT and MASK bound to various
products of applying ALIGNMENT to VALUE.
ALIGNED denotes the floor alignment, LEFT denotes the difference
between VALUE and ALIGNED and RIGHT denotes the difference between
VALUE and the sum of ALIGNED and ALIGNMENT."
  (let ((once-value (gensym))
        (mask (or mask (gensym))))
    `(let* ((,once-value ,value)
            (,mask (1- ,alignment))
            ,@(when aligned `((,aligned (logandc1 ,mask ,once-value))))
            ,@(when left `((,left (logand ,mask ,once-value))))
            ,@(when right `((,right (logand ,mask (- ,mask -1 ,(if left left `(logand ,mask ,once-value))))))))
       ,@body)))

(defun map-alignment-head (left right fn)
  (let ((granule-start left)
        (inner-part right)
        (inner-start 0))
    (iter (for g from granule-start)
          (for i from inner-start)
          (repeat inner-part)
          (funcall fn g i))))

(defun map-alignment-tail (length left fn)
  (let* ((granule-start 0)
         (inner-part left)
         (inner-start (- length inner-part)))
    (iter (for g from granule-start)
          (for i from inner-start)
          (repeat inner-part)
          (funcall fn g i))))

(defun operate-on-extremity (length rightp left right fn)
  "Given the split of an alignment granule to LEFT and RIGHT and an extent
LENGTH, map FN over the granule-local and absolute sets of offsets within
intersection of that granule and either the leftmost or rightmost 
corresponding part of the extent, depending on whetner RIGHTP is non-NIL.
FN must accept two arguments: the intra-granule offset, and extent offset."
  (let* ((inner-part (if rightp right left))
         (inner-boundary-offset (if rightp (1- inner-part) (- length inner-part)))
         (granule-start (if rightp (+ left right -1) 0))
         (step (if rightp -1 1)))
    (iter (for g from granule-start by step)
          (for i from inner-boundary-offset by step)
          (repeat inner-part)
          (funcall fn g i))))

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
    (+ (align-down value alignment)
       (if (zerop tail) 0 alignment))))

(defun check-size-alignment (req value)
  (unless (aligned-p req value)
    (error 'size-misalignment :alignment-requirement req :size value)))

(defun check-address-alignment (req value)
  (unless (aligned-p req value)
    (error 'address-misalignment :alignment-requirement req :address value)))

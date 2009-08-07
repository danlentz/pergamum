;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PERGAMUM; Base: 10 -*-
;;;

(in-package :pergamum)

(defun u8-seq-word16le (sequence offset)
  (declare (type sequence sequence))
  (logior (ash (elt sequence (+ offset 0)) 0)
	  (ash (elt sequence (+ offset 1)) 8)))

(defun u8-seq-word16be (sequence offset)
  (declare (type sequence sequence))
  (logior (ash (elt sequence (+ offset 1)) 0)
	  (ash (elt sequence (+ offset 0)) 8)))

(defun u8-seq-word32le (sequence offset)
  (declare (type sequence sequence))
  (logior (ash (elt sequence (+ offset 0)) 0)
	  (ash (elt sequence (+ offset 1)) 8)
	  (ash (elt sequence (+ offset 2)) 16)
	  (ash (elt sequence (+ offset 3)) 24)))

(defun u8-seq-word32be (sequence offset)
  (declare (type sequence sequence))
  (logior (ash (elt sequence (+ offset 3)) 0)
	  (ash (elt sequence (+ offset 2)) 8)
	  (ash (elt sequence (+ offset 1)) 16)
	  (ash (elt sequence (+ offset 0)) 24)))

(defun u8-seq-word64le (sequence offset)
  (declare (type sequence sequence))
  (logior (ash (elt sequence (+ offset 0)) 0)
	  (ash (elt sequence (+ offset 1)) 8)
	  (ash (elt sequence (+ offset 2)) 16)
	  (ash (elt sequence (+ offset 3)) 24)
	  (ash (elt sequence (+ offset 4)) 32)
	  (ash (elt sequence (+ offset 5)) 40)
	  (ash (elt sequence (+ offset 6)) 48)
	  (ash (elt sequence (+ offset 7)) 56)))

(defun u8-seq-word64be (sequence offset)
  (declare (type sequence sequence))
  (logior (ash (elt sequence (+ offset 7)) 0)
	  (ash (elt sequence (+ offset 6)) 8)
	  (ash (elt sequence (+ offset 5)) 16)
	  (ash (elt sequence (+ offset 4)) 24)
	  (ash (elt sequence (+ offset 3)) 32)
	  (ash (elt sequence (+ offset 2)) 40)
	  (ash (elt sequence (+ offset 1)) 48)
	  (ash (elt sequence (+ offset 0)) 56)))

(defun u8-seq-wordle (seq offset length)
  (declare (type sequence seq) (type unsigned-byte offset))
  (loop :for i :from 0 :below length
	:for shift :downfrom 0 :by 8 :with result = 0
     :do (setf result (logior result (elt seq (+ offset i))))
     :finally (return result)))

(defun u8-seq-wordbe (seq offset length)
  (declare (type sequence seq) (type unsigned-byte offset))
  (loop :for i :downfrom (1- length) :to 0
	:for shift :downfrom 0 :by 8 :with result = 0
     :do (setf result (logior result (elt seq (+ offset i))))
     :finally (return result)))

(defun (setf u8-seq-wordle) (val seq offset &optional (length (ceiling (integer-length val) 8)))
  (declare (type unsigned-byte val) (type sequence seq) (type unsigned-byte offset))
  (loop :for i :from 0 :below length
	:for shift :downfrom 0 :by 8
     :do (setf (elt seq (+ offset i)) (logand #xff (ash val shift)))))

(defun (setf u8-seq-wordbe) (val seq offset &optional (length (ceiling (integer-length val) 8)))
  (declare (type unsigned-byte val) (type sequence seq) (type unsigned-byte offset))
  (loop :for i :downfrom (1- length) :to 0
	:for shift :downfrom 0 :by 8
     :do (setf (elt seq (+ offset i)) (logand #xff (ash val shift)))))

(defun u8-vector-word16le (array offset)
  (declare (type (vector (unsigned-byte 8)) array))
  (logior (ash (aref array (+ offset 0)) 0)
	  (ash (aref array (+ offset 1)) 8)))

(defun u8-vector-word16be (array offset)
  (declare (type (vector (unsigned-byte 8)) array))
  (logior (ash (aref array (+ offset 1)) 0)
	  (ash (aref array (+ offset 0)) 8)))

(defun u8-vector-word32le (array offset)
  (declare (type (vector (unsigned-byte 8)) array))
  (logior (ash (aref array (+ offset 0)) 0)
	  (ash (aref array (+ offset 1)) 8)
	  (ash (aref array (+ offset 2)) 16)
	  (ash (aref array (+ offset 3)) 24)))

(defun u8-vector-word32be (array offset)
  (declare (type (vector (unsigned-byte 8)) array))
  (logior (ash (aref array (+ offset 3)) 0)
	  (ash (aref array (+ offset 2)) 8)
	  (ash (aref array (+ offset 1)) 16)
	  (ash (aref array (+ offset 0)) 24)))

(defun (setf u8-vector-word32le) (val array offset)
  (declare (type (unsigned-byte 32) val) (type (vector (unsigned-byte 8)) array))
  (setf (aref array (+ offset 0)) (logand #xff (ash val 0))
	(aref array (+ offset 1)) (logand #xff (ash val -8))
	(aref array (+ offset 2)) (logand #xff (ash val -16))
	(aref array (+ offset 3)) (logand #xff (ash val -24)))
  val)

(defun (setf u8-vector-word32be) (val array offset)
  (declare (type (unsigned-byte 32) val) (type (vector (unsigned-byte 8)) array))
  (setf (aref array (+ offset 3)) (logand #xff (ash val 0))
	(aref array (+ offset 2)) (logand #xff (ash val -8))
	(aref array (+ offset 1)) (logand #xff (ash val -16))
	(aref array (+ offset 0)) (logand #xff (ash val -24)))
  val)

(defun u32le-vector-to-u8 (array)
  (lret ((u8vec (make-array (ash (length array) 2) :element-type '(unsigned-byte 8))))
    (iter (for u32 in-vector array)
          (for j from 0 by 4)
          (setf (u8-vector-word32le u8vec j) u32))))

(defun u32be-vector-to-u8 (array)
  (lret ((u8vec (make-array (ash (length array) 2) :element-type '(unsigned-byte 8))))
    (iter (for u32 in-vector array)
          (for j from 0 by 4)
          (setf (u8-vector-word32be u8vec j) u32))))

(defun u8-vector-to-u32le (array)
  (lret ((u32vec (make-array (ash (length array) -2) :element-type '(unsigned-byte 32))))
    (iter (for i from 0 below (length array) by 4)
          (for j from 0)
          (setf (aref u32vec j) (u8-vector-word32le array i)))))

(defun u8-vector-to-u32be (array)
  (lret ((u32vec (make-array (ash (length array) -2) :element-type '(unsigned-byte 32))))
    (iter (for i from 0 below (length array) by 4)
          (for j from 0)
          (setf (aref u32vec j) (u8-vector-word32be array i)))))

(defun u8-vector-word64le (array offset)
  (declare (type (vector (unsigned-byte 8)) array))
  (logior (ash (aref array (+ offset 0)) 0)
	  (ash (aref array (+ offset 1)) 8)
	  (ash (aref array (+ offset 2)) 16)
	  (ash (aref array (+ offset 3)) 24)
	  (ash (aref array (+ offset 4)) 32)
	  (ash (aref array (+ offset 5)) 40)
	  (ash (aref array (+ offset 6)) 48)
	  (ash (aref array (+ offset 7)) 56)))

(defun u8-vector-word64be (array offset)
  (declare (type (vector (unsigned-byte 8)) array))
  (logior (ash (aref array (+ offset 7)) 0)
	  (ash (aref array (+ offset 6)) 8)
	  (ash (aref array (+ offset 5)) 16)
	  (ash (aref array (+ offset 4)) 24)
	  (ash (aref array (+ offset 3)) 32)
	  (ash (aref array (+ offset 2)) 40)
	  (ash (aref array (+ offset 1)) 48)
	  (ash (aref array (+ offset 0)) 56)))

(defun (setf u8-vector-word64le) (val array offset)
  (declare (type (unsigned-byte 64) val) (type (vector (unsigned-byte 8)) array))
  (setf (aref array (+ offset 0)) (logand #xff (ash val 0))
	(aref array (+ offset 1)) (logand #xff (ash val -8))
	(aref array (+ offset 2)) (logand #xff (ash val -16))
	(aref array (+ offset 3)) (logand #xff (ash val -24))
	(aref array (+ offset 4)) (logand #xff (ash val -32))
	(aref array (+ offset 5)) (logand #xff (ash val -40))
	(aref array (+ offset 6)) (logand #xff (ash val -48))
	(aref array (+ offset 7)) (logand #xff (ash val -56)))
  val)

(defun (setf u8-vector-word64be) (val array offset)
  (declare (type (unsigned-byte 64) val) (type (vector (unsigned-byte 8)) array))
  (setf (aref array (+ offset 7)) (logand #xff (ash val 0))
	(aref array (+ offset 6)) (logand #xff (ash val -8))
	(aref array (+ offset 5)) (logand #xff (ash val -16))
	(aref array (+ offset 4)) (logand #xff (ash val -24))
	(aref array (+ offset 3)) (logand #xff (ash val -32))
	(aref array (+ offset 2)) (logand #xff (ash val -40))
	(aref array (+ offset 1)) (logand #xff (ash val -48))
	(aref array (+ offset 0)) (logand #xff (ash val -56)))
  val)

(defun u8-vector-wordle (array offset length)
  (declare (type (vector (unsigned-byte 8)) array) (type unsigned-byte offset))
  (loop :for i :from 0 :below length
	:for shift :downfrom 0 :by 8 :with result = 0
     :do (setf result (logior result (aref array (+ offset i))))
     :finally (return result)))

(defun u8-vector-wordbe (array offset length)
  (declare (type (vector (unsigned-byte 8)) array) (type unsigned-byte offset))
  (loop :for i :downfrom (1- length) :to 0
	:for shift :downfrom 0 :by 8 :with result = 0
     :do (setf result (logior result (aref array (+ offset i))))
     :finally (return result)))

(defun (setf u8-vector-wordle) (val array offset &optional (length (ceiling (integer-length val) 8)))
  (declare (type unsigned-byte val) (type (vector (unsigned-byte 8)) array) (type unsigned-byte offset))
  (loop :for i :from 0 :below length
	:for shift :downfrom 0 :by 8
     :do (setf (aref array (+ offset i)) (logand #xff (ash val shift)))))

(defun (setf u8-vector-wordbe) (val array offset &optional (length (ceiling (integer-length val) 8)))
  (declare (type unsigned-byte val) (type (vector (unsigned-byte 8)) array) (type unsigned-byte offset))
  (loop :for i :downfrom (1- length) :to 0
	:for shift :downfrom 0 :by 8
     :do (setf (aref array (+ offset i)) (logand #xff (ash val shift)))))

(defun array-displacement-equalp (a1 a2 offset count)
  (iter (for i from offset)
        (repeat count)
        (always (= (aref a1 i) (aref a2 i)))))

(defun write-column-value (stream i x) 
  (format stream "~A" x stream)
  (when (= #x3 (logand i #x3))
    (format stream " " stream)))

(defun write-column (stream vector base)
  (dotimes (i 16) (write-column-value stream i (format nil "~2,'0X" (aref vector (+ base i))))))

(defun write-u8-16-segment (stream vector left right length headp)
  (flet ((write-seg-byte (g i) (write-column-value stream g (format nil "~2,'0X" (aref vector i)))))
    (when headp
      (dotimes (g left) (write-column-value stream g "xx")))
    (if headp
        (map-alignment-head left right #'write-seg-byte)
        (map-alignment-tail length left #'write-seg-byte))
    (unless headp
      (dotimes (g right) (write-column-value stream (+ left g) "xx")))))

(defun print-u8-extremity (stream vector base headp &aux (length (length vector)))
  (let ((extremity (if headp base (+ base length))))
    (with-alignment (granule-base left right mask) 16 extremity
      (unless (= granule-base extremity)
        (format stream "~&~8,'0X:  " granule-base)
        (write-u8-16-segment stream vector left right length headp)
        (format stream "~%")))))

(defun print-u8-extremity-diff (stream baseline actual base headp &aux (length (length baseline)))
  (let ((extremity (if headp base (+ base length))))
    (with-alignment (granule-base left right mask) 16 extremity
      (unless (= granule-base extremity)
        (unless (array-displacement-equalp baseline actual (if headp 0 (- length left)) (if headp right left))
          (format stream "~&~8,'0X:  => " granule-base)
          (write-u8-16-segment stream baseline left right length headp)
          (format stream " <= ")
          (write-u8-16-segment stream actual left right length headp)
          (format stream "~%"))))))

(defun print-u8-sequence (stream vector &key (address 0) &aux (base address))
  (print-u8-extremity stream vector base t)
  (with-alignment (nil nil head) 16 base
    (with-alignment (nil tail) 16 (+ base (length vector))
      (let ((core-length (- (length vector) head tail)))
        (iter (for line below (ash core-length -4))
              (for internal from head by 16)
              (format stream "~&~8,'0X:  " (+ base internal))
              (write-column stream vector internal)
              (format stream "~%")))))
  (print-u8-extremity stream vector base nil))

(defun print-u8-sequence-diff (stream baseline actual &key (base 0) (error-report-limit 16))
  (unless (= (length baseline) (length actual))
    (error "~@<Printing difference between arrays of different length doesn't make sense.~:@>"))
  (print-u8-extremity-diff stream baseline actual base t)
  (with-alignment (nil nil head) 16 base
    (with-alignment (nil tail) 16 (+ base (length baseline))
      (let ((errors 0)
            (core-length (- (length baseline) head tail)))
        (iter (for line below (ash core-length -4))
              (for internal from head by 16)
              (unless (array-displacement-equalp baseline actual internal 16)
                (unless (and error-report-limit (>= (incf errors) error-report-limit))
                  (format stream "~&~8,'0X:  => " (+ base internal))
                  (write-column stream baseline internal)
                  (format stream " <= ")
                  (write-column stream actual internal)
                  (format stream " ~2F% errors~%" (/ errors (1+ line) 0.01))))
              (finally (return errors))))))
  (print-u8-extremity-diff stream baseline actual base nil))
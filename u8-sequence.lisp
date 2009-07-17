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

(defun print-u8-sequence (stream seq &key (start 0) (end (length seq)) (address 0) (endianness :little-endian))
  (declare (type sequence seq) (type (integer 0) start end address) (type (member :little-endian :big-endian) endianness))
  (let ((accessor-fn (case endianness (:little-endian #'u8-seq-word32le) (:big-endian #'u8-seq-word32be))))
    (with-aligned-extent-spec-pieces #x10 ((nil prehead) (headbase head) (bodybase body) (tailbase tail))
        (extent start (- end start))
      (unless (zerop head)
        (format stream "~&0x~8,'0X:" (logandc1 #xf (+ address start)))
        (iter (for i below (ash prehead -2))
              (format stream "         "))
        (iter (for i from (logand #x3 headbase) below bodybase by 4)
              (format stream " ~8,'0X" (funcall accessor-fn seq i)))
        (format stream "~%"))
      (iter (for i from bodybase below tailbase by 16)
            (for addr from (+ address start head) by 16)
            (format stream "0x~8,'0X: ~8,'0X ~8,'0X ~8,'0X ~8,'0X~%"
                    addr
                    (funcall accessor-fn seq (+ i 0)) (funcall accessor-fn seq (+ i 4))
                    (funcall accessor-fn seq (+ i 8)) (funcall accessor-fn seq (+ i 12))))
      (unless (zerop tail)
        (format stream "0x~8,'0X: " (+ address start head body))
        (with-aligned-extent-spec-pieces #x4 ((nil nil) (nil nil) (bodybase body) (tailbase tail)) (extent tailbase tail)
          (iter (for i from bodybase below (+ bodybase body) by 4)
                (format stream "~8,'0X " (funcall accessor-fn seq i)))
          (iter (for i from tailbase below (+ tailbase tail))
                (format stream "~2,'0X" (aref seq i))))
        (format stream "~%")))))

(defun print-u8-sequence-diff (stream baseline actual &key (base 0) (format "~&~8,'0X:  -> ~8,'0X  <- ~8,'0X, ~8,'0X, ~8,'0X,  ~2F% errors") (error-report-limit 16))
  (declare (type (array (unsigned-byte 8)) baseline actual))
  (loop :with errors = 0 :with length = (min (length baseline) (length actual)) :with word-length = (ash length -2)
        :for i :below word-length :do
     (unless (= (u8-seq-word32le baseline (ash i 2))
		(u8-seq-word32le actual (ash i 2)))
       (incf errors)
       (unless (and error-report-limit (>= errors error-report-limit))
	 (format stream format
		 (+ base (ash i 2)) (u8-seq-word32le baseline (ash i 2))
		 (when (plusp i)
		   (u8-seq-word32le actual (ash (1- i) 2)))
		 (u8-seq-word32le actual (ash i 2))
		 (when (and (plusp word-length) (< i (1- word-length)))
		   (u8-seq-word32le actual (ash (1+ i) 2)))
		 (/ errors (1+ i) 0.01))))
     :finally (progn (terpri stream) (return errors))))
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

(defun print-u8-sequence (stream seq &key (start 0) (end (length seq)) (base-address 0) (endianness :little-endian))
  (declare (type sequence seq) (type (integer 0) start end base-address) (type (member :little-endian :big-endian) endianness))
  (let* ((total (- end start))
	 (head (logand #xf base-address))
	 (tail (logand #xf (- end head)))
	 (main (- total head tail))
	 (accessor-fn (case endianness
			(:little-endian #'u8-seq-word32le)
			(:big-endian #'u8-seq-word32be))))
    (unless (zerop head)
      (format stream "~&0x~8,'0X:" (logandc1 #xf base-address))
      (iter (for i below (ash (- #x10 head) -2))
	    (format stream "         "))
      (iter (for i below head by 4)
	    (format stream " ~8,'0X" (funcall accessor-fn seq i)))
      (format stream "~%"))
    (iter (for i from head below main by 16)
	  (for address from (+ head base-address) by 16)
	  (format stream "0x~8,'0X: ~8,'0X ~8,'0X ~8,'0X ~8,'0X~%"
		  address
		  (funcall accessor-fn seq (+ i 0)) (funcall accessor-fn seq (+ i 4))
		  (funcall accessor-fn seq (+ i 8)) (funcall accessor-fn seq (+ i 12))))
    (unless (zerop tail)
      (let ((tailstart (+ head main)))
	(format stream "0x~8,'0X:" (+ base-address tailstart))
	(iter (for i from tailstart below total by 4)
	      (format stream " ~8,'0X" (funcall accessor-fn seq i)))
	(format stream "~%")))))

;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PERGAMUM; Base: 10 -*-
;;;

(in-package :pergamum)

(defun %typespec-unsigned-byte-32-p (spec)
  (equal spec '(unsigned-byte 32)))

(defun %typespec-unsigned-byte-8-p (spec)
  (equal spec '(unsigned-byte 8)))

(deftype typespec-unsigned-byte-32 () `(satisfies %typespec-unsigned-byte-32-p))
(deftype typespec-unsigned-byte-8 () `(satisfies %typespec-unsigned-byte-8-p))
(deftype permissible-extent-list-typespec () `(or typespec-unsigned-byte-8 typespec-unsigned-byte-32))

(defclass extent-list ()
  ((element-type :accessor extent-list-element-type :initarg :element-type :type permissible-extent-list-typespec)
   (extents :accessor extent-list-extents :type list :initarg :extents))
  (:default-initargs
   :extents nil))

(defmethod initialize-instance :after ((extent-list extent-list) &key spec &allow-other-keys)
  (when spec
    (setf (extent-list-extents extent-list)
          (iter (for (base . length) in spec)
                (collect (cons base (make-array length :element-type (extent-list-element-type extent-list) :initial-element 0)))))))

(defclass u32-extent-list (extent-list)
  ((element-type :type typespec-unsigned-byte-32))
  (:default-initargs
   :element-type '(unsigned-byte 32)))

(defclass u8-extent-list (extent-list)
  ((element-type :type typespec-unsigned-byte-8))
  (:default-initargs
   :element-type '(unsigned-byte 8)))

(defun extent-list-spec (extent-list)
  (declare (type extent-list extent-list))
  (mapcar #'extent-spec (extent-list-extents extent-list)))

(defun print-extent-list-spec (stream spec colon at-sign)
  (declare (ignore colon at-sign))
  (pprint-logical-block (stream spec)
    (iter (for spec = (pprint-pop))
          (while spec)
          (write-char #\space stream) (print-extent-spec stream spec nil nil))))

(defun extent-list-matches-spec-p (spec extent-list)
  (every #'equalp spec (extent-list-spec extent-list)))

(define-condition extent-list-spec-mismatch (error)
  ((extent-list :accessor condition-extent-list :initarg :extent-list)
   (spec :accessor condition-spec :initarg :spec))
  (:report (lambda (condition stream)
             (format stream "extent list ~S doesn't match the spec ~/pergamum:print-extent-list-spec/"
                     (condition-extent-list condition) (condition-spec condition)))))

(defmethod print-object ((obj extent-list) stream)
  (let ((*print-length* nil))
    (pprint-logical-block (stream (extent-list-extents obj) :prefix "#<EXTENT-LIST" :suffix ">")
      (iter (for extent = (pprint-pop))
            (while extent)
            (format stream " (~X:~X)" (extent-base extent) (+ (extent-base extent) (extent-length extent)))))))

(defun extent-list-push* (extent-list type vector base &key (element-type (array-element-type vector) element-type-p))
  (declare (type extent-list extent-list) (type (unsigned-byte 32) base) (type vector vector))
  (push (apply #'make-extent type base vector (when element-type-p (list :element-type element-type))) (extent-list-extents extent-list)))

(defun extent-list-grow (extent-list type length base)
  (declare (type extent-list extent-list) (type (unsigned-byte 32) length) (type (unsigned-byte 32) base))
  (first (extent-list-push* extent-list type (make-array length :element-type (extent-list-element-type extent-list)) base)))

(defun extent-list-extent-collides-p (extent-list extent)
  (declare (type extent-list extent-list) (type extent extent))
  (find-if (curry #'extents-intersect-p extent) (extent-list-extents extent-list)))

(defun extent-list-adjoin (extent-list extent)
  "Insert the EXTENT into EXTENT-LIST, if only it doesn't intersect any inhabitants to date, in which case an error is raised."
  (declare (type extent-list extent-list) (type extent extent))
  (when-let ((collidee (extent-list-extent-collides-p extent-list extent)))
      (error "extent ~S collides with extent ~S in extent list ~S." extent collidee extent-list))
  (push extent (extent-list-extents extent-list))
  extent-list)

(defun extent-list-adjoin* (extent-list type base vector &key (element-type (array-element-type vector) element-type-p))
  "Like EXTENT-LIST-ADJOIN, but fuse the spread extent constituents."
  (declare (type extent-list extent-list) (type (unsigned-byte 32) base) (type vector vector))
  (let ((extent (apply #'make-extent type base vector (when element-type-p (list :element-type element-type)))))
    (extent-list-adjoin extent-list extent)))

(defun merge-extent-lists (to what)
  "Adjoin all extents from the list WHAT to the list TO. Suboptimal."
  (declare (type extent-list what to))
  (dolist (extent (extent-list-extents what))
    (extent-list-adjoin to extent))
  to)

(defun extent-list-vector-compatible-p (extent-list vector)
  "See if VECTOR is compatible with EXTENT-LIST element type -wise."
  (subtypep (array-element-type vector) (extent-list-element-type extent-list)))

(defmacro do-extent-list ((binding-spec extent-list) &body body)
  "Iterate over EXTENT-LIST's extents, with optional destructuring."
  (with-gensyms (extentvar)
    `(iter (for ,(if (symbolp binding-spec) binding-spec extentvar) in (extent-list-extents ,extent-list))
           ,(if (symbolp binding-spec)
                `(progn ,@body)
                (destructuring-bind (basevar vectorvar &optional desired-extentvar) binding-spec
                  (declare (ignore desired-extentvar))
                  `(let ((,basevar (extent-base ,extentvar))
                         (,vectorvar (extent-data ,extentvar)))
                     ,@body))))))

(defun serialize-extent-list (stream extent-list)
  (let ((*print-base* 16) (*print-array* t) (*print-length* nil))
    (and (print (list :extent-list (extent-list-element-type extent-list)) stream)
	 (mapcar (curry #'serialize-extent stream) (extent-list-extents extent-list))
	 t)))

(defun unserialize-extent-list (stream &optional (type 'extent))
  (let ((*read-base* 10))
    (destructuring-bind (magic element-type) (read stream)
      (unless (eq magic :extent-list)
	(error "Unrecognized extent list serialization format: magic mismatch: ~S instead of ~S." magic :extent-list))
      (unless (typep element-type 'permissible-extent-list-typespec)
	(error "Bad extent list element type: ~S." element-type))
      (let ((*read-base* 16) (*read-eval* nil))
        (make-instance
         (ecase (second element-type)
           (8 'u8-extent-list)
           (32 'u32-extent-list))
         :element-type element-type
         :extents (iter (for (base . data) in (read stream))
                        (collect (make-extent type base data :element-type element-type))))))))

(defun alignment-relaxed-vector-equalp (v1 v2 relax-factor &aux (relax-mask (1- relax-factor)))
  (let ((test-length (logand (length v1) relax-mask)))
    (and (= test-length (logand (length v2) relax-mask))
         (not (mismatch v1 v2 :end1 test-length :end2 test-length)))))

(defun extent-lists-equalp (a b &key (relax-alignment 1) report-stream (error-report-limit 16)
                           (report-format "~8,'0X:  -> ~8,'0X  <- ~8,'0X, ~8,'0X, ~8,'0X,  ~2,'0D% errors~%"))
  (and (= (length (extent-list-extents a)) (length (extent-list-extents b)))
       (loop :for ex-a :in (extent-list-extents a)
	     :for ex-b :in (extent-list-extents b)
             :with status = t
	  :do (with-slots ((a-data data) (a-base base)) ex-a
		(with-slots ((b-data data) (b-base base)) ex-b
		  (unless (and (= a-base b-base)
			       (alignment-relaxed-vector-equalp a-data b-data relax-alignment)
			       (equalp a-data b-data))
		    (when report-stream
		      (format report-stream "Error in ~S:~%" ex-a)
		      (print-u8-sequence-diff report-stream a-data b-data
					      :base a-base 
					      :format report-format
					      :error-report-limit error-report-limit))
		    (setf status nil))))
	  :finally (return status))))

(defun dump-u8-extent-list (stream extent-list &key (endianness :little-endian))
  (dolist (extent (extent-list-extents extent-list))
    (print-extent stream extent endianness)))
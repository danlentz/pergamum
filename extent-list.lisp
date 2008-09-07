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

(defun extent-list-insert (extent-list vector base)
  (declare (type extent-list extent-list) (type (unsigned-byte 32) base) (type vector vector))
  (push (make-extent base vector) (extent-list-extents extent-list)))

(defun extent-list-adjoin (extent-list vector base)
  (declare (type extent-list extent-list) (type (unsigned-byte 32) base) (type vector vector))
  (let ((new-extent (make-extent base vector)))
    (when (find-if (curry #'extents-intersect-p new-extent) (extent-list-extents extent-list))
      (error "vector ~S at base ~S collides with extent list ~S." vector base extent-list))
    (push new-extent (extent-list-extents extent-list))))

(defun merge-extent-lists (to what)
  "Adjoin all extents from the list WHAT to the list TO."
  (declare (type extent-list what to))
  (dolist (extent (extent-list-extents what))
    (extent-list-adjoin to (extent-data extent) (extent-base extent))))

(defun extent-list-compatible-vector-p (extent-list vector)
  (equal (extent-list-element-type extent-list) (array-element-type vector)))

(defun extent-list-grow (extent-list length base &key check-p)
  (let ((vector (make-array length :element-type (extent-list-element-type extent-list) :initial-element 0)))
    (first
     (if check-p
	 (extent-list-adjoin extent-list vector base)
	 (extent-list-insert extent-list vector base)))))

(defmacro do-extent-list ((basevar vectorvar) extent-list &body body)
  `(iter (for (,basevar . ,vectorvar) in (extent-list-extents ,extent-list))
	 ,@body))

(defun extent-list-vector-by-base (extent-list base)
  (extent-data (find base (extent-list-extents extent-list) :key #'extent-base)))

(defun serialize-extent-list (stream extent-list)
  (let ((*print-base* 16) (*print-array* t) (*print-length* nil))
    (and (print (list :extent-list (extent-list-element-type extent-list)) stream)
	 (mapcar (curry #'serialize-extent stream) (extent-list-extents extent-list))
	 t)))

(defun unserialize-extent-list (stream)
  (let ((*read-base* 10))
    (destructuring-bind (magic element-type) (read stream)
      (unless (eq magic :extent-list)
	(error "Unrecognized extent list serialization format: magic mismatch: ~S instead of ~S." magic :extent-list))
      (unless (typep element-type 'permissible-extent-list-typespec)
	(error "Bad extent list element type: ~S." element-type))
      (let ((*read-base* 16) (*read-eval* nil)
	    (extent-list (make-instance (ecase (second element-type)
					  (8 'u8-extent-list)
					  (32 'u32-extent-list))
					:element-type element-type)))
	(loop :for (base . data) :in (read stream) :do
	  (map-into (extent-data (extent-list-grow extent-list (length data) base)) #'identity data))
	(setf (extent-list-extents extent-list) (nreverse (extent-list-extents extent-list)))
	extent-list))))

(defun alignment-relaxed-vector-equalp (v1 v2 relax-factor &aux (relax-mask (1- relax-factor)))
  (let ((test-length (logand (length v1) relax-mask)))
    (and (= test-length (logand (length v2) relax-mask))
         (not (mismatch v1 v2 :end1 test-length :end2 test-length)))))

(defun extent-lists-equal (a b &key (relax-alignment 1) report-stream (error-report-limit 16)
                           (report-format "~8,'0X:  -> ~8,'0X  <- ~8,'0X, ~8,'0X, ~8,'0X,  ~2,'0D% errors~%"))
  (and (= (length (extent-list-extents a)) (length (extent-list-extents b)))
       (loop :for (a-base . a-data) :in (extent-list-extents a)
	     :for (b-base . b-data) :in (extent-list-extents b)
	  :do (unless (and (= a-base b-base)
                           (alignment-relaxed-vector-equalp a-data b-data relax-alignment))
                (when report-stream
                  (print-u8-sequence-diff report-stream a-data b-data report-format
                   :error-report-limit error-report-limit))
		(return nil))
	  :finally (return t))))

(defun dump-u8-extent-list (stream extent-list &key (endianness :little-endian))
  (dolist (extent (extent-list-extents extent-list))
    (print-extent stream extent endianness)))
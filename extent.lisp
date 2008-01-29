(in-package :pergamum)

(deftype extent () `(cons (integer 0) vector))

(defun make-extent (base vector)
  (declare ((integer 0) base) (vector vector))
  (cons base vector))

(defun extent-base (extent)
  (declare (extent extent))
  (car extent))

(defun extent-data (extent)
  (declare (extent extent))
  (cdr extent))

(defun extent-length (extent)
  (declare (extent extent))
  (array-dimension (extent-data extent) 0))

(defun point-in-extent-p (extent p)
  (declare ((integer 0) p) (extent extent))
  (and (>= p (extent-base extent)) (< p (+ (extent-base extent) (extent-length extent)))))

(defun extents-intersect-p (x y)
  (declare (extent x y))
  (and (plusp (extent-length x)) (plusp (extent-length y))
       (or (point-in-extent-p x (extent-base y)) (point-in-extent-p x (+ (extent-base y) (extent-length y) -1)))))

(defun extent-spec (extent)
  (declare (extent extent))
  (cons (car extent) (length (cdr extent))))

(deftype extent-spec () `(cons (integer 0) (integer 0)))

(defun make-extent-spec (base length)
  (cons base length))

(defun print-extent-spec (stream spec colon at-sign)
  (declare (ignore colon at-sign))
  (pprint-logical-block (stream spec)
    (format stream "(~X:~X)" (car spec) (+ (car spec) (cdr spec)))))

(defmacro with-aligned-extent-spec-pieces (alignment (head body tail) extent &body innards)
  "Bind the HEAD, BODY and TAIL pieces of EXTENT's length, as mandated by aligning it by ALIGNMENT (evaluated)."
  (with-gensyms (base length alignment-mask)
    (once-only (alignment extent)
      `(let* ((,alignment-mask (1- ,alignment))
              (,length (extent-length ,extent))
              (,base (extent-base ,extent))
              (,head (logand ,alignment-mask (- ,alignment (logand ,alignment-mask ,base))))
              (,body (logandc1 ,alignment-mask (- ,length ,head)))
              (,tail (- ,length ,head ,body)))
         ,@innards))))

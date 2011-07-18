;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PERGAMUM; Base: 10 -*-
;;;

(in-package :pergamum)


(defun nthref (tree &rest indices)
  "Use INDICES to index into TREE.  Think AREF for lists.
An error is raised when a non-list appears at any point (except the
leaf) during tree descent."
  (reduce #'nth (append (nreverse indices) (list tree)) :from-end t))

(defun try-nthref (tree &rest indices)
  "Like NTHREF, but return NIL when a non-list appears at any point
during tree descent."
  (reduce (named-lambda rec (i tree)
            (cond ((not (consp tree)) nil)
                  ((not (plusp i))    (car tree))
                  ((consp tree)       (rec (1- i) (cdr tree)))))
          (append (nreverse indices) (list tree)) :from-end t))

(defun uncirculate-list (xs)
  "Return a non-circular list of all elements of a potentially
circular list XS."
  (loop :for (x . rest) :on xs
     :collect x
     :until (eq rest xs)))

(defun intersperse (x xs)
  "Return a sequence of elements, with X inserted between every two
adjacent elements of XS."
  (loop :for (e . rest) :on xs
     :collect e
     :when rest :collect x))

(defun maybe-prop (pred key value)
  "Return a list (KEY VALUE), whenever PRED is non-NIL."
  (when pred
    (list key value)))

(defun maybe-prop* (key value)
  "Return a list (KEY VALUE), whenever VALUE is non-NIL."
  (when value
    (list key value)))

(defun nfsubst (fn tree)
  "Pipe non-CONSes in TREE through FN, producing a new one."
  (cond ((null tree) nil)
        ((consp tree)
         (cons (nfsubst fn (car tree)) (nfsubst fn (cdr tree))))
        (t (funcall fn tree))))

(defun make-queue ()
  "Make a PAIP queue."
  (lret ((q (cons nil nil)))
    (setf (car q) q)))

(defun enqueue (elt q)
  "En-Norvig-queue."
  (setf (car q)
        (setf (cdar q) (cons elt nil))))

(defun dequeue (q)
  "De-Norvig-queue."
  (prog1 (pop (cdr q))
    (if (null (cdr q)) (setf (car q) q))))

(defun queue-contents (q)
  "The contents of a PAIP-queue."
  (cdr q))

(defun queue-empty-p (q)
  "Test the PAIP-queue for emptiness."
  (null (cdr q)))

(defun mapqueue (fn &rest lists)
  "Like MAPCAR (modulo multiple parameter lists), but returns a Norvig-queue."
  (lret ((q (make-queue)))
    (iter (for tails on lists)
          (while (every #'consp tails))
          (enqueue (apply fn (mapcar #'car tails)) q))))

(defun mapcons (fn initial &rest lists)
  "Like MAPCAR, but specifies the cdr of the last cons cell."
  (let ((q (make-queue)))
    (apply #'mapqueue fn lists)
    (setf (cdar q) initial)
    (queue-contents q)))

(defun unzip (fn sequence &key (key #'identity))
  (iter (for elt in sequence)
        (if (funcall fn (funcall key elt))
            (collect elt into yes)
            (collect elt into no))
        (finally (return (values yes no)))))

(defun plist-difference (plist keys)
  "Return a copy of PLIST with all key-value pairs present in KEYS
   removed."
  (iter (for (key value . nil) on plist by #'cddr)
        (unless (find key keys :test #'eq)
          (collect key)
          (collect value))))

(defun plist-intersection (plist keys)
  "Return a copy of PLIST with all key-value pairs not present in KEYS
   removed."
  (iter (for (key value . nil) on plist by #'cddr)
        (when (find key keys :test #'eq)
          (collect key)
          (collect value))))

(defun plist-merge (overridee overrider)
  "Return a combination of OVERRIDEE and OVERRIDER plists, such that
the set of keys present is their union, whereas the keys of their intersection
have associated values from OVERRIDER."
  (lret ((result (copy-list overridee)))
    (iter (for (k v) on overrider by #'cddr)
          (setf (getf result k) v))))

(defun diff-lists (list1 list2 &key (skip-mismatches 0))
  "Recursively look for differences between LIST1 and LIST2.
   When such a difference is found, its formpath is returned as first value,
   while the differing leaves are returned as second and third values."
  (declare (special skip-mismatches))
  (iter (for f1 in list1)
        (for f2 in list2)
        (collect (car (ensure-cons f1)) into path)
        (unless (or (equal f1 f2) (< 0 (decf skip-mismatches)))
          (multiple-value-bind (ret diff1 diff2) (when (eq (car (ensure-cons f1)) (car (ensure-cons f2)))
                                                   (diff-lists f1 f2))
            (return (values (list (butlast path) ret)
                            (if ret diff1 f1)
                            (if ret diff2 f2)))))))

(define-modify-macro set-differencef (&rest sets) set-difference
  "Modify-macro for SET-DIFFERENCE. Substracts SETS from the place designated
   by the first argument.")

(define-modify-macro nset-differencef (&rest sets) nset-difference
  "Modify-macro for NSET-DIFFERENCE. Substracts SETS from the place designated
   by the first argument. All arguments are subject to potential mutation.")

(define-modify-macro set-intersectionf (&rest sets) set-intersection
  "Modify-macro for SET-INTERSECTION. Updates the place designated by the
   first argument to its union with SETS.")

(define-modify-macro nset-intersectionf (&rest sets) nset-intersection
  "Modify-macro for NSET-INTERSECTION. Updates the place designated by the
   first argument to its union with SETS. All arguments are subject to
   potential mutation.")

(defun maptree (fn atomp child-list-fn root-node &key collect-non-atoms)
  "Return a listified copy of a generic tree, defined by its ROOT-NODE,
the ATOMP predicate and the CHILD-LIST-FN node accessor.
Atoms of the tree are substituted with the result of applying FN to them.
When COLLECT-NON-ATOMS is non-NIL, "
  (labels ((trec (node)
             (if (funcall atomp node)
                 (funcall fn node)
                 (let ((processed-childs (mapcar #'trec (funcall child-list-fn node))))
                   (if collect-non-atoms
                       (cons (funcall fn node) processed-childs)
                       processed-childs)))))
    (trec root-node)))

(defun map-list-tree (fn tree)
  "Like MAPTREE with ATOM being the atomicity predicate, IDENTITY being the
child list accessor and without non-atom collection."
  (labels ((trec (tree)
             (when tree
               (cons (if (atom (car tree))
                         (funcall fn (car tree))
                         (trec (car tree)))
                     (trec (cdr tree))))))
    (trec tree)))
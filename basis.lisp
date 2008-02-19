(in-package :pergamum)

(defmacro progn-1 (&body body)
  `(prog1
       (progn
         ,@(butlast body))
     ,(lastcar body)))

(defmacro lret (bindings &body body) 
  "A @macro{let}-construct which returns its last binding." 
  `(let ,bindings ,@body 
        ,(let ((x (car (last bindings)))) 
              (if (atom x) 
                  x 
                  (car x))))) 

(defmacro lret* (bindings &body body) 
  "A @macro{let*}-construct which returns its last binding." 
  `(let* ,bindings ,@body 
         ,(let ((x (car (last bindings)))) 
               (if (atom x) 
                   x 
                   (car x)))))

(defun xform/filter-if (xform test &rest sequences)
  (labels ((iterate (acc sequences)
	     (if (notany #'null sequences)
		 (let ((crop (mapcar #'car sequences))
		       (rest (mapcar #'cdr sequences)))
		   (iterate (if (apply test crop)
				(cons (apply xform crop) acc)
				acc)
			    rest))
		 acc)))
    (nreverse (iterate nil sequences))))

(defun maybe-capture-subform (condition form accessor)
  `(or (and ,condition (,accessor ,form)) (gensym)))

(defmacro with-optional-subform-captures ((&rest specs) &body body)
  `(let ,(iter (for (vars accessors condition form) in specs)
               (appending (mapcar (lambda (var accessor)
                                    (list var (maybe-capture-subform condition form accessor)))
                                  vars accessors)))
     ,@body))

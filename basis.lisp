;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PERGAMUM; Base: 10 -*-
;;;

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

(defun fif (if-fn then-fn else-fn)
  "Return a function APPLYing either THEN-FN or ELSE-FN to its arguments, 
   depending on the return value of IF-FN, applied to the same arguments."
  (lambda (&rest x)
    (if (apply if-fn x)
        (apply then-fn x)
        (apply else-fn x))))

(defun fif-1 (if-fn then-fn else-fn)
  "Return a function FUNCALLing either THEN-FN or ELSE-FN with its argument, 
   depending on the return value of IF-FN, passed the same argument."
  (lambda (x)
    (if (funcall if-fn x)
        (funcall then-fn x)
        (funcall else-fn x))))

(defun map-remove-if-not (xform test &rest sequences)
  "Essentially (mapcar XFORM (remove nil (apply #'mapcar TEST SEQUENCES))), 
   but works when NIL is present in SEQUENCES."
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

;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PERGAMUM; Base: 10 -*-
;;;

(in-package :pergamum)

(defun strconcat (strings)
  "Concatenate STRINGS into a single one."
  (declare (optimize speed))
  (apply #'concatenate 'string strings))

(defun strconcat* (&rest strings)
  "Concatenate STRINGS into a single one."
  (declare (optimize speed))
  (apply #'concatenate 'string strings))

(defun escape (x escape-char chars-to-escape)
  "Given a string X, produce another with characters within the CHARS-TO-ESCAPE
list prepended with ESCAPE-CHAR."
  (declare (optimize speed) (string x) (character escape-char) (list chars-to-escape))
  (if (some (lambda (c)
              (declare (type character c))
              (find c x))
            chars-to-escape)
      (coerce (iter (declare (iterate:declare-variables))
                    (for (the character c) in-string x)
                    (when (some (lambda (x)
                                  (declare (type character x))
                                  (char= c x))
                                chars-to-escape)
                      (collect escape-char))
                    (collect c))
              'string)
      x))

(defun escape* (x escape-char &rest chars-to-escape)
  "Given a string X, produce another with characters within the CHARS-TO-ESCAPE
list prepended with ESCAPE-CHAR."
  (escape x escape-char chars-to-escape))

(defun down-case-string (x)
  "Given a string designator X, return the down-cased version of the
associated string."
  (string-downcase (string x)))

(defun up-case-string (x)
  "Given a string designator X, return the up-cased version of the
associated string."
  (string-upcase (string x)))

(defun decoded-time-string (second minute hour date month year dow daylight-p zone)
  (declare (ignorable second daylight-p zone))
  (format nil "~[Mon~;Tue~;Wed~;Thu~;Fri~;Sat~;Sun~] ~[Jan~;Feb~;Mar~;Apr~;May~;Jun~;Jul~;Aug~;Sep~;Oct~;Nov~;Dec~] ~2,'0D ~4,'0D, ~2,'0D:~2,'0D"
          dow (1- month) date year hour minute))

(defun prefixp (prefix sequence)
  "A predicate, determining whether PREFIX is a prefix of SEQUENCE."
  (nth-value 1 (starts-with-subseq prefix sequence :return-suffix t)))

(defun duplicates (xs &key key test &aux
                   (test (or test 'eql))
                   (key (or key #'identity)))
  "Return those elements of XS, which occur more than once, according
to KEY and TEST."
  (let ((hash (make-hash-table :test test)))
    (iter (for x in-sequence xs)
          (push x (gethash (funcall key x) hash)))
    (coerce (iter (for (nil v) in-hashtable hash)
                  (when (> (length v) 1)
                    (appending v)))
            (typecase xs
              (list          'list)
              (simple-vector 'simple-vector)
              (vector        'vector)
              (t             (type-of xs))))))

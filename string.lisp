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

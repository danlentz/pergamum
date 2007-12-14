(in-package :pergamum)

(defun array-reader (stream &optional sharp char)
  (declare (ignore sharp char))
  (let ((list (read stream)))
    `(make-array ,(length list)
                 :initial-contents (list ,@list))))

(defun enable-array-reader ()
  "Enable #A(a b c) syntax for element-evaluated constant arrays."
  (set-dispatch-macro-character #\# #\A 'array-reader))

(defun curry-reader (stream char)
  (declare (ignorable char))
  (let ((contents (read-delimited-list #\] stream t)))
    `(curry #',(car contents) ,@(cdr contents))))

(defun enable-curry-reader ()
  "Enable [fn a b c] syntax for currying."
  (set-macro-character  #\[ #'curry-reader t)
  (set-syntax-from-char #\] #\)))

(defun compose-reader (stream char)
  (declare (ignorable char))
  `(compose ,@(read-delimited-list #\} stream t)))

(defun enable-compose-reader ()
  "Enable {#'a #'b #'c} syntax for function composition."
  (set-macro-character  #\{ #'compose-reader t)
  (set-syntax-from-char #\} #\)))

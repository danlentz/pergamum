;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PERGAMUM; Base: 10 -*-
;;;

(in-package :pergamum)

(defparameter *marred* nil)

(defun mar (&rest syms)
  (setf *marred* (remove-duplicates (nunion *marred* syms))))

(defun export-unmarred (pred &optional (package *package*) (marred *marred*))
  (let (syms)
    (do-symbols (sym package)
      (when (and (funcall pred sym) (eq (symbol-package sym) package))
        (push sym syms)))
    (export (set-difference syms marred) package)))

(defun tunnel-package (source &optional (package *package*))
  (let (syms)
    (do-external-symbols (sym source)
      (push sym syms))
    (export (set-difference syms (package-shadowing-symbols package) :key #'symbol-name :test #'string=))))

(defun remove-nickname (package nickname)
  "Remove the nickname from the list of nicknames of PACKAGE.
Return the package designated by PACKAGE.

Stolen from Pascal Bourgignon's ORG.INFORMATIMAGO.COMMON-LISP.CESARUM library."
  (let ((package (find-package package)))
    (rename-package package
                    (package-name package)
                    (remove nickname (package-nicknames package)
                            :test (function string=)))))

(defun add-nickname (package nickname)
  "Add the nickname to the package.  Return the package.

A reduced version of the original, from Pascal Bourgignon's
ORG.INFORMATIMAGO.COMMON-LISP.CESARUM library."
  ;; !!! The consequences are undefined if new-name or any new-nickname conflicts with any existing package names.
  (let* ((pack     (find-package package))
         (nickpack (find-package nickname)))
    (cond
      ((null pack)
       (error "~@<~S does not name an existing package.~:@>" package))
      ((eq nickpack pack))
      ((null nickpack)
       (rename-package pack (package-name pack)
                       (cons nickname (copy-seq (package-nicknames pack)))))
      (t
       (error "~@<~S names an existing package.~:@>" nickname)))
    pack))

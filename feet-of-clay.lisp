;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FEET-OF-CLAY; Base: 10; indent-tabs-mode: nil; show-trailing-whitespace: t -*-
;;;

(cl:defpackage :feet-of-clay
  (:use :common-lisp)
  (:export
   #:posix-working-directory
   #:set-posix-working-directory
   #:make-directory
   #:remove-file
   #:make-symlink
   #:backtrace
   #:backtrace-as-list
   #:print-backtrace-frame
   #:function-insn-vector
   #:disassemble-insn-vector
   ))

(cl:in-package :feet-of-clay)

(defun posix-working-directory ()
  "Return the POSIX idea of the current working directory."
   #-(or sbcl ecl ccl) (not-implemented 'posix-working-directory)
  #+sbcl (sb-posix:getcwd)
  #+ecl (si:getcwd)
  #+ccl (ccl::current-directory-name))

(defun set-posix-working-directory (pathname)
  "Change the POSIX view of the current directory."
  (zerop 
   #-(or sbcl ecl ccl) (not-implemented 'set-posix-working-directory)
   #+sbcl (sb-posix:chdir pathname)
   #+ecl (si:chdir pathname)
   #+ccl (ccl::%chdir (namestring pathname))))

(defsetf posix-working-directory set-posix-working-directory)

(defun make-directory (pathname &optional (mode #o755))
  #-(or sbcl ccl ecl) (not-implemented 'make-directory)
  #+sbcl (sb-posix:mkdir pathname mode)
  #+ccl (ccl::%mkdir pathname mode)
  #+ecl (si:mkdir pathname))

(defun remove-file (p)
  "We need this because DELETE-FILE deletes the symlink target,
instead of the symlink itself."
  #-(or sbcl ccl) (not-implemented 'remove-file)
  #+sbcl (sb-posix:unlink p)
  #+ccl (ccl::%delete-file p))

(defun make-symlink (symlink target)
  #-(or (and sbcl (not win32)) ccl) (not-implemented 'make-symlink)
  #+(and sbcl (not win32)) (sb-posix:symlink target symlink))

(defun backtrace (&optional (count most-positive-fixnum) (stream *debug-io*))
  #-(or sbcl ecl clisp) (not-implemented 'backtrace)
  #+sbcl (sb-debug:backtrace count stream)
  #+ecl (si::tpl-backtrace)
  #+clisp (system::debug-backtrace-1)
  #+ccl "")

(defun backtrace-as-list (&optional (count most-positive-fixnum))
  #-(or sbcl ccl) (not-implemented 'backtrace-as-list)
  #+sbcl (sb-debug:backtrace-as-list count)
  #+ccl (ccl::backtrace-as-list count))

(defun print-backtrace-frame (frame &optional (stream *debug-io*))
  #-(or sbcl) (not-implemented 'print-frame-call)
  #+sbcl (sb-debug::print-frame-call frame stream))

(defun function-insn-vector (name)
  #-(or sbcl) (not-implemented 'function-insn-vector)
  #+sbcl
  (sb-sys:without-gcing
    (let* ((function (fdefinition name))
           (object (sb-disassem::fun-code function))
           (vector-sap (sb-kernel:code-instructions object))
           (vector-length (sb-disassem::code-inst-area-length object)))
      (let ((final-vec (make-array vector-length :element-type '(unsigned-byte 8))))
        (loop :for i :below vector-length
           :do (setf (aref final-vec i) (sb-vm::sap-ref-8 vector-sap i)))
        final-vec))))

(defun disassemble-insn-vector (vector &optional (start 0) end)
  #-(or sbcl) (not-implemented 'disassemble-insn-vector)
  #+sbcl
  (sb-sys:without-gcing
    (sb-disassem:disassemble-memory (sb-vm::sap+ (sb-sys:vector-sap vector) start) (- (or end (length vector)) start))))
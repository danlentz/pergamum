;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PERGAMUM; Base: 10 -*-
;;;

(defpackage pergamum
  (:use :common-lisp :alexandria :iterate #+sbcl :sb-mop)
  (:import-from :cl-fad #:directory-exists-p #:file-exists-p)
  (:export
   ;; basis.lisp
   #:symbol-macro-p #:progn-1 #:lret #:lret* #:if-let* #:case-let #:fcase #:fcase-1 #:fif #:fif-1 #:map-remove-if-not #:syncformat
   ;; forms.lisp
   #:quoted-p #:quoted-form
   ;; binary.lisp
   #:logandf #:logiorf #:logandcf
   ;; classes.lisp
   #:define-protocol-class
   ;; objects.lisp
   #:copy-slots #:slot-value* #:define-print-object-method #:primary-method-not-required #:most-specific-last
   ;; conditions.lisp
   #:*debug-condition* #:dsignal #:derror
   #:with-condition-recourses #:call-next-recourse-and-retry
   #:make-condition-raiser #:make-error-raiser 
   #:with-retry-restarts #:with-ignore-restart
   #:make-fixed-restarter
   #:with-condition-restart-binding #:with-condition-printing #:with-error-resignaling #:with-condition-collection #:returning-conditions
   #:condition-bind-default
   #:define-reported-condition #:report-simple-condition #:define-simple-error
   #:simple-condition-reporter #:redefinition #:simple-redefinition #:bad-redefinition #:warn-redefinition
   ;; functions.lisp
   #:feq #:feql #:fequal #:fequalp #:f=
   #:latch #:bukkake-combine #:maybe #:maybecall #:xform #:xform-if #:xform-if-not #:apply/find-if
   #:iterate-until #:collect-until #:or-p #:and-p
   #:define-execute-with-special
   ;; lists.lisp
   #:nfsubst #:make-queue #:enqueue #:dequeue #:queue-contents #:queue-empty-p #:mapqueue #:mapcons #:unzip #:diff-lists
   #:plist-difference #:plist-intersection #:set-differencef #:nset-differencef #:set-intersectionf #:nset-intersectionf
   ;; mop.lisp
   #+sbcl #:rename-class-slot #:slot-definition-documentation
   ;; numbers.lisp
   #:power-of-2-p #:ilog2-ceiling #:ilog2-cover #:bcd-integer-length #:decode-bcd #:bisect #:split-number
   #:with-alignment #:map-alignment-left #:map-alignment-right #:operate-on-extremity
   ;; pergamum.lisp
   #:with-condition-printing
   #:nand #:nor #:andf #:orf #:notf #:xorf
   #:quote-when #:quote-if-non-self-evaluating
   #:op-parameter-destructurer
   #:order #:order-funcalling
   #:ensure-destructurisation #:destructure-binding-form-body #:destructure-def-body
   #:prepend
   #:emit-let #:emit-lambda #:emit-named-lambda #:with-named-lambda-emission #:emit-defun #:with-defun-emission
   #:emit-declarations
   #:measuring-time-lapse #:measuring-time-lapse-1 #:measuring-performance
   ;; lambda-lists.lisp
   #:&mandatory
   #:map-lambda-list #:map-lambda-list-bindings-actuals #:map-lambda-list-defaults-actuals
   #:lambda-list-binds #:lambda-list-1
   ;; streams.lisp
   #:stream-as-vector #:all-stream-forms
   ;; pathnames.lisp
   #:flatten-path-list #:fuse-downcased-string-path-list
   #:subwild #:subfile #:subfile* #:subdirectory #:subdirectory* 
   ;; files.lisp
   #:pathname-busy #:pathname-not-present
   #:posix-working-directory #:set-posix-working-directory
   #:within-directory
   #:file-as-vector #:file-as-string #:with-output-to-file
   #:symlink-to-p #:symlink-target-file-present-p #:ensure-symlink
   ;; hash-table.lisp
   #:hash-table-next #:hash-table-pop #:hash-table-itearate
   #:xform-hash-table #:make-hash-table-injector #:hash-table-key-present-p #:maphash*
   #:define-container-hash-accessor #:container-condition #:container-missing-cell-error #:copy-hash-table-empty #:with-empty-hash-containers
   ;; alignment.lisp
   #:alignment-condition #:address-misalignment #:size-misalignment
   #:aligned-p #:align-down #:align-up
   #:aligned-4 #:aligned-16
   #:check-size-alignment #:check-address-alignment
   ;; extent.lisp
   #:baseless-extent #:extent #:make-extent #:extent-base #:extent-data #:extent-length #:extent-end #:point-extent-base-p #:point-in-extent-p #:extents-intersect-p
   #:split-extent #:with-split-extent
   #:rebase-extent #:rebase-extent-inplace #:coerce-extent #:coerce-extent-inplace
   #:extent-data-equalp #:extent-equalp
   #:extent-spec #:extent #:print-extent-spec #:print-extent #:serialize-extent
   #:extent-spec-base #:extent-spec-length #:extent-spec-length
   #:rebase-extent-spec
   #:subextent #:subextent-extent #:subextent-parent #:subextent-absolutize #:subextent-relativize
   #:do-extent-spec-aligned-blocks #:with-aligned-extent-spec-pieces
   ;; u8-sequence.lisp
   #:u8-vector-wordle #:u8-vector-wordbe
   #:u8-vector-word16le #:u8-vector-word16be
   #:u8-vector-word32le #:u8-vector-word32be
   #:u8-vector-word64le #:u8-vector-word64be
   #:u32le-vector-to-u8 #:u32be-vector-to-u8 #:u8-vector-to-u32le #:u8-vector-to-u32be
   #:u8-seq-word16le #:u8-seq-word16be
   #:u8-seq-word32le #:u8-seq-word32be
   #:u8-seq-word64le #:u8-seq-word64be
   #:u8-seq-wordle #:u8-seq-wordbe
   #:align-extend-u8-extent #:align-extend-u8-extent-inplace
   #:print-u8-sequence #:print-u8-sequence-diff
   ;; bioable.lisp
   #:bioable #:bioable-range
   #:read-block #:write-block #:write-blocks
   #:u8-extent #:write-u8-extent
   #:u8-extents #:write-u8-extents
   ;; read.lisp
   #:make-unreadable-object #:enable-array-reader #:enable-compose-reader #:enable-curry-reader
   ;; packages.lisp
   #:mar #:export-unmarred
   #:tunnel-package
   ;; types.lisp
   #:coerce-to-sequence
   ;; to-expunge.lisp
   ))

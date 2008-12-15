;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PERGAMUM; Base: 10 -*-
;;;

(defpackage pergamum
  (:use :common-lisp :alexandria :iterate #+sbcl :sb-mop)
  (:import-from :cl-fad #:directory-exists-p #:file-exists-p)
  (:export
   ;; basis.lisp
   #:progn-1 #:lret #:lret* #:if-let* #:case-let #:fif #:fif-1 #:map-remove-if-not
   ;; forms.lisp
   #:quoted-p #:quoted-form
   ;; binary.lisp
   #:logandf #:logiorf #:logandcf
   ;; objects.lisp
   #:copy-slots #:slot-value* #:define-print-object-method #:primary-method-not-required #:most-specific-last
   ;; conditions.lisp
   #:make-condition-raiser #:make-error-raiser 
   #:with-retry-restart #:with-ignore-restart
   #:make-fixed-restarter
   #:with-condition-restart-binding #:with-condition-printing #:with-error-resignaling #:with-condition-collection #:returning-conditions
   #:condition-bind-default
   #:define-reported-condition
   #:simple-condition-reporter #:redefinition #:simple-redefinition #:bad-redefinition #:warn-redefinition
   ;; functions.lisp
   #:latch-fn #:latch-args #:bukkake-combine #:bukkake-combine-1 #:maybe #:maybecall #:xform #:xform-if #:xform-if-not #:iterate-until #:collect-until #:or-p #:and-p
   ;; lists.lisp
   #:make-queue #:enqueue #:dequeue #:queue-contents #:queue-empty-p #:mapqueue #:mapcons #:unzip #:diff-lists
   ;; mop.lisp
   #+sbcl #:rename-class-slot #:slot-definition-documentation
   ;; numbers.lisp
   #:power-of-2-p #:ilog2-ceiling #:ilog2-cover #:bisect #:split-number
   ;; pergamum.lisp
   #:with-condition-printing
   #:nand #:nor #:andf #:orf #:notf #:xorf
   #:quote-when
   #:op-parameter-destructurer
   #:order #:order-funcalling
   #:ensure-destructurisation #:destructure-binding-form-body #:destructure-def-body
   #:prepend
   #:emit-let #:emit-lambda #:emit-named-lambda #:with-named-lambda-emission #:emit-defun #:with-defun-emission
   #:emit-declarations
   #:measuring-time-lapse
   ;; lambda-lists.lisp
   #:&mandatory
   #:map-lambda-list #:map-lambda-list-bindings-actuals #:map-lambda-list-defaults-actuals
   #:lambda-list-binds #:lambda-list-1
   ;; streams.lisp
   #:stream-as-vector
   ;; pathnames.lisp
   #:flatten-path-list
   #:subfile #:subfile* #:subdirectory #:subdirectory* 
   ;; files.lisp
   #:pathname-busy #:pathname-not-present
   #:current-working-directory #:change-working-directory
   #:within-directory
   #:file-as-vector #:file-as-string #:with-output-to-file
   ;; hash-table.lisp
   #:hash-table-next #:hash-table-pop #:hash-table-itearate
   #:xform-hash-table #:make-hash-table-injector #:hash-table-key-present-p #:maphash*
   #:define-container-hash-accessor #:copy-hash-table-empty #:with-empty-hash-containers
   ;; alignment.lisp
   #:alignment-condition #:address-misalignment #:size-misalignment
   #:aligned-p #:align-down #:align-up
   #:aligned-4 #:aligned-16
   #:check-size-alignment #:check-address-alignment
   ;; extent.lisp
   #:baseless-extent #:extent #:make-extent #:extent-base #:extent-data #:extent-length #:extent-end #:point-extent-base-p #:point-in-extent-p #:extents-intersect-p
   #:extent-spec #:make-extent-spec #:print-extent-spec #:print-extent #:serialize-extent
   #:extent-spec-base #:extent-spec-length #:extent-spec-length
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
   #:print-u8-sequence #:print-u8-sequence-diff
   ;; extent-list.lisp
   #:extent-list #:u32-extent-list #:u8-extent-list
   #:extent-list-extents #:extent-list-element-type
   #:extent-list-spec #:print-extent-list-spec #:extent-list-matches-spec-p #:extent-list-spec-mismatch
   #:extent-list-push* #:extent-list-grow #:extent-list-adjoin #:extent-list-adjoin*
   #:merge-extent-lists
   #:extent-list-vector-compatible-p
   #:do-extent-list
   #:serialize-extent-list #:unserialize-extent-list
   #:extent-lists-equalp
   #:dump-u8-extent-list
   ;; extentable.lisp
   #:extentable #:u8-extent #:extentable-u8-vector #:set-u8-extent #:set-u8-extent-list #| #:u8-extent-list -- generic function |#
   #:subextent #:subextent-extent #:subextent-parent #:subextent-absolutize #:subextent-relativize
   ;; read.lisp
   #:make-unreadable-object #:enable-array-reader #:enable-compose-reader #:enable-curry-reader
   ;; packages.lisp
   #:mar #:export-unmarred
   #:tunnel-package
   ;; types.lisp
   #:coerce-to-sequence
   ;; to-expunge.lisp
   ))

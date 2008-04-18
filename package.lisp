(defpackage pergamum
  (:use :common-lisp :alexandria :iterate)
  (:export
   ;; basis.lisp
   #:xform/filter-if
   ;; forms.lisp
   #:quoted-p #:quoted-form
   ;; binary.lisp
   #:logandf #:logiorf #:logandcf
   ;; objects.lisp
   #:copy-slots #:slot-value* #:most-specific-last
   ;; conditions.lisp
   #:make-condition-raiser #:make-error-raiser 
   #:with-retry-restart #:with-ignore-restart
   #:make-fixed-restarter
   #:with-condition-restart-binding #:with-condition-printing #:with-condition-collection
   #:condition-bind-default
   ;; functions.lisp
   #:latch-fn #:latch-args #:bukkake-combine #:bukkake-combine-1 #:maybe #:maybecall #:xform #:xform-if #:xform-if-not #:iterate-until #:collect-until #:or-p #:and-p
   ;; lists.lisp
   #:make-queue #:enqueue #:dequeue #:queue-contents #:queue-empty-p #:mapqueue #:mapcons
   ;; pergamum.lisp
   #:progn-1
   #:with-condition-printing
   #:lret #:lret*
   #:nand #:nor #:andf #:orf #:notf #:xorf
   #:quote-when
   #:op-parameter-destructurer
   #:map-lambda-list
   #:order
   #:lambda-list-application-types-match-p
   #:ensure-destructurisation #:destructure-binding-form-body #:destructure-def-body
   #:lambda-list-binds
   #:prepend
   #:emit-let #:emit-lambda #:emit-named-lambda #:with-named-lambda-emission #:emit-defun #:with-defun-emission
   #:emit-declarations
   #:lambda-list-satisfied-p
   #:define-evaluation-domain
   #:define-function-evaluations
   #:define-macro-evaluations
   ;; hash-table.lisp
   #:hash-table-next #:hash-table-pop #:hash-table-itearate
   #:make-hash-table-injector #:hash-table-key-present-p
   #:define-container-hash-accessor
   ;; alignment.lisp
   #:alignment-condition #:address-misalignment #:size-misalignment
   #:aligned-p #:align-down #:align-up
   #:aligned-4 #:aligned-16
   #:check-size-alignment #:check-address-alignment
   ;; extent.lisp
   #:extent #:make-extent #:extent-base #:extent-data #:extent-length #:point-in-extent-p #:extents-intersect-p
   #:extent-spec #:make-extent-spec #:print-extent-spec
   #:extent-spec-base #:extent-spec-length #:extent-spec-length
   #:do-extent-spec-aligned-blocks #:with-aligned-extent-spec-pieces
   ;; u8-sequence.lisp
   #:u8-vector-wordle #:u8-vector-wordbe
   #:u8-vector-word16le #:u8-vector-word16be
   #:u8-vector-word32le #:u8-vector-word32be
   #:u8-vector-word64le #:u8-vector-word64be
   #:u8-seq-word16le #:u8-seq-word16be
   #:u8-seq-word32le #:u8-seq-word32be
   #:u8-seq-word64le #:u8-seq-word64be
   #:print-u8-sequence
   ;; extent-list.lisp
   #:extent-list #:u32-extent-list #:u8-extent-list
   #:extent-list-extents #:extent-list-element-type
   #:extent-list-spec #:print-extent-list-spec #:extent-list-matches-spec-p #:extent-list-spec-mismatch
   #:extent-list-insert #:extent-list-adjoin #:extent-list-grow
   #:merge-extent-lists
   #:extent-list-compatible-vector-p
   #:do-extent-list-vectors
   #:extent-list-vector-by-base
   #:serialize-extent-list #:unserialize-extent-list
   #:extent-lists-equal
   #:dump-u8-extent-list
   ;; extentable.lisp
   #:extentable #:u8-extent #:extentable-u8-vector #:set-u8-extent #:set-u8-extent-list #| #:u8-extent-list -- generic function |#
   #:subextent #:subextent-extent #:subextent-parent #:subextent-absolutize #:subextent-relativize
   ;; read.lisp
   #:make-unreadable-object #:enable-array-reader #:enable-compose-reader #:enable-curry-reader
   ;; packages.lisp
   #:mar #:export-unmarred))

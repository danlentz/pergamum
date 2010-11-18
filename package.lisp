;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PERGAMUM; Base: 10; indent-tabs-mode: nil; show-trailing-whitespace: t -*-
;;;

(defpackage pergamum
  (:use :common-lisp :alexandria :iterate :feet-of-clay)
  (:import-from :cl-fad
                #:directory-exists-p
                #:file-exists-p)
  (:import-from #+sbcl :sb-mop #+(or ecl clisp) :clos #+clozure :ccl
                #:slot-definition-name
                #:class-slots
                #:class-direct-slots
                #:class-direct-superclasses
                #:direct-slot-definition
                #:standard-direct-slot-definition)
  (:export
   ;; everything hinges on that!
   #:not-implemented
   ;; reexport feet-of-clay
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
   ;; basis.lisp
   #:symbol-macro-p
   #:progn-1
   #:lret
   #:lret*
   #:if-let*
   #:case-let
   #:fcase
   #:fcase-1
   #:fif
   #:fif-1
   #:map-remove-if-not
   #:syncformat
   ;; control-flow.lisp
   #:when-lret
   #:if-lret
   #:if-lret*
   #:cond-let
   ;; forms.lisp
   #:ensure-form
   #:quoted-p
   #:quoted-form
   ;; binary.lisp
   #:logandf
   #:logiorf
   #:logandcf
   ;; string.lisp
   #:strconcat
   #:strconcat*
   #:escape
   #:escape*
   ;; binding.lisp
   #:define-execute-with-bound-variable
   ;; classes.lisp
   #:define-protocol-class
   ;; objects.lisp
   #:object-identity
   #:named-object
   #:named-structure
   #:object-name
   #:copy-slots
   #:slot-value*
   #:define-print-object-method
   #:primary-method-not-required
   #:most-specific-last
   ;; conditions.lisp
   #:*debug-condition*
   #:dsignal
   #:derror
   #:with-condition-recourses
   #:call-next-recourse-and-retry
   #:make-condition-raiser
   #:make-error-raiser
   #:with-retry-restarts
   #:with-ignore-restart
   #:make-fixed-restarter
   #:with-condition-restart-binding
   #:with-maybe-just-printing-conditions
   #:with-condition-printing
   #:with-error-resignaling
   #:with-collected-conditions
   #:with-recorded-status
   #:condition-bind-default
   #:call-with-maybe-break-on-signals-set
   #:with-maybe-set-break-on-signals
   #:define-reported-condition
   #:report-simple-condition
   #:define-simple-error
   #:simple-condition-reporter
   ;; conditions.lisp :: canned
   #:redefinition
   #:simple-redefinition
   #:bad-redefinition
   #:warn-redefinition
   #:missing-implementation
   #:not-implemented
   ;; functions.lisp
   #:compose*
   #:reduce*
   #:feq
   #:feql
   #:fequal
   #:fequalp
   #:f=
   #:latch
   #:arg
   #:bukkake-combine
   #:maybe
   #:maybecall
   #:xform
   #:xform-if
   #:xform-if-not
   #:apply/find-if
   #:iterate-until
   #:collect-until
   #:or-p
   #:and-p
   ;; lists.lisp
   #:nfsubst
   #:make-queue
   #:enqueue
   #:dequeue
   #:queue-contents
   #:queue-empty-p
   #:mapqueue
   #:mapcons
   #:unzip
   #:diff-lists
   #:plist-difference
   #:plist-intersection
   #:plist-merge
   #:set-differencef
   #:nset-differencef
   #:set-intersectionf
   #:nset-intersectionf
   #:maptree
   #:map-list-tree
   ;; mop.lisp
   #+sbcl #:rename-class-slot
   #:slot-definition-documentation
   ;; numbers.lisp
   #:excess
   #:shortage
   #:with-measure
   #:><
   #:>=<
   #:>=/<
   #:>/=<
   #:power-of-2-p
   #:ilog2-ceiling
   #:ilog2-cover
   #:bcd-integer-length
   #:decode-bcd
   #:bisect
   #:split-number
   #:with-alignment
   #:map-alignment-left
   #:map-alignment-right
   #:operate-on-extremity
   ;; pergamum.lisp
   #:with-condition-printing
   #:nand
   #:nor
   #:andf
   #:orf
   #:notf
   #:xorf
   #:quote-when
   #:quote-if-non-self-evaluating
   #:op-parameter-destructurer
   #:order
   #:order-funcalling
   #:vector-=
   #:ensure-destructurisation
   #:prepend
   #:emit-let
   #:emit-lambda
   #:emit-named-lambda
   #:with-named-lambda-emission
   #:emit-defun
   #:with-defun-emission
   #:emit-declarations
   #:with-measured-time-lapse
   #:with-time-lapse-measure
   #:with-measured-performance
   ;; lambda-lists.lisp
   #:&mandatory
   #:map-lambda-list
   #:map-lambda-list-bindings-actuals
   #:map-lambda-list-defaults-actuals
   #:lambda-list-binds
   #:lambda-list-1
   ;; streams.lisp
   #:stream-as-vector
   #:stream-as-string
   #:all-stream-forms
   ;; pathnames.lisp
   #:flatten-path-list
   #:fuse-downcased-string-path-list
   #:subwild
   #:subfile
   #:subfile*
   #:subdirectory
   #:subdirectory*
   ;; files.lisp
   #:pathname-busy
   #:pathname-not-present
   #:check-pathname-not-occupied
   #:within-directory
   #:directory-created-p
   #:directory-existed-p
   #:maybe-within-directory
   #:file-as-vector
   #:file-as-string
   #:file-line
   #:with-output-to-file
   #:with-standard-output-to-file
   #:rename-to-directory
   #:symlink-to-p
   #:symlink-target-file
   #:ensure-symlink
   ;; hash-table.lisp
   #:hash-table-next
   #:hash-table-pop
   #:hash-table-itearate
   #:xform-hash-table
   #:make-hash-table-injector
   #:hash-table-key-present-p
   #:maphash*
   #:puthash-unique
   #:gethash-unique
   #:define-subcontainer
   #:define-root-container
   #:with-container
   #:container-condition
   #:container-missing-cell-error
   #:copy-hash-table-empty
   #:with-empty-hash-containers
   ;; alignment.lisp
   #:alignment-condition
   #:address-misalignment
   #:size-misalignment
   #:aligned-p
   #:align-down
   #:align-up
   #:aligned-4
   #:aligned-16
   #:check-size-alignment
   #:check-address-alignment
   ;; extent.lisp
   #:baseless-extent
   #:extent
   #:make-extent
   #:base
   #:size
   #:end
   #:extent-data
   #:inp
   #:intersectp
   #:subextent*
   #:subextent-abs*
   #:subextent
   #:subextent-abs
   #:split-extent
   #:with-split-extent
   #:extent-mask
   #:rebase
   #:nrebase
   #:coerce-extent
   #:ncoerce-extent
   #:extent-data-equalp
   #:extent-equalp
   #:extent-spec
   #:extent
   #:print-extent-spec
   #:print-extent
   #:serialize-extent
   #:serialize-extent-list
   #:extent-reader
   #:read-extent-list
   #:do-extent-spec-aligned-blocks
   #:with-aligned-extent-spec-pieces
   ;; u8-sequence.lisp
   #:swap-word16
   #:swap-word32
   #:swap-word64
   #:u8-vector-wordle
   #:u8-vector-wordbe
   #:u8-vector-word16le
   #:u8-vector-word16be
   #:u8-vector-word32le
   #:u8-vector-word32be
   #:u8-vector-word64le
   #:u8-vector-word64be
   #:u32le-vector-to-u8
   #:u32be-vector-to-u8
   #:u8-vector-to-u32le
   #:u8-vector-to-u32be
   #:u8-seq-word16le
   #:u8-seq-word16be
   #:u8-seq-word32le
   #:u8-seq-word32be
   #:u8-seq-word64le
   #:u8-seq-word64be
   #:u8-seq-wordle
   #:u8-seq-wordbe
   #:align-extend-u8-extent
   #:align-extend-u8-extent-inplace
   #:print-u8-sequence
   #:print-u8-sequence-diff
   ;; bioable.lisp
   #:bioable
   #:bioable-range
   #:read-block
   #:write-block
   #:read-aligned-block
   #:write-aligned-block
   #:write-blocks
   #:u8-extent
   #:write-u8-extent
   #:u8-extents
   #:write-u8-extents
   ;; read.lisp
   #:make-unreadable-object
   #:enable-array-reader
   #:enable-compose-reader
   #:enable-curry-reader
   #:invoke-with-safe-reader-context
   #:with-safe-reader-context
   ;; types.lisp
   #:coerce-to-sequence
   #:remove-if-not-subtype-of
   ;; packages.lisp
   #:mar
   #:export-unmarred
   #:tunnel-package
   ;; to-expunge.lisp
   ))

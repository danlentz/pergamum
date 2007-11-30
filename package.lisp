(defpackage pergamum
  (:use :common-lisp :alexandria :iterate)
  (:export
   ;; basis.lisp
   ;; pergamum.lisp
   #:quote-when
   #:op-parameter-destructurer
   #:map-lambda-list
   #:order
   #:lambda-list-application-types-match-p
   #:mklist #:ensure-destructurisation
   #:lambda-list-binds
   #:emit-let #:emit-lambda #:emit-named-lambda #:with-named-lambda-emission
   #:emit-declarations
   #:lambda-list-satisfied-p
   #:define-evaluation-domain
   #:define-function-evaluations
   #:define-macro-evaluations
   ;; hash-table.lisp
   #:hash-table-next #:hash-table-key-present-p
   ;; extent-list.lisp
   #:extent-list #:u32-extent-list #:u8-extent-list
   #:extent-list-extents #:extent-list-element-type
   #:extent #:make-extent #:extent-base #:extent-data #:extent-length
   #:extent-list-insert #:extent-list-adjoin #:extent-list-grow
   #:merge-extent-lists
   #:extent-list-compatible-vector-p
   #:do-extent-list-vectors
   #:extent-list-vector-by-base
   #:serialize-extent-list #:unserialize-extent-list
   #:extent-lists-equal
   ;; u8-sequence.lisp
   #:u8-vector-wordle #:u8-vector-wordbe
   #:u8-vector-word16le #:u8-vector-word16be
   #:u8-vector-word32le #:u8-vector-word32be
   #:u8-seq-word16le #:u8-seq-word16be
   #:u8-seq-word32le #:u8-seq-word32be
   #:print-u8-sequence))

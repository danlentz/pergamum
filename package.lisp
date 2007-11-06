(defpackage pergamum
  (:use :common-lisp :alexandria :iterate)
  (:export
   ;; pergamum.lisp
   #:quote-when
   #:op-parameter-destructurer
   #:lambda-xform
   #:lambda-list-binds
   #:mklist
   #:emit-lambda #:emit-declarations
   #:define-evaluation-domain
   #:define-function-evaluations
   #:define-macro-evaluations
   ;; u8-sequence.lisp
   #:u8-vector-wordle #:u8-vector-wordbe
   #:u8-vector-word16le #:u8-vector-word16be
   #:u8-vector-word32le #:u8-vector-word32be
   #:u8-seq-word16le #:u8-seq-word16be
   #:u8-seq-word32le #:u8-seq-word32be
   #:print-u8-sequence))

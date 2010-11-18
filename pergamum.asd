;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defsystem :pergamum
  :depends-on (:alexandria :iterate :cl-fad)
  :components
  (;; tier dep
   (:file "feet-of-clay")
   ;; tier 0
   (:file "package" :depends-on ("feet-of-clay"))
   ;; tier 1
   (:file "basis" :depends-on ("package"))
   ;; tier 2
   (:file "macrology" :depends-on ("basis"))
   ;; tier 3
   (:file "alignment" :depends-on ("macrology"))
   (:file "binary" :depends-on ("macrology"))
   (:file "bindings" :depends-on ("macrology"))
   (:file "classes" :depends-on ("macrology"))
   (:file "conditions" :depends-on ("macrology"))
   (:file "control-flow" :depends-on ("macrology"))
   (:file "extent" :depends-on ("macrology" "alignment"))
   (:file "forms" :depends-on ("macrology"))
   (:file "functions" :depends-on ("macrology"))
   (:file "hash-table" :depends-on ("macrology" "conditions"))
   (:file "lists" :depends-on ("macrology"))
   (:file "packages" :depends-on ("macrology"))
   (:file "mop" :depends-on ("macrology"))
   (:file "numbers" :depends-on ("macrology"))
   (:file "objects" :depends-on ("forms"))
   (:file "pathnames" :depends-on ("macrology"))
   (:file "pergamum" :depends-on ("macrology" "forms"))
   (:file "read" :depends-on ("macrology"))
   (:file "streams" :depends-on ("macrology"))
   (:file "string" :depends-on ("macrology"))
   ;; tier 4
   (:file "files" :depends-on ("streams"))
   (:file "lambda-lists" :depends-on ("pergamum"))
   (:file "u8-sequence" :depends-on ("alignment" "extent"))
   (:file "types" :depends-on ("extent"))
   ;; tier 5
   (:file "bioable" :depends-on ("extent" "objects" "u8-sequence"))
   ;; expunge tier
   (:file "to-expunge" :depends-on ("bioable"))))

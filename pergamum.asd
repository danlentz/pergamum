;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defsystem :pergamum
  :depends-on (:alexandria :iterate :cl-fad)
  :components
  (;; tier dep
   (:file "feet-of-clay")
   ;; tier 0
   (:file "package" :depends-on ("feet-of-clay"))
   ;; tier 1
   (:file "alignment" :depends-on ("package"))
   (:file "basis" :depends-on ("package"))
   (:file "binary" :depends-on ("package"))
   (:file "bindings" :depends-on ("package"))
   (:file "classes" :depends-on ("package"))
   (:file "conditions" :depends-on ("package"))
   (:file "forms" :depends-on ("package"))
   (:file "functions" :depends-on ("package"))
   (:file "numbers" :depends-on ("package"))
   (:file "packages" :depends-on ("package"))
   (:file "mop" :depends-on ("package"))
   (:file "read" :depends-on ("package"))
   (:file "string" :depends-on ("package"))
   ;; tier 2
   (:file "control-flow" :depends-on ("basis"))
   (:file "lists" :depends-on ("basis"))
   (:file "streams" :depends-on ("basis"))
   (:file "pathnames" :depends-on ("basis"))
   (:file "hash-table" :depends-on ("basis" "conditions"))
   (:file "pergamum" :depends-on ("basis" "forms"))
   (:file "objects" :depends-on ("forms"))
   (:file "extent" :depends-on ("basis" "alignment"))
   ;; tier 3
   (:file "files" :depends-on ("streams"))
   (:file "lambda-lists" :depends-on ("pergamum"))
   (:file "u8-sequence" :depends-on ("alignment" "extent"))
   (:file "types" :depends-on ("extent"))
   ;; tier 4
   (:file "bioable" :depends-on ("extent" "objects" "u8-sequence"))
   ;; expunge tier
   (:file "to-expunge" :depends-on ("bioable"))))

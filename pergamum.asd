(defpackage :pergamum.system
  (:use :cl :asdf))

(in-package :pergamum.system)

(defsystem :pergamum
  :depends-on (:alexandria :iterate)
  :components
  (;; tier 0
   (:file "package")
   ;; tier 1
   (:file "alignment" :depends-on ("package"))
   (:file "basis" :depends-on ("package"))
   (:file "binary" :depends-on ("package"))
   (:file "conditions" :depends-on ("package"))
   (:file "forms" :depends-on ("package"))
   (:file "functions" :depends-on ("package"))
   (:file "hash-table" :depends-on ("package"))
   (:file "packages" :depends-on ("package"))
   (:file "numbers" :depends-on ("package"))
   (:file "read" :depends-on ("package"))
   ;; tier 2
   (:file "lists" :depends-on ("basis"))
   ;; tier 3
   (:file "extent" :depends-on ("basis" "alignment"))
   (:file "pergamum" :depends-on ("basis"))
   (:file "objects" :depends-on ("forms"))
   ;; tier 4
   (:file "u8-sequence" :depends-on ("extent"))
   ;; tier 5
   (:file "extent-list" :depends-on ("u8-sequence"))
   ;; tier 6
   (:file "extentable" :depends-on ("extent-list"))))

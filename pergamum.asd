(defpackage :pergamum.system
  (:use :cl :asdf))

(in-package :pergamum.system)

(defsystem :pergamum
  :depends-on (:alexandria :iterate)
  :components
  (;; tier 0
   (:file "package")
   ;; tier 1
   (:file "forms" :depends-on ("package"))
   (:file "objects" :depends-on ("package"))
   (:file "read" :depends-on ("package"))
   (:file "basis" :depends-on ("package"))
   (:file "binary" :depends-on ("package"))
   (:file "conditions" :depends-on ("package"))
   (:file "functions" :depends-on ("package"))
   (:file "hash-table" :depends-on ("package"))
   (:file "u8-sequence" :depends-on ("package"))
   ;; tier 2
   (:file "pergamum" :depends-on ("basis"))
   (:file "extent-list" :depends-on ("u8-sequence"))
   ;; tier 3
   (:file "extentable" :depends-on ("extent-list"))))

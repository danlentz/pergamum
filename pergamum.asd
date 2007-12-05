(defpackage :pergamum.system
  (:use :cl :asdf))

(in-package :pergamum.system)

(defsystem :pergamum
  :depends-on (:alexandria :iterate)
  :components
  ((:file "package")
   (:file "basis" :depends-on ("package"))
   (:file "functions" :depends-on ("package"))
   (:file "pergamum" :depends-on ("basis"))
   (:file "hash-table" :depends-on ("package"))
   (:file "u8-sequence" :depends-on ("package"))
   (:file "extent-list" :depends-on ("u8-sequence"))))

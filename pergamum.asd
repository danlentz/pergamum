(defpackage :pergamum.system
  (:use :cl :asdf))

(in-package :pergamum.system)

(defsystem :pergamum
  :depends-on (:alexandria :iterate)
  :components
  ((:file "package")
   (:file "basis" :depends-on ("package"))
   (:file "pergamum" :depends-on ("basis"))
   (:file "extent-list" :depends-on ("package"))
   (:file "u8-sequence" :depends-on ("package"))))

(defpackage :pergamum.system
  (:use :cl :asdf))

(in-package :pergamum.system)

(defsystem :pergamum
  :depends-on (:alexandria :iterate)
  :components
  ((:file "package")
   (:file "basis")
   (:file "pergamum" :depends-on ("basis"))
   (:file "u8-sequence" :depends-on ("package"))))

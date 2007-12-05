(in-package :pergamum)

(defun style-warn (format-control &rest format-arguments)
  (warn 'simple-style-warning
   :format-control format-control
   :format-arguments format-arguments))
(in-package :pergamum)

(defclass slot-renaming-mixin () 
  ((from :initarg from)
   (to :initarg to)))

(defmethod update-instance-for-redefined-class ((o slot-renaming-mixin) added discarded plist &rest initargs)
  (declare (ignore added discarded plist initargs))
  (call-next-method)
  (with-slots (from to) o
    (setf (slot-value o to) (slot-value o from))))

#+sbcl
(defun essentialize-slotd (slotd)
  "Scavenge the slot definition and re-build /its/ definition form, as used by ENSURE-CLASS."
  (loop :for slot :in '(sb-pcl::name sb-pcl::initform sb-pcl::initfunction sb-pcl::readers sb-pcl::writers sb-pcl::initargs sb-pcl::%type)
     :for initarg in '(:name :initform :initfunction :readers :writers :initargs :type)
     :collect initarg :collect (slot-value slotd slot)))

#+sbcl
(defun rename-class-slot (class from to)
  "Change name of the slot of CLASS from FROM to TO, 
   arranging for its value to be carried over in all instances.

   Caveats:
   1. Has a low-pass filter effect, due to not being able
      to preserve all slotd slots.
   2. SBCL-only."
  (declare (type (or symbol class) class))
  (let ((class (if (symbolp class) (find-class class) class)))
    (unless (find from (class-slots class) :key #'slot-definition-name)
      (error "The source slot ~S not found in class ~S." from class))
    (when (find to (class-slots class) :key #'slot-definition-name)
      (error "The destination slot ~S already present in class ~S." to class))
    (unless (eq (class-name (class-of class)) 'standard-class)
      (error "Can only rename slots in instances of instances of standard-class."))
    (let* ((from-slotd (find from (class-slots class) :key #'slot-definition-name))
           (new-slotd (make-instance 'standard-direct-slot-definition :name to)))
      (flet ((make-default-initarg (name val) (list name val (lambda () val))))
        (dolist (slot '(sb-pcl::initform sb-pcl::initfunction sb-pcl::readers sb-pcl::writers sb-pcl::initargs sb-pcl::%type))
          (when (slot-boundp from-slotd slot)
            (setf (slot-value new-slotd slot) (slot-value from-slotd slot))))
        (reinitialize-instance
         class
         :direct-slots (mapcar #'essentialize-slotd (cons new-slotd (class-direct-slots class)))
         :direct-superclasses (cons (find-class 'slot-renaming-mixin) (class-direct-superclasses class)))
        (reinitialize-instance
         class
         :direct-default-initargs (list* (make-default-initarg 'from from)
                                         (make-default-initarg 'to to)
                                         (class-direct-default-initargs class)))
        (reinitialize-instance
         class
         :direct-slots (mapcar #'essentialize-slotd (remove from (class-direct-slots class) :key #'slot-definition-name))
         :direct-default-initargs (set-difference (class-direct-default-initargs class) '((from) (to)) :key #'first)
         :direct-superclasses (remove 'slot-renaming-mixin (class-direct-superclasses class) :key #'class-name))))))

(defun slot-definition-documentation (slotd)
  #+sbcl
  (sb-pcl::%slot-definition-documentation slotd))

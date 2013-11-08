(cl:in-package :ps)

;; TODO: Fix this so it doesn't use initarg/forms, and directly
;;       control it with `INITIALIZE-INSTANCE'.
(defclass decoder ()
  ;; TODO: Document
   ; ps-decoder-t
  ((decoder-ptr)
   (config
    :initarg :config
    :initform (error "Must provide a `CONFIG'.")
    :reader config)))

(defmethod initialize-instance :after ((decoder decoder) &key config)
  ;; TODO: Need to test these finalizers.
  ;; TODO: Error checking/null-ptr checking.
  (check-type config config)
  ;; TODO: This _seriously_ needs null-pointer checking!!
  (let ((decoder-ptr (ps-init (slot-value config 'config-ptr))))
    (setf (slot-value decoder 'decoder-ptr) decoder-ptr)
    (tg:finalize decoder (lambda () (ps-free decoder-ptr)))))


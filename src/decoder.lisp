(cl:in-package :ps)

(defclass decoder ()
  ;; TODO: Document
   ; ps-decoder-t
  ((decoder-ptr)))

(defmethod initialize-instance :after ((decoder decoder) &key config)
  ;; TODO: Need to test these finalizers.
  ;; TODO: Error checking/null-ptr checking.
  (check-type config config)
  ;; TODO: This _seriously_ needs null-pointer checking!!
  (let ((decoder-ptr (ps-init (slot-value config 'config-ptr))))
    (setf (slot-value decoder 'decoder-ptr) decoder-ptr)
    (tg:finalize decoder (lambda () (ps-free decoder-ptr)))))

(defgeneric config (decoder)
  (:documentation "Return the current `CONFIG' object for the decoder
  `DECODER'."))

(defmethod config ((decoder decoder))
  (with-slots (decoder-ptr) decoder
    (make-instance 'config :config-ptr (ps-get-config decoder-ptr))))

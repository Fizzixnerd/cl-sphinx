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
  (with-slots (config-ptr) config
    (with-slots (decoder-ptr) decoder
      (setf decoder-ptr (ps-sys:ps-init config-ptr))
      (tg:finalize decoder (lambda () (ps-sys:ps-free decoder-ptr))))))

(defmethod hypothesis ((decoder decoder))
  (with-slots (decoder-ptr) decoder
    (with-hypothesis-pointers score-ptr uttid-ptr
      (let* ((hypothesis-string (ps-sys:ps-get-hyp decoder-ptr score-ptr uttid-ptr))
	     (hypothesis (unless (null hypothesis-string)
			   (make-instance 'hypothesis
					  :hyp-string hypothesis-string
					  :score (cffi:mem-ref score-ptr :int32)
					  :utterance-id (cffi:mem-ref uttid-ptr :string)))))
	hypothesis))))

;; (defmethod hypothesis ((decoder decoder))
;;   ;; TODO: DOCUMENTATION: Returns the best hypothesis so far
;;   ;; TODO: Make this exception safe with respect to the foreign allocs.
;;   ;; TODO: Fix and make sure this doesn't break with null-pointers.
;;   (with-slots (decoder-ptr) decoder
;;     (let* ((score-ptr (cffi:foreign-alloc :int32))
;; 	   (uttid-ptr (cffi:foreign-alloc :string))
;; 	   (hypothesis-string (ps-sys:ps-get-hyp decoder-ptr score-ptr uttid-ptr))
;; 	   (hypothesis (unless (null hypothesis-string)
;; 			 (make-instance 'hypothesis
;; 					:hyp-string hypothesis-string
;; 					:score (cffi:mem-ref score-ptr :int32)
;; 					:utterance-id (cffi:mem-ref uttid-ptr :string)))))
;;       (cffi:foreign-free score-ptr)
;;       (cffi:foreign-free uttid-ptr)
;;       hypothesis)))

;; TODO: Should utterances be first-class objects?  It seems like it...
(defgeneric start-utterance (decoder &optional utterance-id)
  (:documentation "Start utterance processing.  This function should
  be called before any utterance data is passed to the decoder.  It
  marks the start of a new utterance and reinitializes internal data
  structures. `UTTERANCE-ID' is a string which uniquely identifies the
  utterance.  If not supplied, one will be created."))

(defmethod start-utterance ((decoder decoder) &optional utterance-id)
  ;; TODO: FIX and make sure this doesn't break with NULL pointers.
  (with-slots (decoder-ptr) decoder
    (let ((uttid (or utterance-id cffi:null-pointer)))
      (ps-sys:ps-start-utt decoder-ptr uttid))))

(defgeneric end-utterance (decoder)
  (:documentation "End processing the current utterance."))

(defmethod end-utterance ((decoder decoder))
  (with-slots (decoder-ptr) decoder
    (ps-sys:ps-end-utt decoder-ptr)))

(defgeneric utterance-id (decoder)
  (:documentation "Return the utterance id of the current utterance.
  Valid only until the beginning of the next utterance."))

(defmethod utterance-id ((decoder decoder))
  (with-slots (decoder-ptr) decoder
    (ps-sys:ps-get-uttid decoder-ptr)))

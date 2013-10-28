(cl:in-package :ps)

(defclass hypothesis ()
  ;; TODO: Document
  ;; NOTE: This does not require a finalizer, because we can free the
  ;;       pointers right after translation.
  ((hyp-string
    ;; string
    :initarg :hyp-string
    :reader hyp-string)
   (score
    ;; int32
    :initarg :score
    :reader score)
   (utterance-id
    ;; string
    :initarg :utterance-id
    :reader utterance-id)))

(defgeneric best-hypothesis (decoder)
  (:documentation "Return a `HYPOTHESIS' object containing the best
  hypothesis at this point in decoding, or `NIL' if no hypothesis is
  available."))

(defmethod best-hypothesis ((decoder decoder))
  ;; TODO: Make this exception safe with respect to the foreign allocs.
  ;; TODO: Fix and make sure this doesn't break with null-pointers.
  (with-slots (decoder-ptr) decoder
    (let* ((score-ptr (cffi:foreign-alloc :int32))
	   (uttid-ptr (cffi:foreign-alloc :string))
	   (hyp-string (ps-get-hyp decoder-ptr score-ptr uttid-ptr))
	   (hypothesis (when (not (cffi:null-pointer-p hyp-string))
			 (make-instance 'hypothesis
					:hyp-string hyp-string
					:score (cffi:mem-ref score-ptr :int32)
					:utterance-id (cffi:mem-ref uttid-ptr :string)))))
      (cffi:foreign-free score-ptr)
      (cffi:foreign-free uttid-ptr)
      hypothesis)))

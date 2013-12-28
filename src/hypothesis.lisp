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

(defgeneric hypothesis (hypothesis-source)
  (:documentation "Return a `HYPOTHESIS' object, or `NIL' if no
  hypothesis is available.  Valid `HYPOTHESIS-SOURCE's include
  `DECODER's and `NBEST's"))

(defmacro with-hypothesis-pointers (score-ptr-name uttid-ptr-name &body body)
  `(let* ((,score-ptr-name (cffi:foreign-alloc :int32))
	 (,uttid-ptr-name (cffi:foreign-alloc :string))
	 (dummy ,@body))
     (cffi:foreign-free ,score-ptr-name)
     (cffi:foreign-free ,uttid-ptr-name)
     dummy))


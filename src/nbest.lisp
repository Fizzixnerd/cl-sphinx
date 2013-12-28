(cl:in-package :ps)

(defclass nbest (ps-utils:iterator)
  ((nbest-ptr)
   (utterance-id
    :initarg :utterance-id
    :reader utterance-id)))

(defmethod hypothesis ((nbest nbest))
  (with-slots (nbest-ptr) nbest
    (with-hypothesis-pointers 'score-ptr '%not-used%
      (let* ((hypothesis-string (ps-sys:ps-nbest-hyp nbest-ptr score-ptr))
	     (hypothesis (unless (null hypothesis-string)
			   (make-instance 'hypothesis
					  :hyp-string hypothesis-string
					  :score (cffi:mem-ref score-ptr :int32)
					  :utterance-id (utterance-id nbest)))))
	hypothesis))))

;; Iterator methods
(defmethod next ((nbest nbest))
  (with-slots (nbest-ptr) nbest
    (ps-sys:ps-nbest-next nbest-ptr)))

(defmethod value ((nbest nbest))
  (hypothesis nbest))

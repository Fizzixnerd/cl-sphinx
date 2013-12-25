(cl:in-package :ps-sys)

(define-foreign-library libpocketsphinx
    (:unix (:or "libpocketsphinx.so.1" "libpocketsphinx.so"))
  (t (:default "libpocketsphinx")))

(use-foreign-library libpocketsphinx)

(defctype ps-decoder-t :pointer)
(defctype ps-nbest-t :pointer)
(defctype ps-seg-t :pointer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; POCKETSPHINX FUNCTIONS ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun "ps_args" arg-t)

(defcfun "ps_init" ps-decoder-t
  (config cmd-ln-t))

(defcfun "ps_free" :int
  (ps ps-decoder-t))

(defcfun "ps_decode_raw" :int
  (ps ps-decoder-t)
  (fh file)
  (uttid :string)
  (maxsamps :long))

(defcfun "ps_get_hyp" :string
  (ps ps-decoder-t)
  (out-best-score :pointer)
  (out-uttid :pointer))

(defcfun "ps_get_config" cmd-ln-t
  (ps ps-decoder-t))


(defcfun "ps_nbest" ps-nbest-t
  (ps ps-decoder-t)
  (sf :int)
  (ef :int)
  (ctx1 :string)
  (ctx2 :string))

(defcfun "ps_nbest_next" ps-nbest-t
  (nbest ps-nbest-t))

(defcfun "ps_nbest_hyp" :string
  (nbest ps-nbest-t)
  (out-score :pointer))

(defcfun "ps_nbest_seg" ps-seg-t
  (nbest ps-nbest-t)
  (out-score :pointer))

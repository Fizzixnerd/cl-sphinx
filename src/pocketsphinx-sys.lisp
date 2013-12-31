(cl:in-package :ps-sys)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LOAD POCKETSPHINX LIBRARY ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-foreign-library libpocketsphinx
    (:unix (:or "libpocketsphinx.so.1" "libpocketsphinx.so"))
  (t (:default "libpocketsphinx")))

(use-foreign-library libpocketsphinx)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; POCKETSPHINX TYPES ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defctype ps-decoder-t :pointer)
(defctype ps-nbest-t :pointer)
(defctype ps-seg-t :pointer)
(defctype ps-lattice-t :pointer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; POCKETSPHINX FUNCTIONS ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DECODER ;;

(defcfun "ps_args" arg-t)

(defcfun "ps_init" ps-decoder-t
  (config cmd-ln-t))

(defcfun "ps_free" :int
  (ps ps-decoder-t))

(defcfun "ps_start_utt" :int
  (ps ps-decoder-t)
  (uttid :string))

(defcfun "ps_end_utt" :int
  (ps ps-decoder-t))

(defcfun "ps_get_uttid" :string
  (ps ps-decoder-t))

(defcfun "ps_decode_raw" :int
  (ps ps-decoder-t)
  (fh file)
  (uttid :string)
  (maxsamps :long))

(defcfun "ps_process_raw" :int
  (ps ps-decoder-t)
  (data :pointer)
  (n-samples size-t)
  (no-search :int)
  (full-utt :int))

;; HYPOTHESIS ;;

(defcfun "ps_get_hyp" :string
  (ps ps-decoder-t)
  (out-best-score :pointer)
  (out-uttid str-array))

(defcfun "ps_get_prob" :int32
  (ps ps-decoder-t)
  (out_uttid str-array))

;; CONFIG ;;

(defcfun "ps_get_config" cmd-ln-t
  (ps ps-decoder-t))

;; LATTICE ;;

(defcfun "ps_get_lattice" ps-lattice-t
  (ps ps-decoder-t))

;; SEGMENT ;;

(defcfun "ps_seg_iter" ps-seg-t
  (ps ps-decoder-t)
  (out-best-score :pointer)) ;int32

(defcfun "ps_seg_next" ps-seg-t
  (seg ps-seg-t))

;; WARNING: this string is only valid until next call to ps-seg-next!
(defcfun "ps_seg_word" :string
  (seg ps-seg-t))

(defcfun "ps_seg_frames" :void
  (seg ps-seg-t)
  (out-sf :pointer) ;pointers to ints
  (out-ef :pointer))

;; TODO: This C function is a mess.  Refer to the header for details
;;       on arguments.
(defcfun "ps_seg_prob" :int32
  (seg ps-seg-t)
  (out_ascr :pointer) ; pointers to int32's
  (out_lscr :pointer)
  (out_lback :pointer)) ; only useful for ngram models

(defcfun "ps_seg_free" :void
  (seg ps-seg-t))

;; NBEST ;;

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
  (out-score :pointer)) ; pointer to int32

(defcfun "ps_nbest_seg" ps-seg-t
  (nbest ps-nbest-t)
  (out-score :pointer))

(defcfun "ps_nbest_free" :void
  (nbest ps-nbest-t))

(defcfun "ps_get_utt_time" :void
  (ps ps-decoder-t)
  (out-nspeech :pointer) ; pointers to doubles
  (out-ncpu :pointer)
  (out-nwall :pointer))

(defcfun "ps_get_all_time" :void
  (ps ps-decoder-t)
  (out-nspeech :pointer) ; pointers to doubles
  (out-ncpu :pointer)
  (out-nwall :pointer))


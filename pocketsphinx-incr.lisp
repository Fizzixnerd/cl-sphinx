;; Me incrementally developing the bindings for Pocketsphinx.

(require 'asdf)

(asdf:load-system :cffi)
(asdf:load-system :alexandria)
(asdf:load-system :trivial-garbage)

(defpackage :pocketsphinx
  (:use :common-lisp
	:cffi
	:alexandria
	:trivial-garbage)
  (:nicknames :ps)
  (:export libpocketsphinx
	   libsphinxbase
	   ps-decoder-t
	   cmd-ln-t
	   arg-t
	   cmd-ln-str-r
	   ps-args
	   ps-init
	   without-float-traps
	   *cmd-ln*
	   *decoder*)
  (:shadow foreign-funcall
	   defcfun))

(in-package :pocketsphinx)

(define-foreign-library libpocketsphinx
  (:unix (:or "libpocketsphinx.so.1" "libpocketsphinx.so"))
  (t (:default "libpocketsphinx")))

(define-foreign-library libsphinxbase
  (:unix (:or "libsphinxbase.so.1" "libsphinxbase.so"))
  (t (:default "libsphinxbase")))

(use-foreign-library libpocketsphinx)
(use-foreign-library libsphinxbase)

(defctype ps-decoder-t :pointer)
(defctype cmd-ln-t :pointer)
(defctype arg-t :pointer)
(defctype file :pointer)
(defctype str-array :pointer)

;; helpers
(defmacro without-float-traps (&body body)
  "Disable float traps within body."
  `(sb-int:with-float-traps-masked (:invalid :divide-by-zero :inexact :underflow :overflow)
     ,@body))

(defmacro foreign-funcall (name-and-options &rest args)
  `(ps:without-float-traps
     (cffi:foreign-funcall ,name-and-options ,@args)))

;; TESTED
(defun locate-defun/defmacro-form (macroexpanded-defcfun-form)
  "Return as multiple values:

The DEFUN or DEFMACRO form within the form returned by a call
to (MACROEXPAND-1 '(CFFI:DEFCFUN ...)).

The form(s) before the located form."
  ;; Make this throw an error actually, and check instead for
  ;; `DEFMACRO' or `PROGN'
  (cond ((string= (car macroexpanded-defcfun-form) 'progn)
	 (values (lastcar macroexpanded-defcfun-form)
		 (butlast macroexpanded-defcfun-form)))
	((string= (car macroexpanded-defcfun-form) 'defmacro)
	 (values macroexpanded-defcfun-form
		 '()))))

;; TESTED
(defun locate-defun/defmacro-body (macroexpanded-defcfun-form)
  (let ((forms-before-body (if (stringp (fourth macroexpanded-defcfun-form))
			       4
			       3)))
    (values (car (subseq macroexpanded-defcfun-form forms-before-body))
	    (subseq macroexpanded-defcfun-form 0 forms-before-body))))

;; TESTED
(defun wrap-form (wrapper-form form)
  (if wrapper-form
      (append wrapper-form `(,form))
      form))

;; TESTED
;; FIXME: NEED TO FIX THIS FOR MACROS, IT CURRENTLY DOESN'T WORK AS
;;        INTENDED (just wraps the macro in the _function call_,
;;        doesn't make a macro.
(defmacro defcfun (name-and-options return-type &rest args)
  (multiple-value-bind (defun/defmacro-form prior-forms)
      (locate-defun/defmacro-form (macroexpand-1
				   `(cffi:defcfun ,name-and-options ,return-type ,@args)))
    (multiple-value-bind (defun/defmacro-body body-prior-forms)
	(locate-defun/defmacro-body defun/defmacro-form)
      (wrap-form prior-forms
	(wrap-form body-prior-forms
	  (wrap-form '(ps:without-float-traps) defun/defmacro-body))))))

;; C library functions
(defcfun "fopen" file
  (pathname :string)
  (mode :string))

(defcfun "fclose" :void
  (filehandle file))

;; sphinxbase functions
(defcfun "cmd_ln_init" cmd-ln-t
  (inout-cmdln cmd-ln-t)
  (defn arg-t)
  (strict :boolean)
  &rest)

(defcfun "cmd_ln_str_r" :string
  (cmdln cmd-ln-t)
  (name :string))

(defcfun "cmd_ln_str_list_r" str-array
  (cmdln cmd-ln-t)
  (name :string))

(defcfun "cmd_ln_int_r" :long
  (cmdln cmd-ln-t)
  (name :string))

(defcfun "cmd_ln_float_r" :double
  (cmdln cmd-ln-t)
  (name :string))

;; Is set as a macro in cmd_ln.h
(defun cmd-ln-boolean-r (cmdln name)
  (not (zerop (cmd-ln-int-r cmdln name))))

(defcfun "cmd_ln_set_str_r" :void
  (cmdln cmd-ln-t)
  (name :string)
  (str :string))

(defcfun "cmd_ln_set_int_r" :void
  (cmdln cmd-ln-t)
  (name :string)
  (int :long))

(defcfun "cmd_ln_set_float_r" :void
  (cmdln cmd-ln-t)
  (name :string)
  (int :long))

(defun cmd-ln-set-boolean-r (cmdln name bool)
  (cmd-ln-set-int-r cmdln name (not (zerop bool))))

(defcfun "cmd_ln_exists_r" :int
  (cmdln cmd-ln-t)
  (name :string))

(defcfun "cmd_ln_print_help_r" :void
  (cmdln cmd-ln-t)
  (fp file)
  (defn arg-t))

;; pocketsphinx functions
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

;; CLOS API
(defclass config ()
  ;; TODO: Document
  ;; TODO: In the initialize-instance method, make sure to check for
  ;;       null pointers!
   ; cmd-ln-t
  ((config-ptr
    :initarg :config-ptr)))

(defgeneric option (config config-keyword)
  ;; TODO: Maybe make this not `SETF'-able.  We'll see...
  (:documentation "Return the config value indicated by
  `CONFIG-KEYWORD'. This is a `SETF'-able place."))

(defmethod option ((config config) config-keyword)
  (let ((config-string (config-keyword->string config-keyword))
	(config-value-type (gethash config-keyword +config-type-hash+)))
    (with-slots (config-ptr) config
      ;; FIXME:
      ;; TODO: THIS MIGHT NOT BE PORTABLE SINCE CASE COMPARES WITH
      ;;       EQL.
      (case config-value-type
	('integer (cmd-ln-int-r config-ptr config-string))
	('float (cmd-ln-float-r config-ptr config-string))
	('boolean (cmd-ln-boolean-r config-ptr config-string))
	('string (cmd-ln-str-r config-ptr config-string))
	(otherwise (progn
		     (warn "Type of ~A is unknown; assuming ~A." config-keyword 'string)
		     (cmd-ln-str-r config-ptr config-string)))))))

(defun make-testing-config ()
  ;; This is just for testing stuff
  (make-instance 'config
		 :config-ptr
		 (cmd-ln-init (cffi:null-pointer)
			      (ps-args)
			      t
			      :string "-hmm"
			      :string "/usr/local/share/pocketsphinx/model/hmm/en_US/hub4wsj_sc_8k"
			      :string "-lm"
			      :string "/usr/local/share/pocketsphinx/model/lm/en_US/hub4.5000.DMP"
			      :string "-dict"
			      :string "/usr/local/share/pocketsphinx/model/lm/en_US/hub4.5000.dic"
			      :pointer (cffi:null-pointer))))

(defclass decoder ()
  ;; TODO: Document
   ; ps-decoder-t
  ((decoder-ptr)))

(defgeneric config (decoder)
  (:documentation "Return the current `CONFIG' object for the decoder
  `DECODER'."))

(defmethod config ((decoder decoder))
  (with-slots (decoder-ptr) decoder
    (make-instance 'config :config-ptr (ps-get-config decoder-ptr))))

(defmethod initialize-instance :after ((decoder decoder) &key config)
  ;; TODO: Need to test these finalizers.
  ;; TODO: Error checking/null-ptr checking.
  (check-type config config)
  ;; TODO: This _seriously_ needs null-pointer checking!!
  (let ((decoder-ptr (ps-init (slot-value config 'config-ptr))))
    (setf (slot-value decoder 'decoder-ptr) decoder-ptr)
    (tg:finalize decoder (lambda () (ps-free decoder-ptr)))))

(defclass hypothesis ()
  ;; TODO: Document
  ;; NOTE: This does not require a finalizer, because we can free the
  ;;       pointers right after translation.
  ((hyp-string
    ; string
    :initarg :hyp-string
    :reader hyp-string)
   (score
    ; int32
    :initarg :score
    :reader score)
   (utterance-id
    ; string
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

;; stuff that I've typed into the REPL
(defparameter *cmd-ln*
  (cmd-ln-init (cffi:null-pointer)
	       (ps-args)
	       t
	       :string "-hmm"
	       :string "/usr/local/share/pocketsphinx/model/hmm/en_US/hub4wsj_sc_8k"
	       :string "-lm"
	       :string "/usr/local/share/pocketsphinx/model/lm/en_US/hub4.5000.DMP"
	       :string "-dict"
	       :string "/usr/local/share/pocketsphinx/model/lm/en_US/hub4.5000.dic"
	       :pointer (cffi:null-pointer)))
(print *cmd-ln*)

(defparameter *decoder* (ps-init *cmd-ln*))
(print *decoder*)

(defparameter *file-handle* (fopen "/home/matt/src/cmusphinx/matt-tests/c/test_file.raw" "rb"))
(print *file-handle*)

(defparameter *return-value* (ps-decode-raw *decoder* *file-handle* "testfile" -1))
(print *return-value*)

(defparameter *score-addr* (foreign-alloc :int32))
(print *score-addr*)

(defparameter *uttid-addr* (foreign-alloc :string))
(print *uttid-addr*)

(defparameter *hypothesis* (ps-get-hyp *decoder* *score-addr* *uttid-addr*))
(print *hypothesis*)

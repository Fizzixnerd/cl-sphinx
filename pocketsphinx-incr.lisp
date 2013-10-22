;; Me incrementally developing the bindings for Pocketsphinx.

(require 'asdf)

(defpackage :pocketsphinx
  (:use :common-lisp
	:cffi
	:alexandria)
  (:nicknames :ps)
  (:export libpocketsphinx
	   libsphinxbase
	   ps-decoder-t
	   cmd-ln-t
	   arg-t
	   cmd-ln-init-m
	   cmd-ln-str-r
	   ps-args
	   ps-init
	   without-float-traps
	   *cmd-ln*
	   *decoder*)
  (:shadow foreign-funcall
	   defcfun))

(in-package :pocketsphinx)

(asdf:load-system :cffi)
(asdf:load-system :alexandria)

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
  (values (lastcar macroexpanded-defcfun-form)
	  (butlast macroexpanded-defcfun-form)))

;; TESTED
(defun locate-defun/defmacro-body (macroexpanded-defcfun-form)
  (let ((forms-before-body (if (stringp (fourth macroexpanded-defcfun-form))
			       4
			       3)))
    (values (car (subseq macroexpanded-defcfun-form forms-before-body))
	    (subseq macroexpanded-defcfun-form 0 forms-before-body))))

;; TESTED
(defun wrap-form (wrapper-form &rest forms)
  (append wrapper-form forms))

;; TESTED
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

;; (defmacro defcfun (name-and-options return-type &rest args)
;;   ;; TODO: Maybe add an only-once form here?
;;   (let ((defcfun-macro-expansion
;; 	 (macroexpand-1 `(cffi:defcfun ,name-and-options ,return-type ,@args))))
;;     (append (butlast defcfun-macro-expansion)
;; 	    (append (subseq (car (last defcfun-macro-expansion)) 0 3)
;; 		    (list (list 'without-float-traps
;; 				(subseq (car (last defcfun-macro-expansion)) 3)))))))
		       
;; sphinxbase functions

(defcfun "cmd_ln_init" cmd-ln-t
  (inout-cmdln cmd-ln-t)
  (defn arg-t)
  (strict :boolean)
  &rest)

(defcfun "cmd_ln_str_r" :string
  (cmdln cmd-ln-t)
  (name :string))

;; pocketsphinx functions
(defcfun "ps_args" arg-t)

(defcfun "ps_init" ps-decoder-t
  (config cmd-ln-t))

(defcfun "ps_decode_raw" :int
  (ps ps-decoder-t)
  (fh file)
  (uttid :string)
  (maxsamps :long))

(defcfun "ps_get_hyp" :string
  (ps ps-decoder-t)
  (out-best-score :pointer)
  (out-uttid :pointer))

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
	       :string "/usr/local/share/pocketsphinx/model/lm/en_US/hub4.5000.dic"))
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

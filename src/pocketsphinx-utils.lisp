(cl:in-package :ps-utils)

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
			    (wrap-form '(without-float-traps) defun/defmacro-body))))))

(defmacro without-float-traps (&body body)
  ;; TODO: Make this portable to more lisp environments.
  "Disable float traps within body."
  `(sb-int:with-float-traps-masked (:invalid :divide-by-zero :inexact :underflow :overflow)
     ,@body))

(defmacro foreign-funcall (name-and-options &rest args)
  `(without-float-traps
       (cffi:foreign-funcall ,name-and-options ,@args)))

;; TESTED
(defun locate-defun/defmacro-form (macroexpanded-defcfun-form)
  "Return as multiple values:

The DEFUN or DEFMACRO form within the form returned by a call
to (MACROEXPAND-1 '(CFFI:DEFCFUN ...)).

The form(s) before the located form."
  ;; TODO: Make this throw an error actually, and check instead for
  ;;       `DEFMACRO' or `PROGN'
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

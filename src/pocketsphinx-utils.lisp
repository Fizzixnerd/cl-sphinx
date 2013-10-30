(cl:in-package :ps-utils)

;;; WORKAROUNDS FOR CFFI/SBCL FLOAT-TRAPS

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

;;; CONFIG/CMDLN INFORMATION GATHERING
;;; NOTE: This is going to be a bit hackish, and will rely on the
;;;       format used in pocketsphinx/include/cmdln_macro.h a little
;;;       more than it probably should.

(defclass config-option ()
  ;; TODO: Document
  ((name
    :initarg :name
    :reader name)
   (value-type
    :initarg :value-type
    :reader value-type)
   (value
    :initarg value
    :initform :none
    :reader value)
   (default-value
    :initarg :default-value
    :reader default-value)
   (description
    :initarg :description
    :reader description)))

(defparameter +cmdln-macro-header-path+ "../../../include/cmdln_macro.h")

(defun char-list->string (list)
  (check-type list list)
  (check-type (car list) character)
  (reduce #'(lambda (s c) (concatenate 'string s c)) list :key #'string))

(defun parse-cmdln-macro-option (stream)
  ;; TODO: See about using `CL-PPCRE' to make this "simpler".
  ;; HACK: This is really brittle, and should be remade using either
  ;;       regexes as said above, or using a full parser just for
  ;;       safety's sake.
  ;;
  ;; The expected form of the option looks like:
  ;;
  ;; { "-mfclogdir",                                    \
  ;; ARG_STRING,                                \
  ;; NULL,                                      \
  ;; "Directory to log feature files to"        \
  ;; },                                         \
  ;;
  ;; Or it could have a string where "NULL" is. There may or may not
  ;; be comma after the closing brace.
  ;;
  ;; First discard the garbage before the open brace.
  (assert (input-stream-p stream) (stream)
	  "Value ~A given as `STREAM' is not an input-stream."
	  stream)
  (loop
     for peek = (peek-char t stream)
     when (char= peek #\{)
       return nil
     do
       (read-char stream))
  ;; Now, read the braces and their contents into a string
  (let* ((option-string (char-list->string (append (loop
						      for char = (read-char stream)
						      until (char= char #\})
						      collect char)
						   (list #\}))))
	 ;; Locate the braces, and the starting and ending indices of
	 ;; each value.  Note that we discard the enclosing quotes
	 ;; for string.
	 (open-brace-index (position #\{ option-string))
	 (name-start-index (1+ (position #\" option-string :start (1+ open-brace-index))))
	 (name-end-index (position #\" option-string :start (1+ name-start-index)))
	 ;; We ignore the "REQ" in "REQARG_STRING" types (for
	 ;; example); it gets returned as "ARG_STRING".
	 (type-start-index (position #\A option-string :start (1+ name-end-index)))
	 (type-end-index (position #\, option-string :start (1+ type-start-index)))
	 ;; Here we check if "N" appears outside of a double quote, in
	 ;; which case we know that the default value is NULL.
	 (next-n-position (position #\N option-string :start (1+ type-end-index)))
	 (next-double-quote-position (position #\" option-string :start (1+ type-end-index)))
	 (default-value-is-null-p (< next-n-position next-double-quote-position))
	 ;; If it's not NULL, then we skip the opening double-quote.
	 (default-value-start-index (if default-value-is-null-p
					next-n-position
					(1+ next-double-quote-position)))
	 ;; We take everything up until the next comma if it's NULL,
	 ;; otherwise, take up till the closing quote.
	 (default-value-end-delimiter (if default-value-is-null-p
					  #\,
					  #\"))
	 (default-value-end-index  (position default-value-end-delimiter option-string
					     :start (1+ default-value-start-index)))
	 (description-start-index (1+ (position #\" option-string
						:start (1+ default-value-end-index))))
	 (description-end-index (position #\" option-string
					  :start (1+ description-start-index)))
	 (close-brace-index (position #\} option-string
				      :start (1+ description-end-index))))
    ;; Now we return a list of the values and braces (in order), each
    ;; as a string.
    (list (string (elt option-string open-brace-index))
	  (subseq option-string name-start-index name-end-index)
	  (subseq option-string type-start-index type-end-index)
	  (subseq option-string default-value-start-index default-value-end-index)
	  (subseq option-string description-start-index description-end-index)
	  (string (elt option-string close-brace-index)))))

(defun read-option-name (option-name-string)
  "Return a keyword representing the name contained in
`OPTION-NAME-STRING'."
  (check-type option-name-string string)
  (option-name-string->keyword option-name-string))

(defun read-option-type (option-type-string)
  "Return a type specifier corresponding to the value of
`OPTION-TYPE-STRING'.
\"ARG_STRING\"                                     -> `STRING'
\"ARG_INT32\", \"ARG_INT64\", \"ARG_INTEGER\"      -> `INTEGER'
\"ARG_FLOAT64\", \"ARG_FLOAT32\", \"ARG_FLOATING\" -> `FLOAT'
\"ARG_BOOLEAN\"                                    -> `BOOLEAN'"
  (check-type option-type-string string)
  (cond
    ((string= option-type-string "ARG_STRING")
     'string)
    ((or (string= option-type-string "ARG_INT32")
	 (string= option-type-string "ARG_INT64")
	 (string= option-type-string "ARG_INTEGER"))
     'integer)
    ((or (string= option-type-string "ARG_FLOAT32")
	 (string= option-type-string "ARG_FLOAT64")
	 (string= option-type-string "ARG_FLOATING"))
     'float)
    ((string= option-type-string "ARG_BOOLEAN")
     'boolean)
    (t
     (error "~A is a bad value for `OPTION-TYPE-STRING'.  Check the
     docstring for valid values." option-type-string))))

(defun read-option-value (option-value-string option-type-specifier)
  "Return the value contained in the string `OPTION-VALUE-STRING',
according to `OPTION-TYPE-SPECIFIER'.  OPTION-TYPE-SPECIFIER is a Lisp
type specifier symbol, and one of `STRING', `INTEGER', `FLOAT', or
`BOOLEAN'. Might also return `:NULL', if OPTION-TYPE-SPECIFIER is
STRING and OPTION-VALUE-STRING is \"NULL\"."
  (check-type option-value-string string)
  (check-type option-type-specifier symbol)
  (cond
    ((string= option-type-specifier 'string)
     (if (string= option-value-string "NULL")
	 :null
	 option-value-string))
    ((or (string= option-type-specifier 'integer)
	 (string= option-type-specifier 'float))
     (read-from-string option-value-string))
    ((string= option-type-specifier 'boolean)
     (cond
       ((string= option-value-string "yes")
	t)
       ((string= option-value-string "no")
	nil)
       (t
	(error "Expected either \"yes\" or \"no\", for
	       `OPTION-VALUE-STRING' of type `BOOLEAN' but got ~A."
	       option-value-string))))))

(defun read-option-description (option-description-string)
  (check-type option-description-string string)
  option-description-string)

(defun keyword= (&rest objects)
  "Return `T' if `OBJECTS' are of type `KEYWORD' and are `STRING=' to
each other; `NIL' otherwise."
  (and (every #'keywordp objects)
       (every #'(lambda (x) (string= (car objects) x)) objects)))

(defun read-option (parsed-option)
  (check-type parsed-option list)
  (assert (= (length parsed-option) 6)
	  (parsed-option)
	   "Expected argument `PARSED-OPTION' to have a length of 6;
length is ~A instead."
	   (length parsed-option))
  (assert (and (string= (first parsed-option) "{")
	       (string= (lastcar parsed-option) "}")
	       (stringp (second parsed-option))
	       (stringp (third parsed-option))
	       (or (stringp (fourth parsed-option))
		   (keyword= (fourth parsed-option) :null))
	       (stringp (fifth parsed-option)))
	  (parsed-option)
	  "Expected argument `PARSED-OPTION' to be a list of
	  strings (with the exception of the fourth element, which can
	  be the keyword `:NULL' instead), with first element \"{\"
	  and last element \"}\"; got ~A instead."  parsed-option)
  (let* ((option-name (read-option-name (second parsed-option)))
	 (option-type (read-option-type (third parsed-option)))
	 (option-value (read-option-value (fourth parsed-option) option-type))
	 (option-description (read-option-description (fifth parsed-option))))
    (make-instance 'config-option
		   :name option-name
		   :value-type option-type
		   :default-value option-value
		   :description option-description)))

(defun option-name-string->keyword (option-name-string)
  (check-type option-name-string string)
  (assert (and (> (length option-name-string) 0)
	       (char= (char option-name-string 0) #\-)))
  (let ((sanitized-option-name-string (subseq (substitute-if #\-
							     #'(lambda (c) (char= c #\_))
							     (string-upcase option-name-string))
					      1)))
    (values (intern sanitized-option-name-string "KEYWORD"))))

(defun option-keyword->name-string (name)
  (check-type name keyword)
  (concatenate 'string "-" (substitute-if #\_
					  #'(lambda (c) (char= c #\-))
					  (string-downcase (string name)))))

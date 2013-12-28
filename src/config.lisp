(cl:in-package :ps)

;;; CONSTANT DEFINITIONS ;;;
;; Path to pocketsphinx/include/cmdln_macro.h
;; TODO: Hook into autotools or something...
(defparameter +cmdln-macro-header-path+ "/usr/local/include/pocketsphinx/cmdln_macro.h")

;;; CLASS DEFINITIONS ;;;
;; CONFIG
(defclass config ()
  ;; TODO: Document
  ;; TODO: In the initialize-instance method, make sure to check for
  ;;       null pointers!
  ((config-ptr
    :initarg :config-ptr
    :initform (cffi:null-pointer))
   (options-hash
    :initarg :options-hash
    :initform nil
    ;; This wont be a reader in the final, just making it quick
    :reader options-hash)))

;; OPTION
(defclass option ()
  ((name
    :initarg :name)
   (value-type
    :initarg :value-type
    :reader value-type)
   (default-value
    :initarg :default-value
    :reader default-value)
   (description
    :initarg :description
    :reader description)))

(defclass default-option (option) ())

;; TODO: Test to make sure VALUE works as expected.
(defclass user-option (option)
  ((value
    :initarg :value
    :initform :none
    :reader value)))

;;; OBJECT INITIALIZATION DEFINITIONS ;;;
;; CONFIG
(defmethod initialize-instance :after ((config config) &key)
  ;; TODO: Need to fix the asserts so that they check the actual
  ;;       keyword arguments passed, not just the values of the
  ;;       `CONFIG' slots.
  (with-slots (config-ptr options-hash) config
    (assert (cffi:pointerp config-ptr)
	    (config-ptr)
	    "`CONFIG-PTR' must be a type of `CFFI' `POINTER' (as
determined by `CFFI:POINTERP'), not ~A."
	    (type-of config-ptr))
    (check-type options-hash (or null hash-table))
    (assert (alexandria:xor (cffi:null-pointer-p config-ptr)
			    (null options-hash))
	    ()
	    "You cannot initialize a `CONFIG' without providing
_exactly_ one of the `:INITARGS' `:CONFIG-PTR' or `:OPTIONS-HASH'.")
    (cond
      ((cffi:null-pointer-p config-ptr)
       (setf config-ptr (ps-sys:make-default-config-ptr))
       (synchronize-foreign-values config))
      ((null options-hash)
       (assert nil
	       ()
	       "Initializing a `CONFIG' using `:INITARG' `:CONFIG-PTR'
       has not been implemented yet.")))))

;;; CLASS ACCESSORS ;;;
;; OPTION
(defmethod name ((option option) &key as-string)
  (with-slots (name) option
    (if as-string
	(option-keyword->name-string name)
	name)))

;; `USER-OPTION' objects override this. 
(defmethod value ((option option))
  (default-value option))

;;; LISP/C SYNCHRONIZATION GENERICS ;;;
;; CONFIG
(defgeneric synchronize-foreign-values (config &rest name-keywords)
  (:documentation "Update all foreign values in the `CONFIG-PTR' slot
  of `CONFIG' indicated by `NAME-KEYWORDS' so that they are equivalent
  to the `VALUE' of the corresponding `OPTION' of `CONFIG'. If only
  `T' is passed as `NAME-KEYWORDS' then update all foreign values so
  they are equivalent to the value of the corresponding `OPTION' of
  `CONFIG', by iterating over the `OPTION's."))

(defmethod synchronize-foreign-values ((config config) &rest name-keywords)
  (assert (or (every #'keywordp name-keywords)
	      (equalp name-keywords (list t)))
	  (name-keyword)
	  "Expected only keywords or the singleton `T' for
`NAME-KEYWORDS', got ~A instead."
	  name-keywords)
  (with-slots (config-ptr options-hash) config
    (if (equalp name-keywords (list t))
	(loop
	   for option being the hash-values in options-hash
	   do
	     (synchronize-foreign-value option config-ptr))
	(loop
	   for name-keyword in name-keywords
	   for option = (gethash name-keyword options-hash)
	   do
	     (synchronize-foreign-value option config-ptr)))))

;; OPTION
(defgeneric synchronize-foreign-value (option config-ptr &key)
  (:documentation "Update the foreign value in `CONFIG-PTR' to that in
  the corresponding given `OPTION'."))

(defmethod synchronize-foreign-value ((option option) config-ptr &key)
  (let ((name-string (name option :as-string t)))
    (assert (cffi:pointerp config-ptr))
    (assert (ps-sys:cmd-ln-exists-r config-ptr name-string))
    (with-accessors ((value-type value-type)
		     (value value)) option
      (cond
	((string= value-type 'string)
	 (ps-sys:cmd-ln-set-str-r config-ptr name-string value))
	((string= value-type 'integer)
	 (ps-sys:cmd-ln-set-int-r config-ptr name-string value))
	((string= value-type 'float)
	 (ps-sys:cmd-ln-set-float-r config-ptr name-string value))
	((string= value-type 'boolean)
	 (ps-sys:cmd-ln-set-boolean-r config-ptr name-string value))
	(t
	 (assert nil
		 (value-type)
		 "Expected one of 'STRING, 'INTEGER, 'FLOAT, or
		 'BOOLEAN for (VALUE-TYPE CONFIG), but got ~A."
		 value-type))))))

(defun option-name-string->keyword (option-name-string)
  (check-type option-name-string string)
  (assert (and (plusp (length option-name-string))
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

;;;; C CMDLN HEADER PARSING
;; FIXME: NULL is being returned as a string NULL from parse. Fix
;;        this.
(defun parse-cmdln-macro-option (stream)
  ;; TODO: This function is fucking huge. I need to split it up into
  ;;       smaller ones.
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
  (assert (input-stream-p stream)
	  (stream)
	  "Value ~A given as `STREAM' is not an input-stream."
	  stream)
  ;; First discard the garbage before the open brace.
  (loop
     for peek = (peek-char t stream)
     when (char= peek #\{) return nil
     do
       (read-char stream))
  ;; Now, read the braces and their contents into a string
  (let ((option-string (ps-utils:char-list->string (append (loop
							      for char = (read-char stream)
							      until (char= char #\})
							      collect char)
							   (list #\})))))
    (when (string= option-string "{ NULL, 0, NULL, NULL }")
      (error 'end-of-file :stream stream))
    (let*
	;; Locate the braces, and the starting and ending indices of
	;; each value.  Note that we discard the enclosing quotes
	;; for string.
	((open-brace-index (position #\{ option-string))
	 (name-start-index (1+ (position #\" option-string :start (1+ open-brace-index))))
	 (name-end-index (position #\" option-string :start (1+ name-start-index)))
	 ;; HACK: We ignore the "REQ" in "REQARG_STRING" types (for
	 ;;       example); it gets returned as "ARG_STRING".
	 (type-start-index (position #\A option-string :start (1+ name-end-index)))
	 (type-end-index (position #\, option-string :start (1+ type-start-index)))
	 ;; Here we check if "N" appears outside of a double quote, in
	 ;; which case we know that the default value is NULL.
	 (next-n-position (position #\N option-string :start (1+ type-end-index)))
	 (next-double-quote-position (position #\" option-string :start (1+ type-end-index)))
	 (default-value-is-null-p (when next-n-position (< next-n-position next-double-quote-position)))
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
	    (string (elt option-string close-brace-index))))))

(defun read-default-option (parsed-option)
  (check-type parsed-option list)
  (assert (= (length parsed-option) 6)
	  (parsed-option)
	  "Expected argument `PARSED-OPTION' to have a length of 6;
length is ~A instead."
	  (length parsed-option))
  (assert (and (string= (first parsed-option) "{")
	       (string= (alexandria:lastcar parsed-option) "}")
	       (stringp (second parsed-option))
	       (stringp (third parsed-option))
	       (or (stringp (fourth parsed-option))
		   (ps-utils:keyword= (fourth parsed-option) :null))
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
    (make-instance 'default-option
		   :name option-name
		   :value-type option-type
		   :default-value option-value
		   :description option-description)))

(defun read-option-name (option-name-string)
  "Return a keyword representing the name contained in
`OPTION-NAME-STRING'."
  (check-type option-name-string string)
  (option-name-string->keyword option-name-string))

(defun read-option-type (option-type-string)
  ;; FIXME: If `PARSE-CMDLN-MACRO-OPTION' is changed to return
  ;;        REQARG_STRING and friends instead of striping off the REQ,
  ;;        then this function will fail and will need to be changed!
  ;;        Should probably just fix it now, but I'm busy! ;_;
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
     (assert nil
	     (option-type-string)
	     "~A is a bad value for `OPTION-TYPE-STRING'.  Check the
docstring for valid values."
	     option-type-string))))

(defun read-option-value (option-value-string option-type-specifier)
  "Return the value contained in the string `OPTION-VALUE-STRING',
according to `OPTION-TYPE-SPECIFIER'.  `OPTION-TYPE-SPECIFIER' is a Lisp
type specifier symbol, and one of `STRING', `INTEGER', `FLOAT', or
`BOOLEAN'. Might also return `:NULL', if `OPTION-TYPE-SPECIFIER' is
`STRING' and OPTION-VALUE-STRING is `:NULL'."
  (check-type option-value-string (or string keyword))
  (check-type option-type-specifier symbol)
  (cond
    ((string= option-type-specifier 'string)
     option-value-string)
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
	(assert nil
		(option-value-string)
		"Expected either \"yes\" or \"no\", for
	       `OPTION-VALUE-STRING' of type `BOOLEAN' but got ~A."
		option-value-string))))
    (t
     (assert nil
	     (option-type-specifier)
	     "Expected one of `STRING', `INTEGER', `FLOAT', or
     `BOOLEAN' for `OPTION-TYPE-SPECIFIER', but got ~A instead."
     option-type-specifier))))

(defun read-option-description (option-description-string)
  (check-type option-description-string string)
  option-description-string)

(defun make-default-options-hash-table ()
  (with-open-file (header-stream +cmdln-macro-header-path+)
    (loop
       with default-options-hash = (make-hash-table)
       for default-option = (handler-case
				 (read-default-option (parse-cmdln-macro-option header-stream))
			       (end-of-file (e) nil))
       while default-option do
         (setf (gethash (name default-option) default-options-hash) default-option)
       finally
         (return default-options-hash))))

(defun make-default-config ()
  (make-instance 'config
		 :options-hash
		 (make-default-options-hash-table)))


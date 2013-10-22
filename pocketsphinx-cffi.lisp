;; TODO: ASK ABOUT WTF IS GOING ON AT LINE 51 OF ps_decoder.i!

(cl:eval-when (:compile-toplevel :load-toplevel)
  (cl:unless (cl:fboundp 'swig-lispify)
    (cl:defun pocketsphinx-lispify (name flag cl:&optional (package cl:*package*))
      (cl:labels ((helper (lst last rest cl:&aux (c (cl:car lst)))
                    (cl:cond
                      ((cl:null lst)
                       rest)
                      ((cl:upper-case-p c)
                       (helper (cl:cdr lst) 'upper
                               (cl:case last
                                 ((lower digit) (cl:list* c #\- rest))
                                 (cl:t (cl:cons c rest)))))
                      ((cl:lower-case-p c)
                       (helper (cl:cdr lst) 'lower (cl:cons (cl:char-upcase c) rest)))
                      ((cl:digit-char-p c)
                       (helper (cl:cdr lst) 'digit 
                               (cl:case last
                                 ((upper lower) (cl:list* c #\- rest))
                                 (cl:t (cl:cons c rest)))))
                      ((cl:char-equal c #\_)
                       (helper (cl:cdr lst) '_ (cl:cons #\- rest)))
                      (cl:t
                       (cl:error "Invalid character: ~A" c))))
		  (without-prefix (prefix name)
		    (cl:when (cl:string-equal prefix name
					      :end2 (cl:length prefix))
		      (cl:subseq name (cl:length prefix)))))
        (cl:let ((fix (cl:case flag
                        ((constant enumvalue) "+")
                        (variable "*")
                        (cl:t "")))
		 (postfix (cl:case flag
			    (classname "-cstruct")
			    (cl:t "")))
		 (name (without-prefix "ps_" name)))
          (cl:intern
           (cl:concatenate
            'cl:string
            fix
            (cl:nreverse (helper (cl:concatenate 'cl:list name) cl:nil cl:nil))
	    postfix
            fix)
           package))))))

(defun cfunction-p (cffi-form)
  "Return non-NIL if cffi-form is a `CFFI:DEFCFUN' form, else return NIL."
  (equal (first cffi-form) 'cffi:defcfun))

(defun cfunction-name (defcfun-form)
  "Return the name of the symbol (as a STRING) which will be used to
reference the foreign function defined in `DEFCFUN-FORM'. Signal an
error if `DEFCFUN-FORM' is not a `CFFI:DEFCFUN' form."
  (unless (cfunction-p defcfun-form)
    (error "`CFUNCTION-NAME' called on a form which is not a
    `CFFI:DEFCFUN' form."))
  (symbol-name (cadr (second defcfun-form))))

(defun method-p (cffi-form first-argument)
  "Return non-NIL if `CFFI-FORM' is a `CFFI:DEFCFUN' form and the
first argument to the foreign function is `EQUAL' to the form given as
`FIRST-ARGUMENT', else return NIL."
  (and (cfunction-p cffi-form)
       (equal (fourth cffi-form) first-argument)))

(defun decoder-method-p (cffi-form)
  "Return non-NIL if `CFFI-FORM' is a method on a Pocketsphinx
Decoder, else return NIL."
  (method-p cffi-form '(ps :decoder)))

(defun decoder-getter-p (cffi-form)
  "Return non-NIL if `CFFI-FORM' is Decoder method, and which is
referenced through a Lisp symbol whose `SYMBOL-NAME' begins with
\"GET\", else return NIL."
  (let ((method-name (cfunction-name cffi-form)))
    (and (decoder-method-p cffi-form)
	 (>= (length method-name) (length "GET"))
	 (equal (subseq method-name 0 3) "GET"))))

(defun decoder-setter-p (cffi-form)
  "Return non-NIL if `CFFI-FORM' is Decoder method, and which is
referenced through a Lisp symbol whose `SYMBOL-NAME' begins with
\"UPDATE\", else return NIL."
  (let ((method-name (cfunction-name cffi-form)))
    (and (decoder-method-p cffi-form)
	 (>= (length method-name) (length "UPDATE"))
	 (equal (subseq method-name 0 6) "UPDATE"))))

(defun segment-method-p (cffi-form)
  "Return non-NIL if `CFFI-FORM' is a method on a Pocketsphinx
Segment, else return NIL."
  (method-p cffi-form '(seg :pointer)))

(defun nbest-method-p (cffi-form)
  "Return non-NIL if `CFFI-FORM' is a method of a Pocketsphinx NBest,
else return NIL."
  (method-p cffi-form '(nbest :pointer)))

(defun symbol-without-prefix (symbol prefix)
  "Return a new symbol formed by removing the prefix designated by the
symbol `PREFIX' from the symbol `SYMBOL'."
  (let ((symbol-name-length (length (symbol-name symbol)))
	(prefix-name-length (length (symbol-name prefix)))
	(symbol-name-string (symbol-name symbol))
	(prefix-name-string (symbol-name prefix)))
    (if (and (>= symbol-name-length prefix-name-length)
	     (equal (subseq symbol-name-string 0 prefix-name-length)
		    prefix-name-string))
	(intern (subseq symbol-name-string prefix-name-length))
	;; else
	symbol)))

(defun n-cfunction-arguments (defcfun-form)
  ;; Tested and seems to work.
  "Return the number of arguments required by the foreign function
defined by `DEFCFUN-FORM'. Signal an error if DEFCFUN-FORM is not a
`CFFI:DEFCFUN' form."
  (unless (cfunction-p defcfun-form)
    (error "`N-CFUNCTION-ARGUMENTS' called on a form which is not a
    `CFFI:DEFCFUN' form."))
  (length (cdddr defcfun-form)))

(defun cstruct-p (cffi-form)
  "Return non-NIL if `CFFI-FORM' is a `CFFI:DEFCSTRUCT' form, else
return NIL."
  (equal (first cffi-form) 'cffi:defcstruct))

(defun cstruct-name (defcstruct-form)
  "Return the name of the symbol (as a string) which will be used to
reference the foreign struct defined in `DEFCSTRUCT-FORM'.  Signal an
error if `DEFCSTRUCT-FORM' is not a `CFFI:DEFCSTRUCT' form."
  (unless (cstruct-p defcstruct-form)
    (error "`CSTRUCT-NAME' called on an argument which is not a
    `CFFI:DEFCSTRUCT' form."))
  (symbol-name (second defcstruct-form)))

(defun cstruct-slots (defcstruct-form)
  "Return a list of slot-identifiers for `DEFCSTRUCT-FORM', where each
slot-identifier has the form (SLOT-NAME-SYMBOL :CFFI-SLOT-TYPE).
Signal an error if `DEFCSTRUCT-FORM' is not a `CFFI:DEFCSTRUCT' form."
  (unless (cstruct-p defcstruct-form)
    (error "`CSTRUCT-SLOTS' called on an argument which is not a
    `CFFI:DEFCSTRUCT' form."))
  (cddr defcstruct-form))

(defun parse-cffi-output (filename)
  (with-open-file
      (cffi-file filename :direction :input)
    (loop
       for form = (read cffi-file nil :eof-error)
       while (not (equal form :eof-error))
	 ;; Find and sort through all the functions.
         if (decoder-method-p form)
           if (decoder-getter-p form)
             collect form into decoder-getters
           else if (decoder-setter-p form)
             collect form into decoder-setters
           else
             collect form into decoder-other-methods
         else if (and (cfunction-p form)
		      (equal (cfunction-name form) "INIT"))
           collect form into decoder-initializer
       
         else if (segment-method-p form)
           collect form into segment-methods

         else if (nbest-method-p form)
           collect form into nbest-methods

	 else if (and (cfunction-p form)
		      (equal (cfunction-name form) "ARGS"))
	   collect form into args-function

	 else if (cfunction-p form)
	   collect form into other-functions

	 ;; Collect all the cstructures
	 else if (cstruct-p form)
	   collect form into cstructs
       
       finally
         (return (list decoder-getters decoder-setters
		       decoder-other-methods decoder-initializer
		       segment-methods nbest-methods args-function
		       other-functions cstructs)))))

(defmacro define-hypothesis-class (hypothesis-cstruct-form)

  '(defclass hypothesis ()
     (hypothesis-cstruct))

  (defmethod initialize-instance :after ((ps-hypothesis hypothesis))))

;; TODO: Send and email to CFFI guys with a diff for including how to
;; actually use the :class keyword for `CFFI:DEFCSTRUCT'. It was
;; fucking ridiculous to figure out. It looks like:
;; (cffi:defcstruct ("struct-name" :class class-name)
;;   (slot-name slot-value))
;; 
;; TODO: fix minor typographical error in
;; sphinxbase/src/libsphinxbase/util/cmd_ln.c on line 755: an extra
;; space between `*' and `defn' exists.
(defmacro define-argument-class (arg-t-cstruct-form)

  '(defclass argument ()
    (argument-cstruct
     :initarg argument-cstruct
     :initform (error "Must provide a default argument for
     argument-cstruct")))


(defmacro define-config-class (config-cstruct-form)

  '(defclass config ()
     (config-cstruct))

  '(defmethod initialize-instance :after ((ps-config config)
					  &rest keyword-string-pairs)
    
    
    
    

(defmacro define-decoder-class (decoder-getters decoder-setters
				decoder-other-methods
				decoder-initializer)
  '(defclass decoder ()
    ((decoder-foreign-pointer
      :documentation "Foreign pointer to the C decoder object.")))

  '(defmethod initialize-instance :after ((ps-decoder decoder) (decoder-config config))
    (with-slots (decoder-foreign-pointer) decoder
      (setf decoder-foreign-pointer (init decoder-config))))
     
  (loop
     for defcfun-form in decoder-getters
     and defcfun-name = (cfunction-name defcfun-form)
     and getter-method-name = (symbol-without-prefix defcfun-name 'get-)
       ;; Some decoder 'get-' defcfuns require more than one argument
       ;; (ie, `GET-HYP' and friends).
       ;; 
       ;; Note however, that the last two arguments to the original C
       ;; function ps_get_hyp are actually passed as pointers for
       ;; _output_ purposes.
       ;; 
       ;; FIXME: Make sure to deal with the special cases for > 1
       ;;        arguments.
     (when (= (n-cfunction-arguments defcfun-form) 1)
       ;; Evaluate the definition of the defcfun-form...
       defcfun-form
       ;; then define a method using it...
       `(defmethod ,getter-method-name ((ps-decoder decoder))
	  (with-slots ((decoder-foreign-pointer)) ps-decoder
	    (,defcfun-name decoder-foreign-pointer)))
       ;; and finally export the method.
       `(export ,getter-method-name))))

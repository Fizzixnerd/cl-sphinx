(cl:in-package :ps)

(defclass config ()
  ;; TODO: Document
  ;; TODO: In the initialize-instance method, make sure to check for
  ;;       null pointers!
  ;; cmd-ln-t
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
	(integer (cmd-ln-int-r config-ptr config-string))
	(float (cmd-ln-float-r config-ptr config-string))
	(boolean (cmd-ln-boolean-r config-ptr config-string))
	(string (cmd-ln-str-r config-ptr config-string))
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

(defun config-string->keyword (name)
  (check-type name string)
  (assert (and (> (length name) 0)
	       (char= (char name 0) #\-)))
  (let ((sanitized-name (subseq (substitute-if #\-
					       #'(lambda (c) (char= c #\_))
					       (string-upcase name))
				1)))
    (values (intern sanitized-name "KEYWORD"))))

(defun config-keyword->string (name)
  (check-type name keyword)
  (concatenate 'string "-" (substitute-if #\_
					  #'(lambda (c) (char= c #\-))
					  (string-downcase (string name)))))
(defun split-on-whitespace (string)
  ;; TODO: Should change the call site, not add a null specifier as a
  ;;       hack.
  (check-type string (or string null))
  (ppcre:split "\\s+" string))

(defun parse-config-value-type (string)
  "Return the type of the option value contained in string."
  (check-type string string)
  (let ((config-value (read-from-string string)))
    (cond ((integerp config-value) 'integer)
	  ((floatp config-value) 'float)
	  ((or (string= config-value 'yes)
	       (string= config-value 'no))
	   'boolean)
	  (t 'string))))

(defun list-config-keywords ()
  (with-open-file (config-file "src/config.txt")
    (loop
       initially
       ;; Read the header and discard it.
	 (read-line config-file nil)
       for split-line = (split-on-whitespace (read-line config-file nil))
       for config-value-string = (when split-line (first split-line))
       for config-value-type = (cond ((= (length split-line) 2)
				      (parse-config-value-type (second split-line)))
				     ((= (length split-line) 3)
				      (parse-config-value-type (third split-line)))
				     (t nil))
       while split-line
       when split-line
       collect (cons (config-string->keyword config-value-string)
		     config-value-type))))

(defparameter +config-keywords-type-alist+ (list-config-keywords))
(defparameter +config-type-hash+ (alexandria:alist-hash-table +config-keywords-type-alist+))

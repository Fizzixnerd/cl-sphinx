;; Parse the options in options.txt which is obtained from
;; cmd_ln_print_help_r() from sphinxbase, declared in cmd_ln.h.
;; File created 25 October 2013
;; Author: Matt Walker (matt.g.d.walker@gmail.com)

(require 'cl-ppcre)
;; FIXME: 
;; TODO: MOVE THIS TO THE PS PACKAGE OR DIE!!
;;       Probably need to put this under an `EVAL-WHEN' or something
;;       too...

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
  (check-type string string)
  "Return the type of the option value contained in string."
  (let ((config-value (read-from-string string)))
    (cond ((integerp config-value) 'integer)
	  ((floatp config-value) 'float)
	  ((or (string= config-value 'yes)
	       (string= config-value 'no))
	   'boolean)
	  (t 'string))))
	  
(defun list-config-keywords ()
  (with-open-file (config-file "config.txt")
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
(defparameter +config-type-hash+ (alist-hash-table +config-keywords-type-alist+))

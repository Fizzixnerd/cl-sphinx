(asdf:load-system :cl-pocketsphinx)

(cl:defpackage :ps-user
  (:use #:cl
	#:ps))

(cl:in-package :ps-user)

;; This generates a default config based on the default options giving
;; in the file `+CMDLN-MACRO-HEADER-PATH+', defined in config.lisp
;; (which in this case is
;; "/usr/local/include/pocketsphinx/cmdln_macro.h").
(defparameter *config* (make-default-config))
(defparameter *decoder* (make-instance 'decoder
				       :config *config*))

;; output of shell command 'pkg-config --variable=modeldir pocketsphinx'
(defparameter *model-dir* "/usr/local/share/pocketsphinx/model")
(defparameter *file-to-be-decoded* "../data/test_file.raw")

;; The C file opening will be wrapped in a method, or otherwise made
;; sane.  Will also UNWIND-PROTECT it, obviously.  Will probably end
;; up just reading it into an int16 array and passing it to
;; ps_process_raw().
(defparameter *opened-file* (ps-sys:fopen *file-to-be-decoded* "rb"))

;; This will be made nicer by making it automatically change class
;; when you set value, or by just making there be only one option
;; class.
(let ((options-hash (options-hash (config *decoder*))))
  (change-class (gethash :hmm options-hash) 'user-option
		:value (concatenate 'string *model-dir* "/hmm/en_US/hub4wsj_sc_8k"))
  (change-class (gethash :lm options-hash) 'user-option
		:value (concatenate 'string *model-dir* "/lm/en_US/hub4.5000.DMP"))
  (change-class (gethash :dict options-hash) 'user-option
		:value (concatenate 'string *model-dir* "/lm/en_US/hub4.5000.dic")))

;; This will be automated in the (SETF (VALUE OPTION)) or whatever
(synchronize-foreign-values (config *decoder*) :lm :hmm :dict)

;; This doesn't have a method yet, because I have been working on
;; configs.  Ho-hum.  Now that they are pretty much working now
;; though, I can move onto the more interesting nbest structures and
;; so on.
(with-slots (decoder-ptr) *decoder*
  (if (minusp (ps-sys:ps-decode-raw decoder-ptr *opened-file* "testfile" -1))
    (print "Something bad happened when decoding!")
    ;; This should (read: might) output the nonsense string "you know
    ;; is the biggest circle i know if that", which is pocketsphinx's
    ;; rather impressive attempt to decode me saying "Yellow is the
    ;; biggest circle I know" (which is the first thing that came to
    ;; my mind when recording a clip for testing), using only the
    ;; default language and hidden markov models and default
    ;; dictionary in the svn tree, using a shitty netbook microphone
    ;; with a poor signal-to-noise ratio (and probably too little
    ;; gain).  Cool!
    (format t "Best guess is: ~A~%" (hyp-string (best-hypothesis *decoder*)))))

(ps-sys:fclose *opened-file*)

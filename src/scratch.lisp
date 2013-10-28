
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

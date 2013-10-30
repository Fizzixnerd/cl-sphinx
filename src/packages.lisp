(cl:defpackage :pocketsphinx-utils
  (:nicknames #:ps-utils)
  (:use #:cl
	#:cffi
	#:alexandria)
  (:shadow #:defcfun
	   #:foreign-funcall)
  (:export #:defcfun
	   #:config-option
	   #:read-option-name
	   #:read-option-type
	   #:read-option-value
	   #:read-option-description
	   #:option-name-string->keyword
	   #:option-keyword->name-string))

(cl:defpackage :pocketsphinx-sys
  (:nicknames #:ps-sys)
  (:shadowing-import-from #:pocketsphinx-utils #:defcfun)
  (:use #:cl
	#:pocketsphinx-utils
	#:cffi)
  (:export #:libpocketsphinx
	   #:libsphinxbase

	   #:ps-decoder-t
	   #:cmd-ln-t
	   #:arg-t
	   #:file
	   #:str-array
	   
	   #:fopen
	   #:fclose

	   #:cmd-ln-init
	   #:cmd-ln-str-r
	   #:cmd-ln-str-list-r
	   #:cmd-ln-int-r
	   #:cmd-ln-float-r
	   #:cmd-ln-boolean-r
	   #:cmd-ln-set-str-r
	   #:cmd-ln-set-int-r
	   #:cmd-ln-set-float-r
	   #:cmd-ln-set-boolean-r
	   #:cmd-ln-exists-r
	   #:cmd-ln-print-help-r

	   #:ps-args
	   #:ps-init
	   #:ps-free
	   #:ps-decode-raw
	   #:ps-get-hyp
	   #:ps-get-config))

(cl:defpackage :pocketsphinx
  (:nicknames #:ps)
  (:use #:cl
	#:pocketsphinx-sys
	#:cffi
	#:trivial-garbage
	#:cl-ppcre
	#:alexandria)
  (:export #:decoder
	   
	   #:config
	   #:option
	   
	   #:hypothesis
	   #:hyp-string
	   #:score
	   #:utterance-id
	   #:best-hypothesis))

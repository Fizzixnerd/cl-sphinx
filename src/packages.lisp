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
	   #:option-keyword->name-string
	   #:char-list->string
	   #:keyword=))

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
	   #:arg-type
	   
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
	   #:ps-get-config

	   #:make-default-config-ptr
	   #:make-default-config-pointer))

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
	   #:default-option
	   #:user-option
	   #:name
	   #:value-type
	   #:default-value
	   #:value
	   #:description
	   #:synchronize-foreign-values
	   #:make-default-config
	   
	   #:hypothesis
	   #:hyp-string
	   #:score
	   #:utterance-id
	   #:best-hypothesis))

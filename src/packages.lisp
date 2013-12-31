(cl:defpackage :pocketsphinx-utils
  (:nicknames #:ps-utils)
  (:use #:cl
	#:cffi
	#:alexandria)
  ;; TODO: is foreign-funcall still shadowed?
  (:shadow #:defcfun
	   #:foreign-funcall)
  ;; TODO: organize this.
  (:export #:defcfun
	   #:config-option
	   #:read-option-name
	   #:read-option-type
	   #:read-option-value
	   #:read-option-description
	   #:option-name-string->keyword
	   #:option-keyword->name-string
	   #:char-list->string
	   #:keyword=
	   #:iterator
	   #:next
	   #:value))


(cl:defpackage :pocketsphinx-posix
  (:nicknames #:ps-posix)
  (:shadowing-import-from #:pocketsphinx-utils #:defcfun)
  (:use #:cl
	#:pocketsphinx-utils
	#:cffi)
  (:export #:file
	   #:str-array
	   #:size-t

	   #:fopen
	   #:fclose))

(cl:defpackage :sphinxbase-sys
  ;; NOTE: can't use sb-sys as nickname because of SBCL using it.
  (:nicknames #:sbase-sys)
  (:shadowing-import-from #:pocketsphinx-utils #:defcfun)
  (:use #:cl
	#:pocketsphinx-utils
	#:ps-posix
	#:cffi)
  (:export #:libsphinxbase

	   #:cmd-ln-t
	   #:arg-t
	   #:arg-type

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
	   #:cmd-ln-print-help-r))

(cl:defpackage :pocketsphinx-sys
  (:nicknames #:ps-sys)
  (:shadowing-import-from #:pocketsphinx-utils #:defcfun)
  (:use #:cl
	#:pocketsphinx-utils
	#:ps-posix
	#:sphinxbase-sys
	#:cffi)
  (:export #:libpocketsphinx

	   #:ps-decoder-t
	   
	   #:ps-args
	   #:ps-init
	   #:ps-free
	   #:ps-start-utt
	   #:ps-end-utt
	   #:ps-get-uttid
	   #:ps-decode-raw
	   #:ps-process-raw
	   #:ps-get-hyp
	   #:ps-get-prob
	   #:ps-get-config
	   #:ps-get-lattice
	   #:ps-seg-iter
	   #:ps-seg-next
	   #:ps-seg-word
	   #:ps-seg-frames
	   #:ps-seg-prob
	   #:ps-seg-free
	   #:ps-nbest
	   #:ps-nbest-next
	   #:ps-nbest-hyp
	   #:ps-nbest-seg
	   #:ps-nbest-free
	   #:ps-get-utt-time
	   #:ps-get-all-time

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

(require "asdf")

(asdf:defsystem #:cl-pocketsphinx
  :name "cl-pocketsphinx"
  :version "0.1.0"
  :description "cl-pocketsphinx: Bindings for CMU PocketSphinx."
  :maintainer "Matt Walker"
  :author "Matt Walker <matt.g.d.walker@gmail.com>"
  :license "Simplified BSD License"
  :depends-on (#:cffi
	       #:alexandria
	       #:trivial-garbage
	       #:cl-ppcre)
  :components ((:module "src"
			:components ((:file "packages")
				     (:file "pocketsphinx-utils"
					    :depends-on ("packages"))
				     (:file "posix"
					    :depends-on ("packages"
							 "pocketsphinx-utils"))
				     (:file "sphinxbase-sys"
					    :depends-on ("packages"
							 "pocketsphinx-utils"
							 "posix"))
				     (:file "pocketsphinx-sys"
					    :depends-on ("packages"
							 "pocketsphinx-utils"
							 "sphinxbase-sys"
							 "posix"))
				     (:file "config"
					    :depends-on ("packages"
							 "pocketsphinx-utils"
							 "sphinxbase-sys"
							 "pocketsphinx-sys"))
				     (:file "hypothesis"
					    :depends-on ("packages"
							 "pocketsphinx-sys"))
				     (:file "decoder"
					    :depends-on ("packages"
							 "pocketsphinx-utils"
							 "sphinxbase-sys"
							 "pocketsphinx-sys"
							 "hypothesis"
							 "config"))
				     (:file "nbest"
					    :depends-on ("packages"
							 "pocketsphinx-utils"
							 "pocketsphinx-sys"
							 "hypothesis"))
				     (:file "segmentation"
					    :depends-on ("packages"
							 "pocketsphinx-utils"
							 "pocketsphinx-sys"))))))



(cl:in-package :sphinxbase-sys)

(define-foreign-library libsphinxbase
    (:unix (:or "libsphinxbase.so.1" "libsphinxbase.so"))
  (t (:default "libsphinxbase")))

(use-foreign-library libsphinxbase)

(defctype cmd-ln-t :pointer)
(defctype arg-t :pointer)
(defctype anytype-t :pointer)

(cffi:defcenum arg-type
    ;; FIXME: I put the literal integers here because CFFI was
    ;;        complaining about their type. This is neither ideal, nor
    ;;        likely portable.
  (:arg-required 1)
  (:arg-integer 2)
  (:arg-floating 4)
  (:arg-string 8)
  (:arg-boolean 16)
  (:arg-string-list 32)
  (:reqarg-integer 3)
  (:reqarg-floating 5)
  (:reqarg-string 9)
  (:reqarg-boolean 17))

;; TESTED
;; C library functions
(defcfun "fopen" file
  (pathname :string)
  (mode :string))

(defcfun "fclose" :void
  (filehandle file))

;; sphinxbase functions
(defcfun "cmd_ln_init" cmd-ln-t
  (inout-cmdln cmd-ln-t)
  (defn arg-t)
  (strict :boolean)
  &rest)

(defcfun "cmd_ln_free_r" :int
  (cmdln cmd-ln-t))

(defcfun "cmd_ln_access_r" anytype-t
  (cmdln cmd-ln-t)
  (name :string))

(defcfun "cmd_ln_str_r" :string
  (cmdln cmd-ln-t)
  (name :string))

(defcfun "cmd_ln_str_list_r" str-array
  (cmdln cmd-ln-t)
  (name :string))

(defcfun "cmd_ln_int_r" :long
  (cmdln cmd-ln-t)
  (name :string))

(defcfun "cmd_ln_float_r" :double
  (cmdln cmd-ln-t)
  (name :string))

;; Is set as a macro in cmd_ln.h
(defun cmd-ln-boolean-r (cmdln name)
  (not (zerop (cmd-ln-int-r cmdln name))))

(defcfun "cmd_ln_set_str_r" :void
  (cmdln cmd-ln-t)
  (name :string)
  (str :string))

(defcfun "cmd_ln_set_int_r" :void
  (cmdln cmd-ln-t)
  (name :string)
  (int :long))

(defcfun "cmd_ln_set_float_r" :void
  (cmdln cmd-ln-t)
  (name :string)
  (int :long))

(defun cmd-ln-set-boolean-r (cmdln name bool)
  (cmd-ln-set-int-r cmdln name (not (zerop bool))))

(defcfun "cmd_ln_exists_r" :boolean
  (cmdln cmd-ln-t)
  (name :string))

(defcfun "cmd_ln_print_help_r" :void
  (cmdln cmd-ln-t)
  (fp file)
  (defn arg-t))


(defun make-default-config-ptr ()
  (cmd-ln-init (cffi:null-pointer)
	       (ps-args)
	       t
	       :pointer (cffi:null-pointer)))

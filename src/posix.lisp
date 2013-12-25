(cl:in-package :ps-posix)

(defctype file :pointer)
(defctype str-array :pointer)

;; TESTED
;; C library functions
(defcfun "fopen" file
  (pathname :string)
  (mode :string))

(defcfun "fclose" :void
  (filehandle file))

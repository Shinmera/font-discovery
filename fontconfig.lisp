#|
 This file is a part of font-discovery
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.font-discovery)

(cffi:define-foreign-library fontconfig
  (T (:or "libfontconfig.so.1" "libfontconfig.so")))

(defvar +FAMILY+ "family")              ; String
(defvar +FILE+ "file")                  ; String
(defvar +SLANT+ "slant")                ; Int
(defvar +WEIGHT+ "weight")              ; Int
(defvar +SIZE+ "size")                  ; Double Range
(defvar +SPACING+ "spacing")            ; Int
(defvar +WIDTH+ "width")                ; Int

(cffi:defcenum weight
  (:thin 0)
  (:extra-light 40)
  (:light 50)
  (:semi-light 55)
  (:book 75)
  (:regular 80)
  (:medium 100)
  (:semi-bold 180)
  (:bold 200)
  (:extra-bold 205)
  (:black 210)
  (:extra-black 215))

(cffi:defcenum slant
  (:roman 0)
  (:italic 100)
  (:oblique 110))

(cffi:defcenum stretch
  (:ultra-condensed 50)
  (:extra-condensed 63)
  (:condensed 75)
  (:semi-condensed 87)
  (:normal 100)
  (:semi-expanded 113)
  (:expanded 125)
  (:extra-expanded 150)
  (:ultra-expanded 200))

(cffi:defcenum spacing
  (:proportional 0)
  (:dualspace 90)
  (:monospace 100)
  (:charcell 110))

(cffi:defcenum match-kind
  :pattern
  :font
  :scan)

(cffi:defcenum result
  :match
  :no-match
  :type-mismatch
  :no-id
  :out-of-memory)

(cffi:defcstruct (font-set :conc-name font-set-)
  (nfont :int)
  (sfont :int)
  (fonts :pointer))

(cffi:defcfun (fini "FcFini") :void)

(cffi:defcfun (init-load-config-and-fonts "FcInitLoadConfigAndFonts") :pointer)

(cffi:defcfun (config-destroy "FcConfigDestroy") :void
  (config :pointer))

(cffi:defcfun (create-pattern "FcPatternCreate") :pointer)

(cffi:defcfun (pattern-add-integer "FcPatternAddInteger") :bool
  (pattern :pointer)
  (object :string)
  (i :int))

(cffi:defcfun (pattern-add-string "FcPatternAddString") :bool
  (pattern :pointer)
  (object :string)
  (string :string))

(cffi:defcfun (pattern-add-range "FcPatternAddRange") :bool
  (pattern :pointer)
  (object :string)
  (range :pointer))

(cffi:defcfun (pattern-get-string "FcPatternGetString") result
  (pattern :pointer)
  (object :string)
  (n :int)
  (string :pointer))

(cffi:defcfun (pattern-get-integer "FcPatternGetInteger") result
  (pattern :pointer)
  (object :string)
  (n :int)
  (integer :pointer))

(cffi:defcfun (pattern-get-double "FcPatternGetDouble") result
  (pattern :pointer)
  (object :string)
  (n :int)
  (double :pointer))

(cffi:defcfun (pattern-get-range "FcPatternGetRange") result
  (pattern :pointer)
  (object :string)
  (id :int)
  (range :pointer))

(cffi:defcfun (destroy-pattern "FcPatternDestroy") :void
  (pattern :pointer))

(cffi:defcfun (config-substitute "FcConfigSubstitute") :bool
  (config :pointer)
  (pattern :pointer)
  (kind match-kind))

(cffi:defcfun (default-substitute "FcDefaultSubstitute") :void
  (pattern :pointer))

(cffi:defcfun (font-match "FcFontMatch") :pointer
  (config :pointer)
  (pattern :pointer)
  (result :pointer))

(cffi:defcfun (font-sort "FcFontSort") :pointer
  (config :pointer)
  (pattern :pointer)
  (trim :bool)
  (charset :pointer)
  (result :pointer))

(cffi:defcfun (create-range "FcRangeCreateDouble") :pointer
  (begin :double)
  (end :double))

(cffi:defcfun (destroy-range "FcRangeDestroy") :void
  (range :pointer))

(cffi:defcfun (range-get "FcRangeGetDouble") :bool
  (range :pointer)
  (begin :pointer)
  (end :pointer))

(cffi:defcfun (destroy-set "FcFontSetDestroy") :void
  (set :pointer))

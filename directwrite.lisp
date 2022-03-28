#|
 This file is a part of font-discovery
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.font-discovery)

(cffi:define-foreign-library directwrite
  (T "Dwrite.dll"))

;; https://github.com/Alexpux/mingw-w64/blob/master/mingw-w64-headers/include/dwrite.h

(cffi:defctype word :uint16)
(cffi:defctype dword :uint32)
(cffi:defctype ulong :unsigned-long)
(cffi:defctype refiid :pointer)
(cffi:defctype wchar :uint16)

(com:define-guid IID-IDWriteFactory
  #xb859ee5a #xd838 #x4b5b #xa2 #xe8 #x1a #xdc #x7d #x93 #xdb #x48)

(cffi:defcenum factory-type
  :shared
  :isolated)

(cffi:defcenum weight
  (:thin 100)
  (:extra-light 200)
  (:light 300)
  (:semi-light 350)
  (:book 375)
  (:regular 400)
  (:medium 500)
  (:semi-bold 600)
  (:bold 700)
  (:extra-bold 800)
  (:black 900)
  (:extra-black 1000))

(cffi:defcenum stretch
  (:ultra-condensed 1)
  (:extra-condensed 2)
  (:condensed 3)
  (:semi-condensed 4)
  (:normal 5)
  (:semi-expanded 6)
  (:expanded 7)
  (:extra-expanded 8)
  (:ultra-expanded 9))

(cffi:defcenum slant
  (:roman 0)
  :oblique
  :italic)

(cffi:defcenum simulations
  (:none 0)
  (:bold 1)
  (:oblique 2))

(cffi:defcenum face-type
  (:cff 0)
  :truetype
  :truetype-collection
  :type1
  :vector
  :bitmap
  :unknown)

(cffi:defcenum informational-string-id
  (:none 0)
  :copyright-notice
  :version-strings
  :trademark
  :manufacturer
  :designer
  :designer-url
  :description
  :font-vendor-url
  :license-description
  :license-info-url
  :win32-family-names
  :win32-subfamily-names
  :preferred-family-names
  :preferred-subfamily-names
  :sample-text
  :full-name
  :postscript-name
  :postscript-cid-name)

(cffi:defcfun (create-factory "DWriteCreateFactory") com:hresult
  (type factory-type)
  (iid refiid)
  (factory :pointer))

(com:define-comstruct dwrite-factory
  (get-system-font-collection com:hresult
    (collection :pointer)
    (check-for-updates :bool)))

(com:define-comstruct dwrite-font-collection
  (get-font-family-count :uint32)
  (get-font-family com:hresult
    (index :uint32)
    (family :pointer))
  (find-family-name com:hresult
    (family-name :pointer)
    (index :pointer)
    (exists :pointer))
  (get-font-from-font-face com:hresult
    (font-face :pointer)
    (font :pointer)))

(com:define-comstruct dwrite-font-family
  (get-font-collection com:hresult
    (collection :pointer))
  (get-font-count :uint32)
  (get-font com:hresult
    (index :uint32)
    (font :pointer))
  (get-family-names com:hresult
    (names :pointer))
  (get-first-matching-font com:hresult
    (weight :uint32)
    (stretch :uint32)
    (slant :uint32)
    (matching-font :pointer))
  (get-matching-fonts com:hresult
    (weight :uint32)
    (stretch :uint32)
    (slant :uint32)
    (matching-fonts :pointer)))

(com:define-comstruct dwrite-font
  (get-font-family com:hresult
    (font-family :pointer))
  (get-weight :uint32)
  (get-stretch :uint32)
  (get-slant :uint32)
  (is-symbol-font :bool)
  (get-face-names com:hresult
    (names :pointer))
  (get-informational-strings com:hresult
    (id informational-string-id)
    (strings :pointer)
    (exists :pointer))
  (get-simulations simulations)
  (get-metrics :void
    (metrics :pointer))
  (has-character com:hresult
    (unicode-value :uint32)
    (exists :pointer))
  (create-font-face com:hresult
    (font-face :pointer)))

(com:define-comstruct dwrite-font-face
  (get-type face-type)
  (get-files com:hresult
    (number-of-files :pointer)
    (font-files :pointer))
  (get-index :uint32)
  (get-simulations simulations)
  (is-symbol-font :bool)
  (get-metrics :void
    (metrics :pointer))
  (get-glyph-count :uint16)
  (get-design-glyph-metrics com:hresult
    (glyph-indices :pointer)
    (glyph-count :uint32)
    (glyph-metcirs :pointer)
    (is-sideways :bool))
  (get-glyph-indices com:hresult
    (code-points :pointer)
    (code-point-count :uint32)
    (glyph-indices :pointer))
  (try-get-font-table com:hresult
    (open-type-table-tag :uint32)
    (table-data :pointer)
    (table-size :pointer)
    (table-context :pointer)
    (exists :pointer))
  (release-font-table :void
    (table-context :pointer))
  (get-glyph-run-outline com:hresult
    (em-size :float)
    (glyph-indices :pointer)
    (glyph-advances :pointer)
    (glyph-offsets :pointer)
    (glyph-count :uint32)
    (is-sideways :bool)
    (is-right-to-left :bool)
    (geometry-sink :pointer))
  (get-gdi-compatible-metrics com:hresult
    (em-size :float)
    (pixels-per-dip :float)
    (transform :pointer)
    (font-face-metrics :pointer))
  (get-gdi-compatible-glyph-metrics com:hresult
    (em-size :float)
    (pixels-per-dip :float)
    (transform :pointer)
    (use-gdi-natural :bool)
    (glyph-indices :pointer)
    (glyph-count :uint32)
    (glyph-metrics :pointer)
    (is-sideways :bool)))

(com:define-comstruct dwrite-font-file
  (get-reference-key com:hresult
    (font-file-reference-key :pointer)
    (font-file-reference-key-size :pointer))
  (get-loader com:hresult
    (font-file-loader :pointer))
  (analyze com:hresult
    (is-supported-font-file :pointer)
    (font-file-type :pointer)
    (font-face-type :pointer)
    (number-of-faces :pointer)))

(com:define-comstruct dwrite-local-font-file-loader
  (create-stream-from-key com:hresult
    (font-file-reference-key :pointer)
    (font-file-reference-key-size :uint32)
    (file-stream :pointer))
  (get-file-path-length-from-key com:hresult
    (font-file-reference-key :pointer)
    (font-file-reference-key-size :uint32)
    (file-path-length :pointer))
  (get-file-path-from-key com:hresult
    (font-file-reference-key :pointer)
    (font-file-reference-key-size :uint32)
    (file-path :pointer)
    (file-path-size :uint32))
  (get-last-write-time-from-key com:hresult
    (font-file-reference-key :pointer)
    (font-file-reference-key-size :uint32)
    (last-write-time :pointer)))

(com:define-comstruct dwrite-localized-strings
  (get-count :uint32)
  (find-locale-name com:hresult
    (locale-name :pointer)
    (index :pointer)
    (exists :pointer))
  (get-locale-name-length com:hresult
    (index :uint32)
    (length :pointer))
  (get-locale-name com:hresult
    (index :uint32)
    (name :pointer)
    (size :uint32))
  (get-string-length com:hresult
    (index :uint32)
    (length :pointer))
  (get-string com:hresult
    (index :uint32)
    (buffer :pointer)
    (size :uint32)))

(com:define-comstruct dwrite-font-list
  (get-font-collection com:hresult
    (collection :pointer))
  (get-font-count :uint32)
  (get-font com:hresult
    (index :uint32)
    (font :pointer)))

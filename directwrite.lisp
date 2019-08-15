#|
 This file is a part of font-discovery
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.font-discovery)

(cffi:define-foreign-library directwrite
  (T "Dwrite.dll"))

;; https://github.com/Alexpux/mingw-w64/blob/master/mingw-w64-headers/include/dwrite.h

(cffi:defctype refiid :pointer)
(cffi:defctype hresult :uint32)

(defcstruct (guid :conc-name guid-)
  (data1 dword)
  (data2 word)
  (data3 word)
  (data4 byte :count 8))

(defcstruct (com :conc-name ||)
  (vtbl :pointer))

(defmacro defcomfun ((struct method &rest options) return-type &body args)
  (let* ((*print-case* (readtable-case *readtable*))
         (structg (gensym "STRUCT"))
         (name (intern (format NIL "~a-~a" struct method))))
    `(progn
       (declaim (inline ,name))
       (defun ,name (,structg ,@(mapcar #'first args))
         (foreign-funcall-pointer
          (,(intern (format NIL "%~a" name))
           (vtbl ,structg))
          ,options
          :pointer ,structg
          ,@(loop for (name type) in args
                  collect type collect name)
          ,return-type)))))

(defmacro defcomstruct (name &body methods)
  (let ((methods (list* `(query-interface hresult)
                        `(add-ref ulong)
                        `(release ulong)
                        methods)))
    `(progn
       (defcstruct (,name :conc-name ,(format NIL "%~a-" name))
         ,@(loop for method in methods
                 collect (list (first method) :pointer)))

       ,@(loop for (method return . args) in methods
               collect `(defcomfun (,name ,method) ,return
                          ,@args)))))

(trivial-indent:define-indentation defcomstruct (4 &rest (&whole 2 4 &rest 2)))

(defun make-guid (d1 d2 d3 &rest d4)
  (let ((ptr (foreign-alloc '(:struct guid))))
    (setf (guid-data1 ptr) d1)
    (setf (guid-data2 ptr) d2)
    (setf (guid-data3 ptr) d3)
    (loop for i from 0 below 8
          for d in d4
          do (setf (mem-aref (foreign-slot-pointer ptr '(:struct guid) 'data4) 'byte i)
                   d))
    ptr))

(defvar IID-IDWriteFactory
  (make-guid #xb859ee5a #xd838 #x4b5b #xa2 #xe8 #x1a #xdc #x7d #x93 #xdb #x48))
(defvar IID-IDwriteLocalFontFileLoader
  (make-guid #xb2d9f3ec #xc9fe #x4a11 #xa2 #xec #xd8 #x62 #x08 #xf7 #xc0 #xa2))

(cffi:defcenum factory-type
  :shared
  :isolated)

(cffi:defcenum weight
  (:thin 100)
  (:extra-light 200)
  (:light 300)
  (:regular 400)
  (:medium 500)
  (:semibold 600)
  (:bold 700)
  (:extra-bold 800)
  (:black 900))

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
  (:normal 0)
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

(cffi:decfenum informational-string-id
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

(cffi:defcfun (create-factory "DWriteCreateFactory") hresult
  (type factory-type)
  (iid refiid)
  (factory :pointer))

(defcomstruct dwrite-factory
  (get-system-font-collection hresult
    (collection :pointer)
    (check-for-updates :bool)))

(defcomstruct dwrite-collection
  (get-font-family-count :uint32)
  (get-font-family hresult
    (index :uint32)
    (family :pointer))
  (get-font-from-font-face hresult
    (font-face :pointer)
    (font :pointer)))

(defcomstruct dwrite-font-family
  (get-font-collection hresult
    (collection :pointer))
  (get-font-count :uint32)
  (get-font hresult
    (index :uint32)
    (font :pointer))
  (get-family-names hresult
    (names :pointer))
  (get-first-matching-font hresult
    (weight weight)
    (stretch stretch)
    (slant slant)
    (matching-font :pointer))
  (get-matching-fonts hresult
    (weight weight)
    (stretch stretch)
    (slant slant)
    (matching-fonts :pointer)))

(defcomstruct dwrite-font
  (get-font-family hresult
    (font-family :pointer))
  (get-weight weight)
  (get-stretch stretch)
  (get-slant slant)
  (is-symbol-font :bool)
  (get-face-names hresult
    (names :pointer))
  (get-informational-strings hresult
    (id informational-string-id)
    (strings :pointer)
    (exists :pointer))
  (get-simulations simulations)
  (get-metrics :void
    (metrics :pointer))
  (has-character hresult
    (unicode-value :uint32)
    (exists :pointer))
  (create-font-face hresult
    (font-face :pointer)))

(defcomstruct dwrite-font-face
  (get-type face-type)
  (get-files hresult
    (number-of-files :pointer)
    (font-files :pointer))
  (get-index :uint32)
  (get-simulations simulations)
  (is-symbol-font :bool)
  (get-metrics :void
    (metrics :pointer))
  (get-glyph-count :uint16)
  (get-design-glyph-metrics hresult
    (glyph-indices :pointer)
    (glyph-count :uint32)
    (glyph-metcirs :pointer)
    (is-sideways :bool))
  (get-glyph-indices hresult
    (code-points :pointer)
    (code-point-count :uint32)
    (glyph-indices :pointer))
  (try-get-font-table hresult
    (open-type-table-tag :uint32)
    (table-data :pointer)
    (table-size :pointer)
    (table-context :pointer)
    (exists :pointer))
  (release-font-table :void
    (table-context :pointer))
  (get-glyph-run-outline hresult
    (em-size :float)
    (glyph-indices :pointer)
    (glyph-advances :pointer)
    (glyph-offsets :pointer)
    (glyph-count :uint32)
    (is-sideways :bool)
    (is-right-to-left :bool)
    (geometry-sink :pointer))
  (get-gdi-compatible-metrics hresult
    (em-size :float)
    (pixels-per-dip :float)
    (transform :pointer)
    (font-face-metrics :pointer))
  (get-gdi-compatible-glyph-metrics hresult
    (em-size :float)
    (pixels-per-dip :float)
    (transform :pointer)
    (use-gdi-natural :bool)
    (glyph-indices :pointer)
    (glyph-count :uint32)
    (glyph-metrics :pointer)
    (is-sideways :bool)))

(defcomstruct dwrite-font-file
  (get-reference-key hresult
    (font-file-reference-key :pointer)
    (font-file-reference-key-size :pointer))
  (get-loader hresult
    (font-file-loader :pointer))
  (analyze hresult
    (is-supported-font-file :pointer)
    (font-file-type :pointer)
    (font-face-type :pointer)
    (number-of-faces :pointer)))

(defcomstruct dwrite-local-font-file-loader
  (create-stream-from-key hresult
    (font-file-reference-key :pointer)
    (font-file-reference-key-size :uint32)
    (file-stream :pointer))
  (get-file-path-length-from-key hresult
    (font-file-reference-key :pointer)
    (font-file-reference-key-size :uint32)
    (file-path-length :pointer))
  (get-file-path-from-key hresult
    (font-file-reference-key :pointer)
    (font-file-reference-key-size :uint32)
    (file-path :pointer)
    (file-path-size :uint32))
  (get-last-write-time-from-key hresult
    (font-file-reference-key :pointer)
    (font-file-reference-key-size :uint32)
    (last-write-time :pointer)))

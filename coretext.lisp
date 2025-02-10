(in-package #:org.shirakumo.font-discovery)

(cffi:define-foreign-library foundation
  (T (:framework "Foundation")))

(cffi:define-foreign-library coretext
  (T (:framework "CoreText")))

(set 'cl-user::*foreign-system-libraries*
     (union (when (boundp 'cl-user::*foreign-system-libraries*)
              (symbol-value 'cl-user::*foreign-system-libraries*))
            '(foundation coretext)))

(cffi:defctype cgfloat :double)
(cffi:defctype cfindex :long)

(cffi:defcvar (symbolic-trait "kCTFontSymbolicTrait" :read-only T) :pointer)
(cffi:defcvar (weight-trait "kCTFontWeightTrait" :read-only T) :pointer)
(cffi:defcvar (width-trait "kCTFontWidthTrait" :read-only T) :pointer)
(cffi:defcvar (slant-trait "kCTFontSlantTrait" :read-only T) :pointer)
(cffi:defcvar (url-attribute "kCTFontURLAttribute" :read-only T) :pointer)
(cffi:defcvar (family-name-attribute "kCTFontFamilyNameAttribute" :read-only T) :pointer)
(cffi:defcvar (traits-attribute "kCTFontTraitsAttribute" :read-only T) :pointer)

(cffi:defcenum (string-encoding :uint32)
  (:utf-8 #x08000100))

(cffi:defcenum (number-type cfindex)
  (:int32 3)
  (:float 12)
  (:double 13))

(cffi:defcenum (symbolic-traits :uint32)
  (:italic    #b00000000001)
  (:bold      #b00000000010)
  (:expanded  #b00000100000)
  (:condensed #b00001000000)
  (:monospace #b10000000000))

(cffi:defcenum (path-style cfindex)
  (:unix 0))

(cffi:defcfun (release "CFRelease") :void
  (object :pointer))

;;; Font
(cffi:defcfun (create-font-with-font-descriptor "CTFontCreateWithFontDescriptor") :pointer
  (descriptor :pointer)
  (size cgfloat)
  (matrix :pointer))

(cffi:defcfun (font-copy-traits "CTFontCopyTraits") :pointer
  (font :pointer))

(cffi:defcfun (font-get-symbolic-traits "CTFontGetSymbolicTraits") :uint32
  (font :pointer))

;;; Font descriptor
(cffi:defcfun (create-font-descriptor "CTFontDescriptorCreateWithAttributes") :pointer
  (attributes :pointer))

(cffi:defcfun (font-descriptor-create-matching-font-descriptor "CTFontDescriptorCreateMatchingFontDescriptor") :pointer
  (descriptor :pointer)
  (mandatory-attributes :pointer))

(cffi:defcfun (font-descriptor-create-matching-font-descriptors "CTFontDescriptorCreateMatchingFontDescriptors") :pointer
  (descriptor :pointer)
  (mandatory-attributes :pointer))

(cffi:defcfun (font-descriptor-copy-attribute "CTFontDescriptorCopyAttribute") :pointer
  (descriptor :pointer)
  (attribute :pointer))

;;; Font collection
(cffi:defcfun (create-font-collection-from-available-fonts "CTFontCollectionCreateFromAvailableFonts") :pointer
  (options :pointer))

(cffi:defcfun (font-collection-create-matching-font-descriptors "CTFontCollectionCreateMatchingFontDescriptors") :pointer
  (collection :pointer))

;;; Number crap
(cffi:defcfun (create-number "CFNumberCreate") :pointer
  (allocator :pointer)
  (type number-type)
  (value :pointer))

(cffi:defcfun (number-get-value "CFNumberGetValue") :bool
  (number :pointer)
  (type number-type)
  (value :pointer))

;;; Dictionary crap
(cffi:defcfun (create-dictionary "CFDictionaryCreate") :pointer
  (allocator :pointer)
  (keys :pointer)
  (values :pointer)
  (num-values cfindex)
  (key-callbacks :pointer)
  (value-callbacks :pointer))

(cffi:defcfun (dictionary-contains-key "CFDictionaryContainsKey") :pointer
  (dictionary :pointer)
  (key :pointer))

(cffi:defcfun (dictionary-get-value "CFDictionaryGetValue") :pointer
  (dictionary :pointer)
  (key :pointer))

;;; Set crap
(cffi:defcfun (create-set "CFSetCreate") :pointer
  (allocator :pointer)
  (values :pointer)
  (num-values cfindex)
  (callbacks :pointer))

;;; Array crap
(cffi:defcfun (array-get-count "CFArrayGetCount") cfindex
  (array :pointer))

(cffi:defcfun (array-get-value-at-index "CFArrayGetValueAtIndex") :pointer
  (array :pointer)
  (index cfindex))

;;; URL crap
(cffi:defcfun (url-copy-file-system-path "CFURLCopyFileSystemPath") :pointer
  (url :pointer)
  (style path-style))

;;; String crap
(cffi:defcfun (create-string-with-cstring "CFStringCreateWithCString") :pointer
  (allocator :pointer)
  (bytes :pointer)
  (encoding string-encoding)
  (is-external-representation :bool))

(cffi:defcfun (string-get-length "CFStringGetLength") cfindex
  (string :pointer))

(cffi:defcfun (string-get-cstring "CFStringGetCString") :bool
  (string :pointer)
  (buffer :pointer)
  (length cfindex)
  (encoding string-encoding))

(cffi:defcfun (string-get-cstring-ptr "CFStringGetCStringPtr") :pointer
  (string :pointer)
  (encoding string-encoding))

(defun cfstring->string (pointer)
  (let ((buffer (string-get-cstring-ptr pointer :utf-8)))
    (cond ((cffi:null-pointer-p buffer)
           (let ((length (1+ (* 2 (string-get-length pointer)))))
             (if (= 0 length)
                 (make-string 0)
                 (cffi:with-foreign-object (buffer :uint8 length)
                   (if (string-get-cstring pointer buffer length :utf-8)
                       (cffi:foreign-string-to-lisp buffer :encoding :utf-8)
                       (error "Failed to convert string to lisp!"))))))
          (T
           (cffi:foreign-string-to-lisp buffer :encoding :utf-8)))))

(defun string->cfstring (string)
  (cffi:with-foreign-string (buffer string :encoding :utf-8)
    (create-string-with-cstring (cffi:null-pointer) buffer :utf-8 NIL)))

(defmacro with-cfstring ((var string) &body body)
  `(let ((,var (string->cfstring ,string)))
     (with-protection (release ,var)
       ,@body)))

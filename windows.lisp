#|
 This file is a part of font-discovery
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.font-discovery)

(defvar *factory*)
(defvar *collection*)

(defun check-result (form)
  (unless (= 0 (ldb (byte 1 31) form))
    (error "Windows API call failed.")))

(defun init (&key reinit)
  (unless (boundp '*factory*)
    (cffi:with-foreign-object (factory :pointer)
      (check-result
       (create-factory :shared IID-IDWriteFactory factory))
      (setf *factory* (cffi:mem-ref factory :pointer))
      (setf reinit T)))
  (when reinit
    (cffi:with-foreign-object (collection :pointer)
      (check-result
       (dwrite-factory-get-system-font-collection *factory* collection T))
      (setf *collection* (cffi:mem-ref collection :pointer)))))

(defun deinit ()
  (when (boundp '*factory*)
    (com-release *collection*)
    (makunbound '*collection*)
    (com-release *factory*)
    (makunbound '*factory*)))

(defmacro with-getter-value ((var getter) &body body)
  `(cffi:with-foreign-object (,var :pointer)
     (check-result ,getter)
     (let ((,var (cffi:mem-ref ,var :pointer)))
       (with-protection (com-release ,var)
         ,@body))))

(defmacro with-getter-values (values &body body)
  (if values
      `(with-getter-value ,(pop values)
         (with-getter-values ,values
           ,@body))
      `(progn ,@body)))

(defun get-font-path (font)
  (cffi:with-foreign-objects ((count :uint32)
                              (key :pointer)
                              (size :uint32)
                              (length :uint32))
    (setf (cffi:mem-ref count :uint32) 1)
    (with-getter-values
        ((face (dwrite-font-create-font-face font face))
         (file (dwrite-font-face-get-files face count file))
         (loader (dwrite-font-file-get-loader file loader)))
      (check-result
       (dwrite-font-file-get-reference-key file key size))
      (check-result
       (dwrite-local-font-file-loader-get-file-path-length-from-key
        loader
        (cffi:mem-ref key :pointer)
        (cffi:mem-ref size :uint32)
        length))
      (cffi:with-foreign-object (path 'wchar (1+ (cffi:mem-ref length :uint32)))
        (check-result
         (dwrite-local-font-file-loader-get-file-path-from-key
          loader
          (cffi:mem-ref key :pointer)
          (cffi:mem-ref size :uint32)
          path
          (1+ (cffi:mem-ref length :uint32))))
        (wstring->string path)))))

(defun get-font-family (family)
  (with-getter-value (names (dwrite-font-family-get-family-names family names))
    (cffi:with-foreign-objects ((index :uint32)
                                (length :uint32)
                                (exists :bool))
      (check-result
       (dwrite-localized-strings-find-locale-name names locale index exists))
      (let ((index (if (cffi:mem-ref exists :bool)
                       (cffi:mem-ref index :uint32)
                       0)))
        (check-result
         (dwrite-localized-strings-get-string-length names index length))
        (cffi:with-foreign-object (string 'wchar (1+ (cffi:mem-ref length :uint32)))
          (check-result
           (dwrite-localized-strings-get-string names index string (1+ (cffi:mem-ref length :uint32))))
          (wstring->string string))))))

(defun translate-font (font family)
  (make-instance 'font
                 :file (get-font-path font)
                 :family (get-font-family family)
                 :slant (dwrite-font-get-slant font)
                 :weight (dwrite-font-get-weight font)
                 :stretch (dwrite-font-get-stretch font)))

(defun find-font (&rest args &key family slant weight size spacing stretch)
  (declare (ignore family slant weight size spacing width))
  (init)
  (cond (family
         (cffi:with-foreign-objects ((index :uint32)
                                     (exists :bool))
           (let ((family (string->wstring family)))
             (unwind-protect
                  (check-result
                   (dwrite-font-collection-find-family-name *collection* family index exists))
               (cffi:foreign-free family)))
           (when (cffi:mem-ref exists :bool)
             (with-getter-values
                 ((family (dwrite-font-collection-get-font-family
                           *collection*
                           (cffi:mem-ref index :uint32)
                           family))
                  (font (dwrite-font-family-get-first-matching-font
                         family
                         (maybe-enum 'weight weight)
                         (maybe-enum 'stretch stretch)
                         (maybe-enum 'slant slant)
                         font)))
               (translate-font font family)))))
        (T
         (first (apply #'list-fonts args)))))

(defun list-fonts (&rest args &key family slant weight size spacing width)
  (declare (ignore family slant weight size spacing width))
  (init)
  )



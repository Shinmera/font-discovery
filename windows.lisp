(in-package #:org.shirakumo.font-discovery)

(defclass directwrite (backend)
  ((factory :initform NIL :accessor factory)
   (collection :initform NIL :accessor collection)))

(register-backend 'directwrite)

(defmethod init* ((backend directwrite))
  (com:init)
  (unless (cffi:foreign-library-loaded-p 'directwrite)
    (cffi:load-foreign-library 'directwrite))
  (unless (factory backend)
    (setf (factory backend) (com:with-deref (factory :pointer)
                              (create-factory :shared IID-IDWriteFactory factory)))
    (refresh))
  T)

(defmethod refresh* ((backend directwrite))
  (when (collection backend)
    (com:release (collection backend)))
  (setf (collection backend) (com:with-deref (collection :pointer)
                               (dwrite-factory-get-system-font-collection (factory backend) collection T)))
  T)

(defmethod deinit* ((backend directwrite))
  (when (collection backend)
    (com:release (collection backend))
    (setf (collection backend) NIL))
  (when (factory backend)
    (com:release (factory backend))
    (setf (factory backend) NIL)))

(defmacro with-getter-value ((var getter) &body body)
  `(cffi:with-foreign-object (,var :pointer)
     (com:check-hresult ,getter)
     (let ((,var (cffi:mem-ref ,var :pointer)))
       (with-protection (com:release ,var)
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
      (com:check-hresult
       (dwrite-font-file-get-reference-key file key size))
      (com:check-hresult
       (dwrite-local-font-file-loader-get-file-path-length-from-key
        loader
        (cffi:mem-ref key :pointer)
        (cffi:mem-ref size :uint32)
        length))
      (cffi:with-foreign-object (path 'wchar (1+ (cffi:mem-ref length :uint32)))
        (com:check-hresult
         (dwrite-local-font-file-loader-get-file-path-from-key
          loader
          (cffi:mem-ref key :pointer)
          (cffi:mem-ref size :uint32)
          path
          (1+ (cffi:mem-ref length :uint32))))
        (com:wstring->string path)))))

(defun get-font-family (family)
  (with-getter-value (names (dwrite-font-family-get-family-names family names))
    (cffi:with-foreign-objects ((index :uint32)
                                (length :uint32)
                                (exists :bool))
      (com:with-wstring (locale "en-us")
        (com:check-hresult
         (dwrite-localized-strings-find-locale-name names locale index exists)))
      (let ((index (if (cffi:mem-ref exists :bool)
                       (cffi:mem-ref index :uint32)
                       0)))
        (com:check-hresult
         (dwrite-localized-strings-get-string-length names index length))
        (cffi:with-foreign-object (string 'wchar (1+ (cffi:mem-ref length :uint32)))
          (com:check-hresult
           (dwrite-localized-strings-get-string names index string (1+ (cffi:mem-ref length :uint32))))
          (com:wstring->string string))))))

(defun translate-font (font family)
  (make-instance 'font
                 :file (parse-file (get-font-path font))
                 :family (get-font-family family)
                 :slant (maybe-enum-val 'slant (dwrite-font-get-slant font))
                 :weight (maybe-enum-val 'weight (dwrite-font-get-weight font))
                 :stretch (maybe-enum-val 'stretch (dwrite-font-get-stretch font))))

(defmethod find-font* ((backend directwrite) &rest args &key family slant weight spacing stretch)
  (declare (ignore spacing))
  (cond (family
         (cffi:with-foreign-objects ((index :uint32)
                                     (exists :bool))
           (com:with-wstring (family family)
             (com:check-hresult
              (dwrite-font-collection-find-family-name (collection backend) family index exists)))
           (when (cffi:mem-ref exists :bool)
             (with-getter-values
                 ((family (dwrite-font-collection-get-font-family
                           (collection backend)
                           (cffi:mem-ref index :uint32)
                           family))
                  (font (dwrite-font-family-get-first-matching-font
                         family
                         (maybe-enum 'weight (or weight :regular))
                         (maybe-enum 'stretch (or stretch :normal))
                         (maybe-enum 'slant (or slant :roman))
                         font)))
               (translate-font font family)))))
        (T
         (first (apply #'list-fonts args)))))

(defmethod list-fonts* ((backend directwrite) &key family slant weight spacing stretch)
  (declare (ignore spacing))
  (let ((fonts ()))
    (flet ((handle-family (index)
             (with-getter-values
                 ((family (dwrite-font-collection-get-font-family
                           (collection backend)
                           index
                           family))
                  (list (dwrite-font-family-get-matching-fonts
                         family
                         (maybe-enum 'weight (or weight :regular))
                         (maybe-enum 'stretch (or stretch :normal))
                         (maybe-enum 'slant (or slant :roman))
                         list)))
               (loop for i from 0 below (dwrite-font-list-get-font-count list)
                     do (with-getter-value (font (dwrite-font-list-get-font list i font))
                          (push (translate-font font family) fonts))))))
      (cond (family
             (cffi:with-foreign-objects ((index :uint32)
                                         (exists :bool))
               (com:with-wstring (family family)
                 (com:check-hresult
                  (dwrite-font-collection-find-family-name (collection backend) family index exists)))
               (when (cffi:mem-ref exists :bool)
                 (handle-family (cffi:mem-ref index :uint32)))))
            (T
             (loop for i from 0 below (dwrite-font-collection-get-font-family-count (collection backend))
                   do (handle-family i))
             ;; FIXME: reorder collected list by proximity to requested properties.
             )))
    (nreverse fonts)))

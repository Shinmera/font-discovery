(in-package #:org.shirakumo.font-discovery)

(defclass fontconfig (backend)
  ((handle :initform NIL :accessor handle)))

(register-backend 'fontconfig)

(defmethod init* ((backend fontconfig))
  (unless (handle backend)
    (unless (cffi:foreign-library-loaded-p 'fontconfig)
      (cffi:load-foreign-library 'fontconfig))
    (setf (handle backend) (init-load-config-and-fonts)))
  T)

(defmethod refresh* ((backend fontconfig))
  (config-destroy (handle backend))
  (setf (handle backend) (init-load-config-and-fonts))
  T)

(defmethod deinit* ((backend fontconfig))
  (when (handle backend)
    (config-destroy (handle backend))
    (fini))
  T)

(defmacro with-result (result &body success)
  (let ((resultg (gensym "RESULT")))
    `(let ((,resultg ,result))
       (case ,resultg
         (:match ,@success)
         (:no-match NIL)
         (T (error "Font match failed: ~s" ,resultg))))))

(defun translate-match (font)
  (cffi:with-foreign-object (pointer :pointer)
    (flet ((value (object type default)
             (case (ecase type
                     (:string (pattern-get-string font object 0 pointer))
                     (:int (pattern-get-integer font object 0 pointer))
                     (:double (pattern-get-double font object 0 pointer))
                     (:range (pattern-get-range font object 0 pointer)))
               (:match
                   (case type
                     (:range
                      (cffi:with-foreign-objects ((begin :double) (end :double))
                        (range-get (cffi:mem-ref pointer :pointer) begin end)
                        (list (cffi:mem-ref begin :double)
                              (cffi:mem-ref end :double))))
                     (T
                      (cffi:mem-ref pointer type))))
               (T
                default))))
      (make-instance 'font
                     :file (parse-file (value +FILE+ :string NIL))
                     :family (value +FAMILY+ :string NIL)
                     :slant (maybe-enum-val 'slant (value +SLANT+ :int 0))
                     :weight (maybe-enum-val 'weight (value +WEIGHT+ :int 80))
                     :spacing (maybe-enum-val 'spacing (value +SPACING+ :int 0))
                     :stretch (maybe-enum-val 'stretch (value +WIDTH+ :int 100))))))

(defun init-pattern (backend pattern &key family slant weight spacing stretch)
  (when family
    (pattern-add-string pattern +FAMILY+ family))
  (when slant
    (pattern-add-integer pattern +SLANT+ (maybe-enum 'slant slant)))
  (when weight
    (pattern-add-integer pattern +WEIGHT+ (maybe-enum 'weight weight)))
  (when spacing
    (pattern-add-integer pattern +SPACING+ (maybe-enum 'spacing spacing)))
  (when stretch
    (pattern-add-integer pattern +WIDTH+ (maybe-enum 'stretch stretch)))
  (config-substitute (handle backend) pattern :pattern)
  (default-substitute pattern)
  pattern)

(defmethod find-font* ((backend fontconfig) &rest args &key &allow-other-keys)
  (let ((pattern (create-pattern)))
    (with-protection (destroy-pattern pattern)
      (apply #'init-pattern backend pattern args)
      (cffi:with-foreign-object (result 'result)
        (let ((font (font-match (handle backend) pattern result)))
          (with-result (cffi:mem-ref result 'result)
            (unwind-protect
                 (translate-match font)
              (destroy-pattern font))))))))

(defmethod list-fonts* ((backend fontconfig) &rest args &key &allow-other-keys)
  (let ((pattern (create-pattern)))
    (with-protection (destroy-pattern pattern)
      (apply #'init-pattern backend pattern args)
      (cffi:with-foreign-object (result 'result)
        (let ((set (font-sort (handle backend) pattern NIL (cffi:null-pointer) result)))
          (with-result (cffi:mem-ref result 'result)
            (unwind-protect
                 (loop for i from 0 below (font-set-nfont set)
                       for font = (cffi:mem-aref (font-set-fonts set) :pointer i)
                       collect (translate-match font))
              (destroy-set set))))))))

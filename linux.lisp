#|
 This file is a part of font-discovery
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.font-discovery)

(defvar *config*)

(defun init ()
  (unless (boundp '*config*)
    (cffi:use-foreign-library fontconfig)
    (setf *config* (init-load-config-and-fonts))))

(defun refresh ()
  (init)
  (config-destroy *config*)
  (setf *config* (init-load-config-and-fonts)))

(defun deinit ()
  (when (boundp '*config*)
    (config-destroy *config*)
    (makunbound '*config*)
    (fini)))

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
                     :file (value +FILE+ :string NIL)
                     :family (value +FAMILY+ :string NIL)
                     :slant (maybe-enum-val 'slant (value +SLANT+ :int 0))
                     :weight (maybe-enum-val 'weight (value +WEIGHT+ :int 80))
                     :spacing (maybe-enum-val 'spacing (value +SPACING+ :int 0))
                     :stretch (maybe-enum-val 'stretch (value +WIDTH+ :int 100))))))

(defun init-pattern (pattern &key family slant weight spacing stretch)
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
  (config-substitute *config* pattern :pattern)
  (default-substitute pattern)
  pattern)

(defun find-font (&rest args &key family slant weight spacing stretch)
  (declare (ignore family slant weight spacing stretch))
  (init)
  (let ((pattern (create-pattern)))
    (with-protection (destroy-pattern pattern)
      (apply #'init-pattern pattern args)
      (cffi:with-foreign-object (result 'result)
        (let ((font (font-match *config* pattern result)))
          (with-result (cffi:mem-ref result 'result)
            (unwind-protect
                 (translate-match font)
              (destroy-pattern font))))))))

(defun list-fonts (&rest args &key family slant weight spacing stretch)
  (declare (ignore family slant weight spacing stretch))
  (init)
  (let ((pattern (create-pattern)))
    (with-protection (destroy-pattern pattern)
      (apply #'init-pattern pattern args)
      (cffi:with-foreign-object (result 'result)
        (let ((set (font-sort *config* pattern NIL (cffi:null-pointer) result)))
          (with-result (cffi:mem-ref result 'result)
            (unwind-protect
                 (loop for i from 0 below (font-set-nfont set)
                       for font = (cffi:mem-aref (font-set-fonts set) :pointer i)
                       collect (translate-match font))
              (destroy-set set))))))))

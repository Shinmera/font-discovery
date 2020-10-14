#|
 This file is a part of font-discovery
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.font-discovery)

(defvar *init* NIL)

(defun init ()
  (unless *init*
    (cffi:use-foreign-library foundation)
    (cffi:use-foreign-library coretext)
    (setf *init* T)))

(defun refresh ())

(defun deinit ()
  (when *init*
    (setf *init* NIL)
    T))

(defun translate-value (value table)
  (etypecase value
    (real
     ;; Find closest value in table
     (if (< value (second (first table)))
         (first (first table))
         (loop for (prev entry) on table
               do (cond ((null entry)
                         (return (first prev)))
                        ((<= (second prev) value (second entry))
                         (return (if (< (- value (second prev))
                                        (- (second entry) value))
                                     (first prev)
                                     (first entry))))))))
    (symbol
     (or (second (find value table :key #'first))
         (error "No such name ~s." value)))))

(defun translate-weight (value)
  (translate-value
   value
   '((:thin        -0.8)
     (:extra-light -0.5)
     (:light       -0.4)
     (:semi-light  -0.3)
     (:book        -0.2)
     (:regular      0.0)
     (:medium      +0.2)
     (:semi-bold   +0.3)
     (:bold        +0.4)
     (:extra-bold  +0.5)
     (:black       +0.6)
     (:extra-black +0.8))))

(defun translate-slant (value)
  (translate-value
   value
   '((:roman    0.0)
     (:italic  +0.06)
     (:oblique +0.1))))

(defun translate-stretch (value)
  (translate-value
   value
   '((:ultra-condensed -0.4)
     (:extra-condensed -0.3)
     (:condensed       -0.2)
     (:semi-condensed  -0.1)
     (:normal           0.0)
     (:semi-expanded   +0.1)
     (:expanded        +0.2)
     (:extra-expanded  +0.3)
     (:ultra-expanded  +0.4))))

(defun number-value (number type)
  (cffi:with-foreign-object (value type)
    (unless (number-get-value number type value)
      (error "Failed to retrieve numerical value."))
    (cffi:mem-ref value type)))

(defun value-number (number type)
  (cffi:with-foreign-object (value type)
    (setf (cffi:mem-ref value type) (ecase type
                                      (:double (float number 0d0))
                                      (:int32 (truncate number))))
    (create-number (cffi:null-pointer) type value)))

(defmacro with-foundation-object ((var init &optional fail) &body body)
  `(let ((,var ,init))
     (if (cffi:null-pointer-p ,var)
         ,(or fail `(error "The OS call failed:~%  ~s" ',init))
         (with-protection (release ,var)
           ,@body))))

(defmacro with-dictionary ((dictionary max) entries &body body)
  `(cffi:with-foreign-objects ((keys :pointer ,max)
                               (values :pointer ,max))
     (let ((count 0))
       (flet (((setf entry) (value key)
                (setf (cffi:mem-aref keys :pointer count) key)
                (setf (cffi:mem-aref values :pointer count) value)
                (incf count)))
         ,@entries
         (with-protection (loop for i from 0 below count
                                do (release (cffi:mem-aref values :pointer i)))
           (let ((,dictionary (create-dictionary (cffi:null-pointer) keys values count (cffi:null-pointer) (cffi:null-pointer))))
             ,@body))))))

(trivial-indent:define-indentation with-dictionary (6 4 &body))

(defun call-with-attributes (function &key family slant weight spacing stretch)
  (with-dictionary (traits 4)
      ((when weight
         (setf (entry weight-trait) (value-number (translate-weight weight) :double)))
       (when stretch
         (setf (entry width-trait) (value-number (translate-stretch stretch) :double)))
       (when slant
         (setf (entry slant-trait) (value-number (translate-slant slant) :double)))
       (when (eq spacing :monospace)
         (setf (entry symbolic-trait) (value-number (cffi:foreign-enum-value 'symbolic-traits :monospace) :int32))))
    (with-dictionary (attributes 2)
        ((when family
           (setf (entry family-name-attribute) (string->cfstring family)))
         (setf (entry traits-attribute) traits))
      (with-foundation-object (set (create-set (cffi:null-pointer) keys count (cffi:null-pointer)))
        (funcall function attributes set)))))

(defmacro with-attributes ((attributes mandatory fontspec) &body body)
  `(apply #'call-with-attributes (lambda (,attributes ,mandatory) ,@body) ,fontspec))

(defun translate-descriptor (descriptor)
  (macrolet ((with-attribute ((var attr) &body body)
               `(with-foundation-object (,var (font-descriptor-copy-attribute descriptor ,attr) NIL)
                  ,@body))
             (with-trait ((var trait &optional default) &body body)
               `(if (dictionary-contains-key traits ,trait)
                    (with-foundation-object (,var (dictionary-get-value traits ,trait))
                      ,@body)
                    ,default)))
    (with-attribute (traits traits-attribute)
      (make-instance 'font :file (with-attribute (url url-attribute)
                                   (with-foundation-object (path (url-copy-file-system-path url :unix))
                                     (parse-file (cfstring->string path))))
                           :family (with-attribute (family family-name-attribute)
                                     (cfstring->string family))
                           :slant (with-trait (slant slant-trait)
                                    (translate-slant (number-value slant :double)))
                           :weight (with-trait (weight weight-trait)
                                     (translate-weight (number-value weight :double)))
                           :spacing (with-trait (symbolic-traits symbolic-trait)
                                      (when (/= 0 (logand (cffi:foreign-enum-value 'symbolic-traits :monospace)
                                                          (number-value symbolic-traits :int32)))
                                        :monospace))
                           :stretch (with-trait (stretch width-trait)
                                      (translate-stretch (number-value stretch :double)))))))

(defun find-font (&rest args &key family slant weight spacing stretch)
  (declare (ignore family slant weight spacing stretch))
  (init)
  (with-attributes (attributes mandatory args)
    (with-foundation-object (descriptor (create-font-descriptor attributes))
      (let ((normalized (font-descriptor-create-matching-font-descriptor descriptor mandatory)))
        (unless (cffi:null-pointer-p normalized)
          (with-protection (unless (cffi:pointer-eq descriptor normalized)
                             (release normalized))
            (translate-descriptor normalized)))))))

(defun list-fonts (&rest args &key family slant weight spacing stretch)
  (declare (ignore family slant weight spacing stretch))
  (init)
  (with-attributes (attributes mandatory args)
    (with-foundation-object (descriptor (create-font-descriptor attributes))
      (with-foundation-object (array (font-descriptor-create-matching-font-descriptors descriptor mandatory) NIL)
        (loop for i from 0 below (array-get-count array)
              for normalized = (array-get-value-at-index array i)
              collect (translate-descriptor normalized))))))

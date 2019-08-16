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
    (setf *init* NIL)))

(defun translate-value (value table)
  (etypecase value
    (real   (or (first (find value table :key #'second))
                value))
    (symbol (or (second (find value table :key #'first))
                (error "No such name ~s." value)))))

(defun translate-weight (value)
  (translate-value
   value
   '((:thin        -1.0)
     (:extra-light -0.0)
     (:light       -0.0)
     (:semi-light  -0.0)
     (:book        -0.0)
     (:regular      0.0)
     (:medium      +0.0)
     (:semi-bold   +0.0)
     (:bold        +0.0)
     (:extra-bold  +0.0)
     (:black       +0.0)
     (:extra-black +1.0))))

(defun translate-slant (value)
  (translate-value
   value
   '((:roman    0.0)
     (:italic  +0.0)
     (:oblique +0.0))))

(defun translate-stretch (value)
  (translate-value
   value
   '((:ultra-condensed -1.0)
     (:extra-condensed -0.0)
     (:condensed       -0.0)
     (:semi-condensed  -0.0)
     (:normal           0.0)
     (:semi-expanded   +0.0)
     (:expanded        +0.0)
     (:extra-expanded  +0.0)
     (:ultra-expanded  +1.0))))

(defmacro with-foundation-object ((var init &optional fail) &body body)
  `(let ((,var ,init))
     (if (cffi:null-pointer-p ,var)
         ,(or fail `(error "The OS call failed:~%  ~s" ',init))
         (with-protection (release ,var)
           ,@body))))

(defun create-traits (weight stretch slant spacing)
  (let ((count 0))
    (cffi:with-foreign-objects ((keys :pointer 4)
                                (values :pointer 4))
      (flet (((setf entry) (value key)
               (setf (cffi:mem-aref keys count) key)
               (setf (cffi:mem-aref values count) value)
               (incf count)))
        (when weight
          (setf (entry weight-trait) (translate-weight weight)))
        (when stretch
          (setf (entry width-trait) (translate-stretch stretch)))
        (when slant
          (setf (entry slant-trait) (translate-slant slant)))
        (when (eq spacing :monospace)
          (setf (entry symbolic-trait) (cffi:foreign-enum-value 'symbolic-traits :monospace)))
        (create-dictionary (cffi:null-pointer) keys values count (cffi:null-pointer) (cffi:null-pointer))))))

(defun call-with-attributes (function &key family slant weight spacing stretch)
  (let ((count 0))
    (cffi:with-foreign-objects ((keys :pointer 2)
                                (values :pointer 2))
      (with-protection
          (loop for i from 0 below count
                do (release (cffi:mem-aref values i)))
        (when family
          (setf (cffi:mem-aref keys count) family-name-attribute)
          (setf (cffi:mem-aref values count) (string->cfstring family))
          (incf count))
        (when (or weight stretch slant spacing)
          (setf (cffi:mem-aref keys count) traits-attribute)
          (setf (cffi:mem-aref values count) (create-traits weight stretch slant spacing))
          (incf count))
        (with-foundation-object (dictionary (create-dictionary (cffi:null-pointer) keys values count (cffi:null-pointer) (cffi:null-pointer)))
          (with-foundation-object (set (create-set (cffi:null-pointer) keys count (cffi:null-pointer)))
            (funcall function dictionary set)))))))

(defmacro with-attributes ((attributes mandatory fontspec) &body body)
  `(apply #'call-with-attributes (lambda (,attributes ,mandatory) ,@body) ,fontspec))

(defun number-value (number type)
  (cffi:with-foreign-object (value type)
    (unless (number-get-value number type value)
      (error "Failed to retrieve numerical value."))
    (cffi:mem-ref value type)))

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
                                     (cfstring->string path)))
                           :family (with-attribute (family family-name-attribute)
                                     (cfstring->string family))
                           :slant (with-trait (slant slant-trait)
                                    (translate-slant (number-value slant :float)))
                           :weight (with-trait (weight weight-trait)
                                     (translate-weight (number-value weight :float)))
                           :spacing (with-trait (symbolic-traits symbolic-trait)
                                      (when (/= 0 (logand (cffi:foreign-enum-value 'symbolic-traits :monospace)
                                                          (number-value symbolic-traits :int32)))
                                        :monospace))
                           :stretch (with-trait (stretch width-trait)
                                      (translate-stretch (number-value stretch :float)))))))

(defun find-font (&rest args &key family slant weight spacing stretch)
  (declare (ignore family slant weight spacing stretch))
  (init)
  (with-attributes (attributes mandatory args)
    (with-foundation-object (descriptor (create-font-descriptor attributes))
      (let ((normalized (font-descriptor-create-matching-font-descriptor descriptor mandatory)))
        (with-protection (unless (cffi:pointer-eq descriptor normalized)
                           (release normalized))
          (translate-descriptor normalized))))))

(defun list-fonts (&rest args &key family slant weight spacing stretch)
  (declare (ignore family slant weight spacing stretch))
  (init)
  (with-attributes (attributes mandatory args)
    (with-foundation-object (descriptor (create-font-descriptor attributes))
      (with-foundation-object (array (font-descriptor-create-matching-font-descriptors descriptor mandatory))
        (loop for i from 0 below (array-get-count array)
              for normalized = (array-get-value-at-index array i)
              collect (translate-descriptor normalized)
              do (unless (cffi:pointer-eq descriptor normalized)
                   (release normalized)))))))

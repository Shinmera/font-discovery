#|
 This file is a part of font-discovery
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.font-discovery)

(defclass font ()
  ((file :initarg :file :initform (error "FILE required.") :reader file)
   (family :initarg :family :initform (error "FAMILY required.") :reader family)
   (slant :initarg :slant :initform :roman :reader slant)
   (weight :initarg :weight :initform :regular :reader weight)
   (spacing :initarg :spacing :initform :proportional :reader spacing)
   (stretch :initarg :stretch :initform :normal :reader stretch)))

(defmethod print-object ((font font) stream)
  (print-unreadable-object (font stream :type T)
    (format stream "~s ~a ~a ~a" (family font) (slant font) (weight font) (stretch font))))

(defun maybe-enum (type thing)
  (etypecase thing
    (keyword (cffi:foreign-enum-value type thing))
    (integer thing)))

(defun maybe-enum-val (type thing)
  (or (cffi:foreign-enum-keyword type thing :errorp NIL)
      thing))

(defmacro with-protection (unwind &body protected)
  `(unwind-protect (progn ,@protected) ,unwind))

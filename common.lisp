(in-package #:org.shirakumo.font-discovery)

(defvar *backends* ())

(defclass backend ()
  ())

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

(defun parse-file (string)
  (pathname-utils:parse-native-namestring string))

(defgeneric init* (backend))
(defgeneric refresh* (backend))
(defgeneric deinit* (backend))
(defgeneric find-font* (backend &key family slant weight spacing stretch))
(defgeneric list-fonts* (backend &key family slant weight spacing stretch))

(defun register-backend (type)
  (or (find type *backends* :key #'type-of)
      (let ((backend (make-instance type)))
        (push backend *backends*)
        backend)))

(defun init ()
  (mapc #'init* *backends*))

(defun refresh ()
  (init)
  (mapc #'refresh* *backends*))

(defun deinit ()
  (mapc #'deinit* *backends*))

(defun find-font (&rest args &key family slant weight spacing stretch)
  (declare (ignore family slant weight spacing stretch))
  (init)
  (loop for backend in *backends*
        thereis (apply #'find-font* backend args)))

(defun list-fonts (&rest args &key family slant weight spacing stretch)
  (declare (ignore family slant weight spacing stretch))
  (init)
  (loop for backend in *backends*
        nconc (apply #'list-fonts* backend args)))

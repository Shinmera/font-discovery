#|
 This file is a part of font-discovery
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.font-discovery)


(defun init (&key reinit)
  )

(defun deinit ()
  )

(defun find-font (&rest args &key family slant weight size spacing width)
  (declare (ignore family slant weight size spacing width))
  (init)
  )

(defun list-fonts (&rest args &key family slant weight size spacing width)
  (declare (ignore family slant weight size spacing width))
  (init)
  )

#|
 This file is a part of font-discovery
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.font-discovery
  (:use #:cl)
  (:export
   #:init
   #:deinit
   #:find-font
   #:list-fonts))

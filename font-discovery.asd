#|
 This file is a part of font-discovery
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem font-discovery
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Find system font files matching a font spec."
  :homepage "https://github.com/Shinmera/font-discovery"
  :serial T
  :components ((:file "package")
               (:file "common")
               (:file "fontconfig" :if-feature :linux)
               (:file "linux" :if-feature :linux)
               (:file "coretext" :if-feature :darwin)
               (:file "macos" :if-feature :darwin)
               (:file "directwrite" :if-feature :win32)
               (:file "windows" :if-feature :win32)
               (:file "documentation"))
  :depends-on (:documentation-utils
               :trivial-features
               :trivial-indent
               :cffi))

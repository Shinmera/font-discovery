(asdf:defsystem font-discovery
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Find system font files matching a font spec."
  :homepage "https://shinmera.com/project/font-discovery"
  :serial T
  :defsystem-depends-on (:trivial-features)
  :components ((:file "package")
               (:file "common")
               (:file "generic")
               (:file "fontconfig" :if-feature (:or :linux (:and :bsd (:not :darwin))))
               (:file "linux" :if-feature (:or :linux (:and :bsd (:not :darwin))))
               (:file "coretext" :if-feature :darwin)
               (:file "macos" :if-feature :darwin)
               (:file "directwrite" :if-feature :win32)
               (:file "windows" :if-feature :win32)
               (:file "documentation"))
  :depends-on (:documentation-utils
               :pathname-utils
               :trivial-indent
               :zpb-ttf
               (:feature :windows :com-on)
               :cffi))

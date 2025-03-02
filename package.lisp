(defpackage #:org.shirakumo.font-discovery
  (:use #:cl)
  #+win32 (:local-nicknames (#:com #:org.shirakumo.com-on))
  (:export
   #:font
   #:file
   #:family
   #:slant
   #:weight
   #:size
   #:spacing
   #:stretch
   #:init
   #:refresh
   #:deinit
   #:find-font
   #:list-fonts
   #:*system-font-search-paths*
   #:*font-search-paths*))

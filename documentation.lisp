#|
 This file is a part of font-discovery
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.font-discovery)

;; common.lisp
(docs:define-docs
  (type font
    "A representation of a system font and its font attributes.

See FILE
See FAMILY
See SLANT
See WEIGHT
See SPACING
See STRETCH")

  (function file
    "Returns the pathname to the font file.

This may be a TTF or OTF file.

See FONT")

  (function family
    "Returns the font family name.

See FONT")

  (function slant
    "Returns the slant of the font.

This may be one of the following keywords, in order of increasing
slant, or a system-dependent numeric value for the font's slant.

  :ROMAN
  :ITALIC
  :OBLIQUE

See FONT")

  (function weight
    "Returns the weight of the font.

This may be one of the following keywords, in order of increasing
weight, or a system-dependent numeric value for the font's weight.

  :THIN
  :EXTRA-LIGHT
  :LIGHT
  :SEMI-LIGHT
  :BOOK
  :REGULAR
  :MEDIUM
  :SEMI-BOLD
  :BOLD
  :EXTRA-BOLD
  :BLACK
  :EXTRA-BLACK

See FONT")

  (function spacing
    "Returns the font's spacing.

This may be one of the following keywords, in order of increasing
spacing, or a system-dependent numeric value for the font's spacing.

  :PROPORTIONAL
  :DUALSPACE
  :MONOSPACE
  :CHARCELL

See FONT")

  (function stretch
    "Returns the font's stretch factor.

This may be one of the following keywords, in order of increasing
stretch factor, or a system-dependent numeric value for the font's
stretch factor.

 :ULTRA-CONDENSED
 :EXTRA-CONDENSED
 :CONDENSED
 :SEMI-CONDENSED
 :NORMAL
 :SEMI-EXPANDED
 :EXPANDED
 :EXTRA-EXPANDED
 :ULTRA-EXPANDED

See FONT"))

;; interface
(docs:define-docs
  (function init
    "Initializes the library for use.

This will load foreign libraries and set up internal state if
necessary. Returns T if it did so.

It is safe to call this function multiple times.

See REFRESH
See DEINIT")

  (function refresh
    "Refreshes the font cache.

This should cause new system fonts to be recognised. You should call
this function whenever you think the list of system fonts might have
changed. Returns T if the cache was updated.

Doing this will automatically call INIT.

See INIT")

  (function deinit
    "Uninitializes the library.

This will shut down foreign libraries and deallocate cache memory.
Returns T if it did so.

It will not close the foreign libraries that were opened by INIT.

It is safe to call this function multiple times.

See INIT")

  (function find-font
    "Attempts to find a font with specifications that match the query as close as possible.

Returns a FONT instance, or NIL if no match can be found.
Note that on some systems the returned font may not be an exact
match, but rather a closest approximation.

For the possible values of the arguments, see the FONT class and its
reader functions.

Doing this will automatically call INIT.

See LIST-FONTS
See INIT
See FONT")

  (function list-fonts
    "Returns a list of fonts with specifications that match the query.

Returns a list of FONT instances that match the query.

For the possible values of the arguments, see the FONT class and its
reader functions.

Doing this will automatically call INIT.

See FIND-FONT
See INIT
See FONT"))

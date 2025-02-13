(in-package #:org.shirakumo.font-discovery)

(defvar *font-search-paths*
  (remove NIL
          (list 
           #+(and unix (not darwin)) #p"/usr/share/fonts/"
           #+(and unix (not darwin)) #p"/usr/local/share/fonts/"
           #+(and unix (not darwin)) #p"~/.local/share/fonts/"
           #+(and unix (not darwin)) #p"~/.fonts/"
           #+(and unix (not darwin)) (let ((path (pathname-utils::getenv "XDG_DATA_HOME")))
                                       (when (and path (string/= "" path))
                                         (pathname-utils:parse-native-namestring path)))
           #+darwin #p"/System/Library/Fonts/"
           #+darwin #p"/Library/Fonts/"
           #+darwin #p"~/Library/Fonts/"
           #+windows #p"%WINDIR%/Fonts"
           #+windows #p"%USERPROFILE%/AppData/Local/Microsoft/Windows/Fonts")))

(defclass generic (backend)
  ((registry :initform NIL :accessor registry)))

(register-backend 'generic)

(defmethod init* ((backend generic))
  (unless (registry backend)
    (refresh* backend))
  T)

(defmethod refresh* ((backend generic))
  (let ((fonts (make-array 0 :adjustable T :fill-pointer T)))
    (loop for dir in *font-search-paths*
          do (discover-fonts dir fonts))
    (setf (registry backend) fonts))
  T)

(defmethod deinit* ((backend generic))
  (setf (registry backend) NIL)
  T)

(defmethod find-font* ((backend generic) &rest args &key &allow-other-keys)
  (let ((max NIL) (max-score 0))
    (loop for font across (registry backend)
          for score = (apply #'match-font font args)
          do (when (and score (< max-score score))
               (setf max font max-score score)))
    max))

(defmethod list-fonts* ((backend generic) &rest args &key &allow-other-keys)
  (mapcar #'car (sort (loop for font across (registry backend)
                            for score = (apply #'match-font font args)
                            when score collect (cons font score))
                      #'> :key #'cdr)))

(defun match-font (font &key family slant weight spacing stretch)
  (if (or family slant weight spacing stretch)
      (let ((score 0))
        (flet ((score (value ref)
                 (when (and value ref)
                   (cond ((equal value ref)
                          (incf score 100))
                         ((and (realp value) (realp ref))
                          (incf score (max 0 (- 100 (abs (- value ref))))))))))
          (when family
            (cond ((string-equal family (family font))
                   (incf score 1000))
                  ((search family (family font) :test #'char-equal)
                   (incf score 500))))
          (score slant (slant font))
          (score weight (weight font))
          (score spacing (spacing font))
          (score stretch (stretch font)))
        (when (< 0 score)
          score))
      1))

(defun discover-fonts (dir &optional (fonts (make-array 0 :adjustable T :fill-pointer T)))
  (dolist (file (directory (make-pathname :name pathname-utils:*wild-component* :type "ttf" :defaults dir)))
    (handler-case
        (zpb-ttf:with-font-loader (font file)
          (vector-push-extend
           (make-instance 'font
                          :file file
                          :family (zpb-ttf:family-name font)
                          :slant ()
                          :weight ()
                          :stretch ()
                          :spacing (if (zpb-ttf:fixed-pitch-p font)
                                       :monospace
                                       :proportional))
           fonts))
      (condition ())))
  (dolist (dir (directory (merge-pathnames pathname-utils:*wild-directory* dir)) fonts)
    (discover-fonts dir fonts)))

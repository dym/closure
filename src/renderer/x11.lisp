;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: WS/X11; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: X11 specific stuff
;;;   Created: 1998-11-11
;;;    Author: Gilbert Baumann <gilbert@base-engineering.com>
;;;   License: MIT style (see below)
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1998,1999 by Gilbert Baumann

;;;  Permission is hereby granted, free of charge, to any person obtaining
;;;  a copy of this software and associated documentation files (the
;;;  "Software"), to deal in the Software without restriction, including
;;;  without limitation the rights to use, copy, modify, merge, publish,
;;;  distribute, sublicense, and/or sell copies of the Software, and to
;;;  permit persons to whom the Software is furnished to do so, subject to
;;;  the following conditions:
;;; 
;;;  The above copyright notice and this permission notice shall be
;;;  included in all copies or substantial portions of the Software.
;;; 
;;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. 
;;;  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;  SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Changes

;;; 1999-08-11  GB      REAL-BITS-PER-RGB - new function
;;;                     PIXEL-TRANSLATOR-CODE uses it.

(in-package :WS/X11)

;;;; --------------------------------------------------------------------------
;;;;  Pixel translation
;;;;

(defparameter *dither-threshold* 4)
(defparameter *code-optimization*
    '#.cl-user:+optimize-very-fast-trusted+)

(defvar *dither-map*
  '#2A((0    192   48  240   12  204   60  252)
       (128   64  176  112  140   76  188  124)
       (32   224   16  208   44  236   28  220)
       (160   96  144   80  172  108  156   92) 
       (8    200   56  248    4  196   52  244) 
       (136   72  184  120  132   68  180  116)
       (40   232   24  216   36  228   20  212) 
       (168  104  152   88  164  100  148   84)))

(defun colormap-plist (colormap)
  (cdr (assoc colormap
              (getf (xlib:display-plist (xlib:colormap-display colormap))
                    'colormap-plisten)
              :test #'xlib:colormap-equal)))

(defun (setf colormap-plist) (value colormap)
  (let ((x (assoc colormap
                  (getf (xlib:display-plist (xlib:colormap-display colormap))
                        'colormap-plisten)
                  :test #'xlib:colormap-equal)))
    (if x 
        (setf (cdr x) value)
      (push (cons colormap value) 
            (getf (xlib:display-plist (xlib:colormap-display colormap))
                  'colormap-plisten)))))

;; static color is merkwürdig, denn es
;; hat red/green/blue masken

(defun real-bits-per-rgb (vi)
  (let ((n (min (logcount (xlib:visual-info-red-mask vi))
                (logcount (xlib:visual-info-green-mask vi))
                (logcount (xlib:visual-info-blue-mask vi)))))
    (cond ((zerop n)
           (xlib:visual-info-bits-per-rgb vi))
          (t
           n))))

(defun pixel-translator-code (colormap)
  (or (getf (colormap-plist colormap) 'pixel-translator-code)
      (setf (getf (colormap-plist colormap) 'pixel-translator-code)
        (let ((vi (xlib:colormap-visual-info colormap)))
          (case (xlib:visual-info-class vi)
            ((:static-gray)
             (cond ((<= (real-bits-per-rgb vi) *dither-threshold*)
                    ;;(warn "Static gray dithered")
                    (static-gray-ditherer colormap))
                   (t
                    ;;(warn "Static gray")
                    (static-gray-translator colormap))))
            ((:true-color)
             (cond ((<= (real-bits-per-rgb vi) *dither-threshold*)
                    ;;(warn "True color dithered")
                    (true-color-ditherer colormap))
                   (t
                    ;;(warn "True color")
                    (true-color-translator colormap))))
            ((:pseudo-color)
             ;; (warn "Pseudo color")
             ;; XXX -- we need a better guess for the size of the cube
             (or
              (rgb-cube-ditherer colormap)
              (fallback-b/w-ditherer colormap)))
            (otherwise
             (warn "Weird visual class -- falling back to black and white dithering.")
             (fallback-b/w-ditherer colormap)) )) )))

(defun pixel-translator (colormap)
  (or (getf (colormap-plist colormap) 'pixel-translator)
      (setf (getf (colormap-plist colormap) 'pixel-translator)
        (compile nil (pixel-translator-code colormap)))))

(defun component-deposition-expr (component-expr dest-byte ramp linearp
                                  &optional (shifted-p nil))
  (let ((value-map (if linearp 
                       `(lambda (x) (dpb x (byte ,(byte-size dest-byte)
                                                 ,(byte-position dest-byte))
                                         0))
                     `(lambda (x) (aref (the ,(type-of ramp) ',ramp) x)))))
    (let ((n (byte-size dest-byte)))
      (assert (<= n 8))
      `(the (unsigned-byte ,(+ (byte-position dest-byte) (byte-size dest-byte)))
         (,value-map ,(if shifted-p 
                          component-expr 
                        `(the (unsigned-byte ,n) 
                           (ldb (byte ,n ,(- 8 n)) 
                                ,component-expr))))))))

(defun static-gray-translator (colormap)
  (let ((gray-byte (visual-info-gray-byte (xlib:colormap-visual-info colormap))))
    (multiple-value-bind (ramp linearp) (allocate-gray-ramp colormap)
      `(lambda (x y sample)
         (declare (ignore x y)
                  (type (unsigned-byte 24) sample)
                  ,*code-optimization*)
         ,(component-deposition-expr '(luminance sample) gray-byte ramp linearp)))))
      
(defun static-gray-ditherer (colormap)
  (let* ((gray-byte (visual-info-gray-byte (xlib:colormap-visual-info colormap)))
         (n (ash 1 (byte-size gray-byte))))
    (multiple-value-bind (ramp linearp) (allocate-gray-ramp colormap)
      `(lambda (x y sample)
         (declare (type (unsigned-byte 24) sample)
                  ,*code-optimization*)
         ,(component-deposition-expr `(the (integer 0 (,n))
                                        (,(generic-ditherer n) x y (luminance sample)))
                                     gray-byte 
                                     ramp linearp
                                     t)))))

(defun identity-mapping-p (vector)
  (dotimes (i (length vector) t)
    (unless (eql (aref vector i) i)
      (return nil))))

(defun luminance (sample)
  (declare (type (unsigned-byte 24) sample))
  (floor (the (unsigned-byte 18)
           (+ (* 307 (the (unsigned-byte 8) (ldb (byte 8 0) sample)))           ;red
              (* 599 (the (unsigned-byte 8) (ldb (byte 8 8) sample)))           ;green
              (* 118 (the (unsigned-byte 8) (ldb (byte 8 16) sample)))))        ;blue
         1024))

(define-compiler-macro luminance (sample)
  `((lambda (sample)
      (the (unsigned-byte 8)
        (floor (the (unsigned-byte 18)
                 (+ (* 307 (the (unsigned-byte 8) (ldb (byte 8 0) sample)))     ;red
                    (* 599 (the (unsigned-byte 8) (ldb (byte 8 8) sample)))     ;green
                    (* 118 (the (unsigned-byte 8) (ldb (byte 8 16) sample)))))  ;blue
               1024)))
    ,sample))

(defun generic-ditherer (m)
  `(lambda (x y s)
     (multiple-value-bind (c0 delta)
         (floor (the (integer 0 ,(* (1- m) 255)) (* ,(1- m) s)) 255)
       (declare (type (unsigned-byte 8) delta)
                (type (integer 0 (,m)) c0))
       (if (<= (the (unsigned-byte 8) delta) 
               (the (unsigned-byte 8)
                 (aref *dither-map*
                       (logand x #x7)
                       (logand y #x7))))
           c0
         (+ c0 1)))))

(defun true-color-translator (colormap)
  (multiple-value-bind (red-ramp red-linear) (allocate-component-ramp colormap :red)
    (multiple-value-bind (green-ramp green-linear) (allocate-component-ramp colormap :green)
      (multiple-value-bind (blue-ramp blue-linear) (allocate-component-ramp colormap :blue)
        `(lambda (x y sample)
           (declare (ignore x y)
                    (type (unsigned-byte 24) sample)
                    ,*code-optimization*)
           (logior ,(component-deposition-expr
                     '(the (unsigned-byte 8) (ldb (byte 8 0) sample))
                     (mask->byte
                      (xlib:visual-info-red-mask
                       (xlib:colormap-visual-info colormap)))
                     red-ramp
                     red-linear)
                   ,(component-deposition-expr
                     '(the (unsigned-byte 8) (ldb (byte 8 8) sample))
                     (mask->byte
                      (xlib:visual-info-green-mask
                       (xlib:colormap-visual-info colormap)))
                     green-ramp
                     green-linear)
                   ,(component-deposition-expr
                     '(the (unsigned-byte 8) (ldb (byte 8 16) sample))
                     (mask->byte
                      (xlib:visual-info-blue-mask
                       (xlib:colormap-visual-info colormap)))
                     blue-ramp
                     blue-linear)))))))

(defun allocate-component-ramp (colormap component)
  (let ((byte (mask->byte
               (ecase component
                 (:red   (xlib:visual-info-red-mask (xlib:colormap-visual-info colormap)))
                 (:green (xlib:visual-info-green-mask (xlib:colormap-visual-info colormap)))
                 (:blue  (xlib:visual-info-blue-mask (xlib:colormap-visual-info colormap)))))))
    (let ((res (make-array (ash 1 (byte-size byte)) :element-type '(unsigned-byte 32)))
          (linearp t))
      (dotimes (i (ash 1 (byte-size byte)))
        (let ((color (case component
                       (:red (xlib:make-color :red (/ i (1- (ash 1 (byte-size byte)))) :green 0 :blue 0))
                       (:green (xlib:make-color :green (/ i (1- (ash 1 (byte-size byte)))) :red 0 :blue 0))
                       (:blue (xlib:make-color :blue (/ i (1- (ash 1 (byte-size byte)))) :red 0 :green 0)))))
          (let ((pixel (xlib:alloc-color colormap color))
                (naiv (dpb i byte 0)))
            (when (/= naiv pixel)
              (setf linearp nil))
            (setf (aref res i) pixel))))
      (values
       res
       linearp))))

(defun allocate-gray-ramp (colormap)
  (let ((byte (visual-info-gray-byte (xlib:colormap-visual-info colormap))))
    (let ((linearp t)
          (res (make-array (ash 1 (byte-size byte)) :element-type '(unsigned-byte 32))))
      (dotimes (i (ash 1 (byte-size byte)))
        (let ((color (xlib:make-color :red   (/ i (1- (ash 1 (byte-size byte))))
                                      :green (/ i (1- (ash 1 (byte-size byte))))
                                      :blue  (/ i (1- (ash 1 (byte-size byte)))) )))
          (let ((pixel (xlib:alloc-color colormap color))
                (naiv (dpb i byte 0)))
            (when (/= naiv pixel)
              (setf linearp nil))
            (setf (aref res i) pixel))))
      (values res linearp))))

(defun visual-info-gray-byte (vi)
  (let ((m (integer-length (1- (xlib:visual-info-colormap-entries vi))))
        (n (xlib:visual-info-bits-per-rgb vi)))
    (byte n (- m n))))

(defun true-color-ditherer (colormap)
  (multiple-value-bind (red-ramp red-linear) (allocate-component-ramp colormap :red)
    (multiple-value-bind (green-ramp green-linear) (allocate-component-ramp colormap :green)
      (multiple-value-bind (blue-ramp blue-linear) (allocate-component-ramp colormap :blue)
        (let ((rm (xlib:visual-info-red-mask (xlib:colormap-visual-info colormap)))
              (gm (xlib:visual-info-green-mask (xlib:colormap-visual-info colormap)))
              (bm (xlib:visual-info-blue-mask (xlib:colormap-visual-info colormap))))
          (let ((nr (ash 1 (byte-size (mask->byte rm))))
                (nb (ash 1 (byte-size (mask->byte bm))))
                (ng (ash 1(byte-size (mask->byte gm)))))
            `(lambda (x y sample)
               (declare (type (unsigned-byte 24) sample)
                        ,*code-optimization*)
               (logior ,(component-deposition-expr 
                         `(the (integer 0 (,nr))
                            (,(generic-ditherer nr) x y (ldb (byte 8 0) sample)))
                         (mask->byte rm)
                         red-ramp
                         red-linear
                         t)
                       ,(component-deposition-expr 
                         `(the (integer 0 (,ng))
                            (,(generic-ditherer ng) x y (ldb (byte 8 8) sample)))
                         (mask->byte gm)
                         green-ramp
                         green-linear
                         t)
                       ,(component-deposition-expr 
                         `(the (integer 0 (,nb))
                            (,(generic-ditherer nb) x y (ldb (byte 8 16) sample)))
                         (mask->byte bm)
                         blue-ramp
                         blue-linear
                         t)))))))))


(defparameter *colormap-niceness-max* 20
  "When allocating many colors, maximum number of colors to leave free for other applications. 
   see *COLORMAP-NICENESS-RATIO*.")
  
(defparameter *colormap-niceness-ratio* 1/10
  "When allocating many colors, ratio of available colors to leave free for other applications.
   see *COLORMAP-NICENESS-MAX*.")

#||
(defun allocate-rgb-cube (colormap)
  "Allocates a RGB cube using the colormap 'colormap';
   Returns NIL, if not enough colors could be allocated.
   On success returns a three dimensional array of the allocated pixel values."
  (let* ((pixels (allocate-all-available-pixels colormap))
         (m      (length pixels))) 
    ;; Be nice to our neighbor and leave some colors available.
    (let ((nice (min m *colormap-niceness-max*
                     (floor (* m *colormap-niceness-ratio*)))))
      (xlib:free-colors colormap (subseq pixels (- (length pixels) nice)))
      (setf pixels (butlast pixels nice))
      (setf m (length pixels))
      (let ((d (floor (expt m 1/3))))
        (cond ((< d 2)
               ;; no useful RGB cube to allocate
               (xlib:free-colors colormap pixels)
               nil)
              (t
               (let ((needed (expt d 3))
                     (cube (make-array (list d d d) :element-type '(unsigned-byte 32) :initial-element 0)))
                 ;; return what we don't need
                 (xlib:free-colors colormap (subseq pixels needed))
                 (setf pixels (subseq pixels 0 needed))
                 ;; now actually allocate the cube
                 (dotimes (red d)
                   (dotimes (green d)
                     (dotimes (blue d)
                       (let ((pixel (pop pixels)))
                         (xlib:store-color colormap pixel
                                     (xlib:make-color :red   (/ red   (1- d))
                                                      :green (/ green (1- d))
                                                      :blue  (/ blue  (1- d))))
                         (setf (aref cube red green blue) pixel)))))
                 cube)) )))))
||#

(defun color-difference (c1 c2)
  (+ (expt (- (xlib:color-red c1) (xlib:color-red c2)) 2)
     (expt (- (xlib:color-green c1) (xlib:color-green c2)) 2)
     (expt (- (xlib:color-blue c1) (xlib:color-blue c2)) 2)))

(defun allocate-rgb-cube (colormap)
  ;; We'd better grab the server, so nobody else can choose to
  ;; allocate colors we have.
  (let* ((my (allocate-all-available-pixels colormap))
         (their (set-difference (loop for i from 0 below (xlib:visual-info-colormap-entries
                                                          (xlib:colormap-visual-info colormap))
                                    collect i)
                                my))
         (their-colors
          (mapcar #'list (xlib:query-colors colormap their)
                  their)))
    (unwind-protect
        (progn
          ;; Now for each desired color value find the best match
          (let* ((d 6)
                 (cube (make-array (list d d d)))
                 nice)
            (dotimes (red d)
              (dotimes (green d)
                (dotimes (blue d)
                  (let ((needed (xlib:make-color :red   (/ red   (1- d))
                                                 :green (/ green (1- d))
                                                 :blue  (/ blue  (1- d)))))
                    (let ((best-match (first their-colors))
                          (best-delta (color-difference needed (first (first their-colors)))))
                      (dolist (x their-colors)
                        (let ((delta (color-difference (first x) needed)))
                          (when (< delta best-delta)
                            (setf best-match x
                                  best-delta delta))))
                      (setf (aref cube red green blue) (list best-match best-delta needed)))))))
            ;; Be nice to our neighbor and leave some colors available.
            (setf nice (min (length my)
                            *colormap-niceness-max*
                            (floor (* (length my) *colormap-niceness-ratio*))))
            (xlib:free-colors colormap (subseq my (- (length my) nice)))
            (setf my (butlast my nice))
            (warn "We have ~D colors on our own!" (length my))
            ;; Then pick the m worst matches and replace them by private colors
            (dotimes (k (length my))
              (let ((worst-delta (second (aref cube 0 0 0)))
                    (worst-index (list 0 0 0)))
                (dotimes (red d)
                  (dotimes (green d)
                    (dotimes (blue d)
                      (when (> (second (aref cube red green blue))
                               worst-delta)
                        (setf worst-index (list red green blue)
                              worst-delta (second (aref cube red green blue)))))))
                (destructuring-bind (bm bd nd) (apply #'aref cube worst-index)
                  (setf (apply #'aref cube worst-index)
                    (list (list nd (elt my k))
                          0
                          nd)))))
            ;;
            (let ((res (make-array (list d d d) :element-type '(unsigned-byte 32) :initial-element 0)))
              (dotimes (r d)
                (dotimes (g d)
                  (dotimes (b d)
                    (setf (aref res r g b)
                      (cond ((member (second (first (aref cube r g b))) my)
                             (xlib:store-color colormap 
                                               (second (first (aref cube r g b)))
                                               (first (first (aref cube r g b))))
                             (second (first (aref cube r g b))))
                            (t
                             ;; We need to allocate them never the less
                             (xlib:alloc-color colormap (first (first (aref cube r g b))))))))))
              (setq my nil)
              res)))
      (xlib:free-colors colormap my) )))

(defun number-of-available-colors (colormap)
  ;; This does a silly search to find the number of available colors
  ;; in the color map 'colormap'.
  (let ((x 0))                           ;number of colors to allocate
    (loop
        for i
        from (ceiling (log (xlib:visual-info-colormap-entries (xlib:colormap-visual-info colormap))
                           2))
        downto 0
        do (let ((d (expt 2 i)))
             (let ((pixels nil))
               (unwind-protect
                   (progn
                     (setf pixels (ignore-errors (xlib:alloc-color-cells colormap (+ x d))))
                     (when (not (null pixels))
                       (incf x d)))
                 (when (not (null pixels))
                   (xlib:free-colors colormap pixels))))))
    x))

(defun allocate-all-available-pixels (colormap)
  "Given a colormap allocate all available color cells; returns a list of allocated pixels."
  (let ((pixels nil))
    (loop for i
        from (ceiling (log (xlib:visual-info-colormap-entries (xlib:colormap-visual-info colormap))
                           2))
        downto 0
        do
          (setf pixels (append pixels (ignore-errors (xlib:alloc-color-cells colormap (expt 2 i))))))
    pixels))

(defun rgb-cube-ditherer (colormap)
  "Returns an RGB cube ditherer; If there are not enough available colors NIL is returned."
  (let ((cube (allocate-rgb-cube colormap)))
    (and cube
         `(lambda (x y sample)
            (declare (type (unsigned-byte 16) x y)
                     (type (unsigned-byte 24) sample)
                     ,*code-optimization*)
            (aref (the ,(type-of cube) ',cube)
                  (,(generic-ditherer (array-dimension cube 0)) x y (ldb (byte 8 0) sample))
                  (,(generic-ditherer (array-dimension cube 1)) x y (ldb (byte 8 8) sample))
                  (,(generic-ditherer (array-dimension cube 2)) x y (ldb (byte 8 16) sample)))))))

(defun fallback-b/w-ditherer (colormap)
  (let ((black (xlib:alloc-color colormap (xlib:make-color :red 0 :blue 0 :green 0)))
        (white (xlib:alloc-color colormap (xlib:make-color :red 1 :blue 1 :green 1))))
    `(lambda (x y sample)
       (declare (type (unsigned-byte 24) sample)
                ,*code-optimization*)
       (if (zerop (,(generic-ditherer 2) x y (luminance sample)))
           ,black
         ,white)) ))

(defun mask->byte (mask)
  (let ((h (integer-length mask)))
    (let ((l (integer-length (logxor mask (1- (ash 1 h))))))
      (byte (- h l) l))))

;;;; ==========================================================================================

(defun make-ximage-for-aimage (aimage depth translator)
  #+EXCL (declare (:explain :calls))
  (let* ((width (imagelib:aimage-width aimage))
         (height (imagelib:aimage-height aimage))
         (idata (imagelib:aimage-data aimage))
	 ;; FIXME: this (and the :BITS-PER-PIXEL, below) is a hack on
	 ;; top of a hack.  At some point in the past, XFree86 and/or
	 ;; X.org decided that they would no longer support pixmaps
	 ;; with 24 bpp, which seems to be what most AIMAGEs want to
	 ;; be.  For now, force everything to a 32-bit pixmap.
         (xdata (make-array (list height width) :element-type '(unsigned-byte 32)))
         (ximage (xlib:create-image :width  width
                                    :height height
                                    :depth  depth
				    :bits-per-pixel 32
                                    :data   xdata)))
    (declare (type (simple-array (unsigned-byte 32) (* *)) idata)
             #+NIL(type (simple-array (unsigned-byte 8) (* *)) xdata)
             )
    (loop for x fixnum from 0 below width do
      (loop for y fixnum from 0 below height do
        (setf (aref xdata y x) 
          (funcall translator x y (ldb (byte 24 0) (aref idata y x))))))
    ximage))

(defun ximage-translator** (window)
  (ximage-translator* (pixel-translator-code (xlib:window-colormap window))
                      (xlib:drawable-depth window)))

(defun ximage-translator* (tr depth)
  `(lambda (aimage)
     (declare (type imagelib:aimage aimage)
              (:explain :calls)
              ,*code-optimization*)
     (let* ((width (imagelib:aimage-width aimage))
            (height (imagelib:aimage-height aimage))
            (idata (imagelib:aimage-data aimage))
            (xdata (make-array (list height width) :element-type '(unsigned-byte ,depth)))
            (ximage (xlib:create-image :width  width
                                       :height height
                                       :depth  ,depth
                                       :data   xdata)))
       (declare (type fixnum width height)
                (type (simple-array (unsigned-byte ,depth) (* *)) xdata)
                (type (simple-array (unsigned-byte 32) (* *)) idata))
       (do ((y (1- height) (- y 1)))
           ((< y 0))
         (do ((x (1- width) (- x 1)))
             ((< x 0))
           (setf (aref xdata (the (unsigned-byte 16) y) (the (unsigned-byte 16) x))
             (,tr x y (ldb (byte 24 0)
                           (aref idata
                                 (the (unsigned-byte 16) y)
                                 (the (unsigned-byte 16) x)))))))
       ximage)))

(defun ximage-translator* (tr depth)
  `(lambda (aimage)
     (declare (type imagelib:aimage aimage)
              (:explain :calls)
              ,*code-optimization*)
     (let* ((width  (imagelib:aimage-width aimage))
            (height (imagelib:aimage-height aimage))
            (idata  (imagelib:aimage-data aimage))
            (xdata  (make-array (list height width) :element-type '(unsigned-byte ,depth)))
            (ximage (xlib:create-image :width  width
                                       :height height
                                       :depth  ,depth
                                       :data   xdata)))
       (declare (type fixnum width height)
                (type (simple-array (unsigned-byte ,depth) (* *)) xdata)
                (type (simple-array (unsigned-byte 32) (* *)) idata))
       (let ((i (1- (* width height))))
         (declare (type fixnum i))
         (loop for y fixnum from (1- height) downto 0 do
               (loop for x fixnum from (1- width) downto 0 do
                     (setf (row-major-aref (the (simple-array (unsigned-byte ,depth) (* *)) xdata)
                                           (the fixnum i))
                       (,tr x y (the (unsigned-byte 24)
                                  (ldb (byte 24 0)
                                       (row-major-aref (the (simple-array (unsigned-byte 32) (* *)) idata)
                                                       (the fixnum i))))))
                     (decf i))))
       ximage)))

(defun ximage-translator (window)
  (or (getf (colormap-plist (xlib:window-colormap window)) 'ximage-translator)
      (setf (getf (colormap-plist (xlib:window-colormap window)) 'ximage-translator)
        (compile nil (ximage-translator** window)))))

#+NIL ;; not yet trusted
(defun aimage->ximage (drawable aimage)
  (funcall (ximage-translator drawable) aimage))

(defun aimage->ximage (drawable aimage)
  (make-ximage-for-aimage aimage
                          (xlib:drawable-depth drawable) 
                          (pixel-translator (xlib:window-colormap drawable))))

(defun make-mask-from-aimage (drawable aim)
  (let* ((width (imagelib:aimage-width aim))
         (height (imagelib:aimage-height aim))
         (bitmap (xlib:create-pixmap :drawable drawable
                                     :width width 
                                     :height height
                                     :depth 1))
         (gc (xlib:create-gcontext :drawable bitmap :foreground 1 :background 0))
         (idata (imagelib:aimage-data aim))
         (xdata (make-array (list height width) :element-type '(unsigned-byte 1)))
         (im (xlib:create-image :width width
                                :height height
                                :depth 1
                                :data xdata)) )
    (dotimes (y width)
      (dotimes (x height)
        (if (> (aref idata x y) #x80000000)
            (setf (aref xdata x y) 0)
          (setf (aref xdata x y) 1))))
    (unless (or (>= width 2048) (>= height 2048)) ;### CLX breaks here
      (xlib:put-image bitmap gc im :src-x 0 :src-y 0 :x 0 :y 0 :width width :height height
		      :bitmap-p nil))
    (xlib:free-gcontext gc)
    bitmap))

;;;; --------------------------------------------------------------------------
;;;;  colours
;;;;

(defparameter *color-names*
    '(("black"   . "#000000")
      ("green"   . "#008000")
      ("silver"  . "#C0C0C0")
      ("lime"    . "#00FF00")
      ("gray"    . "#808080")
      ("olive"   . "#808000")
      ("white"   . "#FFFFFF")
      ("yellow"  . "#FFFF00")
      ("maroon"  . "#800000")
      ("navy"    . "#000080")
      ("red"     . "#FF0000")
      ("blue"    . "#0000FF")
      ("purple"  . "#800080")
      ("teal"    . "#008080")
      ("fuchsia" . "#FF00FF")
      ("aqua"    . "#00FFFF")))

(defun parse-color (string)
  "Attemps to parse a color."
  ;; The color names defined by *color-names* are unterstood.
  ;; Otherwise the syntax is '#rgb' or '#rrggbb'. None of the fancier
  ;; X11 conventions are understood.
  (setq string (or (cdr (assoc string *color-names* :test #'string-equal))
                   string))
  (cond ((and (= (length string) 7)
              (char= (char string 0) #\#))
         (let ((r (maybe-parse-integer (subseq string 1 3) :radix 16))
               (g (maybe-parse-integer (subseq string 3 5) :radix 16))
               (b (maybe-parse-integer (subseq string 5 7) :radix 16)))
           (and r g b 
                (xlib:make-color :red (/ r 255) :green (/ g 255) :blue (/ b 255)))))
        ((and (= (length string) 4)
              (char= (char string 0) #\#))
         (let ((r (maybe-parse-integer (subseq string 1 2) :radix 16))
               (g (maybe-parse-integer (subseq string 2 3) :radix 16))
               (b (maybe-parse-integer (subseq string 3 4) :radix 16)))
           (and r g b 
                (xlib:make-color :red (/ r 15) :green (/ g 15) :blue (/ b 15)))))))

(defun color->24-bit (color)
  "Turns the string representation of a color into a 24-bit RGB-value."
  (let ((color (or (ignore-errors
                    (or (parse-color color)
                        (parse-color (concatenate 'string "#" color))))
                   (progn
                     (warn "Color `~A' does not parse." color)
                     (xlib:make-color
                      :red .5 :green .5 :blue .5)))))
    (let* ((r (min 255 (max 0 (round (* 255 (xlib:color-red color))))))
           (g (min 255 (max 0 (round (* 255 (xlib:color-green color))))))
           (b (min 255 (max 0 (round (* 255 (xlib:color-blue color))))))
           (p (dpb r (byte 8 0)
                   (dpb g (byte 8 8)
                        (dpb b (byte 8 16)
                             0)))))
      p)))

(defun color-cache (colormap)
  "The `colormap's color cache for FIND-COLOR."
  (or (getf (colormap-plist colormap) 'color-cache)
      (setf (getf (colormap-plist colormap) 'color-cache)
        (make-hash-table :test #'equal))))

(defun x11-find-color (window colorspec)
  "Returns the X11 pixel value of the color given by the string `colorspec'
   suitable for window `window'."
  ;; We go through the pixel-translator to accommodate pseudo color
  ;; screens. On those screen, it would harm to allocate new pixel
  ;; values instead of using approximations from the rgb-cube.
  ;; Generally there is no need to go thru' xlib:alloc-color.
  (let ((cache (color-cache (xlib:window-colormap window))))
    (or (gethash colorspec cache)
        (setf (gethash colorspec cache)
          (let ((p (color->24-bit colorspec)))
            (funcall (pixel-translator (xlib:window-colormap window))
                     0 0 p) )))))

;;;; ------------------------------------------------------------------------------------------
;;;;  Drawing Borders
;;;;

;;;; Border styles:

;;;;   none 
;;;;        no border is drawn (regardless of the 'border-width' value) 

;;;;   dotted 
;;;;        the border is a dotted line drawn on top of the background of the
;;;;        element 

;;;;   dashed 
;;;;        the border is a dashed line drawn on top of the background of the
;;;;        element 

;;;;   solid 
;;;;        the border is a solid line 

;;;;   double 
;;;;        the border is a double line drawn on top of the background of the
;;;;        element. The sum of the two single lines and the space between
;;;;        equals the <border-width> value. 

;;;;   groove 
;;;;        a 3D groove is drawn in colors based on the <color> value. 

;;;;   ridge 
;;;;        a 3D ridge is drawn in colors based on the <color> value. 

;;;;   inset 
;;;;        a 3D inset is drawn in colors based on the <color> value. 

;;;;   outset 
;;;;        a 3D outset is drawn in colors based on the <color> value.

(defun draw-border (drawable gcontext 
		    x0 y0 w h
		    ts tw tc
		    rs rw rc
		    bs bw bc
		    ls lw lc)
  (let ((x0 x0)
	(x1 (+ x0 lw))
	(x2 (- (+ x0 w) rw))
	(x3 (+ x0 w))
	(y0 y0)
	(y1 (+ y0 tw))
	(y2 (- (+ y0 h) bw))
	(y3 (+ y0 h)))
    (draw-border-left   drawable gcontext ls lc x0 x1 y0 y1 y2 y3)
    (draw-border-right  drawable gcontext rs rc x2 x3 y0 y1 y2 y3)
    (draw-border-top    drawable gcontext ts tc y0 y1 x0 x1 x2 x3)
    (draw-border-bottom drawable gcontext bs bc y2 y3 x0 x1 x2 x3) ))

(defun draw-border-left (drawable gcontext style color x0 x1 y0 y1 y2 y3)
  (ecase style
    (:solid 
     (xlib:with-gcontext (gcontext :foreground (x11-find-color drawable color))
       (xlib:draw-lines drawable gcontext (mapcar #'floor (list x0 y0  x1 y1  x1 y2  x0 y3)) :fill-p t) ))
    (:inset
     (draw-border-left drawable gcontext :solid (3d-dark-color color) x0 x1 y0 y1 y2 y3))
    (:outset
     (draw-border-left drawable gcontext :solid (3d-light-color color) x0 x1 y0 y1 y2 y3))
    (:ridge
     (draw-border-left drawable gcontext :outset color x0 (+ x0 (floor (- x1 x0) 2)) 
		       y0 (+ y0 (floor (- y1 y0) 2)) (+ y2 (floor (- y3 y2) 2)) y3)
     (draw-border-left drawable gcontext :inset color (+ x0 (floor (- x1 x0) 2)) x1 
		       (+ y0 (floor (- y1 y0) 2)) y1 y2 (+ y2 (ceiling (- y3 y2) 2))))
    (:groove
     (draw-border-left drawable gcontext :inset color x0 (+ x0 (floor (- x1 x0) 2)) 
		       y0 (+ y0 (floor (- y1 y0) 2)) (+ y2 (floor (- y3 y2) 2)) y3)
     (draw-border-left drawable gcontext :outset color (+ x0 (floor (- x1 x0) 2)) x1 
		       (+ y0 (floor (- y1 y0) 2)) y1 y2 (+ y2 (ceiling (- y3 y2) 2))))
    (:double
     (draw-border-left drawable gcontext :solid color x0 (+ x0 (ceiling (- x1 x0) 3)) 
		       y0 (+ y0 (ceiling (- y1 y0) 3)) (- y3 (ceiling (- y3 y2) 3)) y3)

     (draw-border-left drawable gcontext :solid color (- x1 (floor (- x1 x0) 3)) x1 
		       (- y1 (floor (- y1 y0) 3)) y1
		       y2 (+ y2 (floor (- y3 y2) 3))) )

    (:dashed
     (draw-dashed-line drawable gcontext 
                       (floor (/ (+ x1 x0) 2))
                       (floor (/ (+ y0 y1) 2))
                       (floor (/ (+ x1 x0) 2))
                       (floor (/ (+ y2 y3) 2))
                       (abs (- x1 x0))))
    (:dotted
     (draw-dotted-line drawable gcontext 
                       (floor (/ (+ x1 x0) 2))
                       (floor (/ (+ y0 y1) 2))
                       (floor (/ (+ x1 x0) 2))
                       (floor (/ (+ y2 y3) 2))
                       (abs (- x1 x0))))    
    ((:none))))

(defun draw-border-top (drawable gcontext style color y0 y1 x0 x1 x2 x3)
  (ecase style
    (:solid 
     (xlib:with-gcontext (gcontext :foreground (x11-find-color drawable color))
       (xlib:draw-lines drawable gcontext (mapcar #'floor (list x0 y0  x1 y1  x2 y1  x3 y0)) :fill-p t) ))
    (:inset
     (draw-border-top drawable gcontext :solid (3d-dark-color color) y0 y1 x0 x1 x2 x3))
    (:outset
     (draw-border-top drawable gcontext :solid (3d-light-color color) y0 y1 x0 x1 x2 x3))
    (:groove
     (draw-border-top drawable gcontext :inset color y0 (+ y0 (floor (- y1 y0) 2)) 
		       x0 (+ x0 (floor (- x1 x0) 2)) (+ x2 (floor (- x3 x2) 2)) x3)
     (draw-border-top drawable gcontext :outset color (+ y0 (floor (- y1 y0) 2)) y1 
		       (+ x0 (floor (- x1 x0) 2)) x1 x2 (+ x2 (ceiling (- x3 x2) 2))))
    (:ridge
     (draw-border-top drawable gcontext :outset color y0 (+ y0 (floor (- y1 y0) 2)) 
		       x0 (+ x0 (floor (- x1 x0) 2)) (+ x2 (floor (- x3 x2) 2)) x3)
     (draw-border-top drawable gcontext :inset color (+ y0 (floor (- y1 y0) 2)) y1 
		      (+ x0 (floor (- x1 x0) 2)) x1 x2 (+ x2 (ceiling (- x3 x2) 2))) )
    (:double
     (draw-border-top drawable gcontext :solid color y0 (+ y0 (ceiling (- y1 y0) 3)) 
		       x0 (+ x0 (ceiling (- x1 x0) 3)) (- x3 (ceiling (- x3 x2) 3)) x3)
     
     (draw-border-top drawable gcontext :solid color (- y1 (floor (- y1 y0) 3)) y1 
		       (- x1 (floor (- x1 x0) 3)) x1
		       x2 (+ x2 (floor (- x3 x2) 3))) )
    (:dotted
     (draw-dotted-line drawable gcontext
                       (floor (/ (+ x0 x1) 2))
                       (floor (/ (+ y0 y1) 2))
                       (floor (/ (+ x2 x3) 2))
                       (floor (/ (+ y0 y1) 2))
                       (abs (- y1 y0))))
    
    (:dashed
     (draw-dashed-line drawable gcontext
                       (floor (/ (+ x0 x1) 2))
                       (floor (/ (+ y0 y1) 2))
                       (floor (/ (+ x2 x3) 2))
                       (floor (/ (+ y0 y1) 2))
                       (abs (- y1 y0))))

    ((:none))))

(defun draw-border-right (drawable gcontext style color x2 x3 y0 y1 y2 y3)
  (ecase style
    (:solid 
     (xlib:with-gcontext (gcontext :foreground (x11-find-color drawable color))
       (xlib:draw-lines drawable gcontext (mapcar #'floor (list x3 y0  x2 y1  x2 y2  x3 y3)) :fill-p t) ))
    (:inset
     (draw-border-right drawable gcontext :solid (3d-light-color color) x2 x3 y0 y1 y2 y3))
    (:outset
     (draw-border-right drawable gcontext :solid (3d-dark-color color) x2 x3 y0 y1 y2 y3))
    (:ridge
     (draw-border-right drawable gcontext :outset color (+ x2 (floor (- x3 x2) 2)) x3
			y0 (+ y0 (floor (- y1 y0) 2)) (+ y2 (floor (- y3 y2) 2)) y3)
     (draw-border-right drawable gcontext :inset color x2 (+ x2 (floor (- x3 x2) 2))
			(+ y0 (floor (- y1 y0) 2)) y1 y2 (+ y2 (ceiling (- y3 y2) 2))))
    (:groove
     (draw-border-right drawable gcontext :inset color (+ x2 (floor (- x3 x2) 2)) x3
			y0 (+ y0 (floor (- y1 y0) 2)) (+ y2 (floor (- y3 y2) 2)) y3)
     (draw-border-right drawable gcontext :outset color x2 (+ x2 (floor (- x3 x2) 2))
			(+ y0 (floor (- y1 y0) 2)) y1 y2 (+ y2 (ceiling (- y3 y2) 2))))
    (:double
     (draw-border-right drawable gcontext :solid color (- x3 (ceiling (- x3 x2) 3)) x3
		       y0 (+ y0 (ceiling (- y1 y0) 3)) (- y3 (ceiling (- y3 y2) 3)) y3)
     
     (draw-border-right drawable gcontext :solid color x2 (+ x2 (floor (- x3 x2) 3))
		       (- y1 (floor (- y1 y0) 3)) y1
		       y2 (+ y2 (floor (- y3 y2) 3))) )
    
    (:dashed
     (draw-dashed-line drawable gcontext 
                       (floor (/ (+ x2 x3) 2))
                       (floor (/ (+ y0 y1) 2))
                       (floor (/ (+ x2 x3) 2))
                       (floor (/ (+ y2 y3) 2))
                       (abs (- x2 x3))))
    (:dotted
     (draw-dotted-line drawable gcontext 
                       (floor (/ (+ x2 x3) 2))
                       (floor (/ (+ y0 y1) 2))
                       (floor (/ (+ x2 x3) 2))
                       (floor (/ (+ y2 y3) 2))
                       (abs (- x2 x3))))
    
    ((:none))))

(defun draw-border-bottom (drawable gcontext style color y2 y3 x0 x1 x2 x3)
  (ecase style
    (:solid 
     (xlib:with-gcontext (gcontext :foreground (x11-find-color drawable color))
       (xlib:draw-lines drawable gcontext (mapcar #'floor (list x0 y3  x1 y2  x2 y2  x3 y3)) :fill-p t) ))
    (:inset
     (draw-border-bottom drawable gcontext :solid (3d-light-color color) y2 y3 x0 x1 x2 x3))
    (:outset
     (draw-border-bottom drawable gcontext :solid (3d-dark-color color) y2 y3 x0 x1 x2 x3))
    (:ridge
     (draw-border-bottom drawable gcontext :outset color (+ y2 (floor (- y3 y2) 2)) y3
			x0 (+ x0 (floor (- x1 x0) 2)) (+ x2 (floor (- x3 x2) 2)) x3)
     (draw-border-bottom drawable gcontext :inset color y2 (+ y2 (floor (- y3 y2) 2))
			(+ x0 (floor (- x1 x0) 2)) x1 x2 (+ x2 (ceiling (- x3 x2) 2))))
    (:groove
     (draw-border-bottom drawable gcontext :inset color (+ y2 (floor (- y3 y2) 2)) y3
			x0 (+ x0 (floor (- x1 x0) 2)) (+ x2 (floor (- x3 x2) 2)) x3)
     (draw-border-bottom drawable gcontext :outset color y2 (+ y2 (floor (- y3 y2) 2))
			(+ x0 (floor (- x1 x0) 2)) x1 x2 (+ x2 (ceiling (- x3 x2) 2))))
    (:double
     (draw-border-bottom drawable gcontext :solid color (- y3 (ceiling (- y3 y2) 3)) y3
		       x0 (+ x0 (ceiling (- x1 x0) 3)) (- x3 (ceiling (- x3 x2) 3)) x3)
     
     (draw-border-bottom drawable gcontext :solid color y2 (+ y2 (floor (- y3 y2) 3))
		       (- x1 (floor (- x1 x0) 3)) x1
		       x2 (+ x2 (floor (- x3 x2) 3))) )
    
    (:dotted
     (draw-dotted-line drawable gcontext
                       (floor (/ (+ x0 x1) 2))
                       (floor (/ (+ y2 y3) 2))
                       (floor (/ (+ x2 x3) 2))
                       (floor (/ (+ y2 y3) 2))
                       (abs (- y2 y3))))
    
    (:dashed
     (draw-dashed-line drawable gcontext
                       (floor (/ (+ x0 x1) 2))
                       (floor (/ (+ y2 y3) 2))
                       (floor (/ (+ x2 x3) 2))
                       (floor (/ (+ y2 y3) 2))
                       (abs (- y2 y3))))
    
    ((:none)) ))

(defun draw-dashed-line (drawable gcontext x1 y1 x2 y2 w)
  (setf w (ceiling w))
  (when (plusp w)
    (setf (xlib:gcontext-line-width gcontext) w
          (xlib:gcontext-cap-style gcontext) :butt
          (xlib:gcontext-join-style gcontext) :round
          (xlib:gcontext-line-style gcontext) :dash
          (xlib:gcontext-dashes gcontext) (list (* 2 w) (* 2 w)))
    (xlib:draw-line drawable gcontext x1 y1 x2 y2)
    (setf (xlib:gcontext-line-width gcontext) 1
          (xlib:gcontext-line-style gcontext) :solid)) )

(defun draw-dotted-line (drawable gcontext x1 y1 x2 y2 w)
  (setf w (ceiling w))
  (when (plusp w)
    (setf (xlib:gcontext-line-width gcontext) w
          (xlib:gcontext-cap-style gcontext) :round
          (xlib:gcontext-join-style gcontext) :round
          (xlib:gcontext-line-style gcontext) :dash
          (xlib:gcontext-dashes gcontext) (list 1 (* 2 w)))
    (xlib:draw-line drawable gcontext x1 y1 x2 y2)
    (setf (xlib:gcontext-line-width gcontext) 1
          ;;(xlib:gcontext-cap-style gcontext) :butt
          ;;(xlib:gcontext-join-style gcontext) :butt
          (xlib:gcontext-line-style gcontext) :solid) ))

(defun 3d-dark-color (x)  x "#717171" "#444")
(defun 3d-light-color (x) x "#ebebeb" "#ccc")

(defun 3d-dark-color (x)  x "#6e6e6e")
(defun 3d-light-color (x) x "#e8e8e8")

;;;; --------------------------------------------------------------------------

;;;; --------------------------------------------------------------------------

(defclass x11-device ()
  ((font-database :initform nil)
   (display :initarg :display)
   (dpi     :initarg :dpi :initform gui:*closure-dpi*)
   (scale-font-desc-cache :initform (make-hash-table :test #'equal))))

(defmethod r2::device-dpi ((self x11-device))
  (slot-value self 'dpi))

(defmethod r2::device-font-ascent ((self x11-device) font)
  (xlib:font-ascent font))

(defmethod r2::device-font-descent ((self x11-device) font)
  (xlib:font-descent font))

(defmethod r2::device-font-underline-position ((self x11-device) font)
  (ceiling (xlib:font-descent font) 2) )

(defmethod r2::device-font-underline-thickness ((self x11-device) font)
  (declare (ignore font))
  1)

(defmethod r2::device-font-has-glyph-p ((self x11-device) font index)
  (not (or (null (xlib:char-width font index))
           (= 0
              (xlib:char-width font index)
              (xlib:char-ascent font index)
              (xlib:char-descent font index)
              (xlib:char-attributes font index)
              (xlib:char-left-bearing font index)
              (xlib:char-right-bearing font index)))))

(defmethod r2::device-font-glyph-width ((self x11-device) font index)
  (xlib:char-width font index))

(defmethod r2::device-realize-font-desc ((self x11-device) font-desc)
  (with-slots (display) self
    (xlib:open-font display (r2::font-desc-ddp font-desc))))

#||
(defmethod r2::device-font-database ((self x11-device))
  (cond ((slot-value self 'font-database))
        (t
         (setf (slot-value self 'font-database)
           (x11-build-font-database self)))))

(defun x11-build-font-database (self)
  (with-slots (display) self
    (let ((res (r2::make-font-database
                :cache (make-hash-table :test #'equal)
                :device self)))
      (dolist (fn (prog2
                      (progn 
                        (format t "~&;; Querying font list ...")
                        (finish-output))
                    (tailor-font-list
                     (xlib:list-font-names display "-*-*-*-*-*-*-*-*-*-*-*-*-*-*"))
                    (progn (format t " done.") (finish-output))))
        (let ((fd (parse-x11-font-name fn)))
          (when fd
            (r2::font-database-relate res fd))))
      res)))
||#

(defmethod r2::scale-font-desc ((device x11-device) fd size)
  (labels ((hash-key (fd)
             (list 
              (r2::font-desc-family fd)
              (r2::font-desc-weight fd)
              (r2::font-desc-style fd)
              (r2::font-desc-charset fd))))
    ;;Warum ist das wieder von 'dpi' abhängig? 
    (with-slots (scale-font-desc-cache) device
      (or (gethash (list (hash-key fd) size) scale-font-desc-cache)
          (progn
            (setf (gethash (cons (hash-key fd) size) scale-font-desc-cache)
              (cond ((zerop (r2::font-desc-size fd))
                     (let ((r (r2::copy-font-desc fd)))
                       (setf (r2::font-desc-ddp r)
                         (scale-x11-font-name (r2::font-desc-ddp r) size
                                              (r2::device-dpi device)))
                       (setf (r2::font-desc-size r)
                         size)
                       r))
                    (t
                     fd))) ))) ) )

(defun scale-x11-font-name (font-name size dpi)
  (let ((atts (cdr (css::split-by #\- font-name :nuke-empty-p nil))))
    ;; 7 - size
    ;; 8 - resx
    ;; 9 - resy
    (let ((*print-circle* nil))
      (format nil "-~A-~A-~A-~A-~A-~A-~A-~A-~A-~A-~A-~A-~A-~A"
	      (nth 0 atts) (nth 1 atts) (nth 2 atts)
	      (nth 3 atts) (nth 4 atts) (nth 5 atts)
	      (round size)					 ;pixel size
	      "*"						 ;point size
	      dpi dpi
	      (nth 10 atts) "*" (nth 12 atts) (nth 13 atts)) ))) ; spc-avgWdth-rgstry-encoding

;;; ---- Parsing X11 Font Names --------------------------------------------------------

(defun parse-x11-font-weight (str)
  (cdr (assoc str '(("bold" . 700) ("medium" . 400))
	      :test #'string-equal)))

(defun parse-x11-font-slant (str)
  (cdr (assoc str '(("i" . :italic) ("o" . :oblique) ("r" . :normal))
	      :test #'string-equal)))

(defun parse-x11-font-size (str resy)
  (ignore-errors (floor (* (parse-integer resy) (/ (parse-integer str) 720)))))

(defun charset-name-from-partial-x11-font-name (foundry family registry encoding)
  (declare (ignorable foundry family registry encoding))
  (cond ((string-equal registry "iso8859")
         (cond ((string-equal encoding "1") :iso-8859-1)
               ((string-equal encoding "2") :iso-8859-2)
               ((string-equal encoding "3") :iso-8859-3)
               ((string-equal encoding "4") :iso-8859-4)
               ((string-equal encoding "5") :iso-8859-5)
               ((string-equal encoding "6") :iso-8859-6)
               ((string-equal encoding "7") :iso-8859-7)
               ((string-equal encoding "8") :iso-8859-8)
               ((string-equal encoding "9") :iso-8859-9)))
	((and (string-equal registry "iso10646")
              (string-equal encoding "1"))
         :iso-10646-1)
	((and (string-equal registry "unicode")
              (string-equal encoding "1"))
         :iso-10646-1)
	((and (string-equal registry "unicode")
              (string-equal encoding "2"))
         :iso-10646-1)
	((and (string-equal registry "koi8")
              (string-equal encoding "r"))
         :koi8-r)
	((and (string-equal registry "adobe")
              (string-equal encoding "symbol"))
         :adobe-symbol)
	((and (string-equal family "symbol")
              (string-equal registry "adobe"))
         :adobe-symbol)
        ((and (string-equal registry "big5"))
         :big-5)
	(t 
	 nil)))

(defun parse-x11-font-name (font-name)
  ;; fndry-family-weight-slant-swidth-style-pixelsz-pointsz-resx-resy-spc-avgwidth-registry-encoding
  (setq font-name (remove (code-char 0) font-name))
  (let ((atts (cdr (css::split-by #\- font-name :nuke-empty-p nil))))
    (let* ((foundry (nth 0 atts))
	   (family (nth 1 atts))
	   (weight (parse-x11-font-weight (nth 2 atts)))
	   (slant (parse-x11-font-slant (nth 3 atts)))
	   (size (parse-x11-font-size (nth 7 atts) (nth 9 atts)))
	   (registry (nth 12 atts))
	   (encoding (nth 13 atts))
	   (charset (charset-name-from-partial-x11-font-name foundry family registry encoding)) )
      (cond ((and weight slant size (ws/charset:find-charset charset))
	     (r2::make-font-desc :family family 
                                 :weight weight 
                                 :style slant 
                                 :size size 
                                 :ddp font-name
                                 :charset (ws/charset:find-charset charset)))
	    (t
	     nil)))))


#+NEW-CLX
(let ((old-open-font (symbol-function 'xlib:open-font)))
  (defun xlib:open-font (display name)
    (maphash (lambda (key value)
               (when (and (xlib:font-p value)
                          (string-equal (xlib:font-name value) name))
                 (return-from xlib:open-font value)))
             (slot-value display 'xlib::hash-table))
    (funcall old-open-font display name)))

(defstruct (x11-font-name
            (:constructor make-x11-font-name*
                          (fndry family weight slant swidth style
                           pixelsz pointsz resx resy spc avgwidth
                           registry encoding)))
  fndry
  family
  weight
  slant
  swidth
  style
  pixelsz
  pointsz
  resx
  resy
  spc
  avgwidth
  registry
  encoding)

(defun x11-font-name/scalable-p (font-name)
  (and (string= (x11-font-name-pixelsz font-name) "0")
       (string= (x11-font-name-pointsz font-name) "0")
       (string= (x11-font-name-avgwidth font-name) "0")))

(defun font-name-component-match-p (s1 s2)
  (or (eq s1 :wild) (eq s2 :wild) (string-equal s1 s2)))

(defun x11-font-names-match-p (fn1 fn2)
  (and (font-name-component-match-p (x11-font-name-fndry fn1) (x11-font-name-fndry fn2))
       (font-name-component-match-p (x11-font-name-family fn1) (x11-font-name-family fn2))
       (font-name-component-match-p (x11-font-name-weight fn1) (x11-font-name-weight fn2))
       (font-name-component-match-p (x11-font-name-slant fn1) (x11-font-name-slant fn2))
       (font-name-component-match-p (x11-font-name-swidth fn1) (x11-font-name-swidth fn2))
       (font-name-component-match-p (x11-font-name-style fn1) (x11-font-name-style fn2))
       (font-name-component-match-p (x11-font-name-pixelsz fn1) (x11-font-name-pixelsz fn2))
       (font-name-component-match-p (x11-font-name-pointsz fn1) (x11-font-name-pointsz fn2))
       (font-name-component-match-p (x11-font-name-resx fn1) (x11-font-name-resx fn2))
       (font-name-component-match-p (x11-font-name-resy fn1) (x11-font-name-resy fn2))
       (font-name-component-match-p (x11-font-name-spc fn1) (x11-font-name-spc fn2))
       (font-name-component-match-p (x11-font-name-avgwidth fn1) (x11-font-name-avgwidth fn2))
       (font-name-component-match-p (x11-font-name-registry fn1) (x11-font-name-registry fn2))
       (font-name-component-match-p (x11-font-name-encoding fn1) (x11-font-name-encoding fn2))))
   
(defun parse-x11-font-name/2 (font-name)
  (setq font-name (remove (code-char 0) font-name))
  (let ((atts (cdr (split-by #\- font-name :nuke-empty-p nil))))
    (apply #'make-x11-font-name* atts)))

(defun font-really-scalable-p (font all-fonts)
  (and (x11-font-name/scalable-p font)
       (let ((font* (copy-x11-font-name font)))
         (setf (x11-font-name-pixelsz font*) :wild
               (x11-font-name-pointsz font*) :wild
               (x11-font-name-avgwidth font*) :wild)
         (<= (count-if (lambda (x)
                         (and (x11-font-names-match-p x font*)
                              (not (x11-font-name/scalable-p x))))
                       all-fonts)
             1))))

(defun tailor-font-list (font-list)
  (setq font-list (mapcar #'parse-x11-font-name/2 font-list))
  (mapcar #'unparse-font-name
          (remove-if-not (lambda (x)
                           (if (not (x11-font-name/scalable-p x))
                               t
                             (let ((really? (font-really-scalable-p x font-list)))
                               (if really?
                                   t
                                 (progn
                                   ;; (warn "Unscaling ~A." (unparse-font-name x))
                                   nil)))))
                         font-list)))

(defun unparse-font-name (fn)
  (format nil "-~A-~A-~A-~A-~A-~A-~A-~A-~A-~A-~A-~A-~A-~A"
          (x11-font-name-fndry fn)
          (x11-font-name-family fn)
          (x11-font-name-weight fn)
          (x11-font-name-slant fn)
          (x11-font-name-swidth fn)
          (x11-font-name-style fn)
          (x11-font-name-pixelsz fn)
          (x11-font-name-pointsz fn)
          (x11-font-name-resx fn)
          (x11-font-name-resy fn)
          (x11-font-name-spc fn)
          (x11-font-name-avgwidth fn)
          (x11-font-name-registry fn)
          (x11-font-name-encoding fn)))


;;;

(defun fill-rectangle* (window gcontext x y width height color)
  (xlib:with-gcontext (gcontext :foreground (ws/x11::x11-find-color window color))
    (xlib:draw-rectangle window gcontext x y width height t)))

(defun fill-rectangle* (window gcontext x y width height color)
  "Fills a rectangle; `color' should be string as for FIND-COLOR."
  ;; I coded a first attempt to dither these rectangles, if appropriate.
  ;; This is a hack of course, we should better preallocate the needed
  ;; stipples and should not call the pixel translator. But I am too lazy
  ;; to modify the pixel translator generation code above to
  ;; generate code to setup stipple's for filling operations.
  (let* ((c  (color->24-bit color))
         (tr (pixel-translator (xlib:window-colormap window)))
         (c0 )
         (c1 )
         (px (xlib:create-pixmap :drawable window
                                 :width 8 :height 8 
                                 :depth 1))
         (gc1 (xlib:create-gcontext :drawable px :foreground 1))
         (gc0 (xlib:create-gcontext :drawable px :foreground 0)))
    (setf c0 (funcall tr 0 0 c))
    (dotimes (i 8)
      (dotimes (j 8)
        (let ((d (funcall tr i j c)))
          (cond ((/= d c0) 
                 (setf c1 d)
                 (xlib:draw-point px gc1 i j))
                (t
                 (xlib:draw-point px gc0 i j))))))
    (xlib:free-gcontext gc0)
    (xlib:free-gcontext gc1)
    (setf c1 (or c1 c0))
    (xlib:with-gcontext (gcontext :foreground c1
                                  :background c0
                                  :fill-style :opaque-stippled
                                  :stipple px)
      (xlib:draw-rectangle window gcontext x y width height t))
    (xlib:free-pixmap px)))




;;;;

#||
(defun foo (display &aux font-list)
  (setf font-list
    (xlib:list-font-names display "-*-*-*-*-*-*-*-*-*-*-*-*-*-*"))
  (setq font-list (mapcar #'parse-x11-font-name/2 font-list))
  (remove-if-not (lambda (x)
                   (if (not (x11-font-name/scalable-p x))
                       t
                     (let ((really? (font-really-scalable-p x font-list)))
                       (if really?
                           t
                         (progn
                           (warn "Unscaling ~A." (unparse-font-name x))
                           nil)))))
                 font-list))

(defun font-name-real-size (fn)
  (cond ((equal "0" (x11-font-name-pointsz fn))
         :scalable)
        (t
         (/ (* (parse-integer (x11-font-name-pointsz fn))
                   (parse-integer (x11-font-name-resy fn))) 720))))

||#

(defun fill-rectangle* (window gcontext x y width height color)
  "Fills a rectangle; `color' should be string as for FIND-COLOR."
  ;; I coded a first attempt to dither these rectangles, if appropriate.
  ;; This is a hack of course, we should better preallocate the needed
  ;; stipples and should not call the pixel translator. But I am too lazy
  ;; to modify the pixel translator generation code above to
  ;; generate code to setup stipple's for filling operations.
  (let* ((c  (color->24-bit color))
         (tr (pixel-translator (xlib:window-colormap window)))
         (px (xlib:create-pixmap :drawable window
                                 :width 8 :height 8 
                                 :depth (xlib:drawable-depth window)))
         (gc (xlib:create-gcontext :drawable px)))
    (dotimes (i 8)
      (dotimes (j 8)
        (let ((d (funcall tr i j c)))
          (setf (xlib:gcontext-foreground gc) d)
          (xlib:draw-point px gc i j))))
    (xlib:free-gcontext gc)
    (xlib:with-gcontext (gcontext :fill-style :tiled
                                  :tile px)
      (xlib:draw-rectangle window gcontext x y width height t))
    (xlib:free-pixmap px)))

(defun fill-polygon* (window gcontext point-seq color)
  (let* ((c  (color->24-bit color))
         (tr (pixel-translator (xlib:window-colormap window)))
         (px (xlib:create-pixmap :drawable window
                                 :width 8 :height 8 
                                 :depth (xlib:drawable-depth window)))
         (gc (xlib:create-gcontext :drawable px)))
    (dotimes (i 8)
      (dotimes (j 8)
        (let ((d (funcall tr i j c)))
          (setf (xlib:gcontext-foreground gc) d)
          (xlib:draw-point px gc i j))))
    (xlib:free-gcontext gc)
    (xlib:with-gcontext (gcontext :fill-style :tiled
                                  :tile px)
      (xlib:draw-lines window gcontext point-seq :fill-p t))
    (xlib:free-pixmap px)))

(defun compose-rgba (r g b &optional (a 0))
  (dpb r (byte 8 0)
       (dpb g (byte 8 8)
            (dpb b (byte 8 16)
                 (dpb a (byte 8 24)
                      0)))))

;; This finally bases the color convertion on the above translator
;; code. Which is the best thing, we can do in a PseudoColor
;; environment.


(defun parse-x11-color (string &aux sym r gb)
  ;; ### pff this really needs to be more robust.
  (cond ((and (= (length string) 4) (char= (char string 0) #\#))
         (clim:make-rgb-color
          (/ (parse-integer string :start 1 :end 2 :radix 16) #xF)
          (/ (parse-integer string :start 2 :end 3 :radix 16) #xF)
          (/ (parse-integer string :start 3 :end 4 :radix 16) #xF)))
        ((and (= (length string) 7) (char= (char string 0) #\#))
         (clim:make-rgb-color
          (/ (parse-integer string :start 1 :end 3 :radix 16) #xFF)
          (/ (parse-integer string :start 3 :end 5 :radix 16) #xFF)
          (/ (parse-integer string :start 5 :end 7 :radix 16) #xFF)))
        ((and (= (length string) 6) (every #'(lambda (x) (digit-char-p x 16)) string))
         (let ((r (parse-integer (subseq string 0 2) :radix 16))
               (g (parse-integer (subseq string 2 4) :radix 16))
               (b (parse-integer (subseq string 4 6) :radix 16)))
           (warn "Malformed color specifier: ~S" string)
           (and r g b 
                (clim:make-rgb-color (/ r 255) (/ g 255) (/ b 255)))))
        ((and (= (length string) 13) (char= (char string 0) #\#))
         (clim:make-rgb-color
          (/ (parse-integer string :start 1 :end 5 :radix 16) #xFFFF)
          (/ (parse-integer string :start 5 :end 9 :radix 16) #xFFFF)
          (/ (parse-integer string :start 9 :end 13 :radix 16) #xFFFF)))
        ((and (setf sym (find-symbol (concatenate 'string "+" (string-upcase string) "+")
                                     (find-package :clim)))
              (boundp sym)
              (clim:colorp (symbol-value sym)))
         (symbol-value sym))
        (t
         (warn "Malformed color specifier: ~S" string)
         clim:+red+)))


; LocalWords:  colormap RGB

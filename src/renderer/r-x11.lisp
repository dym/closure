;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: RENDERER; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: X11 specific parts of the renderer 
;;;   Created: 1999-05-25 22:12
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: GPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1998,1999 by Gilbert Baumann

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;; Changes
;;
;;  When        Who     What
;; ----------------------------------------------------------------------------
;;  1999-08-21  GB      - DRAW-BBOX-BACKGROUND: new function
;;                      - DRAW-BBOX-AUX: use it
;;                      - BACKGROUND-PIXMAP+MASK, X11-DRAW-BACKGROUND, 
;;                        DRAW-BBOX-BACKGROUND, X11-DRAW-IBOX, DRAW-BBOX-AUX, 
;;                        DRAW-ABOX, DRAW-BBOX: extra document argument
;; 

;;  1999-08-18  GB      - BACKGROUND-PIXMAP+MASK: uses CLUE-GUI2::AIMAGE-FROM-URL
;;                        interface now.
;;                      - BACKGROUND-PIXMAP+MASK fixed buglet
;;                      - WS/X11::X11-PUT-PIXMAP-TILED honors the pictures mask
;;                        X11 is borken -- we winded up to do our own clipping here
;;                        and performance is shitty!
;;                        And could somebody please fix XLIB:GCONTEXT-CLIP-MASK!
;;

(in-package :RENDERER)

;;; ---- X11 Drawing Code ---------------------------------------------------------------------

(defparameter *xoff* 0)
(defparameter *yoff* 0)

(defparameter *no-color-p* nil)

(defun draw-abox (document drawable gcontext box region)
  (cond ((bbox-p box)
         (draw-bbox document drawable gcontext box region))
        ((ibox-p box)
         (x11-draw-ibox document drawable gcontext
                        (ibox-x box)
                        (+ (ibox-height box) (ibox-y-oben box))
                        box region))
        (t
         (error "Bogus argument to DRAW-ABOX: ~S" box)) ))

(defun box-in-region-p (box region)
  (or (ignore-errors
       (gu:region-intersects-rectangle-p region 
                                         (abox-bx0 box) (abox-by0 box)
                                         (abox-bx1 box) (abox-by1 box)))))

(defun box-in-region-p (box region)
  (gu:region-intersects-rectangle-p region 
                                    (abox-bx0 box) (abox-by0 box)
                                    (abox-bx1 box) (abox-by1 box)))

#||
(defun box-in-region-p (box region)
  t)
||#

(defun draw-bbox (document drawable gcontext box region)
  (when (box-in-region-p box region)
    (cond ((and (bbox-pt box)
                (eq (css:style-attr (bbox-pt box) 'css:@position) :fixed))
           ;; hack here
           (multiple-value-bind (*xoff* *yoff*)
               (canvas-physical-origin drawable)
             (draw-bbox-aux document drawable gcontext box region)))
          (t
           (draw-bbox-aux document drawable gcontext box region)) )))

(defun draw-bbox-aux (document drawable gcontext box region)
  (multiple-value-bind (x y w h) (bbox-border-coordinates box)
    (incf x *xoff*)
    (incf y *yoff*)
    (draw-bbox-background document drawable gcontext box x y w h)
    (x11-draw-border drawable gcontext (bbox-border box) x y w h))
  (dolist (k (abox-contents box))
    (draw-abox document drawable gcontext k region)))

(defun draw-bbox-background (document drawable gcontext box x y w h)
  (unless *no-color-p*
    (x11-draw-background document drawable gcontext (bbox-background box) 
                         x y w h
                         ;; CSS-2: include padding!
                         (inner-left-edge box)
                         (inner-top-edge box)
                         (inner-width box)
                         (inner-height box))))

;;;
;;; There are two implementations of draw-text-deco:
;;;

;; [REC-CSS1 5.4.3] states:

;; | (1) This property [text-decoration] describes decorations that are added
;; |     to the text of an element. 
;; |     [And not only to text; that is, if there is text add deco. Nothing
;; |      is said here about possible deco added to replaced elements].
;; |
;; | (2) If the element has no text [...], this property has no effect.
;; |
;; | (3) E.g., if an element is underlined, the line should span the child
;; |     elements.

;; I at first misinterpreted (1) above as "add deco to text only", which is
;; wrong. (3) seems imply that non-text could be underlined.

;; One issue remains while reading this, what does padding and margin do to
;; text decoration? Should underlining span margin and padding?! The spec.
;; is silent here. Also consider this:

;; | A:link, A:visited, A:active { text-decoration: underline }
;; |
;; | The example above would underline the text of all links (i.e., all 'A'
;; | elements with a 'HREF' attribute).

;; it has an informal appearance. and thus my misinterpretation of (1) is
;; encouraged. The sentence says "underline the text"; in casual
;; conversation, it means "only text", since it would be useless to mention
;; the fact, if also non-text would be underlined.

;; Finally I suppose my misinterpretation is the actual intend.

;; This is the first (non-conforming) implementation of `draw-text-deco',
;; which works well with hyperlinks containing both text and images, e.g.
;;
;;  <A href=foo><img src=icon.png> News</A>
;;
;; will then be rendered as:
;;
;;    [icon] News
;;          ~~~~~
;;
;; Thus no underlining beneath the image. This is the implementation, which
;; is IMHO more consistent; and better looking.

(defun draw-text-deco (drawable gcontext xo y box dys)
  (let ((x xo)
        (z0 nil)
        (z1 nil))
    (labels ((walk (box)
               (cond ((ibox-p box)
                      (incf x (abox-left-leading box))
                      (mapc* #'walk (abox-contents box))
                      (incf x (abox-right-leading box)))
                     ((rbox-p box)
                      (when (and z0 z1)
                        (draw-lines z0 z1))
                      (setf z0 nil z1 nil)
                      (incf x (rbox-width box)))
                     ((gbox-p box)
                      (setf z0 (or z0 x))
                      (incf x (gbox-twidth box))
                      (setf z1 x))
                     (t
                      (warn "DRAW-TEXT-DECO: ???"))))
             (draw-lines (x1 x2)
               (dolist (dy dys)
                 (xlib:draw-line drawable gcontext
                                 (floor x1)
                                 (round (+ y dy))
                                 (ceiling x2)
                                 (round (+ y dy))))) )
      (walk box)
      (when (and z0 z1)
        (draw-lines z0 z1)))))

;; This is the second [conforming] implementation of `draw-text-deco'.
;;
;; The phrase "the element has no text" from [REC-CSS1 5.4.3] is interpreted
;; as: The element [or any of its children recursively] contains no gbox.
;;
;; Note that this is highly inconsitent, since the hyperlink example from
;; above:
;;
;;  <A href=foo><img src=icon.png> News</A>
;;
;; will now be rendered as:
;;
;;    [icon] News
;;    ~~~~~~~~~~~
;;
;; in contrast to <A href=foo><img src=icon.png></A>, which in turn will be
;; rendered like:
;;
;;    [icon]
;;
;; That is without underlining.
;;
;; Further the underlining also spans the horizontal margins of the element.
;;
(defun draw-text-deco (drawable gcontext xo y box dys)
  (unless (ibox-contains-only-rboxen? box)
    (dolist (dy dys)
      (xlib:draw-line drawable gcontext
                      (+ *xoff* (floor xo))
                      (+ *yoff* (round (+ y dy)))
                      (+ *xoff* (ceiling (+ xo (ibox-twidth box))))
                      (+ *yoff* (round (+ y dy)))))))

(defun ibox-real-height (box)
  (cond ((gbox-p box)
         (font-desc-ascent 
          (text-style-font (ibox-text-style (gbox-parent box)))))
        ((rbox-p box)
         (rbox-height box))
        ((ibox-p box)
         (let ((res 0))
           (dolist (k (abox-contents box) res)
             (setf res (max res (+ (ibox-real-height k) 
                                   (if (ibox-p k) (ibox-valign k) 0)))))))
        (t
         0)))

(defun ibox-real-depth (box)
  (cond ((gbox-p box)
         (font-desc-descent (text-style-font (ibox-text-style (gbox-parent box)))))
        ((rbox-p box)
         (rbox-depth box))
        ((ibox-p box)
         (let ((res 0))
           (dolist (k (abox-contents box) res)
             (setf res (max res (- (ibox-real-depth k) (if (ibox-p k) (ibox-valign k) 0)))))))
        (t
         0)))

(defun x11-draw-ibox (document drawable gcontext x0 y0 box region)
  (cond ((ibox-p box)
         (unless (and (abox-bx0 box) (abox-by0 box) (abox-bx1 box) (abox-by1 box))
           (error "Box has no bounding box - ~S." box))
         (cond ((box-in-region-p box region)
                (let ((x 0)                    
                      (y (- y0 (ibox-valign box))))
                  (incf x (ibox-margin-left box))
                  (unless *no-color-p*
                    (x11-draw-background document drawable gcontext (ibox-background box)
                                         (+ *xoff* (+ x0 x #|(abox-border-left-width box)|# ))
                                         (+ *yoff* (- y (ibox-real-height box) 
                                                      (ibox-padding-top box)
                                                      (abox-border-top-width box)))
                                         (+ (ibox-padding-left box) (ibox-iwidth box)
                                            (ibox-padding-right box)
                                            (abox-border-left-width box)
                                            (abox-border-right-width box))
                                         (+ (ibox-real-height box) 
                                            (ibox-real-depth box)
                                            (ibox-padding-top box)
                                            (ibox-padding-bottom box)
                                            (abox-border-top-width box)
                                            (abox-border-bottom-width box) )))
                  (when (abox-border box)
                    (x11-draw-border drawable gcontext (abox-border box)
                                     (+ *xoff* (+ x0 x))
                                     (+ *yoff* (- y (ibox-real-height box) 
                                                  (ibox-padding-top box)
                                                  (abox-border-top-width box) ))
                                       (+ (ibox-padding-left box)
                                          (ibox-iwidth box)
                                          (ibox-padding-right box)
                                          (abox-border-left-width box)
                                          (abox-border-right-width box))
                                       (+ (ibox-real-height box) 
                                          (ibox-real-depth box)
                                          (ibox-padding-top box)
                                          (ibox-padding-bottom box)
                                          (abox-border-top-width box)
                                          (abox-border-bottom-width box) )))
                  (unless (eq (ibox-text-decoration box) :none)
                    (xlib:with-gcontext (gcontext :foreground 
                                                  (if *no-color-p*
                                                      (ws/x11::x11-find-color drawable "black");zzz
                                                      (ws/x11::x11-find-color drawable (ibox-color box))))
                      (draw-text-deco drawable gcontext x0 y box
                                      (mapcan (lambda (k)
                                                (case k
                                                  ((:overline)
                                                   (list (floor (- (font-desc-ascent
                                                                    (text-style-font (ibox-text-style box)))
                                                                ))))
                                                  ((:line-through)
                                                   (list (floor (font-desc-x-height
                                                                 (text-style-font (ibox-text-style box)))
                                                                -2)) )
                                                  ((:underline)
                                                   (list (round (font-underline-position 
                                                                 (text-style-font (ibox-text-style box))))))))
                                              (ibox-text-decoration box)))))
                  (when (abox-border box) (incf x (border-left-width (abox-border box))))
                  (incf x (ibox-padding-left box))
                  (dolist (k (abox-contents box))
                    (incf x (x11-draw-ibox document drawable gcontext (+ x x0) y k region)))
                  (incf x (ibox-padding-right box))
                  (when (abox-border box) (incf x (border-right-width (abox-border box))))
                  (incf x (ibox-margin-right box))
                  x))
               (t
                (ibox-twidth box)) ))
        ((gbox-p box)
         (x11-draw-gbox drawable gcontext x0 y0 box)
         (gbox-twidth box))
        ((rbox-p box)
         (x11-draw-robj drawable gcontext (rbox-obj box) box (+ *xoff* x0) (+ *yoff* y0))
         (rbox-width box))
        (t
         (error "~S" (list 'x11-draw-ibox box))) ))

(defun x11-draw-gbox (drawable gcontext x y box)
  (xlib:with-gcontext (gcontext
                       :foreground
                       (if *no-color-p*
                           (ws/x11::x11-find-color drawable "black") ;zzz
                         (ws/x11::x11-find-color drawable (ibox-color (gbox-parent box))))
                       )
    (x11-draw-runes drawable gcontext (+ *xoff* x) (+ *yoff* y)
                    (gbox-runes box) (gbox-start box) (gbox-end box)
                    (ibox-text-style (gbox-parent box))
                    (ibox-white-space (gbox-parent box)))))

(defparameter *buffer* (make-array 1000))

(defun x11-draw-runes (drawable gcontext x0 y0 runes start end text-style white-space)
  (let ((font nil))
    (iterate-over-runes
     (lambda (rune index x cw)
       index
       (let ((fid (css-font-desc-glyph-fid (text-style-font text-style) rune))
             (i   (css-font-desc-glyph-index (text-style-font text-style) rune)))
         (unless (eql font fid)
           (setf (xlib:gcontext-font gcontext) fid)
           (setf font fid))
         (xlib:draw-glyph drawable gcontext 
                          (round (+ x0 x)) (round y0) i
                          :size 16
                          :width (round cw)))) ;??
     runes start end text-style white-space)))

(defun x11-draw-runes (drawable gcontext x0 y0 runes start end text-style white-space)
  (let ((font nil)
        (bptr 0)
        bx0 by0 bw)
    (prog1
        (iterate-over-runes
         (lambda (rune index x cw)
           index
           (let ((fid (css-font-desc-glyph-fid (text-style-font text-style) rune))
                 (i   (css-font-desc-glyph-index (text-style-font text-style) rune)))
             (unless (eql font fid)
               ;; we have to spill
               (unless (= bptr 0)
                 (xlib:draw-glyphs drawable gcontext bx0 by0 *buffer*
                                   :size 16
                                   :width bw
                                   :end bptr))
               (setf bptr 0
                     bx0 (round (+ x0 x))
                     by0 (round y0)
                     bw 0)
               (setf (xlib:gcontext-font gcontext) fid)
               (setf font fid))
             (setf (aref *buffer* bptr) i
                   bptr (+ bptr 1)
                   bw (+ bw (round cw)))))
         runes start end text-style white-space)
      (unless (= bptr 0)
        (xlib:draw-glyphs drawable gcontext bx0 by0 *buffer*
                          :size 16
                          :width bw
                          :end bptr))) ))

(defun resolve-background-position (spec image-dim box-dim)
  (cond ((css:percentage-p spec)
         (let ((i (maybe-resolve-percentage spec image-dim))
               (b (maybe-resolve-percentage spec box-dim)))
           (- b i)))
        (t
         spec)))

(defun background-aimage (drawable bg)
  (error "Dont call me"))

(defun background-pixmap+mask (document drawable bg)
  (cond ((background-%pixmap bg)
         (values (background-%pixmap bg)
                 (background-%mask bg)))
        (t
         (setf (background-%pixmap bg) :none)
         (r2::run-process-on-behalf-of-document document
          (lambda ()  
            (let ((aimage (clue-gui2::aimage-from-url document (background-image bg))))
              (cond ((eq aimage :error)
                     (setf (background-%pixmap bg) :none)
                     (values (background-%pixmap bg)
                             (background-%mask bg)))
                    (t
                     (let ((pm (ws/x11::aimage->pixmap+mask drawable aimage)))
                       (setf (background-%pixmap bg) (car pm)
                             (background-%mask bg) (cadr pm))
                       (clue-gui2::gui-post
                        nil
                        ;; we do it the hard way via an exposure round trip.
                        'xlib:clear-area
                        drawable
                        :exposures-p t))))))
          :name "Lazy Document background fetch.")
         (values (background-%pixmap bg)
                 (background-%mask bg)))))

(defmethod update-lazy-object (document (self null))
  nil)

(defun background-pixmap+mask (document drawable bg)
  (cond ((background-%pixmap bg)
         ;; already there
         (values (background-%pixmap bg)
                 (background-%mask bg)))
        (t
         (let ((aimage (document-fetch-image document nil (background-image bg))))
           ;; arg, jetzt haben wir wieder broken images
           (cond ((eql nil aimage)
                  (values :none))
                 (t
                  (cond ((eq aimage :error)
                         (setf (background-%pixmap bg) :none) )
                        (t
                         (let ((pm (ws/x11::aimage->pixmap+mask drawable aimage)))
                           (setf (background-%pixmap bg) (car pm)
                                 (background-%mask bg) (cadr pm)))))
                  (values (background-%pixmap bg)
                          (background-%mask bg)))))) ))

#.((lambda (x)
     #+:CMU `(eval ',x)                 ;compiler bug
     #-:CMU x)
   '(defun ws/x11::x11-put-pixmap-tiled (drawable ggc pixmap mask x y w h &optional (xo 0) (yo 0))
      (cond ((null mask) ;; xxx
             (xlib:with-gcontext (ggc :exposures :off
                                      :fill-style :tiled
                                      :tile pixmap
                                      :ts-x xo
                                      :ts-y yo)
                                 ;;mask wird momentan noch ignoriert!
                                 (xlib:draw-rectangle drawable ggc x y w h t)))
            (t
             (let* ((old-clip-mask (car (or (ignore-errors (list (xlib:gcontext-clip-mask ggc)))
                                            (list :none))))
                    (clip-region (let ((q old-clip-mask))
                                   (if (consp q)
                                       (gu:region-from-x11-rectangle-list q)
                                     gu:+everywhere+)))
                    (paint-region (gu:region-intersection 
                                   clip-region
                                   (gu:make-rectangle* x y (+ x w) (+ y h)))) )
               ;; There is a bug in CLX wrt to clip-x / clip-y
               ;; Turning off caching helps
               (setf (xlib:gcontext-cache-p ggc) nil)

               ;; we have to do our own clipping here.
               (let ((iw (xlib:drawable-width pixmap))
                     (ih (xlib:drawable-height pixmap)))
                 (loop for i from (floor (- x xo) iw) to (ceiling (- (+ x w) (+ xo iw)) iw)
                   do
                   (loop for j from (floor (- y yo) ih) to (ceiling (- (+ y h) (+ yo ih)) ih)
                     do
                     (let ((rect (gu:make-rectangle*
                                  (+ xo (* i iw))
                                  (+ yo (* j ih))
                                  (+ (+ xo (* i iw)) iw)
                                  (+ (+ yo (* j ih)) ih))))
                       (gu:map-region-rectangles 
                        (lambda (rx0 ry0 rx1 ry1)
                          (xlib:with-gcontext (ggc :exposures :off
                                                   :fill-style :tiled
                                                   :tile pixmap
                                                   :clip-mask mask
                                                   :clip-x (+ xo (* i iw))
                                                   :clip-y (+ yo (* j ih))
                                                   :ts-x xo
                                                   :ts-y yo)
                            (xlib:draw-rectangle drawable ggc
                                                 rx0 ry0 (max 0 (- rx1 rx0)) (max 0 (- ry1 ry0))
                                                 t)))
                        (gu:region-intersection paint-region rect))))) )
               ;; turn on caching again (see above)
               (setf (xlib:gcontext-cache-p ggc) t)
               ;;
               ;; and xlib:with-gcontext also is broken!
               (setf (xlib:gcontext-clip-mask ggc) old-clip-mask))))))

(defun x11-draw-background (document drawable gcontext bg x y width height
                            &optional (bix x) (biy y) (biwidth width) (biheight height))
  (when bg
    (unless (eql (background-color bg) :transparent)
      (ws/x11::fill-rectangle* drawable gcontext
                               (round x) (round y) 
                               (max 0 (round width))
                               (max 0 (round height))
                               (background-color bg)) )
    (unless (eql (background-image bg) :none)
      (multiple-value-bind (pixmap mask) (background-pixmap+mask document drawable bg)
        (unless (eql pixmap :none)
          (let* ((iw (xlib:drawable-width pixmap))
                 (ih (xlib:drawable-height pixmap))
                 (w (case (background-repeat bg)
                      ((:repeat :repeat-x) width)
                      ((:no-repeat :repeat-y) iw)))
                 (h (case (background-repeat bg)
                      ((:repeat :repeat-y) height)
                      ((:no-repeat :repeat-x) ih))) )
            (let ((hp (car (background-position bg)))
                  (vp (cdr (background-position bg))))
              (let ((xo (+ bix (resolve-background-position hp iw biwidth)))
                    (yo (+ biy (resolve-background-position vp ih biheight))))
                (ws/x11::x11-put-pixmap-tiled drawable gcontext pixmap mask
                                              (round (case (background-repeat bg)
                                                       ((:repeat :repeat-x) x)
                                                       ((:no-repeat :repeat-y) (+ xo))))
                                              (round (case (background-repeat bg)
                                                       ((:repeat :repeat-y) y)
                                                       ((:no-repeat :repeat-x) (+ yo))))
                                              (round w) (round h)
                                              (round (+ xo)) (round (+ yo)) )))) ))) ))

(defun x11-draw-border (drawable gcontext border x y w h)
  (when border
    (ws/x11::draw-border drawable gcontext
                        x y w h
                        (border-top-style border)
                        (border-top-width border)
                        (border-top-color border)
                        (border-right-style border)
                        (border-right-width border)
                        (border-right-color border)
                        (border-bottom-style border)
                        (border-bottom-width border)
                        (border-bottom-color border)
                        (border-left-style border)
                        (border-left-width border)
                        (border-left-color border))))

;;; -------------------------------------------------------------------------------------------
;;;

(defun x11-draw-runes (drawable gcontext x0 y0 runes start end text-style white-space)
  (cond ((and '(eq white-space :normal)
              '(member (text-style-letter-spacing text-style) '(:normal 0))
              '(member (text-style-word-spacing text-style) '(:normal 0)))
         (setf x0 (round x0))
         (setf y0 (round y0))
         (let ((buffer (make-array (- end start) :element-type '(unsigned-byte 16)))
               (font (text-style-font text-style))
               (j 0)
               (x00 x0)
               (xfont nil))
           (declare (type (simple-array (unsigned-byte 16) (*)) buffer)
                    (type (simple-array (unsigned-byte 16) (*)) runes)
                    (dynamic-extent buffer)
                    (type fixnum j))
           (prog1
               (iterate-over-runes (lambda (rune index x cw)
                                     index cw
                                     (let ((fid (css-font-desc-glyph-fid font rune))
                                           (i   (css-font-desc-glyph-index font rune)))
                                       (unless (eql xfont fid)
                                         (unless (= j 0)
                                           #+NIL
                                           (frob::add-text x0 y0 xfont (subseq buffer 0 j)
                                                           (xlib:gcontext-foreground gcontext))
                                           (xlib:draw-glyphs drawable gcontext
                                                             x0 y0 buffer
                                                             :end j :size 16))
                                         (setf x0 (+ x00 x))
                                         (setf j 0)
                                         (setf (xlib:gcontext-font gcontext) fid)
                                         (setf xfont fid))
                                       (setf (aref buffer j) i)
                                       (incf j)))
                                   runes start end text-style white-space)
             (unless (= j 0)
               #+NIL
               (frob::add-text x0 y0 xfont (subseq buffer 0 j)
                               (xlib:gcontext-foreground gcontext))
               (xlib:draw-glyphs drawable gcontext x0 y0 buffer :end j :size 16)))))
        (t
         (x11-draw-runes-general drawable gcontext x0 y0 
                                 runes start end
                                 text-style white-space)) ))

(defun x11-draw-runes-general (drawable gcontext x0 y0 runes start end text-style white-space)
  (let ((font nil))
    (iterate-over-runes
     (lambda (rune index x cw)
       index
       (let ((fid (css-font-desc-glyph-fid (text-style-font text-style) rune))
             (i   (css-font-desc-glyph-index (text-style-font text-style) rune)))
         (unless (eql font fid)
           (setf (xlib:gcontext-font gcontext) fid)
           (setf font fid))
         (xlib:draw-glyph drawable gcontext 
                          (round (+ x0 x)) (round y0) i
                          :size 16
                          :width (round cw)))) ;??
     runes start end text-style white-space)))

#||

;;; Experimental caching of X11 graphics

(defparameter *gcache*
    (make-hash-table :test #'eq :weak-keys t)
  "hash table mapping boxen to graphics")

(defun x11-draw-gbox (drawable gcontext x y box)
  (let ((looked (gethash box *gcache*)))
    (unless looked
      (setf looked (setf (gethash box *gcache*)
                          (compute-gofun drawable box x y))))
    (funcall looked drawable gcontext x y)))

(defun compute-gofun (drawable box x0 y0)
  (let ((runes (gbox-runes box))
        (start (gbox-start box))
        (end   (gbox-end box))
        (text-style (ibox-text-style (gbox-parent box)))
        (white-space (ibox-white-space (gbox-parent box))))
    (let ((body nil) (font nil)
          (result nil))
      (setf result
        (iterate-over-runes
         (lambda (rune index x cw)
           index
           (let ((fid (css-font-desc-glyph-fid (text-style-font text-style) rune))
                 (i   (css-font-desc-glyph-index (text-style-font text-style) rune)))
             (unless (eql font fid)
               (push `(setf (xlib:gcontext-font gcontext) ',fid)
                     body)
               (setf font fid))
             (push `(xlib:draw-glyph drawable gcontext 
                                     ,(round (+ x0 x)) ,(round y0) ,i
                                     :size 16
                                     :width ,(round cw))
                   body)))
         runes start end text-style white-space))
      (compile nil
      `(lambda (drawable gcontext x0 y0)
         (xlib:with-gcontext (gcontext
                              :foreground
                              ,(if *no-color-p*
                                   (ws/x11::x11-find-color drawable "black") ;zzz
                                 (ws/x11::x11-find-color drawable (ibox-color (gbox-parent box)))))
           ,@(reverse body)
           ',result)) ))))
||#
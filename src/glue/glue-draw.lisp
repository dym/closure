;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: GLUE; -*-
;;; --------------------------------------------------------------------------------------
;;;     Title: The Glue toolkit based on CLUE
;;;   Created: 1999-03-24 07:18
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: GPL (See file COPYING for details).
;;; --------------------------------------------------------------------------------------
;;;  (c) copyright 1999 by Gilbert Baumann

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

(in-package :GLUE)

;; Graphics Sinks:

(defclass x11-sink ()
  ((gcontext :initform nil)
   (drawable :initform nil :initarg :drawable)
   
   (foreground :initform :black)
   (background :initform :white)
   (font       :initform *default-font*)
   (font-internal :initform nil)
   
   (clipping-region :initform +everywhere+)
   
   (foreground-dirty-p :initform t)
   (background-dirty-p :initform t)
   (clipping-region-dirty-p :initform t)
   
   ;;(font :initform *default-font*)
   ))

(defclass drawable-contact-mixin ()
  ((graphics-sink :initform nil)))

;;;

(defmethod sink-gcontext ((self x11-sink))
  (with-slots (gcontext drawable) self
    (or gcontext
        (progn
          (setf gcontext (xlib:create-gcontext :drawable drawable))
          (sync-gcontext self)
          gcontext))))

(defmethod sync-gcontext ((self x11-sink))
  (with-slots (foreground foreground-dirty-p
               background background-dirty-p
               clipping-region clipping-region-dirty-p
               drawable)
      self
    (let ((gc (sink-gcontext self)))
      (when foreground-dirty-p
        (setf foreground-dirty-p nil)
        (setf (xlib:gcontext-foreground gc) (clue:convert drawable foreground 'xlib:pixel) ))
      (when background-dirty-p
        (setf background-dirty-p nil)
        (setf (xlib:gcontext-background gc) (clue:convert drawable background 'xlib:pixel)))
      (when clipping-region-dirty-p
        (setf clipping-region-dirty-p nil)
        (setf (xlib:gcontext-clip-mask gc)
          (cond ((eql clipping-region +everywhere+)
                 :none)
                (t
                 (region-to-x11-rectangle-list
                  (region-intersection clipping-region (make-rectangle* #x-8000 #x-8000 #x+7FFF #x+7FFF))))))) )))

;; readers

(defmethod sink-foreground ((self x11-sink))
  (slot-value self 'foreground))

(defmethod sink-background ((self x11-sink))
  (slot-value self 'background))

(defmethod sink-clipping-region ((self x11-sink))
  (slot-value self 'clipping-region))

(defmethod sink-font ((self x11-sink))
  (slot-value self 'font))

(defmethod sink-effective-font ((self x11-sink))
  (with-slots (font font-internal drawable) self
    (or font-internal
        (setf font-internal (clue:convert drawable font 'afont)))))

;; writers

(defmethod (setf sink-foreground) (new-value (self x11-sink))
  (with-slots (foreground foreground-dirty-p) self
    (setf foreground new-value
          foreground-dirty-p t)))

(defmethod (setf sink-background) (new-value (self x11-sink))
  (with-slots (background background-dirty-p) self
    (setf background new-value
          background-dirty-p t)))

(defmethod (setf sink-clipping-region) (new-value (self x11-sink))
  (with-slots (clipping-region clipping-region-dirty-p) self
    (setf clipping-region new-value
          clipping-region-dirty-p t)))

(defmethod (setf sink-font) (new-value (self x11-sink))
  (with-slots (font font-internal) self
    (setf font new-value
          font-internal nil)))

;; graphics primitives

(defmacro with-drawing-options-maybe ((sink &rest options) &body body)
  `(invoke-with-drawing-options-maybe ,sink (list ,@options)
                                      (lambda () ,@body)))

(defmethod invoke-with-drawing-options-maybe (sink options continuation)
  (let ((old-clipping-region))
    (destructuring-bind (&key foreground background clipping-region font) options
      (when foreground
        (rotatef foreground (sink-foreground sink)))
      (when background
        (rotatef background (sink-background sink)))
      (when clipping-region
        (setf old-clipping-region (sink-clipping-region sink)
              (sink-clipping-region sink) (region-intersection (sink-clipping-region sink)
                                                               clipping-region)))
      (when font
        (rotatef font (sink-font sink)))
      ;;
      (prog1
          (funcall continuation)
        ;;
        (when foreground
          (rotatef foreground (sink-foreground sink)))
        (when background
          (rotatef background (sink-background sink)))
        (when clipping-region
          (setf (sink-clipping-region sink) old-clipping-region))
        (when font
          (rotatef font (sink-font sink))))) ))

;;;

(defun draw-text* (sink sequence x y
                   &key (start 0) (end (length sequence))
                        foreground
                        background
                        clipping-region
                        font)
  (with-drawing-options-maybe (sink :foreground foreground
                                    :background background
                                    :clipping-region clipping-region
                                    :font font)
    (medium-draw-text sink (sink-effective-font sink) x y sequence start end)))

(defun draw-rectangle* (sink x1 y1 x2 y2 &key (filled t)
                                              clipping-region
                                              foreground)
  (with-drawing-options-maybe (sink :foreground foreground
                                    :clipping-region clipping-region)
    (medium-draw-rectangle* sink x1 y1 x2 y2 filled)))

(defun draw-line* (sink x1 y1 x2 y2 &key clipping-region
                                         foreground)
  (with-drawing-options-maybe (sink :foreground foreground
                                    :clipping-region clipping-region)
    (medium-draw-line* sink x1 y1 x2 y2)))

(defun draw-arc* (sink x1 y1 x2 y2 &key (filled t)
                                        clipping-region
                                        foreground
                                        (angle1 0)
                                        (angle2 (* 2 pi)))
  (with-drawing-options-maybe (sink :foreground foreground
                                    :clipping-region clipping-region)
    (medium-draw-arc* sink x1 y1 x2 y2 angle1 angle2 filled)))
  
(defun text-ascent (sink &key font)
  (with-drawing-options-maybe (sink :font font)
    (medium-text-ascent sink (sink-effective-font sink))))

(defun text-descent (sink &key font)
  (with-drawing-options-maybe (sink :font font)
    (medium-text-descent sink (sink-effective-font sink))))

(defun new-text-width (sink sequence &key font (start 0) (end (length sequence)))
  (with-drawing-options-maybe (sink :font font)
    (medium-text-width sink (sink-effective-font sink) sequence start end)))

;;;

(defmethod medium-draw-text ((sink x11-sink) (font xlib:font) x y sequence start end)
  (with-slots (drawable) sink
    (let ((gc (sink-gcontext sink)))
      (sync-gcontext sink)
      (setf (xlib:gcontext-font gc) font)
      (xlib:draw-glyphs drawable gc (floor x) (floor y) sequence :start start :end end))))

(defmethod medium-text-ascent ((sink x11-sink) (font xlib:font))
  (xlib:font-ascent font))

(defmethod medium-text-descent ((sink x11-sink) (font xlib:font))
  (xlib:font-descent font))

(defmethod medium-text-width ((sink x11-sink) (font xlib:font) sequence start end)
  (xlib:text-width font sequence :start start :end end))

(defmethod medium-draw-rectangle* ((sink x11-sink) x1 y1 x2 y2 filled)
  (with-slots (drawable) sink
    (let ((gc (sink-gcontext sink)))
      (sync-gcontext sink)
      (let ((x1 (min x1 x2))
            (y1 (min y1 y2))
            (x2 (max x1 x2))
            (y2 (max y1 y2)))
        (xlib:draw-rectangle drawable gc x1 y1 (- x2 x1) (- y2 y1) filled) ))))

(defmethod medium-draw-line* ((sink x11-sink) x1 y1 x2 y2)
  (with-slots (drawable) sink
    (let ((gc (sink-gcontext sink)))
      (sync-gcontext sink)
      (xlib:draw-line drawable gc x1 y1 x2 y2) )))

(defmethod medium-draw-arc* ((sink x11-sink) x1 y1 x2 y2 angle1 angle2 filled)
  (with-slots (drawable) sink
    (let ((gc (sink-gcontext sink)))
      (sync-gcontext sink)
      (let ((x1 (min x1 x2))
            (y1 (min y1 y2))
            (x2 (max x1 x2))
            (y2 (max y1 y2)))
        (xlib:draw-arc drawable gc x1 y1 (- x2 x1) (- y2 y1) angle1 (- angle2 angle1) filled)))))

;;;; ---------------------------------------------------------------------------
;;;;  Drawable Contact Mixin
;;;;

(defmethod contact-medium ((self drawable-contact-mixin))
  (with-slots (graphics-sink) self
    (or graphics-sink
        (setf graphics-sink
          (make-instance 'x11-sink :drawable self)))))

(defmethod medium-draw-text ((sink drawable-contact-mixin) font x y sequence start end)
  (medium-draw-text (contact-medium sink) font x y sequence start end))

(defmethod medium-text-ascent ((sink drawable-contact-mixin) font)
  (medium-text-ascent (contact-medium sink) font))

(defmethod medium-text-descent ((sink drawable-contact-mixin) font)
  (medium-text-descent (contact-medium sink) font))

(defmethod medium-text-width ((sink drawable-contact-mixin) font sequence start end)
  (medium-text-width (contact-medium sink) font sequence start end))

(defmethod medium-draw-rectangle* ((sink drawable-contact-mixin) x1 y1 x2 y2 filled)
  (medium-draw-rectangle* (contact-medium sink) x1 y1 x2 y2 filled))

(defmethod medium-draw-arc* ((sink drawable-contact-mixin) x1 y1 x2 y2 angle1 angle2 filled)
  (medium-draw-arc* (contact-medium sink) x1 y1 x2 y2 angle1 angle2 filled))

(defmethod medium-draw-line* ((sink drawable-contact-mixin) x1 y1 x2 y2)
  (medium-draw-line* (contact-medium sink) x1 y1 x2 y2))

(defmethod sink-foreground ((contact drawable-contact-mixin))
  (sink-foreground (contact-medium contact)))

(defmethod sink-background ((contact drawable-contact-mixin))
  (sink-background (contact-medium contact)))

(defmethod sink-clipping-region ((contact drawable-contact-mixin))
  (sink-clipping-region (contact-medium contact)))

(defmethod sink-font ((contact drawable-contact-mixin))
  (sink-font (contact-medium contact)))

(defmethod sink-effective-font ((contact drawable-contact-mixin))
  (sink-effective-font (contact-medium contact)))

(defmethod (setf sink-foreground) (new-value (contact drawable-contact-mixin))
  (setf (sink-foreground (contact-medium contact)) new-value))

(defmethod (setf sink-background) (new-value (contact drawable-contact-mixin))
  (setf (sink-background (contact-medium contact)) new-value))

(defmethod (setf sink-clipping-region) (new-value (contact drawable-contact-mixin))
  (setf (sink-clipping-region (contact-medium contact)) new-value))

(defmethod (setf sink-font) (new-value (contact drawable-contact-mixin))
  (setf (sink-font (contact-medium contact)) new-value))

;;;; ---------------------------------------------------------------------------
;;;;  Simple Canvas Class
;;;;

(defcontact canvas (contact drawable-contact-mixin)
  ((xo :initarg :xo :initform 0)
   (yo :initarg :yo :initform 0) ))

(defmethod medium-draw-text ((sink canvas) font x y sequence start end)
  (with-slots (xo yo) sink
    (medium-draw-text (contact-medium sink) font (+ x xo) (+ y yo) sequence start end)))

(defmethod medium-draw-rectangle* ((sink canvas) x1 y1 x2 y2 filled)
  (with-slots (xo yo) sink
    (medium-draw-rectangle* (contact-medium sink) (+ x1 xo) (+ y1 yo) (+ x2 xo) (+ y2 yo) filled)))

(defmethod medium-draw-arc* ((sink canvas) x1 y1 x2 y2 angle1 angle2 filled)
  (with-slots (xo yo) sink
    (medium-draw-arc* (contact-medium sink) (+ x1 xo) (+ y1 yo) (+ x2 xo) (+ y2 yo) angle1 angle2 filled)))

(defmethod medium-draw-line* ((sink canvas) x1 y1 x2 y2)
  (with-slots (xo yo) sink
    (medium-draw-line* (contact-medium sink) (+ x1 xo) (+ y1 yo) (+ x2 xo) (+ y2 yo))))

(defmethod sink-clipping-region ((contact canvas))
  (with-slots (xo yo) contact
    (translate-region (sink-clipping-region (contact-medium contact))
                      (- xo) (- yo))))

(defmethod (setf sink-clipping-region) (new-value (contact canvas))
  (with-slots (xo yo) contact
    (setf (sink-clipping-region (contact-medium contact)) 
      (translate-region new-value xo yo))))

(defmethod clue:display-region ((canvas canvas) region)
  (with-slots (xo yo) canvas
    (canvas-display-region canvas (translate-region region (- xo) (- yo)))))

(defmethod translate-device-coordinates ((canvas canvas) x y)
  (with-slots (xo yo) canvas
    (values (- x xo) (- y yo))))

(defmethod canvas-move-origin ((canvas canvas) nxo nyo)
  (with-slots (xo yo) canvas
    nxo
    (let ((height (contact-height canvas))
          (width (contact-width canvas)))
      (setf width 1000 height 1000)     ;XXX was ist das??
      (using-gcontext (gc :drawable canvas :exposures :on)
        (cond ((> nyo yo)
               (xlib:copy-area
                canvas gc 0 (- yo nyo) width height
                canvas 0 0))
              (t
               (xlib:copy-area
                canvas gc 0 (- yo nyo) width (- height (- nyo yo))
                canvas 0 0)))
        (setf yo nyo))
      (canvas-display-region canvas
                             (translate-region
                              (reduce #'region-union
                                      (mapcar (lambda (rect)
                                                (destructuring-bind (x y width height) rect
                                                  (make-rectangle* x y (+ x width) (+ y height))))
                                              (collect-graphics-exposures canvas))
                                      :initial-value +nowhere+)
                              (- xo) (- yo))))))

(defun collect-graphics-exposures (the-drawable)
  (let ((display (xlib:drawable-display the-drawable))
	(res nil)
	(final-p nil))
    (xlib:display-force-output display)
    (do ()
	((not (and (not final-p)
		(xlib:event-case (display :timeout 2)
	          (:graphics-exposure (drawable count x y width height)
	             (when (eq drawable the-drawable)
		       (push (list x y width height) res)
		       (setq final-p (= count 0))
		       t))
		  (:no-exposure (drawable)
		     (when (eq drawable the-drawable)
		       (setq final-p t)
		       t)) )))))
    res))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Old Drawing Primitves
;;;;

;; We fake CLIM a bit ...

;; Should we warp calls to round around here?! Should we clamp to the
;; ancient #o77777 pixels coordinate limit?!

(defmethod old-draw-line* ((self contact) x0 y0 x1 y1 &key foreground)
  (using-gcontext (gc :drawable self
                      :foreground (and foreground (convert self foreground 'xlib:pixel)))
    (xlib:draw-line self gc x0 y0 x1 y1)))

(defmethod draw-polygon* ((self contact) points &key foreground filled-p)
  (using-gcontext (gc :drawable self
                      :foreground (and foreground (convert self foreground 'xlib:pixel)))
    (xlib:draw-lines self gc points :fill-p filled-p)))

#+(OR)
(defmethod draw-polygon* ((self contact) points &key foreground filled-p)
  (cond (filled-p
         (using-gcontext (gc :drawable self)
                         (ws/x11::fill-polygon* self gc points foreground))
         )
        (t
         (using-gcontext (gc :drawable self
                             :foreground (and foreground (convert self foreground 'xlib:pixel)))
                         (xlib:draw-lines self gc points :fill-p filled-p)))))

(defmethod old-draw-rectangle* ((self contact) x0 y0 x1 y1 &key foreground filled-p)
  ;; And this fixes the brain dead X11 convention that a non-filled
  ;; rectangle is one pixel wider and taller than the same filled one.
  ;; And to be explict: This function should fill pixels [x0,x1] x [y0,y1].
  (using-gcontext (gc :drawable self
                      :foreground (and foreground (convert self foreground 'xlib:pixel)))
    (xlib:draw-rectangle self gc x0 y0 
                         (max (if filled-p 1 0) (- x1 x0 (if filled-p -1 0)))
                         (max (if filled-p 1 0) (- y1 y0 (if filled-p -1 0)))
                         filled-p)))

(defmethod old-draw-text* ((self contact) x y string &key foreground background image-p font)
  (using-gcontext (gc :drawable self
                      :foreground (and foreground (convert self foreground 'xlib:pixel))
                      :background (and background (convert self background 'xlib:pixel))
                      :font       (and font (convert self font 'xlib:font)))
    (funcall 
     (if image-p #'xlib:draw-image-glyphs #'xlib:draw-glyphs)
     self gc x y string)))

(defun draw-text-into-box (sink x0 y0 x1 y1
                           string 
                           &key (alignment :left)
                                (foreground :black)
                                (font *default-font*))
  (using-gcontext (gc :drawable sink :font font)
    (multiple-value-bind (width) (xlib:text-extents gc string)
      (let* ((a (xlib:font-ascent font))
             (d (xlib:font-descent font))
             (yo (+ y0 (+ a (round (- (- y1 y0) (+ a d)) 2))))
             (xo (case alignment
                   (:left x0)
                   (:right (- x1 width))
                   (:center (+ x0 (round (- (- x1 x0) width) 2))))))
        (old-draw-text* sink xo yo string
                        :font font
                        :foreground foreground))) ))

(defmethod draw-bordered-polygon ((self contact) points 
                                  &key (border "black")
                                       (interior "white"))
  (setf points (append points (list (car points) (cadr points))))
  (draw-polygon* self points :foreground interior :filled-p t)
  (draw-polygon* self points :foreground border :filled-p nil))

;;; Borders

(defun draw-simple-border (sink x y w h style &optional (width 2) (color "black"))
  (when *mono-p* 
    (setf style (if (eq style :none) style :solid)
          width 1))
  (draw-complex-border sink x y w h
                       style width color
                       style width color
                       style width color
                       style width color))

(defun draw-complex-border (sink x0 y0 w h  ts tw tc  rs rw rc  bs bw bc  ls lw lc)
  (let ((x0 x0)
	(x1 (+ x0 lw))
	(x2 (- (+ x0 w) rw))
	(x3 (+ x0 w))
	(y0 y0)
	(y1 (+ y0 tw))
	(y2 (- (+ y0 h) bw))
	(y3 (+ y0 h)))
    (draw-border-left   sink ls lc x0 x1 y0 y1 y2 y3)
    (draw-border-right  sink rs rc x2 x3 y0 y1 y2 y3)
    (draw-border-top    sink ts tc y0 y1 x0 x1 x2 x3)
    (draw-border-bottom sink bs bc y2 y3 x0 x1 x2 x3) ))

(defun draw-border-left (sink style color x0 x1 y0 y1 y2 y3)
  (ecase style
    (:solid 
     (draw-polygon* sink (mapcar #'floor (list x0 y0  x1 y1  x1 y2  x0 y3))
                    :foreground color
                    :filled-p t))
    (:inset
     (draw-border-left sink :solid (3d-dark-color sink color) x0 x1 y0 y1 y2 y3))
    (:outset
     (draw-border-left sink :solid (3d-light-color sink color) x0 x1 y0 y1 y2 y3))
    (:ridge
     (draw-border-left sink :outset color x0 (+ x0 (floor (- x1 x0) 2)) 
		       y0 (+ y0 (floor (- y1 y0) 2)) (+ y2 (floor (- y3 y2) 2)) y3)
     (draw-border-left sink :inset color (+ x0 (floor (- x1 x0) 2)) x1 
		       (+ y0 (floor (- y1 y0) 2)) y1 y2 (+ y2 (ceiling (- y3 y2) 2))))
    (:groove
     (draw-border-left sink :inset color x0 (+ x0 (floor (- x1 x0) 2)) 
		       y0 (+ y0 (floor (- y1 y0) 2)) (+ y2 (floor (- y3 y2) 2)) y3)
     (draw-border-left sink :outset color (+ x0 (floor (- x1 x0) 2)) x1 
		       (+ y0 (floor (- y1 y0) 2)) y1 y2 (+ y2 (ceiling (- y3 y2) 2))))
    (:double
     (draw-border-left sink :solid color x0 (+ x0 (ceiling (- x1 x0) 3)) 
		       y0 (+ y0 (ceiling (- y1 y0) 3)) (- y3 (ceiling (- y3 y2) 3)) y3)

     (draw-border-left sink :solid color (- x1 (floor (- x1 x0) 3)) x1 
		       (- y1 (floor (- y1 y0) 3)) y1
		       y2 (+ y2 (floor (- y3 y2) 3))) )

    (:dashed
     (draw-dashed-line sink 
                       (floor (+ x1 x0) 2)
                       (floor (+ y0 y1) 2)
                       (floor (+ x1 x0) 2)
                       (floor (+ y2 y3) 2)
                       (abs (- x1 x0))))
    (:dotted
     (draw-dotted-line sink 
                       (floor (+ x1 x0) 2)
                       (floor (+ y0 y1) 2)
                       (floor (+ x1 x0) 2)
                       (floor (+ y2 y3) 2)
                       (abs (- x1 x0))))    
    ((:none))))

(defun draw-border-top (sink style color y0 y1 x0 x1 x2 x3)
  (ecase style
    (:solid 
     (draw-polygon* sink (mapcar #'floor (list x0 y0  x1 y1  x2 y1  x3 y0))
                    :foreground color
                    :filled-p t) )
    (:inset
     (draw-border-top sink :solid (3d-dark-color sink color) y0 y1 x0 x1 x2 x3))
    (:outset
     (draw-border-top sink :solid (3d-light-color sink color) y0 y1 x0 x1 x2 x3))
    (:groove
     (draw-border-top sink :inset color y0 (+ y0 (floor (- y1 y0) 2)) 
		       x0 (+ x0 (floor (- x1 x0) 2)) (+ x2 (floor (- x3 x2) 2)) x3)
     (draw-border-top sink :outset color (+ y0 (floor (- y1 y0) 2)) y1 
		       (+ x0 (floor (- x1 x0) 2)) x1 x2 (+ x2 (ceiling (- x3 x2) 2))))
    (:ridge
     (draw-border-top sink :outset color y0 (+ y0 (floor (- y1 y0) 2)) 
		       x0 (+ x0 (floor (- x1 x0) 2)) (+ x2 (floor (- x3 x2) 2)) x3)
     (draw-border-top sink :inset color (+ y0 (floor (- y1 y0) 2)) y1 
		      (+ x0 (floor (- x1 x0) 2)) x1 x2 (+ x2 (ceiling (- x3 x2) 2))) )
    (:double
     (draw-border-top sink :solid color y0 (+ y0 (ceiling (- y1 y0) 3)) 
		       x0 (+ x0 (ceiling (- x1 x0) 3)) (- x3 (ceiling (- x3 x2) 3)) x3)
     
     (draw-border-top sink :solid color (- y1 (floor (- y1 y0) 3)) y1 
		       (- x1 (floor (- x1 x0) 3)) x1
		       x2 (+ x2 (floor (- x3 x2) 3))) )
    (:dotted
     (draw-dotted-line sink
                       (floor (+ x0 x1) 2)
                       (floor (+ y0 y1) 2)
                       (floor (+ x2 x3) 2)
                       (floor (+ y0 y1) 2)
                       (abs (- y1 y0))))
    
    (:dashed
     (draw-dashed-line sink
                       (floor (+ x0 x1) 2)
                       (floor (+ y0 y1) 2)
                       (floor (+ x2 x3) 2)
                       (floor (+ y0 y1) 2)
                       (abs (- y1 y0))))

    ((:none))))

(defun draw-border-right (sink style color x2 x3 y0 y1 y2 y3)
  (ecase style
    (:solid 
     (draw-polygon* sink (mapcar #'floor (list x3 y0  x2 y1  x2 y2  x3 y3))
                    :foreground color 
                    :filled-p t) )
    (:inset
     (draw-border-right sink :solid (3d-light-color sink color) x2 x3 y0 y1 y2 y3))
    (:outset
     (draw-border-right sink :solid (3d-dark-color sink color) x2 x3 y0 y1 y2 y3))
    (:ridge
     (draw-border-right sink :outset color (+ x2 (floor (- x3 x2) 2)) x3
			y0 (+ y0 (floor (- y1 y0) 2)) (+ y2 (floor (- y3 y2) 2)) y3)
     (draw-border-right sink :inset color x2 (+ x2 (floor (- x3 x2) 2))
			(+ y0 (floor (- y1 y0) 2)) y1 y2 (+ y2 (ceiling (- y3 y2) 2))))
    (:groove
     (draw-border-right sink :inset color (+ x2 (floor (- x3 x2) 2)) x3
			y0 (+ y0 (floor (- y1 y0) 2)) (+ y2 (floor (- y3 y2) 2)) y3)
     (draw-border-right sink :outset color x2 (+ x2 (floor (- x3 x2) 2))
			(+ y0 (floor (- y1 y0) 2)) y1 y2 (+ y2 (ceiling (- y3 y2) 2))))
    (:double
     (draw-border-right sink :solid color (- x3 (ceiling (- x3 x2) 3)) x3
		       y0 (+ y0 (ceiling (- y1 y0) 3)) (- y3 (ceiling (- y3 y2) 3)) y3)
     
     (draw-border-right sink :solid color x2 (+ x2 (floor (- x3 x2) 3))
		       (- y1 (floor (- y1 y0) 3)) y1
		       y2 (+ y2 (floor (- y3 y2) 3))) )
    
    (:dashed
     (draw-dashed-line sink 
                       (floor (+ x2 x3) 2)
                       (floor (+ y0 y1) 2)
                       (floor (+ x2 x3) 2)
                       (floor (+ y2 y3) 2)
                       (abs (- x2 x3))))
    (:dotted
     (draw-dotted-line sink 
                       (floor (+ x2 x3) 2)
                       (floor (+ y0 y1) 2)
                       (floor (+ x2 x3) 2)
                       (floor (+ y2 y3) 2)
                       (abs (- x2 x3))))
    
    ((:none))))

(defun draw-border-bottom (sink style color y2 y3 x0 x1 x2 x3)
  (ecase style
    (:solid 
     (draw-polygon* sink (mapcar #'floor (list x0 y3  x1 y2  x2 y2  x3 y3))
                    :foreground color
                    :filled-p t) )
    (:inset
     (draw-border-bottom sink :solid (3d-light-color sink color) y2 y3 x0 x1 x2 x3))
    (:outset
     (draw-border-bottom sink :solid (3d-dark-color sink color) y2 y3 x0 x1 x2 x3))
    (:ridge
     (draw-border-bottom sink :outset color (+ y2 (floor (- y3 y2) 2)) y3
			x0 (+ x0 (floor (- x1 x0) 2)) (+ x2 (floor (- x3 x2) 2)) x3)
     (draw-border-bottom sink :inset color y2 (+ y2 (floor (- y3 y2) 2))
			(+ x0 (floor (- x1 x0) 2)) x1 x2 (+ x2 (ceiling (- x3 x2) 2))))
    (:groove
     (draw-border-bottom sink :inset color (+ y2 (floor (- y3 y2) 2)) y3
			x0 (+ x0 (floor (- x1 x0) 2)) (+ x2 (floor (- x3 x2) 2)) x3)
     (draw-border-bottom sink :outset color y2 (+ y2 (floor (- y3 y2) 2))
			(+ x0 (floor (- x1 x0) 2)) x1 x2 (+ x2 (ceiling (- x3 x2) 2))))
    (:double
     (draw-border-bottom sink :solid color (- y3 (ceiling (- y3 y2) 3)) y3
		       x0 (+ x0 (ceiling (- x1 x0) 3)) (- x3 (ceiling (- x3 x2) 3)) x3)
     
     (draw-border-bottom sink :solid color y2 (+ y2 (floor (- y3 y2) 3))
		       (- x1 (floor (- x1 x0) 3)) x1
		       x2 (+ x2 (floor (- x3 x2) 3))) )
    
    (:dotted
     (draw-dotted-line sink
                       (floor (+ x0 x1) 2)
                       (floor (+ y2 y3) 2)
                       (floor (+ x2 x3) 2)
                       (floor (+ y2 y3) 2)
                       (abs (- y2 y3))))
    
    (:dashed
     (draw-dashed-line sink
                       (floor (+ x0 x1) 2)
                       (floor (+ y2 y3) 2)
                       (floor (+ x2 x3) 2)
                       (floor (+ y2 y3) 2)
                       (abs (- y2 y3))))
    
    ((:none)) ))

(defun draw-dashed-line (sink x1 y1 x2 y2 w)
  (setf w (ceiling w))
  (when (plusp w)
    (using-gcontext (gc :drawable sink
                        :line-width  w
                        :cap-style :butt
                        :join-style :round
                        :line-style  :dash
                        :dashes (list (* 2 w) (* 2 w)))
                    (xlib:draw-line sink gc x1 y1 x2 y2))))

(defun draw-dotted-line (sink x1 y1 x2 y2 w)
  (setf w (ceiling w))
  (when (plusp w)
    (using-gcontext (gc :drawable sink
                        :line-width w
                        :cap-style  :round
                        :join-style :round
                        :line-style :dash
                        :dashes (list 1 (* 2 w)))
                    (xlib:draw-line sink gc x1 y1 x2 y2))))

(defun 3d-dark-color (contact x)  contact x "#6e6e6e")
(defun 3d-light-color (contact x) contact x "#e7e7e7")

(defun draw-up-arrow (sink x0 y0 x1 y1)
  (let ((x2 (floor (+ x0 x1) 2))
        (dark (3d-dark-color sink :black))
        (light (3d-light-color sink :black))
        (d 2))
    (draw-polygon* sink (list x2 y0 x1 y1 x0 y1)
                   :foreground *default-background*
                   :filled-p t)
    (draw-polygon* sink (list x2 y0 
                              x1 y1 
                              (- x1 (ceiling (* 1.2 d))) (- y1 d)
                              x2 (+ y0 (round (* (sqrt 2) d))))
                   :foreground dark
                   :filled-p t)
    (draw-polygon* sink (list x1 y1 
                              (- x1 (ceiling (* 1.2 d))) (- y1 d)
                              (+ x0 (ceiling (* 1.2 d))) (- y1 d)
                              x0 y1)
                   :foreground dark
                   :filled-p t)
    (draw-polygon* sink (list x2 y0
                              x0 y1
                              (+ x0 (ceiling (* 1.2 d))) (- y1 d)
                              x2 (+ y0 (round (* (sqrt 2) d))))
                   :foreground light
                   :filled-p t)))

(defun draw-down-arrow (sink x0 y0 x1 y1)
  (let ((x2 (floor (+ x0 x1) 2))
        (dark (3d-dark-color sink :black))
        (light (3d-light-color sink :black))
        (d 2))
    (draw-polygon* sink (list x2 y1 x1 y0 x0 y0)
                   :foreground *default-background*
                   :filled-p t)
    (draw-polygon* sink (list x2 y1
                              x1 y0 
                              (- x1 (ceiling (* 1.2 d))) (+ y0 d)
                              x2 (- y1 (round (* (sqrt 2) d))))
                   :foreground dark
                   :filled-p t)
    (draw-polygon* sink (list x1 y0
                              (- x1 (ceiling (* 1.2 d))) (+ y0 d)
                              (+ x0 (ceiling (* 1.2 d))) (+ y0 d)
                              x0 y0)
                   :foreground light
                   :filled-p t)
    (draw-polygon* sink (list x2 y1
                              x0 y0
                              (+ x0 (ceiling (* 1.2 d))) (+ y0 d)
                              x2 (- y1 (round (* (sqrt 2) d))))
                   :foreground light
                   :filled-p t)))

(defun draw-left-arrow (sink x0 y0 x1 y1)
  (let ((y2 (floor (+ y0 y1) 2))
        (dark (3d-dark-color sink :black))
        (light (3d-light-color sink :black))
        (d 2))
    (draw-polygon* sink (list x0 y2 x1 y1 x1 y0)
                   :foreground *default-background*
                   :filled-p t)
    (draw-polygon* sink (list x0 y2 
                              x1 y1 
                              (- x1 d) (- y1 (ceiling (* 1.2 d)))
                              (+ x0 (round (* (sqrt 2) d))) y2)
                   :foreground dark
                   :filled-p t)
    (draw-polygon* sink (list x1 y1 
                              (- x1 d) (- y1 (ceiling (* 1.2 d)))
                              (- x1 d) (+ y0 (ceiling (* 1.2 d)))
                              x1 y0)
                   :foreground dark
                   :filled-p t)
    (draw-polygon* sink (list x0 y2
                              x1 y0
                              (- x1 d) (+ y0 (ceiling (* 1.2 d)))
                              (+ x0 (round (* (sqrt 2) d))) y2)
                   :foreground light
                   :filled-p t)))

(defun draw-right-arrow (sink x0 y0 x1 y1)
  (let ((y2 (floor (+ y0 y1) 2))
        (dark (3d-dark-color sink :black))
        (light (3d-light-color sink :black))
        (d 2))
    (draw-polygon* sink (list x1 y2 x0 y1 x0 y0)
                   :foreground *default-background*
                   :filled-p t)
    (draw-polygon* sink (list x1 y2
                              x0 y1 
                              (+ x0 d) (- y1 (ceiling (* 1.2 d)))
                              (- x1 (round (* (sqrt 2) d))) y2)
                   :foreground dark
                   :filled-p t)
    (draw-polygon* sink (list x0 y1
                              (+ x0 d) (- y1 (ceiling (* 1.2 d)))
                              (+ x0 d) (+ y0 (ceiling (* 1.2 d)))
                              x0 y0)
                   :foreground light
                   :filled-p t)
    (draw-polygon* sink (list x1 y2
                              x0 y0
                              (+ x0 d) (+ y0 (ceiling (* 1.2 d)))
                              (- x1 (round (* (sqrt 2) d))) y2)
                   :foreground light
                   :filled-p t)))

(defun draw-diamond (sink x0 y0 x1 y1 &key (shadow-width 2) (shadow-style :outset)
                                           (dark-background *default-dark-background*)
                                           (background *default-background*))
  (when (oddp (- x1 x0))
    (incf y0)
    (incf y1))
  (let ((x2 (floor (+ x0 x1) 2))
        (y2 (floor (+ y0 y1) 2)))
    (incf shadow-width 2)
    (draw-polygon* sink (list x2 y0 x0 y2 x2 y1 x1 y2)
                   :foreground (if (eq shadow-style :inset)
                                   dark-background
                                 background)
                   :filled-p t)
    (draw-polygon* sink (list x2 y0 x0 y2 (+ x0 shadow-width) y2 x2 (+ y0 shadow-width))
                   :foreground (if (eq shadow-style :inset)
                                   (3d-dark-color sink :black)
                                 (3d-light-color sink :black))
                   :filled-p t)
    (draw-polygon* sink (list x2 y0 x1 y2 (- x1 shadow-width) y2 x2 (+ y0 shadow-width))
                   :foreground (if (eq shadow-style :inset)
                                   (3d-dark-color sink :black)
                                 (3d-light-color sink :black))
                   :filled-p t)
    (draw-polygon* sink (list x2 y1 x0 y2 (+ x0 shadow-width) y2 x2 (- y1 shadow-width))
                   :foreground (if (eq shadow-style :inset)
                                   (3d-light-color sink :black)
                                 (3d-dark-color sink :black))
                   :filled-p t)
    (draw-polygon* sink (list x2 y1 x1 y2 (- x1 shadow-width) y2 x2 (- y1 shadow-width))
                   :foreground (if (eq shadow-style :inset)
                                   (3d-light-color sink :black)
                                 (3d-dark-color sink :black))
                   :filled-p t)
    ;; x11's distinguishion between filled and hollow sucks considerable!
    (when *trauerrand-p*
      (if (evenp (- x1 x0))
          (draw-polygon* sink (list x2 y0 
                                    x0 y2 
                                    (1- x2) (1- y1)
                                    x2 (1- y1)
                                    (1- x1) y2
                                    (1- x2) y0)
                         :foreground :black
                         :filled-p nil)
        (draw-polygon* sink (list x2 y0 
                                  x0 y2 
                                  ;;(1- x2) (1- y1)
                                  x2 (1- y1)
                                  (1- x1) y2
                                  x2 y0)
                       :foreground :black
                       :filled-p nil)))))


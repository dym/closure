;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: GLUE; -*-
;;; --------------------------------------------------------------------------------------
;;;     Title: Glue Scrollbars
;;;   Created: 1999-03-24
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: GPL (See file COPYING for details).
;;; --------------------------------------------------------------------------------------
;;;  (c) copyright 1999-2001 by Gilbert Baumann

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Scrollbars
;;;;

(defcontact scrollbar (3d)
  ((thumb-size :initform 1 :initarg :thumb-size :reader thumb-size)
   (thumb-pos  :initform 0 :initarg :thumb-pos :reader thumb-pos)
   (mb-xo :initform 0)
   (mb-yo :initform 0) )
  (:resources
   (background :initform *default-dark-background*)
   (shadow-style :initform :inset)
   (shadow-width :initform 2)
   (shadow-color :initform :black)
   (margin-height :initform 0)   
   (margin-width :initform 0)
   (padding-height :initform 0)   
   (padding-width :initform 0) ))

(defcontact vertical-scrollbar (scrollbar)
  ()
  (:resources
   (background :initform *default-dark-background*)
   (shadow-style :initform :inset)
   (shadow-width :initform 2)
   (shadow-color :initform :black)
   (margin-height :initform 0)   
   (margin-width :initform 0)
   (padding-height :initform 0)   
   (padding-width :initform 0) ))

(defcontact horizontal-scrollbar (scrollbar)
  ()
  (:resources
   (background :initform *default-dark-background*)
   (shadow-style :initform :inset)
   (shadow-width :initform 2)
   (shadow-color :initform :black)
   (margin-height :initform 0)   
   (margin-width :initform 0)
   (padding-height :initform 0)   
   (padding-width :initform 0) ) )

(defmethod preferred-size ((self vertical-scrollbar) &key)
  ;; scheiss egal
  (values 15 100 0))

(defevent vertical-scrollbar :motion-notify motion-notify)
(defevent vertical-scrollbar :button-press button-press)
(defevent vertical-scrollbar :button-release nop)

(defevent horizontal-scrollbar :motion-notify motion-notify)
(defevent horizontal-scrollbar :button-press button-press)
(defevent horizontal-scrollbar :button-release nop)

(defmethod button-press ((self scrollbar))
  (with-slots (mb-xo mb-yo) self
    (with-event (x y)
      (multiple-value-bind (x0 y0 x1 y1) (scrollbar-thumb-rectangle* self)
        (setf mb-xo (max 0 (min (- x1 x0) (- x x0)))
              mb-yo (max 0 (min (- y1 y0) (- y y0))) ) )
      (scrollbar-mouse-update-thumb self x y))))

(defmethod motion-notify ((self scrollbar))
  (with-event (state x y)
    (when (not (zerop (logand state #xf00)))
      (scrollbar-mouse-update-thumb self x y))))

(defmethod scrollbar-mouse-update-thumb ((self vertical-scrollbar) x y)
  (declare (ignorable x))
  (with-slots (mb-yo mb-xo) self
    (multiple-value-bind (x0 y0 x1 y1) (scrollbar-slider-area-rectangle* self)
      (declare (ignorable x0 x1))
      (setf (thumb-pos self) (/ (- (- y mb-yo) y0) (max 1 (- y1 y0)))))))

(defmethod scrollbar-mouse-update-thumb ((self horizontal-scrollbar) x y)
  (declare (ignorable x))
  (with-slots (mb-yo mb-xo) self
    (multiple-value-bind (x0 y0 x1 y1) (scrollbar-slider-area-rectangle* self)
      (declare (ignorable y0 y1))
      (setf (thumb-pos self) (/ (- (- x mb-xo) x0) (max 1 (- x1 x0)))))))

(defmethod (setf thumb-pos) (new (self scrollbar))
  (with-slots (thumb-pos thumb-size) self
    (setf thumb-pos (min (- 1 thumb-size) (max 0 new)))
    (apply-callback self :value-changed thumb-pos)
    (when (realized-p self)
      (redisplay self))))

(defmethod (setf thumb-size) (new (self scrollbar))
  (with-slots (thumb-pos thumb-size) self
    (setf thumb-size (min 1 (max 0 new)))
    (setf thumb-pos (min (- 1 thumb-size) (max 0 thumb-pos)))
    (apply-callback self :value-changed thumb-pos)
    (when (realized-p self)
      (redisplay self))
    new))

(defmethod scrollbar-thumb-rectangle* ((self vertical-scrollbar))
  (with-slots (thumb-pos thumb-size) self
    (let* ((v0 thumb-pos)
           (v1 (+ v0 thumb-size)))
      (multiple-value-bind (x0 y0 x1 y1) (scrollbar-slider-area-rectangle* self)
        (values (+ 1 x0) 
                (+ y0 (round (* v0 (- y1 y0 1))) 1) 
                (- x1 1)
                (+ y0 (round (* v1 (- y1 y0 1)))))))))

;;; ---------------------------------------------------------------------------

(defmethod scrollbar-thumb-rectangle* ((self horizontal-scrollbar))
  (with-slots (thumb-pos thumb-size) self
    (let* ((v0 thumb-pos)
           (v1 (+ v0 thumb-size)))
      (multiple-value-bind (y0 x0 y1 x1) (scrollbar-slider-area-rectangle* self)
        (values (+ y0 (round (* v0 (- y1 y0 1))) 1) 
                (+ 1 x0) 
                (+ y0 (round (* v1 (- y1 y0 1))))
                (- x1 1))))))

(defmethod preferred-size ((self horizontal-scrollbar) &key)
  ;; scheiss egal
  (values 100 13 0))

(defmethod scrollbar-up-arrow-rectangle* ((self vertical-scrollbar))
  (values 
   1 1 (- (contact-width self) 2) (- (contact-width self) 2)))

(defmethod scrollbar-down-arrow-rectangle* ((self vertical-scrollbar))
  (values 
   1 
   (- (contact-height self) (contact-width self) -1)
   (- (contact-width self) 2)
   (- (contact-height self) 2)))

(defmethod scrollbar-slider-area-rectangle* ((self vertical-scrollbar))
  (multiple-value-bind (x0 y0 x1 y1) (interior-rectangle* self)
    (values x0 (+ y0 (- x1 x0) 2)
            x1 (- y1 (- x1 x0) 3))))

(defmethod clue:display-region ((self vertical-scrollbar) region)
  (declare (ignore region))
  (with-slots (thumb-size thumb-pos) self
    (let* ((v0 thumb-pos)
           (v1 (+ v0 thumb-size)))
      (multiple-value-bind (x0 y0 x1 y1) (scrollbar-up-arrow-rectangle* self)
        (draw-up-arrow self x0 y0 x1 y1) )  
      (multiple-value-bind (x0 y0 x1 y1) (scrollbar-down-arrow-rectangle* self)
        (draw-down-arrow self x0 y0 x1 y1))
      (multiple-value-bind (x0 y0 x1 y1) (scrollbar-slider-area-rectangle* self)
        (let ((y2 (+ y0 (round (* v0 (- y1 y0 1)))))
              (y3 (+ y0 (round (* v1 (- y1 y0 1))))))
          (old-draw-rectangle* self 
                           (+ x0) y0 
                           (+ x1) (+ y0 (round (* v0 (- y1 y0 1))))
                           :foreground (contact-background self)
                           :filled-p t)
          (old-draw-rectangle* self 
                           (+ x0) y3 
                           (+ x1) y1
                           :foreground (contact-background self)
                           :filled-p t)

          (old-draw-rectangle* self 
                           (+ x0 2) 
                           (+ y0 2 (round (* v0 (- y1 y0 )))) 
                           (+ x1 -2)
                           (+ y0 -2 (round (* v1 (- y1 y0 )))) 
                           :foreground *default-background*
                           :filled-p t)
          (dolist (j '(-3 0 3))
            (old-draw-line* self (+ x0 2) (+ j (floor (+ y2 y3) 2))
                        (- x1 2) (+ j (floor (+ y2 y3) 2))
                        :foreground (3d-dark-color self :black))
            (old-draw-line* self (+ x0 2) (+ j 1 (floor (+ y2 y3) 2)) 
                        (- x1 2) (+ j 1 (floor (+ y2 y3) 2))
                        :foreground (3d-light-color self :black)))
          (draw-simple-border self
                              (+ 0 x0)
                              (+ y0 (round (* v0 (- y1 y0)))) 
                              (- x1 0 x0 -1)
                              (- (+ y0 (round (* v1 (- y1 y0))))
                                 (+ y0 (round (* v0 (- y1 y0))))
                                 -1)
                              :outset) )))))

(defmethod scrollbar-left-arrow-rectangle* ((self horizontal-scrollbar))
  (values 
   1 1 (- (contact-height self) 2) (- (contact-height self) 2)))

(defmethod scrollbar-right-arrow-rectangle* ((self horizontal-scrollbar))
  (values 
   (- (contact-width self) (contact-height self) -1)
   1
   (- (contact-width self) 2)
   (- (contact-height self) 2)))

(defmethod scrollbar-slider-area-rectangle* ((self horizontal-scrollbar))
  (multiple-value-bind (x0 y0 x1 y1) (interior-rectangle* self)
    (values (+ x0 (- y1 y0) 2) y0
            (- x1 (- y1 y0) 3) y1)))

(defmethod clue:display-region ((self horizontal-scrollbar) region)
  (declare (ignore region))
  (with-slots (thumb-size thumb-pos) self
    (let* ((v0 thumb-pos)
           (v1 (+ v0 thumb-size)))
      (multiple-value-bind (x0 y0 x1 y1) (scrollbar-left-arrow-rectangle* self)
        (draw-left-arrow self x0 y0 x1 y1) )  
      (multiple-value-bind (x0 y0 x1 y1) (scrollbar-right-arrow-rectangle* self)
        (draw-right-arrow self x0 y0 x1 y1))
      (multiple-value-bind (x0 y0 x1 y1) (scrollbar-slider-area-rectangle* self)
        (let ((x2 (+ x0 (round (* v0 (- x1 x0 1)))))
              (x3 (+ x0 (round (* v1 (- x1 x0 1))))))
          (old-draw-rectangle* self 
                           x0 y0 
                           (+ x0 (round (* v0 (- x1 x0 1)))) y1
                           :foreground (contact-background self)
                           :filled-p t)
          (old-draw-rectangle* self 
                           x3 y0 
                           x1 y1
                           :foreground (contact-background self)
                           :filled-p t)
          (old-draw-rectangle* self 
                           (+ x0 2 (round (* v0 (- x1 x0 )))) (+ y0 2 ) 
                           (+ x0 -2 (round (* v1 (- x1 x0 ))))
                           (+ y1 -2) 
                           :foreground *default-background*
                           :filled-p t)
          (dolist (j '(-3 0 3))
            (old-draw-line* self 
                        (+ j (floor (+ x2 x3) 2)) (+ y0 2)
                        (+ j (floor (+ x2 x3) 2)) (- y1 2)
                        :foreground (3d-dark-color self :black))
            (old-draw-line* self 
                        (+ j 1 (floor (+ x2 x3) 2)) (+ y0 2) 
                        (+ j 1 (floor (+ x2 x3) 2)) (- y1 2)
                        :foreground (3d-light-color self :black)))
          (draw-simple-border self
                              (+ x0 (round (* v0 (- x1 x0)))) y0 
                              (- (+ x0 (round (* v1 (- x1 x0))))
                                 (+ x0 (round (* v0 (- x1 x0))))
                                 -1)
                              (- y1 0 y0 -1)
                              :outset) )))))


;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: GLUE; -*-
;;; --------------------------------------------------------------------------------------
;;;     Title: List Boxen
;;;   Created: 1999-03-24
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

;;;; ---------------------------------------------------------------------------
;;;;  List Boxen
;;;;

;;; User API

;;
;; (value <list-box>) -> list of selected items [slot]
;; (items <list-box>) -> list of items [slot]
;; :value-changed new-value [callback]
;;

(defstruct item
  object
  width height depth
  x y)

(defcontact list-box (3d composite simple-geometry-management-mixin)
  ((scrollbar)
   (canvas)
   (items       :initform nil   :initarg :items)
   (ytot        :initform 1)
   (font)
   ;; glitter
   (item-border)
   (canvas-border :initform 3)
   (spacing)
   (selected      :initform nil)
   (foreground)
   (highlight-foreground)
   (highlight-background) 
   (selection-mode)
   (nvisible))
  
  (:Resources
   (font                 :type afont                       :initform *default-font*)
   (foreground           :type xlib:pixel                  :initform :black)
   (highlight-foreground :type xlib:pixel                  :initform :white) 
   (highlight-background :type xlib:pixel                  :initform :black)
   (selection-mode       :type (member :single :multiple)  :initform :single)
   (item-border          :type integer                     :initform 1)
   (spacing              :type integer                     :initform 1)
   (nvisible             :type (or integer (member :auto)) :initform :auto) ))

(defcontact list-box-canvas (canvas)
  ((list-box :initarg :list-box)) )

(defmethod cluei::initialize-contact ((self list-box))
  (with-slots (canvas scrollbar font) self
    (setf scrollbar (make-contact 'vertical-scrollbar
                                  :parent self
                                  :width 1 :height 1))
    (setf canvas
      (make-contact 'list-box-canvas
                    ;; xxx make this tweakable
                    :background :white
                    :border-width 0
                    :parent self
                    :list-box self
                    :width 1 :height 1))
    (setf (sink-font canvas) font)
    (update self)
    (clue:add-callback scrollbar :value-changed (curry #'list-box-hbar-callback self)) ))

(defmethod compute-geometry ((self list-box) child &optional width height)
  width height
  (with-slots (scrollbar canvas) self
    (let ((x1 (max 1 (nth-value 0 (clue:preferred-size scrollbar)))))
      (multiple-value-bind (wx0 wy0 wx1 wy1) (interior-rectangle* self) 
        (cond ((eql child scrollbar)
               (values wx0 wy0 x1 (max 1 (- wy1 wy0 -1)) 0))
              ((eql child canvas)
               (values (+ wx0 x1) wy0
                       (max 1 (- wx1 wx0 x1 -1))
                       (max 1 (- wy1 wy0 -1)) 
                       0))
              (t
               (error "Oops.")))))))

(defmethod clue:preferred-size ((self list-box) &key width height border-width)
  (declare (ignore width height border-width ))
  (with-slots (items scrollbar canvas item-border canvas-border nvisible spacing) self
    (let ((w
           (+ (nth-value 0 (clue:preferred-size scrollbar))
              (left-leading self)
              ;;(left-leading canvas)
              (right-leading self)
              ;;(right-leading canvas)
              (* 2 item-border)
              (* 2 canvas-border)
              (reduce #'max (mapcar #'item-width items) :initial-value 1)))
          (h))
      (setf h
        (cond ((eq nvisible :auto)
               (max 1 (reduce #'+ (mapcar (lambda (x)
                                               (+ (item-height x)
                                                  (item-depth x)
                                                  spacing
                                                  (* 2 item-border)))
                                          items))))
              (t
               (let ((ah (if (= 0 (length items))
                             16         ;
                           (max 1 
                                (/ (reduce #'+ (mapcar (lambda (x)
                                                         (+ (item-height x)
                                                            (item-depth x)
                                                            spacing
                                                            (* 2 item-border)))
                                                       items))
                                   (length items))))))
                 (ceiling (* ah nvisible))))))
      (setf h (+ (top-leading self) (bottom-leading self) h
                 (* 2 canvas-border)))
      (values (max 1 w)
              (max 1 h)
              0))))

(defmethod clue:resize :after ((self list-box) width height border-width)
  width height border-width
  (update self))

(defmethod (setf items) (new-value (self list-box))
  (with-slots (items font canvas) self
    (setf items (mapcar (lambda (obj)
                          (multiple-value-bind (width height depth)
                              (item-size obj canvas)
                            (make-item :object obj
                                       :width width
                                       :height height
                                       :depth depth)))
                        new-value))
    (update self)
    new-value))

;;;;;;;;;

(defmethod list-box/layout-items ((self list-box))
  (with-slots (items item-border spacing canvas-border scrollbar) self
    (let* ((w (+ (* 2 item-border) 20
                 (reduce #'max (mapcar (lambda (item)
                                         (item-width item))
                                       items)
                         :initial-value 0)))
           (avail (- (contact-width self) (* 2 canvas-border) (contact-width scrollbar)))
           (nc (max 1 (floor avail w))))
      (setf nc 1)
      (let ((y canvas-border)
            (column 0))
        (dolist (k items)
          (setf (item-x k) (+ canvas-border (* column w))
                (item-y k) (+ y item-border (item-height k)))
          (incf column)
          (when (= column nc)
            (setf column 0)
            (incf y (+ (item-height k) (item-depth k) (* 2 item-border) spacing))))))))

(defmethod update ((self list-box))
  (with-slots (canvas ytot item-border spacing canvas-border items font scrollbar) self
    (list-box/layout-items self)
    (setf ytot (+ canvas-border
                  (reduce #'max (mapcar (lambda (item)
                                          (+ (item-y item) (item-depth item)))
                                        items)
                          :initial-value 0)))
    (setf (thumb-size scrollbar) (/ (contact-height canvas) (max 1 ytot)))
    (when (realized-p self)
      (redisplay self)
      (xlib:clear-area canvas)
      (redisplay canvas))) )

(defmethod list-box-hbar-callback ((self list-box) value)
  (with-slots (ytot yo canvas) self
    (when (realized-p self)
      (canvas-move-origin canvas 0 (- (round (* value ytot))))) ))

;;;;;;;;;

(defun list-box-item-extents (list-box item)
  (with-slots (item-border canvas-border canvas) list-box
    (values (item-x item)
            (- (item-y item) (item-height item) item-border)
            (- (contact-width canvas) canvas-border)
            (+ (item-y item) (item-depth item) item-border))))


(defmethod canvas-display-region ((self list-box-canvas) region)
  (with-slots (list-box) self
    (with-slots (items item-border selected foreground highlight-foreground highlight-background) list-box
      (dolist (item items)
        (multiple-value-bind (ix0 iy0 ix1 iy1) (list-box-item-extents list-box item)
          (when (region-intersects-rectangle-p region ix0 iy0 ix1 iy1)
            (multiple-value-bind (fg bg)
                (if (member item selected)
                    (values highlight-foreground highlight-background)
                  (values foreground nil))
              (draw-rectangle* self  ix0 iy0 ix1 iy1
                               :foreground :white ;xxx fixme
                               :filled t
                               )      
              '(clear-rectangle* self ix0 iy0 ix1 iy1)
              (when bg
                ;;(setf bg :blue)
                (draw-rectangle* self ix0 iy0 ix1 iy1 :foreground bg :filled t) )
              (with-drawing-options-maybe (self 
                                           :foreground fg
                                           :clipping-region (make-rectangle* ix0 iy0 ix1 iy1))
                (item-paint (item-object item) self 
                            (+ (item-x item) item-border) 
                            (+ (item-y item) item-border) )) )))))))

(defun point-in-item-p (px py item)
  (and (<= (item-x item) px #x7FFF)
       (<= (- (item-y item) (item-height item)) py (+ (item-y item) (item-depth item)))))

(defevent list-box-canvas :button-press button-press)

(defmethod button-press ((self list-box-canvas))
  (let ((list-box (slot-value self 'list-box)))
    (with-slots (items selection-mode) list-box
      (clue:with-event (x y)
        (multiple-value-bind (x y) (translate-device-coordinates self x y)
          (let ((item (find-if (curry #'point-in-item-p x y) items)))
            (when item
              (ecase selection-mode
                (:single
                 (list-box/select-item list-box item))
                (:multiple
                 (list-box/toggle-item list-box item))))))))))

(defmethod list-box-selected-items ((self list-box))
  (slot-value self 'selected))

(defmethod (setf list-box-selected-items) (new-value (self list-box))
  (with-slots (selected canvas) self
    (let ((dirty-region
           (reduce #'region-union 
                   (mapcar (lambda (item)
                             (multiple-value-call #'make-rectangle*
                               (list-box-item-extents self item)))
                           (set-exclusive-or new-value selected))
                   :initial-value +nowhere+)))
      (setf selected new-value)
      (when (realized-p canvas)
        (canvas-display-region canvas dirty-region))
      (clue:apply-callback self :value-changed (value self))
      new-value)))

(defmethod list-box/toggle-item ((self list-box) item)
  (with-slots (selected canvas) self
    (setf (list-box-selected-items self)
      (cond ((member item (list-box-selected-items self))
             (remove item (list-box-selected-items self)))
            (t
             (cons item (list-box-selected-items self)))))))

(defmethod list-box/select-item ((self list-box) item)
  (with-slots (selected canvas) self
    (setf (list-box-selected-items self) (list item))))

(defmethod value ((self list-box))
  (mapcar #'item-object (list-box-selected-items self)))

(defmethod (setf value) (new-value (self list-box))
  (setf (list-box-selected-items self)
    (remove-if-not (lambda (x)
                     (member (item-object x) new-value :test #'eql))
                   (slot-value self 'items))))




;;;; ---------------------------------------------------------------------------
;;;;  item protocol for simple things
;;;;

;; t

(defmethod item-size ((string string) medium)
  (values (new-text-width medium string)
          (text-ascent medium)
          (text-descent medium)))

(defmethod item-paint ((string string) medium x y)
  (draw-text* medium string x y))

;; string

(defmethod item-size ((object t) sink)
  (item-size (format nil "~S" object) sink))

(defmethod item-paint ((object t) sink x y)
  (item-paint (format nil "~S" object) sink x y))


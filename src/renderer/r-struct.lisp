;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: RENDERER; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: The Renderer's Data Structures
;;;   Created: 1999-01-08
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: GPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
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

;;  When        Who     What
;; ----------------------------------------------------------------------------
;;  1999-08-17  GB      - Added sane accessors
;;                        {inner,outer}-{left,right}-edge
;;                        {inner,outer}-{width,height}
;;                        for BBOXen


(in-package :RENDERER)

;;; ---- Data Structures ----------------------------------------------------------------------

(declaim (inline make-abox make-bbox make-rbox make-background make-border))

(defstruct abox
  pt

  padding
  margin-top 
  margin-right 
  margin-bottom
  margin-left 

  ;;padding-top
  ;;padding-right
  ;;padding-bottom
  ;;padding-left

  white-space
  
  border
  background
  contents
  (color "black")
  (map nil)        ;possible (image) map
  bx0 by0 bx1 by1) ;Bounding box (eigentlich reichen hier auch relative Koordinaten).

(defsubst abox-padding-top (x)    (if (setq x (abox-padding x)) (padding-top x) 0))
(defsubst abox-padding-right (x)  (if (setq x (abox-padding x)) (padding-right x) 0))
(defsubst abox-padding-bottom (x) (if (setq x (abox-padding x)) (padding-bottom x) 0))
(defsubst abox-padding-left (x)   (if (setq x (abox-padding x)) (padding-left x) 0))

(defsubst bbox-padding-top (x)    (if (setq x (bbox-padding x)) (padding-top x) 0))
(defsubst bbox-padding-right (x)  (if (setq x (bbox-padding x)) (padding-right x) 0))
(defsubst bbox-padding-bottom (x) (if (setq x (bbox-padding x)) (padding-bottom x) 0))
(defsubst bbox-padding-left (x)   (if (setq x (bbox-padding x)) (padding-left x) 0))

(defsubst ibox-padding-top (x)    (if (setq x (ibox-padding x)) (padding-top x) 0))
(defsubst ibox-padding-right (x)  (if (setq x (ibox-padding x)) (padding-right x) 0))
(defsubst ibox-padding-bottom (x) (if (setq x (ibox-padding x)) (padding-bottom x) 0))
(defsubst ibox-padding-left (x)   (if (setq x (ibox-padding x)) (padding-left x) 0))

(defstruct (bbox (:include abox))
  ;; block box

  ;; This was initially a container for black contents, now it just a
  ;; holder for the vertical dimension of a block elements content. 
  ;; And for the output record of the border and background, in case
  ;; somebody wants to enlarge a box post-mortem.

  width        ;inner width (without padding, margin or border)
  text-align   
  iy           ;top border edge (that is after margin before padding).
  ix           ;inner left edge
  iheight      ;inner height
  decoration-output-record)       ;output record of background (and border).

(defstruct (ibox (:include abox)
            (:constructor make-ibox/boa 
                          (pt 
                           padding 
                           margin-top margin-right margin-bottom margin-left
                           white-space
                           border
                           background
                           contents
                           color
                           map
                           bx0 by0 bx1 by1
                           text-style
                           height
                           depth
                           line-height
                           text-decoration
                           valign
                           y-oben
                           x)
                              
            ))
  ;; inline box
  text-style
  height                        ;inner height (distance from baseline to inner top edge)
  depth                         ;inner depth  (distance from baseline to inner bottom edge)
  line-height
  (text-decoration :none)
  (valign :baseline)            ;vertical align
  y-oben                        ;outer top edge (modulo vertical align)
  x)                            ;outer left edge

(defstruct (gbox 
            (:print-function print-gbox)
            (:constructor make-gbox-boa (runes start end parent)))
  runes
  start
  end
  parent                                ; this slot is to be nuked.
  (%twidth nil))

(defstruct rbox 
  obj)

(defun print-gbox (object sink depth)
  (declare (ignore depth))
  (format sink "#<~S ~S>" 
          (type-of object)
          (map 'string (lambda (x) (or (code-char x) (code-char #o277) #\?))
               (subseq (gbox-runes object) (gbox-start object) (gbox-end object)))))

(defstruct (text-style 
            (:constructor make-text-style-prim 
                          (font font-family font-weight font-size font-style font-variant letter-spacing word-spacing)))
  font
  font-family
  font-weight
  font-size
  font-style
  font-variant
  letter-spacing
  word-spacing)

(defmethod glue::afontp ((x text-style))
  (declare (ignoreable x))
  t)

(defstruct background
  (color      :transparent)             ;simple string or :transparent
  (image      :none)                    ;url
  (repeat     :repeat)                  ;:repeat, :repeat-x, :repeat-y, :no-repeat
  (attachment :scroll)                  ;:fixed, :scroll, :global
  position                              ;A cons of horizontal and vertical background-position.
                                        ; each then is either a real number or a percentage.
  %pixmap
  %mask)

(defstruct border
  top-width    top-style    top-color
  right-width  right-style  right-color
  bottom-width bottom-style bottom-color
  left-width   left-style   left-color)

(defstruct (padding (:constructor make-padding/low/boa (top right bottom left)))
  top right bottom left)

(defvar +null-padding+ (make-padding/low/boa 0 0 0 0))

(defun make-padding (top right bottom left)
  (cond ((and (eql 0 top) (eql 0 right) (eql 0 bottom) (eql 0 left))
         +null-padding+)
        (t
         (make-padding/low/boa top right bottom left))))

(defstruct fl-task
  kind                                  ; :inside, :outside
  box)

(defstruct fbox-desc
  x y w h       ;absolute coordinates of the no-no area
  bbox          ;generated bbox for floating element
  pt            ;pt of floated element
  side)         ;either :LEFT or :RIGHT

(defvar *rcontext*)

(defstruct rc
  device
  y             ;current y coordinate
  x0            ;x-coordinate of current left margin
  x1            ;x-coordinate of current right margin
  vertical-margins
  vertical-margin-callbacks
  first-line-tasks
  left-floating-boxen ;all left floating boxen [list of FBOX-DESC]
  right-floating-boxen                  ;all right floating boxen
  document
  anchors
  container                             ;Die momentane container-box
  abspos                                ;Liste aller absolut positionionieren Boxen
  ;; caches for min/max width
  (min-width-cache (make-hash-table :test #'eq))
  (max-width-cache (make-hash-table :test #'eq))
  )

(declaim (type rc *rcontext*))

;;; some sane readers

(defun inner-width (box)
  (cond ((bbox-p box)
         (bbox-width box))
        ((ibox-p box)
         (- (abox-twidth box)
            (abox-margin-left box)
            (abox-border-left-width box)
            (abox-padding-left box)
            (abox-padding-right box)
            (abox-border-right-width box)
            (abox-margin-right box)))
        (t
         (error "Bad argument - ~S." box))))

(defun inner-height (box)
  (cond ((bbox-p box)
         (bbox-iheight box))
        ((ibox-p box)
         (- (+ (ibox-height box) (ibox-depth box))
            (abox-margin-top box)
            (abox-border-top-width box)
            (abox-padding-top box)
            (abox-padding-bottom box)
            (abox-border-bottom-width box)
            (abox-margin-bottom box)))
        (t
         (error "Bad argument - ~S." box))))

(defun inner-left-edge (box)
  (cond ((bbox-p box)
         (bbox-ix box))
        ((ibox-p box)
         (+ (ibox-x box)
            (abox-padding-left box)
            (abox-margin-left box)
            (abox-border-left-width box)))
        (t
         (error "Bad argument - ~S." box))))

(defun inner-top-edge (box)
  (cond ((bbox-p box)
         (+ (bbox-iy box)
            (abox-border-top-width box)
            (bbox-padding-top box)))
        ((ibox-p box)
         (+ (ibox-y-oben box)
            (if (numberp (ibox-valign box))
                (- (ibox-valign box))
              0)
            (abox-padding-top box)
            (abox-margin-top box)
            (abox-border-top-width box)))
        (t
         (error "Bad argument - ~S." box))))

;; generic (unless very strange things happen)

(defun inner-right-edge (box)
  (+ (inner-left-edge box) (inner-width box)))

(defun outer-right-edge (box)
  (+ (outer-left-edge box) (outer-width box)))

(defun inner-bottom-edge (box)
  (+ (inner-top-edge box) (inner-height box)))

(defun outer-bottom-edge (box)
  (+ (outer-top-edge box) (outer-height box)))

(defun outer-top-edge (box)
  (- (inner-top-edge box)
     (abox-margin-top box)
     (abox-border-top-width box)
     (abox-padding-top box)))

(defun outer-left-edge (box)
  (- (inner-left-edge box)
     (abox-margin-left box)
     (abox-border-left-width box)
     (abox-padding-left box)))

(defun outer-width (box)
  (+ (abox-margin-top box)
     (abox-border-top-width box)
     (abox-padding-top box)
     (inner-width box)
     (abox-padding-bottom box)
     (abox-border-bottom-width box)
     (abox-margin-bottom box)))

(defun outer-height (box)
  (+ (bbox-margin-left box)
     (abox-border-left-width box)
     (bbox-padding-left box)
     (inner-height box)
     (bbox-padding-right box)
     (abox-border-right-width box)
     (bbox-margin-right box)))


;;;;

(defun top-padding-edge (box)
  (- (inner-top-edge box) (abox-padding-top box)))

(defun right-padding-edge (box)
  (+ (inner-right-edge box) (abox-padding-right box)))

(defun bottom-padding-edge (box)
  (+ (inner-bottom-edge box) (abox-padding-bottom box)))

(defun left-padding-edge (box)
  (- (inner-left-edge box) (abox-padding-left box)))

;;;;

(defun top-border-edge (box)
  (- (top-padding-edge box) (abox-border-top-width box)))

(defun right-border-edge (box)
  (+ (right-padding-edge box) (abox-border-right-width box)))

(defun bottom-border-edge (box)
  (+ (bottom-padding-edge box) (abox-border-bottom-width box)))

(defun left-border-edge (box)
  (- (left-padding-edge box) (abox-border-left-width box)))


;;;;;;;;;;;

#+EXCL
(progn
  (define-compiler-macro bbox-p (x)
    `((lambda (x)
        (and (EXCL::STRUCTUREP x)
             (member 'bbox (car (EXCL::STRUCTURE-REF x 0)))))
      ,x))
  (define-compiler-macro abox-p (x)
    `((lambda (x)
        (and (EXCL::STRUCTUREP x)
             (member 'abox (car (EXCL::STRUCTURE-REF x 0)))))
      ,x))
  (define-compiler-macro ibox-p (x)
    `((lambda (x)
        (and (EXCL::STRUCTUREP x)
             (member 'ibox (car (EXCL::STRUCTURE-REF x 0)))))
      ,x))
  (define-compiler-macro gbox-p (x)
    `((lambda (x)
        (and (EXCL::STRUCTUREP x)
             (member 'gbox (car (EXCL::STRUCTURE-REF x 0)))))
      ,x))
  (define-compiler-macro rbox-p (x)
    `((lambda (x)
        (and (EXCL::STRUCTUREP x)
             (member 'rbox (car (EXCL::STRUCTURE-REF x 0)))))
      ,x))
  
  (define-compiler-macro bbox-p (x)
    `((lambda (x)
        (and (EXCL::STRUCTUREP x)
             (eq 'bbox (caar (EXCL::STRUCTURE-REF x 0)))))
      ,x))
  (define-compiler-macro ibox-p (x)
    `((lambda (x)
        (and (EXCL::STRUCTUREP x)
             (eq 'ibox (caar (EXCL::STRUCTURE-REF x 0)))))
      ,x))
  )
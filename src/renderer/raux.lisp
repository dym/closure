;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: R2; -*-
;;; --------------------------------------------------------------------------------------
;;;     Title: Auxiliary render functions
;;;   Created: 1999-02-24 04:33
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: GPL (See file COPYING for details).
;;;    Status: stable
;;; --------------------------------------------------------------------------------------
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
;;  1999-08-20  GB      - PT-EFFECTIVE-TARGET-ATTR: new function
;;

(in-package :R2)

;;; border, background

(defun pt-text-style (device pt)
  (make-text-style 
   device
   :font-family    (css:style-attr pt 'css:@font-family)
   :font-size      (css:style-attr pt 'css:@font-size)
   :font-weight    (css:style-attr pt 'css:@font-weight)
   :font-variant   (css:style-attr pt 'css:@font-variant)
   :font-style     (css:style-attr pt 'css:@font-style)
   :letter-spacing (css:style-attr pt 'css:@letter-spacing :normal)
   ;; manchmal gibt es einfach NIL hier ;-(
   :word-spacing   (css:style-attr pt 'css:@word-spacing :normal)))

(defun pt-backgroud (pt)
  (cond ((and (eq (css:style-attr pt 'css:@background-color) :transparent) 
              (eq (css:style-attr pt 'css:@background-image) :none))
         ;; We have no background at all.
         nil)
        (t
         (make-background
          :color      (css:style-attr pt 'css:@background-color)
          :image      (css:style-attr pt 'css:@background-image)
          :repeat     (css:style-attr pt 'css:@background-repeat)
          :attachment (css:style-attr pt 'css:@background-attachment)
          :position   (css:style-attr pt 'css:@background-position))) ))

(defun pt-border (pt)
  (let ((ts (css:style-attr pt 'css:@border-top-style))
        (rs (css:style-attr pt 'css:@border-right-style))
        (bs (css:style-attr pt 'css:@border-bottom-style))
        (ls (css:style-attr pt 'css:@border-left-style))
        (tw (css:style-attr pt 'css:@border-top-width))
        (rw (css:style-attr pt 'css:@border-right-width))
        (bw (css:style-attr pt 'css:@border-bottom-width))
        (lw (css:style-attr pt 'css:@border-left-width))
        (tc (css:style-attr pt 'css:@border-top-color))
        (rc (css:style-attr pt 'css:@border-right-color))
        (bc (css:style-attr pt 'css:@border-bottom-color))
        (lc (css:style-attr pt 'css:@border-left-color)) )
    (and (not (or (and (eq ts :none) (eq rs :none) (eq bs :none) (eq ls :none))
                  (and (eq tw 0) (eq rw 0) (eq bw 0) (eq lw 0))))
         (make-border 
          :top-width     (ceiling tw)   ;DEVRND
          :top-style     ts
          :top-color     tc             
          :right-width   (ceiling rw)   ;DEVRND
          :right-style   rs
          :right-color   rc             
          :bottom-width  (ceiling bw)   ;DEVRND
          :bottom-style  bs
          :bottom-color  bc             
          :left-width    (ceiling lw)   ;DEVRND
          :left-style    ls
          :left-color    lc))))         

;;; list items

(defun list-item-string (font pt)
  ;;(assert (pt-p p))
  (ecase (css:style-attr pt 'css:@list-style-type 'frob)
    (:disc
     (map 'rod #'identity 
          (vector (choose-glyph font +list-style-type-glyphs/disc+)
                  u/space)))
    (:circle
     (map 'rod #'identity 
          (vector (choose-glyph font +list-style-type-glyphs/circle+)
                  u/space)))
    (:square
     (map 'rod #'identity 
          (vector (choose-glyph font +list-style-type-glyphs/square+)
                  u/space)))
    (:decimal
     (map 'rod #'char-code 
          (format nil "~D. " (+ 1 (list-item-index pt)))))
    (:lower-roman
     (map 'rod #'char-code 
          (format nil "~(~@R~). " (+ 1 (list-item-index pt)))))
    (:upper-roman
     (map 'rod #'char-code 
          (format nil "~:@(~@R~). " (+ 1 (list-item-index pt)))))
    (:lower-alpha
     (map 'rod #'char-code 
          (format nil "~(~A~). " (integer->abc (list-item-index pt)))))
    (:upper-alpha
     (map 'rod #'char-code 
          (format nil "~:@(~A~). " (integer->abc (list-item-index pt)))))
    (:none
     (map 'rod #'identity nil))))


(defun list-item-string* (list-style-type n)
  (ecase list-style-type
    (:disc (coerce (vector (elt +list-style-type-glyphs/disc+ 0)) 'rod))
    (:circle (coerce (vector (elt +list-style-type-glyphs/circle+ 0)) 'rod))
    (:square (coerce (vector (elt +list-style-type-glyphs/square+ 0)) 'rod))
    (:decimal
     (map 'rod #'char-code 
          (format nil "~D" n)))
    (:lower-roman
     (map 'rod #'char-code 
          (format nil "~(~@R~)" n)))
    (:upper-roman
     (map 'rod #'char-code 
          (format nil "~:@(~@R~)" n)))
    (:lower-alpha
     (map 'rod #'char-code 
          (format nil "~(~A~)" (integer->abc n))))
    (:upper-alpha
     (map 'rod #'char-code 
          (format nil "~:@(~A~)" (integer->abc n))))
    (:none
     (map 'rod #'identity nil))))

(defun integer->abc (i)
  ;;(assert (and (integerp i) (>= i 0)))
  (cond ((< i 0) "A")                   ;defensives Programmieren.
        ((< i 26) (string (code-char (+ (char-code #\A) i))))
        (t (concatenate 'string (integer->abc (1- (floor i 26))) (integer->abc (mod i 26)))) ))

(defun choose-glyph (font alternatives)
  ;; Select the first representable glyph from `alternatives'
  (dolist (x alternatives (car (last alternatives)))
    (cond ((glyph-representable-p font x)
           (return x)))))


;;; boxen

(defsubst abox-border-top-width (box)
  (declare (type abox box))
  (if (abox-border box) (border-top-width (abox-border box)) 0))

(defsubst abox-border-right-width (box)
  (declare (type abox box))
  (if (abox-border box) (border-right-width (abox-border box)) 0))

(defsubst abox-border-left-width (box)
  (declare (type abox box))
  (if (abox-border box) (border-left-width (abox-border box)) 0))

(defsubst abox-border-bottom-width (box)
  (declare (type abox box))
  (if (abox-border box) (border-bottom-width (abox-border box)) 0))

(defsubst bbox-twidth (x)
  (+ (abox-left-leading x)
     (bbox-width x)
     (abox-right-leading x)))

(defsubst abox-left-leading (box)
  (declare (type abox box))
  (+ (abox-margin-left box)
     (abox-border-left-width box)
     (abox-padding-left box)))

(defsubst abox-right-leading (box)
  (declare (type abox box))
  (+ (abox-margin-right box)
     (abox-border-right-width box)
     (abox-padding-right box)))

(defun bbox-border-coordinates (box)    ; -> x, y, width, height
  (values
   (bbox-ix box)
   (bbox-iy box)
   (+ (abox-border-left-width box)
      (bbox-padding-left box) 
      (bbox-width box)
      (bbox-padding-right box)
      (abox-border-right-width box) )
   (+ (abox-border-top-width box)
      (abox-padding-top box)
      (bbox-iheight box)
      (abox-padding-bottom box)
      (abox-border-bottom-width box) )))

(defun nmove-box (box dx dy)
  (ignore-errors
   (etypecase box
     (bbox
      (and (abox-bx0 box) (incf (abox-bx0 box) dx))
      (and (abox-bx1 box) (incf (abox-bx1 box) dx))
      (and (abox-by0 box) (incf (abox-by0 box) dy))
      (and (abox-by1 box) (incf (abox-by1 box) dy))
      (incf (bbox-iy box) dy)
      (incf (bbox-ix box) dx)
      (dolist (k (abox-contents box))
        (nmove-box k dx dy)))
     (ibox
      (and (abox-bx0 box) (incf (abox-bx0 box) dx))
      (and (abox-bx1 box) (incf (abox-bx1 box) dx))
      (and (abox-by0 box) (incf (abox-by0 box) dy))
      (and (abox-by1 box) (incf (abox-by1 box) dy))
      (and (ibox-y-oben box) (incf (ibox-y-oben box) dy))
      (and (ibox-x box) (incf (ibox-x box) dx))
      (dolist (k (abox-contents box))
        (nmove-box k dx dy)))
     (gbox
      nil)
     (rbox
      nil))))

(defun ibox-contains-only-rboxen? (ibox)
  (cond ((gbox-p ibox) nil)
        ((rbox-p ibox) t)
        ((ibox-p ibox)
         (dolist (k (abox-contents ibox) t) ;was: EVERY
           (cond ((ibox-contains-only-rboxen? k))
                 (t (return nil)))))
        (t
         (warn "Hugh? IBOX-CONTAINS-ONLY-RBOXEN? ~S" ibox)
         nil)))

#||
(defun ibox-contains-only-rboxen? (ibox)
  (cond ((gbox-p ibox) nil)
        ((rbox-p ibox) t)
        ((ibox-p ibox)
         (let ((cts (abox-contents ibox)))
           (or (null cts)
               (and (null (cdr cts))
                    (rbox-p (car cts))))))
        (t
         (warn "Hugh? IBOX-CONTAINS-ONLY-RBOXEN? ~S" ibox)
         nil)))
||#

(defun map-boxen (fun box)
  "Apply `fun' to each box in the boxen hierarchy `box'."
  (funcall fun box)
  (etypecase box
    ((or BBOX IBOX)
     (dolist (k (abox-contents box))
       (map-boxen fun k)))
    ((or GBOX RBOX)
     nil)))

;;; base url

(defun pt-base-url (pt)
  (let ((base-element (pt-headers-base-element pt)))
    (cond ((and (not (null base-element))
                (not (null (pt-attr* base-element :HREF)))) 
           (url:parse-url (pt-attr/latin1 base-element :HREF)))
          (t
           nil) )))

(defun pt-base-target (pt)
  (let ((base-element (pt-headers-base-element pt)))
    (and base-element
         (pt-attr/latin1 base-element :TARGET NIL))))

(defun pt-x-height (device pt)
  (font-desc-x-height (pt-font device pt)))

(defun pt-headers-base-element (pt)
  "Return the <BASE> element of the document `pt' is a sub-node of."
  (cond ((null pt) nil)
        ((eq (sgml:pt-name pt) :HTML)
         (dolist (q (sgml:pt-children pt))
           (when (eq (sgml:pt-name q) :HEAD)
             (dolist (k (sgml:pt-children q))
               (when (eq (sgml:pt-name k) :BASE)
                 (return-from pt-headers-base-element k))))))
        ((pt-headers-base-element (sgml:pt-parent pt))) ))

;;; more pt attribute query functions

(defun pt-effective-url-attr (doc pt attr)
  "Return the parsed and merged effective URL of an elements attribute."
  (url:merge-url (url:parse-url (pt-attr/latin1 pt attr ""))
                 (document-base-url doc)))

(defun pt-effective-target-attr (pt attr)
  "Return the effective target attribute of `pt' named `attr';
   Does look at the header, if `pt' has no attribute called `attr'."
  (or (pt-attr/latin1 pt attr nil)
      (pt-base-target pt)))

(defun pt-font (device pt)
  (find-css-font-desc (device-font-database device)
                      (css:style-attr pt 'css:@font-family)
                      (css:style-attr pt 'css:@font-weight)
                      (css:style-attr pt 'css:@font-style)
                      (css:style-attr pt 'css:@font-size)
                      (css:style-attr pt 'css:@font-variant)))

(defun pt-body-element (pt)
  (cond ((null pt) nil)
        ((eq (sgml:pt-name pt) :HTML)
         (dolist (q (sgml:pt-children pt))
           (when (eq (sgml:pt-name q) :BODY)
             (return q))))
        ((pt-body-element (sgml:pt-parent pt))) ))

(defun pt-font-size (pt)
  (css:style-attr pt 'css:@font-size))

(defun floating-element-p (pt)
  (neq (css:style-attr pt 'css:@float :none) :none))

(defun pt-effective-line-height (pt)
  (let ((line-height (css:style-attr pt 'css:@line-height)))
    (if (and (consp line-height) (eq (car line-height) '*))
        (round (* (cdr line-height)
                  (css:style-attr pt 'css:@font-size))) ;DEVRND
      line-height)))

;;; yet more pt mungleing (most of these should go to pt.lisp).

(defun pt-data (x)
  (cond ((eq (sgml:pt-name x) :pcdata)
	 (map 'string #'code-char (sgml:pt-cdata x)))
	((apply 'concatenate 'string
                (mapcar #'pt-data (sgml:pt-children x))))))

(defun pt-data-iso10646 (x)
  (cond ((eq (sgml:pt-name x) :pcdata)
	 (sgml:pt-cdata x))
	((apply 'concatenate 'rod
                (mapcar #'pt-data-iso10646 (sgml:pt-children x))))))

(defun pt-all-data (x)
  (cond ((member (sgml:pt-name x) '(:pcdata :comment))
	 (map 'string (lambda (x) (or (code-char x) #\?))
              (sgml:pt-cdata x)))
	((apply 'concatenate 'string
                (mapcar #'pt-all-data (sgml:pt-children x))))))


;;; und das hier gehoert auch wech

(defun white-space-rune-p* (rune)
  (or (eq rune 9)
      (eq rune 10)
      (eq rune 12)
      (eq rune 13)
      (eq rune 32)))

(defun white-space-rune-p*/no-nl (rune)
  (or (eq rune 9)
      (eq rune 12)
      (eq rune 13)
      (eq rune 32)))

(define-compiler-macro white-space-rune-p* (rune)
  `((lambda (rune)
      (or (eq rune 9)
          (eq rune 10)
          (eq rune 12)
          (eq rune 13)
          (eq rune 32)))
    ,rune))

(define-compiler-macro white-space-rune-p*/no-nl (rune)
  `((lambda (rune)
      (or (eq rune 9)
          (eq rune 12)
          (eq rune 13)
          (eq rune 32)))
    ,rune))

;;;

(defun maybe-resolve-percentage (x total)
  (cond ((css:percentage-p x)   (round (* (cdr x) total) 100))
        ((integerp x)           x)
        ((realp x)              (round x))
        (t                      x)))



(defun resolve-width/1 (document device pt ml wd mr width-left)
  ;; Es gilt
  (assert (or (numberp ml) (eq ml :auto)))
  (assert (or (numberp wd) (eq wd :auto)))
  (assert (or (numberp mr) (eq mr :auto)))

  
  
  ;; Bei replaced elements nimm die intrinsic width
  (let ((re (replaced-element-p document device pt)))
    (and (and (eq wd :auto) re (not (sgml::pt-p re)))
         (multiple-value-bind (w h d)
             (ro/intrinsic-size re)
           h d 
           (setf wd w))))
	     
  ;; Alle Faelle betrachten
  (cond 
   ;; CSS1 says: If none of the properties are 'auto', the value
   ;; of 'margin-right' will be assigned 'auto'.
   ((and (neq ml :auto) (neq wd :auto) (neq mr :auto)) ; - - -
    '(setq mr (- width-left ml wd)))
   
   ;; CSS1 says: If exactly one of 'margin-left', 'width' or 'margin-right'
   ;; is 'auto', the UA will assign that property a value that will make the
   ;; sum of the seven equal to the parent's width.
   
   ((and (neq ml :auto) (neq wd :auto) ( eq mr :auto)) ; - - A
    (setq mr (- width-left ml wd)))

   ((and (neq ml :auto) ( eq wd :auto) (neq mr :auto)) ; - A -
    (setq wd (- width-left ml mr)))

   ((and ( eq ml :auto) (neq wd :auto) (neq mr :auto)) ; A - -
    (setq ml (- width-left wd mr)))

   ;; CSS1: If more than one of the three is 'auto', and one of them is
   ;; 'width', than the others ('margin-left' and/or 'margin-right') will be
   ;; set to zero and 'width' will get the value needed to make the sum of
   ;; the seven equal to the parent's width.

   ((and (neq ml :auto) ( eq wd :auto) ( eq mr :auto)) ; - A A
    (setq mr 0)
    (setq wd (- width-left ml mr)))
		 
   ((and ( eq ml :auto) ( eq wd :auto) (neq mr :auto)) ; A A -
    (setq ml 0)
    (setq wd (- width-left ml mr)))
		 
   ((and ( eq ml :auto) ( eq wd :auto) ( eq mr :auto)) ; A A A
    (setq ml 0 mr 0)
    (setq wd (- width-left ml mr)))

   ;; Otherwise, if both 'margin-left' and 'margin-right' are 'auto', they will
   ;; be set to equal values. This will center the element inside its parent. 

   ((and ( eq ml :auto) (neq wd :auto) ( eq mr :auto)) ; A - A
    (setq ml (ceiling (- width-left wd) 2))     ;DEVRND
    (setq mr (floor   (- width-left wd) 2)) ) ) ;DEVRND
	   
  ;; Nun gilt
  (assert (and (numberp ml) (numberp wd) (numberp mr)))
  '(assert (< (abs (- (+ ml wd mr) width-left)) 1))
  
  ;; XXX -- table hack
  (cond ((eq (sgml:gi pt) :table)
         (cond ((> wd width-left)
                (warn "Specified table width of ~D is more than what is available (~D)."
                      wd width-left)
                (setf wd width-left 
                      ml 0 
                      mr 0)))))
  
  (values ml (max 0 wd) mr))


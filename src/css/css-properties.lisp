;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CSS; Encoding: utf-8; Readtable: GLISP; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Parsing CSS1 Style Sheets (according to W3C REC-CSS1-961217)
;;;   Created: 1998-02-08
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: GPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1998-2002 by Gilbert Baumann

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

;;;;;;;;;;;;;;;

(in-package :CSS)

;;; ------------------------------------------------------------------------------------------
;;;  Some Default Values

(defparameter *thin-border*		'(:px . 1))
(defparameter *medium-border*		'(:px . 2))
(defparameter *thick-border*		'(:px . 5))
(defparameter *normal-line-height*	'1.2)
(defparameter *initial-font-family*	'("times"))
(defparameter *initial-color*		'"#000")
;; 3/2 ist eigentlich etwas bißchen heftig!
(defparameter *font-scaling-factor*	3/2)
(defparameter *medium-font-size*	'(:pt . 12))

;;; ------------------------------------------------------------------------------------------
;;;  Concrete Properties
;;;

(define-css-property background-attachment
    :default-value :scroll
    :value (or :scroll :fixed)
    :inheritedp nil)

(define-css-property background-color
    :default-value :transparent
    :value (or <a-color> :transparent)
    :inheritedp nil)

(define-css-property background-image
    :default-value :none
    :value (or <url> :none)
    :inheritedp nil)

(define-css-property background-repeat
    :default-value :repeat
    :value (or :repeat :repeat-x :repeat-y :no-repeat)
    :inheritedp nil)

(define-css-property color
    :default-value *initial-color*
    :value <a-color>
    :inheritedp t)

(define-css-property border-top-style
    :default-value :none
    :inheritedp nil
    :value (or :none :dotted :dashed :solid :double :groove :ridge :inset :outset))

(define-css-property border-right-style
    :default-value :none
    :inheritedp nil
    :value (or :none :dotted :dashed :solid :double :groove :ridge :inset :outset))

(define-css-property border-bottom-style
    :default-value :none
    :inheritedp nil
    :value (or :none :dotted :dashed :solid :double :groove :ridge :inset :outset))

(define-css-property border-left-style
    :default-value :none
    :inheritedp nil
    :value (or :none :dotted :dashed :solid :double :groove :ridge :inset :outset))

(define-css-property line-height
    :inheritedp t
    :value (or :normal
               <non-negative-length>
               <non-negative-percentage>
               <non-negative-number>)
    :default-value '(* . 1.2)
    ;; interpret-line-height
    )

(define-css-property word-spacing
    :inheritedp t
    :value (or :normal <length>)
    :default-value :normal
    ;; interpret-word-spacing
    )

(define-css-property font-style
    :inheritedp t
    :value (or :italic :oblique :normal)
    :default-value :normal
    )

(define-css-property font-variant
    :inheritedp t
    :value (or :small-caps :normal)
    :default-value :normal)

(define-css-property font-weight
    :inheritedp t
    :value <a-font-weight>
    :default-value :normal)

(define-css-property clear
    :inheritedp nil
    :value (or :none :left :right :both)
    :default-value :none)

(define-css-property float
    :value (or :left :right :none)
    :inheritedp nil
    :default-value :none)

(define-css-property display
    :value (or :block :inline :list-item :none
               :run-in
               ;; CSS2
               :run-in :compact :marker 
        
               ;; Table stuff:
               ;;  :table :inline-table
               ;;  :table-row-group :table-header-group
               ;;  :table-footer-group :table-row :table-column-group
               ;;  :table-column :table-cell :table-caption 
               ;; :inherit
               )
    :default-value :block
    :inheritedp nil
    ;;xxx float -> block
    ;;xxx CSS2: default: inline
    )

(define-css-property white-space
    :value (or :normal :pre :nowrap)
    :inheritedp t
    :default-value :normal)

(define-css-property list-style-type
    :value <a-list-style-type>
    :inheritedp t
    :default-value :disc)

(define-css-property list-style-position
    :value (or :inside :outside)
    :inheritedp t
    :default-value :outside)

(define-css-property text-align
    :value (or :left :right :center :justify)
    :inheritedp t
    :default-value :left)

(define-css-property text-transform
    :value (or :capitalize :uppercase :lowercase :none)
    :inheritedp t
    :default-value :none)

(define-css-property text-decoration
    :value (or :none
               (barbar :underline :overline :line-through :blink))
    :inheritedp nil
    :default-value :none)

(define-css-property padding-top
    :value (or <non-negative-length> <non-negative-percentage>)
    :default-value 0
    :inheritedp nil
    :percentage-base ??)

(define-css-property padding-right
    :value (or <non-negative-length> <non-negative-percentage>)
    :default-value 0
    :inheritedp nil
    :percentage-base ??)

(define-css-property padding-bottom
    :value (or <non-negative-length> <non-negative-percentage>)
    :default-value 0
    :inheritedp nil
    :percentage-base ??)

(define-css-property padding-left
    :value (or <non-negative-length> <non-negative-percentage>)
    :default-value 0
    :inheritedp nil
    :percentage-base ??)

(define-css-property margin-top
    :value <a-margin>
    :default-value 0
    :inheritedp nil
    :percentage-base ??)

(define-css-property margin-right
    :value <a-margin>
    :default-value 0
    :inheritedp nil
    :percentage-base ??)

(define-css-property margin-bottom
    :value <a-margin>
    :default-value 0
    :inheritedp nil
    :percentage-base ??)

(define-css-property margin-left
    :value <a-margin>
    :default-value 0
    :inheritedp nil
    :percentage-base ??)

(define-css-property letter-spacing
    :value (or :normal <length>)
    :default-value 0
    :inheritedp t
    ;; xxx #'interpret-letter-spacing
    )

(define-css-property vertical-align
    :value (or :baseline
               :sub :super
               :top :text-top :middle :bottom :text-bottom
               <percentage>
               <length>                 ;css2
               )
    :default-value :baseline
    :inheritedp nil
    :percentage-base ???
    ;; xxx #'interpret-vertical-align
    )

(define-css-property text-indent
    :value (or <length> <percentage>)
    :default-value 0
    :inheritedp t
    :percentage-base ???
    ;; xxx #'interpret-text-indent
    )

(define-css-property border-top-width
    :value (or :thin :medium :thick <non-negative-length>)
    :default-value 2
    :inheritedp nil
    ;; xxx #'interpret-border-width
    )

(define-css-property border-right-width
    :value (or :thin :medium :thick <non-negative-length>)
    :default-value 2
    :inheritedp nil
    ;; xxx #'interpret-border-width
    )

(define-css-property border-bottom-width
    :value (or :thin :medium :thick <non-negative-length>)
    :default-value 2
    :inheritedp nil
    ;; xxx #'interpret-border-width
    )

(define-css-property border-left-width
    :value (or :thin :medium :thick <non-negative-length>)
    :default-value 2
    :inheritedp nil
    ;; xxx #'interpret-border-width
    )

(define-css-property width
    :value (or <non-negative-length> 
               <non-negative-percentage>
               :auto)
    :default-value :auto
    :inheritedp nil
    )

(define-css-property height
    :value (or <non-negative-length>
               :auto)
    :default-value :auto
    :inheritedp nil)

(define-css-property list-style-image
    :value (or <url> :none)
    :default-value :none
    :inheritedp t
    )

(define-css-property font-size
    :value (or <absolute-size>
               <relative-size>
               <non-negative-length>
               <non-negative-percentage>)
    :inheritedp t
    :default-value :medium)

(define-css-property border-top-color
    :value <a-color>
    :default-value "black"
    :inheritedp t                       ;xxx not sure
    )

(define-css-property border-right-color
    :value <a-color>
    :default-value "black"
    :inheritedp t                       ;xxx not sure
    )

(define-css-property border-bottom-color
    :value <a-color>
    :default-value "black"
    :inheritedp t                       ;xxx not sure
    )

(define-css-property border-left-color
    :value <a-color>
    :default-value "black"
    :inheritedp t                       ;xxx not sure
    )

;;; Missing:

;(("background-position" . P/BACKGROUND-POSITION)
; ("border" . P/BORDER)
; )

;;; Missing:
;; BACKGROUND-POSITION -- weird

(define-css-property font-family
    :value <a-font-family>
    :default-value *initial-font-family*
    :inheritedp t)

(define-css-property marker-offset
    :value (or :inherit :auto <length>)
    :default-value :auto
    :inheritedp nil
    ;; css2
    ;;
    )

(define-css-property quotes
    :value (or :inherit
               :none
               (+ (& <string> <string>)))
    :inheritedp t
    :default-value (list (list "\"" "\"")
                         (list "\'" "\'")))

;;;;;;;;;;;;;;;;;;;;

(define-css-property position
    :value (or :static :relative :absolute :fixed :inherit)
    :default-value :static
    :inheritedp nil)

(define-css-property top
    :value (or <length> <percentage> :auto :inherit)
    :default-value :auto
    :inheritedp nil)

(define-css-property right
    :value (or <length> <percentage> :auto :inherit)
    :default-value :auto
    :inheritedp nil)

(define-css-property bottom
    :value (or <length> <percentage> :auto :inherit)
    :default-value :auto
    :inheritedp nil)

(define-css-property left
    :value (or <length> <percentage> :auto :inherit)
    :default-value :auto
    :inheritedp nil)

(define-css-property z-index
    :value (or <integer> :auto :inherit)
    :default-value :auto
    :inheritedp nil)

(define-css-property overflow
    :value (or :visible :hidden :scroll :auto :inherit)
    :default-value :visible
    :inheritedp nil)

(define-css-property clip
    :value (or <shape> :auto :inherit)
    :default-value :auto
    :inheritedp nil)

(define-css-property content
    :value (+ (or (mungle <string>
                          (lambda (x) `(:string ,x)))
                  (mungle <url>
                          (lambda (x) `(:url ,x)))
                  (function :counter <ident>)
                  (function :counter <ident> <a-list-style-type>)
                  (function :counters <ident> <string>)
                  (function :counters <ident> <string> <a-list-style-type>)
                  ;; <counter> <attrX>
                  :open-quote
                  :close-quote
                  :no-open-quote
                  :no-close-quote))
    :default-value nil
    :inheritedp nil)

(define-css-property counter-reset
    :value (or :none
               :inherit
               (+ (& <ident> (mungle (? <integer>) (lambda (x) (or x 0))))))
    :default-value :none
    :inheritedp nil)

(define-css-property counter-increment
    :value (or :none
               :inherit
               (+ (& <ident> (mungle (? <integer>) (lambda (x) (or x 1))))))
    :default-value :none
    :inheritedp nil)

(define-css-property orig-width
    :value :khjdskdjshaks               ;xxx
    )

;;; ------------------------------------------------------------------------------------------
;;;  Shorthand properties
;;;

(define-simple-short-hand-property list-style
    :value (barbar* <list-style-type>
                    <list-style-image>
                    <list-style-position>))

;;; xxx these border assignments have a flaw, since "border-left: 1px"
;;; implies default values on style and color, I guess.

(define-simple-short-hand-property border-left
    :value (barbar* <border-left-width>
                    <border-left-style>
                    <border-left-color>))

(define-simple-short-hand-property border-right
    :value (barbar* <border-right-width>
                    <border-right-style>
                    <border-right-color>))

(define-simple-short-hand-property border-top
    :value (barbar* <border-top-width>
                    <border-top-style>
                    <border-top-color>))

(define-simple-short-hand-property border-bottom
    :value (barbar* <border-bottom-width>
                    <border-bottom-style>
                    <border-bottom-color>))

(define-simple-short-hand-property font
    ;; xxx CSS2: inherit
    :value (&* (? (barbar* <font-style> <font-variant> <font-weight>))
               <font-size>
               (? (&* <slash> <line-height>))
               <font-family>))

(define-edges-short-hand-property margin
    :names (margin-top margin-right margin-bottom margin-left)
    :value <a-margin>)

(define-edges-short-hand-property padding
    ;; xxx negative values are not allowed ...
    :names (padding-top padding-right padding-bottom padding-left)
    :value <a-padding>)

(define-edges-short-hand-property border-width
    :names (border-top-width border-right-width border-bottom-width border-left-width)
    :value <a-border-width>)

(define-edges-short-hand-property border-color
    :names (border-top-color border-right-color border-bottom-color border-left-color)
    :value <a-color>)

(define-edges-short-hand-property border-style
    :names (border-top-style border-right-style border-bottom-style border-left-style)
    :value <a-border-style>)

(define-simple-short-hand-property background
    ;; xxx "The 'background' property always sets all the individual
    ;;      background properties."
    :value (barbar* <background-image>
                    <background-repeat>
                    <background-attachment>
                    <background-position>
                    <background-color>))

;; p/shape !!!
;; only allowed shape so far is:

;; 'rect' '(' { <length> | 'auto' } { "," <length> | 'auto' }*3 ')'

;; wir koennten jedoch sehr leicht auch noch union(...),
;; intersection(...), difference(...), 
;; everything, nothing
;; anbieten. (Könnten wir aus Spaß ja mal machen).


;;; ------------------------------------------------------------------------------------------
;;;  Value Cooking

(define-cooking display
    :applicable-if (not (eql (prop float) :none))
    :value         :block)

(define-cooking border-top-width
    :applicable-if (eql (prop border-top-style) :none)
    :value         0)

(define-cooking border-right-width
    :applicable-if (eql (prop border-right-style) :none)
    :value         0)

(define-cooking border-bottom-width
    :applicable-if (eql (prop border-bottom-style) :none)
    :value         0)

(define-cooking border-left-width
    :applicable-if (eql (prop border-left-style) :none)
    :value         0)

(define-cooking (background-image list-style-image)
    :applicable-if (stringp value)
    :value         (url:parse-url value))

#||
(define-cooking font-size
    :applicable-if (eql @font-size :xx-small)
    :value         (* (expt *font-scaling-factor* -3) (interpret-length device *medium-font-size* nil)))

(define-cooking font-size
    :applicable-if (eql @font-size :x-small)
    :value         (* (expt *font-scaling-factor* -2) (interpret-length device *medium-font-size* nil)))

(define-cooking font-size
    :applicable-if (eql @font-size :small)
    :value         (* (expt *font-scaling-factor* -1) (interpret-length device *medium-font-size* nil)))

(define-cooking font-size
    :applicable-if (eql @font-size :medium)
    :value         (* (expt *font-scaling-factor* 0) (interpret-length device *medium-font-size* nil)))

(define-cooking font-size
    :applicable-if (eql @font-size :large)
    :value         (* (expt *font-scaling-factor* 1) (interpret-length device *medium-font-size* nil)))

(define-cooking font-size
    :applicable-if (eql @font-size :x-large)
    :value         (* (expt *font-scaling-factor* 2) (interpret-length device *medium-font-size* nil)))

(define-cooking font-size
    :applicable-if (eql @font-size :xx-large)
    :value         (* (expt *font-scaling-factor* 3) (interpret-length device *medium-font-size* nil)))
||#

(define-cooking font-size
    :applicable-if (eql value :smaller)
    :value         (round (parent-prop font-size) *font-scaling-factor*))

(define-cooking font-size
    :applicable-if (eql value :larger)
    :value         (round (parent-prop font-size) (/ *font-scaling-factor*)))

(define-cooking font-weight
    :applicable-if (eql value :normal)
    :value         400)

(define-cooking font-weight
    :applicable-if (eql value :bold)
    :value         700)

(define-cooking font-weight
    :applicable-if (eql value :bolder)
    :value         (min 900 (+ (parent-prop font-weight) 300)))

(define-cooking font-weight
    :applicable-if (eql @font-weight :lighter)
    :value         (max 100 (- (parent-prop font-weight) 300)))

(define-cooking (border-top-width border-right-width border-bottom-width border-left-width)
    :applicable-if (eql value :thin) :value *thin-border*)

(define-cooking (border-top-width border-right-width border-bottom-width border-left-width)
    :applicable-if (eql value :medium) :value *medium-border*)

(define-cooking (border-top-width border-right-width border-bottom-width border-left-width)
    :applicable-if (eql value :thick) :value *thick-border*)

(define-length-cooking (border-top-width border-right-width border-bottom-width border-left-width
                        letter-spacing
                        word-spacing
                        vertical-align
                        margin-top margin-right margin-bottom margin-left
                        padding-top padding-right padding-bottom padding-left
                        text-indent width height top right bottom left line-height)
    )

(define-cooking font-size
    :applicable-if (and (consp value)
                    (member (car value) '(:px :in :cm :mm :pt :pc :canvas-h-percentage :canvas-v-percentage)))
    :value         (new-interpret-length value device :nan pt dpi))

(define-cooking font-size
    :applicable-if (and (consp value)
                        (member (car value) '(:em :ex)))
    :value         (new-interpret-length value device (parent-prop font-size) pt dpi))

(define-percentage-cooking font-size
    :base          (parent-prop font-size))

(define-cooking letter-spacing
    :applicable-if (eql value :normal)
    :value         0)

(define-percentage-cooking letter-spacing
    :base (prop font-size))

(define-cooking vertical-align
    :applicable-if (percentage-p value)
    :value         (let ((lh (prop line-height)))
                     (if (and (consp lh) (eq (car lh) '*))
                         (* (cdr lh) (prop font-size))
                         lh)))

(define-cooking line-height
    :applicable-if (eql value :normal)
    :value         (cons '* 1.2))       ;xxx

(define-cooking line-height
    :applicable-if (numberp value)
    :value         (cons '* value))

(define-cooking orig-width
    :applicable-if t
    :value         (prop width))



;;; ------------------------------------------------------------------------------------------

;;;;;;;;;;;

(defun p/border (tokens)
  ;;  <border-width> || <border-style> || <color>
  (let ((r0 (p/border-top tokens))
        (r1 (p/border-right tokens))
        (r2 (p/border-bottom tokens))
        (r3 (p/border-left tokens)))
    (and r0
         (progn
           (assert (and (eq (cdr r0) (cdr r1))
                        (eq (cdr r0) (cdr r2))
                        (eq (cdr r0) (cdr r3))
                        (eq (cdr r1) (cdr r2))
                        (eq (cdr r1) (cdr r3))
                        (eq (cdr r2) (cdr r3))))
           (cons (append (car r0) (car r1) (car r2) (car r3))
                 (cdr r0))))))

;;; ---------------------------------------------------------------------------

(defun p/background-position-1 (tokens)
  ;;  [<percentage> | <length>]{1,2}
  (p/attcons '@background-position
             (let ((r (p/repeated tokens 1 2 (lambda (toks)
                                               (or (p/percentage toks)
                                                   (p/length toks))))))
               (and r
                    (cons (let* ((v (car r))
                                 (x (first v))
                                 (y (or (second v) (cons :% 50))))
                            (cons x y))
                          (cdr r))))))

(defun p/background-position-2 (tokens)
  ;; [top | center | bottom] || [left | center | right]
  ;; we treat this as:
  ;;   [ [ top | bottom ] [ left | center | right ]? ] |
  ;;   [ [ left | right ] [ top | center | bottom ]? ] |
  ;;   [ center [ top | bottom | left | right | center]? ]
  (let ((r (or 
            (p/concat tokens 
                      (rcurry #'p/simple-enum :top :bottom)
                      (rcurry #'p/maybe (rcurry #'p/simple-enum 
                                                :left :center :right)))
            (p/concat tokens
                      (rcurry #'p/simple-enum :left :right)
                      (rcurry #'p/maybe (rcurry #'p/simple-enum 
                                                :top :center :bottom)))
            (p/concat tokens
                      (rcurry #'p/simple-enum :center)
                      (rcurry #'p/maybe 
                              (rcurry #'p/simple-enum 
                                      :left :right :top :center :bottom))) )))
    (and r 
         (let ((v (car r)))
           (multiple-value-bind (x y)
               (cond ((or (equal v '(:top :left))
                          (equal v '(:left :top)))
                      (values '(:% . 0) '(:% . 0)))
                     ((or (equal v '(:top nil))
                          (equal v '(:top :center))
                          (equal v '(:center :top)))
                      (values '(:% . 50) '(:% . 0)))
                     ((or (equal v '(:right :top))
                          (equal v '(:top :right)))
                      (values '(:% . 100) '(:% . 0)))
                     ((or (equal v '(:left nil))
                          (equal v '(:left :center))
                          (equal v '(:center :left)))
                      (values '(:% . 0) '(:% . 50)))
                     ((or (equal v '(:center nil))
                          (equal v '(:center :center)))
                      (values '(:% . 50) '(:% . 50)))
                     ((or (equal v '(:right nil))
                          (equal v '(:right :center))
                          (equal v '(:center :right)))
                      (values '(:% . 100) '(:% . 50)))
                     ((or (equal v '(:bottom nil))
                          (equal v '(:bottom :center))
                          (equal v '(:center :bottom)))
                      (values '(:% . 50) '(:% . 100)))
                     ((or (equal v '(:bottom :left))
                          (equal v '(:left :bottom)))
                      (values '(:% . 0) '(:% . 100)))
                     ((or (equal v '(:right :bottom))
                          (equal v '(:bottom :right)))
                      (values '(:% . 100) '(:% . 100))))
             (p/attcons '@background-position (cons (cons x y) (cdr r))))))))
    
(defun p/background-position (tokens)
  (or (p/background-position-1 tokens)
      (p/background-position-2 tokens)))

;;; ------------------------------------------------------------------------------------------
;;;  Code Generation
;;;

(generate-parsers)
(register-parsers)
(generate-setup-style)


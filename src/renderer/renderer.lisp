;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: RENDERER; Encoding: utf-8; Readtable: GLISP; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: The Renderer's Heart
;;;   Created: long ago
;;;    Author: Gilbert Baumann <gilbert@base-engineering.com>
;;;   License: MIT style (see below)
;;;       $Id$
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1997-2002 by Gilbert Baumann

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


(in-package :R2)

(defpackage :closure/clim-device
    (:use :clim :clim-lisp :clim-sys)
  (:import-from #:R2
                ;;
                #:device-dpi
                #:device-font-ascent
                #:device-font-descent
                #:device-font-underline-position
                #:device-font-underline-thickness
                #:device-font-has-glyph-p
                #:device-font-glyph-width
                #:device-realize-font-desc
                #:device-font-database

                ;; font-desc
                #:make-font-desc
                #:font-desc
                #:font-desc-family
                #:font-desc-weight
                #:font-desc-style
                #:font-desc-size
                #:font-desc-ddp
                #:font-desc-charset
                #:font-desc-widthen
                ;;
                #:make-font-database
                #:font-database-relate
                ;;
                #:scale-font-desc
                ;;
                ;;
                ;;
                ))

;;; ---- Parameters & Constants -----------------------------------------------

;; Some handy unicode characters:
(defconstant u/bullet             #x2022)
(defconstant u/rightwards-arrow   #x2192)
(defconstant u/black-spade-suit   #x2660)
(defconstant u/black-club-suit    #x2663)
(defconstant u/black-heart-suit   #x2665)
(defconstant u/black-diamond-suit #x2666)
(defconstant u/black-square       #x25a0)
(defconstant u/white-square       #x25a0)
(defconstant u/white-circle       #x25cb)
(defconstant u/black-circle       #x25cf)
(defconstant u/white-bullet       #x25E6)

(defconstant u/newline            #x000A)
(defconstant u/space              #x0020)
(defconstant u/hyphen             #x002D)
(defconstant u/soft-hyphen        #x00AD)
(defconstant u/nbsp               #x00A0)

;; List item bullets 
;; Each such list is a list of preferences, there should be a
;; character, which is likely to exist in all fonts.

(defvar +list-style-type-glyphs/disc+
    (list ;;u/black-circle u/bullet u/white-bullet u/white-circle
          (char-rune #\o)))

(defvar +list-style-type-glyphs/circle+
    (list ;;u/white-circle u/white-bullet u/bullet u/black-circle
          (char-rune #\*)))

(defvar +list-style-type-glyphs/square+
    (list ;;u/black-square u/white-square u/white-bullet u/bullet
          (char-rune #\-)))


;;;;

(defvar *default-style-sheet*)

(defparameter *tex-mode-p* nil
  "Whether we use the TeX-like algorithm.")

;;; ---- Vertical Geometry of Lines -------------------------------------------

(defclass replacement-object ()
  ())

;;; ---- Believed to be correct -----------------------------------------------

(defsubst rune-width (font rune)
  (css-font-desc-glyph-width font (rune-code rune)))

(defun parse-url* (url)
  (cond ((url:url-p url) url)
        ((url:parse-url url))))

(let ((memo (make-hash-table :test #'equal)))
  (defun make-text-style (device 
                          &key (font-family '("times"))
                               (font-weight 400)
                               (font-size 14)
                               (font-style :normal)
                               (font-variant :normal)
                               (letter-spacing :normal)
                               (word-spacing :normal))
    (let ((key (list device font-family font-weight font-size font-style font-variant)))
      (or (gethash key memo)
          (setf (gethash key memo)
                (progn
                  (check-type letter-spacing (or (member :normal) real))
                  (check-type word-spacing (or (member :normal) real))
                  (make-text-style-prim 
                   (find-css-font-desc (device-font-database device)
                                       font-family font-weight
                                       font-style font-size font-variant)
                   font-family font-weight
                   font-size font-style font-variant
                   letter-spacing word-spacing)))))))



(defvar +null-simple-array+ (vector))

(defvar *white-space*)

(defvar *replaced-object* nil)          ;xxx kludge!

(declaim (inline add-rune* walk-pcdata/normal))

(defun pt-style-counter-reset (rc pt)
  (style-counter-reset (lookup-style rc pt)))

(defun pt-style-content (rc pt)
  (style-content (lookup-style rc pt)))

(defun make-pseudo-element-1 (class pt)
  (make-instance class :proxee pt))

;;; Hack!

(defun counter-name-equal-p (x y) (string= x y))

(defun counter-value (rc pt name &optional (reset-this-p t))
  ;; functional description
  ;; first walk up until we find a counter reset
  ;;
  ;; Q: are counter values are otherwise affected by properties like
  ;;    'display'?
  (let ((value 0))
    (let ((scope pt) x)
      (loop
          (when (null scope)
            (return))
          (let ((resets (pt-style-counter-reset rc scope)))
            (when (and (not (and (eq pt scope) (not reset-this-p)))
                       (setq x (find name resets :key #'first :test #'counter-name-equal-p)))
              (setf value (cadr x))
              (return)))
        (setq scope (element-parent scope)))
      ;; now walk down
      (block walking
        (labels ((walk (x)
                   (let* ((style (lookup-style rc x))
                          (resets (style-counter-reset style))
                          (incs   (style-counter-increment style)))
                     (unless (and (not (and (eq pt x) (not reset-this-p)))
                                  (find name resets :key #'first :test #'counter-name-equal-p))
                       (let ((y (find name incs :key #'first :test #'counter-name-equal-p)))
                         (when y
                           (incf value (cadr y))))
                       (cond ((eq x pt)
                              (return-from walking))
                             (t
                              (let ((b (gethash x *before-pe-hash*)))
                                (and b (walk b)))
                              (mapc #'walk (element-children x))
                              (let ((a (gethash x *after-pe-hash*)))
                                (and a (walk a))))))) ))
          (mapc #'walk (element-children (or scope (element-root pt))))) )
      (values value scope))))

(defun element-root (pt) 
  (cond ((null (element-parent pt)) pt)
	((element-root (element-parent pt)))))

(defun counter-values (rc pt name &optional (reset-this-p t))
  (multiple-value-bind (value scope) (counter-value rc pt name reset-this-p)
    (if scope
        (list* value (counter-values rc scope name nil)))))


(defun interpret-image (value)
  (cond ((eq value :none) :none)
        ((stringp value)
         (url:parse-url value))
        ((url:url-p value)
         value)
        (t
         (warn "Bad CSS image value: ~S." value)
         :none)))

(defun clim-draw-background (medium x1 y1 x2 y2
                             background-color)
  (when (and background-color (not (eql background-color :transparent)))
    (clim:draw-rectangle* medium
     x1 y1 x2 y2
     :ink (css-color-ink background-color))))

;;;; --------------------------------------------------------------------------------
;;;;  Pseudo Elements

(defclass pseudo-element ()
  ((proxee
    :initarg :proxee)
   (style-cache :initform nil
                :accessor element-style-cache)
   ))

(defmethod element-p ((element pseudo-element))
  t)

(defmethod element-parent ((element pseudo-element))
  (with-slots (proxee) element
    proxee))

(defmethod element-children ((element pseudo-element))
  nil)

(defmethod element-attribute ((element pseudo-element) attribute)
  nil)

(defmethod element-gi ((element pseudo-element))
  (with-slots (proxee) element
    (element-gi proxee)))

(defmethod text-element-p ((element pseudo-element))
  nil)

(defmethod closure-protocol:element-replaced-element-1
    (language user-agent document device (element pseudo-element))
  nil)

;;; css support protocol

(defmethod element-css-class ((element pseudo-element))
  (with-slots (proxee) element
    (element-css-class proxee)))

(defmethod element-css-id ((element pseudo-element))
  (with-slots (proxee) element
    (element-css-id proxee)))

(defmethod element-implicit-style (doc (element pseudo-element))
  nil)

(defmethod element-explicit-style (doc (element pseudo-element))
  nil)

;;; renderer support protocol

(defmethod element-base-url (document-language (element pseudo-element))
  (with-slots (proxee) element
    (element-base-url document-language proxee)))

(defmethod element-imap (document-language document (element pseudo-element))
  nil)

;;; :first-line

(defclass first-line-pseudo-element (pseudo-element)
  ())

(defmethod pseudo-class-matches-p ((pseudo-class (eql :first-line)) (element first-line-pseudo-element))
  t)

(defmethod pseudo-class-matches-p ((pseudo-class t) (element first-line-pseudo-element))
  nil)

(defun make-first-line-pseudo-element (proto)
  (make-instance 'first-line-pseudo-element
                 :proxee proto))

;;; :first-letter

(defclass first-letter-pseudo-element (pseudo-element)
  ())

(defmethod pseudo-class-matches-p ((pseudo-class (eql :first-letter)) (element first-letter-pseudo-element))
  t)

(defmethod pseudo-class-matches-p ((pseudo-class t) (element first-letter-pseudo-element))
  nil)

;;; :before/:after

(defclass around-pseudo-element (pseudo-element)
  ((children
    :initform nil
    :accessor element-children)))

(defclass before-pseudo-element (around-pseudo-element)
  ())

(defclass after-pseudo-element (around-pseudo-element)
  ())

(defmethod pseudo-class-matches-p ((pseudo-class (eql :before)) (element before-pseudo-element))
  t)

(defmethod pseudo-class-matches-p ((pseudo-class t) (element before-pseudo-element))
  nil)

(defmethod pseudo-class-matches-p ((pseudo-class (eql :after)) (element after-pseudo-element))
  t)

(defmethod pseudo-class-matches-p ((pseudo-class t) (element after-pseudo-element))
  nil)

(defmethod clim:draw-design (medium (r CLIM-INTERNALS::STANDARD-RECTANGLE-SET) &rest options)
  (clim:map-over-region-set-regions (lambda (r)
                                      (apply #'clim:draw-design medium r options))
                                    r))

(defun text-style-ascent (text-style)
  (font-desc-ascent (text-style-font text-style)))

(defun text-style-descent (text-style)
  (font-desc-descent (text-style-font text-style)))

(defun dprint (fmt &rest args)
  (fresh-line *trace-output*)
  (apply #'format *trace-output* fmt args)
  (fresh-line *trace-output*)
  (finish-output *trace-output*))

(defun make-text-element (parent thing)
  "Return a new text element containing 'thing'."
  ;; XXX Currently we just reuse SGML:PT
  (sgml::make-pt/low 
   :name :pcdata
   :parent parent
   :attrs (if (stringp thing)
              (string-rod thing)
              thing)))

(defun make-image-element (parent url &key (show-broken-p nil))
  "Return a new element which when rendered yields an image replaced
element showing the image refered to by 'url'. The 'show-broken-p'
argument controls whether a broken image should be shown."
  ;; XXX Kludge
  (sgml::make-pt/low
   :name :img
   :parent parent
   :attrs (list :src (string-rod (url:unparse-url url)))))


;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: RENDERER; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: List Items
;;;   Created: 2002-10-26
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: GPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1997-2002 by Gilbert Baumann

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
 
(in-package :RENDERER)

;;;; --------------------------------------------------------------------------------
;;;;  TODO

;; - abusing SGML:PT IMG elements for list-style-images is bad.
;; - prevent broken images
;; - we should find a way around the list-item-index HTML dependency
;; - we just introduced depreciated references to CSS:STYLE-ATTR.

;;;; --------------------------------------------------------------------------------
;;;;  List Item Pseudo Elements

;; List items are rendered through the introduction of pseudo elements
;; onbehalf of an elements before pseudo element. For that we define a
;; new class which provides both the element and the css support
;; protocol serving as the list item marker.

;; Note that CSS3 seeks to introducte a "::marker" pseudo class which
;; will set further attributes of this list-item-marker pseudo
;; element.

(defclass list-item-marker (before-pseudo-element)
  ((parent      :initarg :parent)
   (style-cache :initform nil
                :accessor element-style-cache)
   (style       :initform nil
                :initarg :style)
   (children    :reader element-children)))

(defun make-list-item-marker-element (list-element style)
  ;; ## make this element in a way that it is still possible to
  ;; overwrite stuff in the style sheet.
  "Given a list item element 'list-element' return a synthetic element
which serves the rôle of an before psuedo element containing the list item marker"
  (let* ((list-style-type        (cooked-style-list-style-type style))
         (list-style-position    (cooked-style-list-style-position style))
         (list-style-image       (cooked-style-list-style-image style))
         (res
          (make-instance 'list-item-marker
                         :parent list-element
                         :style (list (cons 'css:@display
                                            (if (eql list-style-position :inside)
                                                :inline
                                                :marker))
                                      (cons 'css:@content
                                            (list
                                             (cond ((eql list-style-image :none)
                                                    (list :string
                                                          (rod-string ;###
                                                           (list-item-string* list-style-type
                                                                              (1+ (list-item-index list-element))))))
                                                   (t
                                                    ;; ### not sure about this one
                                                    (list :url list-style-image)))))))))
    (setf (slot-value res 'children)
          nil)
    res))

(defun list-item-index (pt)
  "Given a list-item parse tree element return the index (zero-based)
of it wrt to item numbering."
  ;; XXX This still assumes HTML.
  (let ((parent (element-parent pt)))
    (let ((index 0))
      (let ((start (pt-attr/integer parent :start nil)))
        (when start
          (if (>= start 1)
              (setf index (1- (pt-attr/integer parent :start)))
              (warn "Value, ~S, of 'START' is insane." start))))
      (dolist (k (element-children parent))
        (let ((value (pt-attr/integer k :value nil)))
          (cond (value
                 (if (>= value 1)
                     (setf index (1- value))
                     (warn "Value, ~S, of 'VALUE' is insane." value))) ))
        (when (eq k pt)
          (return index))
        (incf index)))))

(defun list-item-string* (list-style-type n)
  (ecase list-style-type
    (:disc   (coerce (vector (elt +list-style-type-glyphs/disc+ 0))   'rod))
    (:circle (coerce (vector (elt +list-style-type-glyphs/circle+ 0)) 'rod))
    (:square (coerce (vector (elt +list-style-type-glyphs/square+ 0)) 'rod))
    (:decimal
     (map 'rod #'char-code 
          (format nil "~D." n)))
    (:lower-roman
     (map 'rod #'char-code 
          (format nil "~(~@R~)." n)))
    (:upper-roman
     (map 'rod #'char-code 
          (format nil "~:@(~@R~)." n)))
    (:lower-alpha
     (map 'rod #'char-code 
          (format nil "~(~A~)." (integer->abc n))))
    (:upper-alpha
     (map 'rod #'char-code 
          (format nil "~:@(~A~)." (integer->abc n))))
    (:none
     (map 'rod #'identity nil))))



(defun integer->abc (i)
  ;;(assert (and (integerp i) (>= i 0)))
  (cond ((< i 0) "A")                   ;defensives Programmieren.
        ((<= i 26) (string (code-char (+ (char-code #\A) (- i 1)))))
        (t (concatenate 'string (integer->abc (+ (floor (- i 1) 26) 1)) (integer->abc (+ 1 (mod (- i 1) 26))))) ))

(defun choose-glyph (font alternatives)
  ;; Select the first representable glyph from `alternatives'
  (dolist (x alternatives (car (last alternatives)))
    (cond ((glyph-representable-p font x)
           (return x)))))

;;; Element protocol

(defmethod element-p ((object list-item-marker))
  (declare (ignorable object))
  t)

(defmethod element-parent ((object list-item-marker))
  (with-slots (parent) object
    parent))

(defmethod element-attribute ((object list-item-marker) attribute-name)
  (declare (ignorable object attribute-name))
  nil)

(defmethod element-gi ((object list-item-marker))
  :|LI::marker|                         ;???
  )

(defmethod text-element-p ((object list-item-marker))
  nil)

;;; CSS Support Protocol

(defmethod element-css-class ((object list-item-marker))
  nil)

(defmethod element-css-id ((object list-item-marker))
  nil)

(defmethod pseudo-class-matches-p (pclass (object list-item-marker))
  (declare (ignorable pclass object))
  nil)

(defmethod element-implicit-style (doc (object list-item-marker))
    (declare (ignorable doc))
  (slot-value object 'style))

(defmethod element-explicit-style (doc (object list-item-marker))
  (declare (ignorable doc object))
  nil)

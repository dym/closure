;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CSS; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: [temporary] parse tree support for the css stuff
;;;   Created: 2002-08-07
;;;    Author: Gilbert Baumann <gilbert@base-engineering.com>
;;;   License: MIT style (see below)
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2002 by Gilbert Baumann

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


(in-package :css)

(defun element-has-class-p (element class)
  (class-eq class (element-css-class element)))

(defun element-has-id-p (element id)
  (id-eq id (element-css-id element)))

;;; hmmm

(defun intern-attribute-name (papyrus)
  ;; XXX hack
  (intern (string-upcase (if (stringp papyrus)
			     papyrus
			     (papyrus-string papyrus)))
	  :keyword))

(defun intern-gi (string)
  (intern-attribute-name string))

(defun css2-class-match-p (string element)
  (attribute-contains-p element #.(map 'vector #'char-code "CLASS") string t))

(defun css2-class-match-p (string element)
  ;; XXX we should search for occurence
  (equalp string (closure-protocol:element-css-class element)))
;;   (attribute-contains-p element #.(map 'vector #'char-code "CLASS") string t))

(defun css2-id-match-p (string element)
  (attribute-equal-p element #.(map 'vector #'char-code "ID") string t))

(defun css2-gi-match-p (string element)
  (eq (element-gi element) string))

;; We probably want
;;   intern-gi    document-language id-rod
;;   intern-class document-language class-rod
;;   intern-id    document-language class-id
;; and
;;   element-css-gi element
;;   element-css-classes element
;;   element-css-id element
;;

(defun class-eq (x y)
  (equalp x y))

(defun id-eq (x y)
  (equalp x y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; we still need to cover these:
;;
;; css-parse.lisp:                      (renderer::maybe-parse-style-sheet-from-url
;; css-setup.lisp: ((interpret-length device value pt (r2::pt-font-size pt)))))
;; css-setup.lisp:                 (*dpi* (r2::device-dpi *device*))
;; css-setup.lisp:                 (dpi (r2::device-dpi *device*)))
;; css-setup.lisp:              (round (* a (r2::device-canvas-width device))
;; css-setup.lisp:              (round (* a (r2::device-canvas-height device))

;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SGML; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Protocol integration of PT
;;;   Created: Somewhen in 1996
;;;    Author: Gilbert Baumann <gilbert@base-engineering.com>
;;;   License: MIT style (see below)
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1996-1999 by Gilbert Baumann

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

;; Changes
;;
;;  When        Who     What
;; ----------------------------------------------------------------------------
;;  1999-08-24  GB      - PT structure: spend PLIST slot
;;  1999-08-24  GB      - PT-ACCESS, PT-PATH: new functions
;;                      - REMOVE-PT, DELETE-PT: new functions
;;                      - ANCESTORP: new function
;;

(in-package :r2)


(defmethod closure-protocol:element-p ((object sgml:pt))
  t)

(defmethod closure-protocol:element-parent ((element sgml:pt))
  (sgml:pt-parent element))

(defmethod closure-protocol:element-children ((element sgml:pt))
  (sgml:pt-children element))

(defmethod closure-protocol:element-attribute
    ((element sgml:pt) attribute-name)
  (getf (sgml:pt-attrs element) attribute-name))

(defmethod closure-protocol:element-gi ((element sgml:pt))
  (sgml:gi element))

(defmethod closure-protocol:text-element-p ((element sgml:pt))
  (eql (sgml:gi element) :pcdata))

(defmethod closure-protocol:element-text ((element sgml:pt))
  (assert (eql (sgml:gi element) :pcdata))
  (sgml:pt-attrs element))

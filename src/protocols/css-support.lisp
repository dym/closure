;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLOSURE-PROTOCOL; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Protocol to support CSS style sheet cascading
;;;   Created: 2002-08-07 03:40
;;;    Author: Gilbert Baumann <gilbert@base-engineering.com>
;;;   License: LGPL (See file COPYING for details).
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
 
(in-package :CLOSURE-PROTOCOL)

(defgeneric element-css-class (element)
  )

(defgeneric element-css-id (element)
  )

(defgeneric pseudo-class-matches-p (pseudo-class element)
  )

(defgeneric element-style-cache (element)
  )

(defgeneric (setf element-style-cache) (new-value element)
  )

(defgeneric element-implicit-style (document element)
  )

(defgeneric element-explicit-style (document element)
  )

;; we want

#||
(defgeneric element-implicit-style (document-language element))
(defgeneric element-explicit-style (document-language element))
(defgeneric element-css-class (document-language element))
(defgeneric element-css-id (document-language element))
(defgeneric element-psuedo-class-matches (document-language pseudo-class element))

together with:

(defclass html-document-language ())
(defclass html-4.0-document-language (html-document-language))
(defclass html-3.2-document-language (html-document-language))

(defclass xml-document-language ())

we might get away with special variables ... and split this into e.g.

like:

(defvar *document-language*)
(defvar *user-agent*)

(defgeneric element-implicit-style-1 (user-agent document-language element))

(defun element-implicit-style (element)
  (element-implicit-style-1 *user-agent* *document-language* element))

;; then we want a user-agent protcol like

(defgeneric post-title (user-agent title))
(defgeneric post-link (user-agent ...)
(defgeneric open-uri (user-agent uri))

||#


;;;;;;;;;;;;; Render Support Protocol

(defvar *document-language*)
(defvar *user-agent*)

(defun element-replaced-element (device element)
  (element-replaced-element-1 *document-language* *user-agent* document device element))

(defgeneric element-replaced-element-1 (document-language user-agent document device element)
  )

(defgeneric element-base-url (document-language element)
  )

(defgeneric element-imap (document-language document element)
  )

(defgeneric render (document-language document device element
                    width
                    &optional flag h 
                    &key selected-style )
  )

(defgeneric root-element-embedded-style (language root-element)
  )



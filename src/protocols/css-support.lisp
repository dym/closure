;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLOSURE-PROTOCOL; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Protocol to support CSS style sheet cascading
;;;   Created: 2002-08-07 03:40
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: LGPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2002 by Gilbert Baumann

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the 
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, 
;;; Boston, MA  02111-1307  USA.
 
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



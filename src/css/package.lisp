;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-USER; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: CSS Package Definition
;;;   Created: 2002-07-23
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: GPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2002 by Gilbert Baumann

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
 
(in-package :CL-USER)

(defpackage :css
  (:use :glisp)
  (:export
   #:MAKE-ASSIGNMENT
   #:ASSIGNMENT-SELECTOR
   #:ASSIGNMENT-SLOT
   #:ASSIGNMENT-VALUE
	   
   #:MAKE-SINGLETON-SELECTOR
   #:SINGLETON-SELECTOR-GI
   #:SINGLETON-SELECTOR-ID
   #:SINGLETON-SELECTOR-CLASS
   #:SINGLETON-SELECTOR-PSEUDO-CLASS

   #:CREATE-STYLE-SHEET
   #:STYLE-SHEET-RELATE
   #:STYLE-SHEET-LOOKUP
   #:PARSE-STYLE-SHEET
   #:DESCRIBE-STYLE-SHEET

   #:setup-style
   #:style-attr
   #:set-style-attr
   #:resolve-width
           
   #:percentage-p
   #:block-element-p
   #:display                            ;als funktion
	   
   ;; Defined Attributes
   #:@BACKGROUND-ATTACHMENT #:@BACKGROUND-COLOR #:@BACKGROUND-IMAGE
   #:@BACKGROUND-REPEAT #:@BORDER-BOTTOM-COLOR #:@BORDER-BOTTOM-STYLE
   #:@BORDER-BOTTOM-WIDTH #:@BORDER-LEFT-COLOR #:@BORDER-LEFT-STYLE
   #:@BORDER-LEFT-WIDTH #:@BORDER-RIGHT-COLOR #:@BORDER-RIGHT-STYLE
   #:@BORDER-RIGHT-WIDTH #:@BORDER-TOP-COLOR #:@BORDER-TOP-STYLE
   #:@BORDER-TOP-WIDTH #:@CLEAR #:@COLOR #:@DISPLAY #:@FLOAT #:@FONT-FAMILY #:@FONT-SIZE
   #:@FONT-STYLE #:@FONT-VARIANT #:@FONT-WEIGHT #:@HEIGHT #:@LETTER-SPACING
   #:@LINE-HEIGHT #:@LIST-STYLE-IMAGE #:@LIST-STYLE-POSITION
   #:@LIST-STYLE-TYPE #:@MARGIN-BOTTOM #:@MARGIN-LEFT #:@MARGIN-RIGHT
   #:@MARGIN-TOP #:@PADDING-BOTTOM #:@PADDING-LEFT #:@PADDING-RIGHT
   #:@PADDING-TOP #:@TEXT-ALIGN #:@TEXT-DECORATION #:@TEXT-INDENT
   #:@TEXT-TRANSFORM #:@VERTICAL-ALIGN #:@WHITE-SPACE #:@WIDTH #:@WORD-SPACING
   #:@orig-width #:@BACKGROUND-POSITION
   ;; New CSS-2 attributes
   #:@POSITION #:@TOP #:@RIGHT #:@BOTTOM #:@LEFT
   #:@CONTENT #:@QUOTES #:@COUNTER-RESET #:@COUNTER-INCREMENT #:@MARKER-OFFSET
   ))


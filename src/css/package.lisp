;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-USER; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: CSS Package Definition
;;;   Created: 2002-07-23
;;;    Author: Gilbert Baumann <gilbert@base-engineering.com>
;;;   License: MIT style (see below)
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2005 by Gilbert Baumann

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

(in-package :CL-USER)
(defpackage :css
  (:use :glisp :runes)
  ;;
  (:import-from "CLOSURE-PROTOCOL"
   ;; basic element protocol
   #:element-parent
   #:element-children
   #:text-element-p
   #:element-gi
   #:element-attribute
   
   ;; css support protocol
   #:element-css-class
   #:element-css-id
   #:pseudo-class-matches-p
   #:element-style-cache
   #:element-implicit-style
   #:element-explicit-style
   )
  ;;
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

   #:KILL-STYLE
	   
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


;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-USER; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Package definition for the Closure Protocols container package
;;;   Created: 2002-08-07 03:26
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

(defpackage :CLOSURE-PROTOCOL
    (:use :glisp)
  (:export
   ;; Basic Element Protocol
   #:element-p
   #:element-parent
   #:element-children
   #:element-attribute
   #:element-gi
   #:text-element-p
   #:element-text
   
   ;; css support protocol
   #:element-css-class
   #:element-css-id
   #:pseudo-class-matches-p
   #:element-style-cache
   #:element-implicit-style
   #:element-explicit-style
   
   ;; renderer support protocol
   #:element-replaced-element-1
   #:element-replaced-element
   #:*user-agent*
   #:*document-language*
   #:element-base-url
   #:element-imap
   #:render
   #:root-element-embedded-style
   ))


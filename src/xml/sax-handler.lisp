;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SAX; readtable: glisp; Encoding: utf-8; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: A SAX2-like API for the xml parser
;;;   Created: 2003-06-30
;;;    Author: Henrik Motakef <hmot@henrik-motakef.de>
;;;   License: BSD
;;; ---------------------------------------------------------------------------
;;;  © copyright 2003 by Henrik Motakef

;;; Redistribution and use  in source and binary   forms, with or  without
;;; modification, are permitted provided that the following conditions are
;;; met:                                                                  
;;; 								      
;;; 1. Redistributions  of  source  code  must retain  the above copyright
;;;    notice, this list of conditions and the following disclaimer.      
;;; 								      
;;; 2. Redistributions in  binary form must reproduce  the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution
;;; 								      
;;; THIS  SOFTWARE   IS PROVIDED ``AS  IS''   AND ANY  EXPRESS  OR IMPLIED
;;; WARRANTIES, INCLUDING, BUT NOT LIMITED  TO, THE IMPLIED WARRANTIES  OF
;;; MERCHANTABILITY  AND FITNESS FOR A  PARTICULAR PURPOSE ARE DISCLAIMED.
;;; IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
;;; INDIRECT,  INCIDENTAL,  SPECIAL, EXEMPLARY,  OR CONSEQUENTIAL  DAMAGES
;;; (INCLUDING, BUT NOT LIMITED TO,   PROCUREMENT OF SUBSTITUTE GOODS   OR
;;; SERVICES;  LOSS OF  USE,  DATA, OR  PROFITS; OR BUSINESS INTERRUPTION)
;;; HOWEVER  CAUSED AND ON ANY THEORY  OF LIABILITY,  WHETHER IN CONTRACT,
;;; STRICT LIABILITY, OR  TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
;;; IN ANY WAY  OUT OF THE  USE OF THIS SOFTWARE,  EVEN IF ADVISED OF  THE
;;; POSSIBILITY OF SUCH DAMAGE.

;;; TODO/ Open Questions:

;; o Should there be a predefined "handler" class, or even several
;;   (like Java SAX' ContentHandler, DTDHandler, LexicalHandler etc? I
;;   don't really see why.
;; o Missing stuff from Java SAX2:
;;   * ignorable-whitespace
;;   * document-locator/(setf document-locator)
;;     (probably implies a handler class with an appropriate slot)
;;   * skipped-entity
;;   * notation-declaration
;;   * unparsed-entity-declaration
;;   * The whole ErrorHandler class, this is better handled using
;;     conditions (but isn't yet)
;;   * The LexicalHandler (start-cdata etc) would be nice
;;   * The DeclHandler interface (element-decl, attribute-decl...)
;;     is useful, but the Java interface sucks.
;; o Despite all the namespace-uri etc arguments, namespaces are not
;;   really supported yet, the xml parser always passes nil. This will
;;   hopefully change Real Soon Now, and I didn't want to have to
;;   rewrite the interface then

(defpackage :sax
  (:use :common-lisp)
  (:export #:start-document
	   #:start-element
	   #:characters
	   #:processing-instruction
	   #:end-element
	   #:end-document
	   #:comment
	   #:start-cdata
	   #:end-cdata))

(in-package :sax)

(defgeneric start-document (handler)
  (:method ((handler t)) nil))

;; How should attributes be represented?
;; Currently its just a (name . value) alist, but this isn't too
;; useful wrt namespaced attributes. Probably a struct.
(defgeneric start-element (handler namespace-uri local-name qname attributes)
  (:method ((handler t) namespace-uri local-name qname attributes) nil))

(defgeneric characters (handler data)
  (:method ((handler t) data) nil))

(defgeneric processing-instruction (handler target data)
  (:method ((handler t) target data) nil))

(defgeneric end-element (handler namespace-uri local-name qname)
  (:method ((handler t) namespace-uri local-name qname) nil))

(defgeneric end-document (handler)
  (:method ((handler t)) nil))

;; LexicalHandler

(defgeneric comment (handler data)
  (:method ((handler t) data) nil))

(defgeneric start-cdata (handler)
  (:method ((handler t)) nil))

(defgeneric end-cdata (handler)
  (:method ((handler t)) nil))
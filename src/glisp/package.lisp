;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: GLISP-TEMP; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Generating a sane DEFPACKAGE for GLISP
;;;   Created: 1999-05-25
;;;    Author: Gilbert Baumann <gilbert@base-engineering.com>
;;;   License: MIT style (see below)
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1999,2000 by Gilbert Baumann

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

(in-package :cl-user)

(defpackage "GLISP"
  (:use :cl)
  (:export "DEFSUBST"
	   "G/MAKE-STRING"
	   "WITH-TIMEOUT"
	   "OPEN-INET-SOCKET"
	   ;; util.lisp :
	   "ALWAYS"
	   "CL-BYTE-STREAM"
	   "CL-CHAR-STREAM"
	   "CL-STREAM"
	   "COMPOSE"
	   "CURRY"
	   "FALSE"
	   "FORCE"
	   "G/CLOSE"
	   "G/FINISH-OUTPUT"
	   "G/PEEK-CHAR"
	   "G/READ-BYTE"
	   "G/READ-BYTE-SEQUENCE"
	   "G/READ-CHAR"
	   "G/READ-CHAR-SEQUENCE"
	   "G/READ-LINE"
	   "G/READ-LINE*"
	   "G/UNREAD-BYTE"
	   "G/UNREAD-CHAR"
	   "G/WRITE-BYTE"
	   "G/WRITE-BYTE-SEQUENCE"
	   "G/WRITE-CHAR"
	   "G/WRITE-STRING"
	   "GSTREAM"
	   "MAP-ARRAY"
	   "MAPFCAR"
	   "MAX*"
	   "MAXF"
	   "MIN*"
	   "MINF"
	   "MULTIPLE-VALUE-OR"
	   "MULTIPLE-VALUE-SOME"
	   "NCONCF"
	   "NEQ"
	   "PROMISE"
	   "RCURRY"
	   "SANIFY-STRING"
	   "SHOW"
	   "SPLIT-BY"
	   "SPLIT-BY-IF"
	   "SPLIT-BY-MEMBER"
	   "SPLIT-STRING"
	   "STRING-BEGIN-EQUAL"
	   "TRUE"
	   "UNTIL"
	   "USE-BYTE-FOR-CHAR-STREAM-FLAVOUR"
	   "USE-CHAR-FOR-BYTE-STREAM-FLAVOUR"
	   "WHILE"
	   "WHITE-SPACE-P"

	   "CL-BYTE-STREAM->GSTREAM"
	   "CL-CHAR-STREAM->GSTREAM"
	   "G/OPEN-INET-SOCKET"
	   "ACCEPT-CONNECTION"

	   "FIND-TEMPORARY-FILE"
	   "DELETE-TEMPORARY-FILE"
	   "WITH-TEMPORARY-FILE"
          
	   "SET-EQUAL"
	   "MAYBE-PARSE-INTEGER"
	   "NOP"
	   "WITH-STRUCTURE-SLOTS"

	   "COMPILE-FUNCALL"
	   "FUNCALL*"
	   "MAPC*"
	   "VREDUCE*"
	   "LREDUCE*"
	   "WITH-UNIQUE-NAMES"
    
	   "G/MAKE-HASH-TABLE"
	   "G/HASHGET"
	   "G/CLRHASH"
	   "STIR-HASH-CODES"
	   "HASH-SEQUENCE"
	   "HASH/STRING-EQUAL"
	   "MAKE-STRING-EQUAL-HASH-TABLE"

	   "PRIMEP"
    
	   ;; match.lisp
	   "DEFINE-MATCH-MACRO"
	   "IF-MATCH"
	   "GSTREAM-AS-STRING"))

(defpackage "GLUSER"
  (:use "CL" "GLISP"))

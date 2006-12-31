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

(defpackage :glisp-temp (:use #:cl))
(in-package :glisp-temp)

(defpackage :glisp (:use))

(eval-when (compile)
  (defvar *export-from-glisp*
    '(
      "DEFSUBST"
      "G/MAKE-STRING"
      "MP/MAKE-LOCK"
      "MP/WITH-LOCK"
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
      "GSTREAM-AS-STRING"
      ))

  (defparameter *packages* 
    #-GCL '(:common-lisp)
    #+GCL '(:lisp :pcl) )

  (defparameter *gray-symbols*
    '("FUNDAMENTAL-STREAM"
      "FUNDAMENTAL-INPUT-STREAM"
      "FUNDAMENTAL-OUTPUT-STREAM"
      "FUNDAMENTAL-CHARACTER-STREAM"
      "FUNDAMENTAL-BINARY-STREAM"
      "FUNDAMENTAL-CHARACTER-INPUT-STREAM"
      "FUNDAMENTAL-CHARACTER-OUTPUT-STREAM"
      "FUNDAMENTAL-BINARY-INPUT-STREAM"

      "STREAM-READ-CHAR"
      "STREAM-UNREAD-CHAR"
      "STREAM-READ-CHAR-NO-HANG"
      "STREAM-PEEK-CHAR"
      "STREAM-LISTEN"
      "STREAM-READ-LINE"
      "STREAM-CLEAR-INPUT"

      "STREAM-WRITE-CHAR"
      "STREAM-LINE-COLUMN"
      "STREAM-START-LINE-P"
      "STREAM-WRITE-STRING"
      "STREAM-TERPRI"
      "STREAM-FRESH-LINE"
      "STREAM-FINISH-OUTPUT"
      "STREAM-FORCE-OUTPUT"
      "STREAM-ADVANCE-TO-COLUMN"

      "STREAM-READ-BYTE"
      "STREAM-WRITE-BYTE" ))

  (defparameter *gray-packages*
    `(
      #+:CLISP                 ,@'(:lisp)
      #+:CMU                   ,@'(:ext)
      #+:sbcl                   ,@'(:sb-gray)
      #+:ALLEGRO               ,@'(:common-lisp :excl :stream)
      #+:HARLEQUIN-COMMON-LISP ,@'(:stream)
      #+:OPENMCL               ,@'(:ccl)
      ))

  (defun seek-symbol (name packages)
    ;; Seek the a symbol named 'name' in `packages'
    (or (some #'(lambda (p) 
                  (multiple-value-bind (sym res) (find-symbol name p)
                    (if (eql res :external)
                        (list sym)
                      nil)))
              packages)
        (progn (format T "~&There is no ~A in ~A." name packages)
               (finish-output)
               nil)))

  (defun dump-defpackage (&aux imports export-gray)
    (labels ((grok (symbols packages)
                   (let ((res nil))
                     (dolist (nam symbols)
                       (let ((sym (seek-symbol nam packages)))
                         (when sym
                           (push (car sym) res)
                           (cond ((multiple-value-bind (sym2 res) (find-symbol nam :glisp)
                                    (and sym2 (eq res :external)))
                                  ;;
                                  (format T "~&;; ~S is pacthed." sym) )
                                 (t
                                  (setf sym (car sym))
                                  ;; CLISP has no (:import ..) ARG!
                                  (push `(:import-from
                                          ,(package-name (symbol-package sym))
                                          ,(symbol-name sym))
                                        imports))))))
                     res)))
      (setf export-gray (grok *gray-symbols* *gray-packages*))
      `(progn
         (defpackage "GLISP"
	   (:use :cl)
           ,@imports
           (:export
            ,@(mapcar #'symbol-name export-gray)
            ,@*export-from-glisp*))
         (defpackage "GLUSER"
           (:use "CL" "GLISP")) )))

  (defmacro define-glisp-package ()
    (dump-defpackage))
  )

(define-glisp-package)


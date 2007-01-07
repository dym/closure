;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: GLUSER; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Some stuff
;;;   Created: ????
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

(in-package :GLUSER)

;;; Ich habe das jetzt satt! Wir lesen das default style sheet jetzt
;;; im rahmen des compilierens, dann gibt es spaeter keine
;;; enttaeuschungen.

  
#+NIL  (defparameter *resources-base-directory*
    *load-truename*)

(let ((load-truename (load-time-value (or #.*compile-file-pathname* *load-pathname*))))
  (setf (url:url-logical-host-translator "closure")
        (lambda (url)
          (let ((res (url:copy-url url)))
            (setf (url:url-protocol res) "file"
                  (url:url-host res) "localhost"
                  (url:url-path res)
                  (append
                   (list :absolute)
                   (butlast (cdr (pathname-directory (truename load-truename))))
                   (cdr (url:url-path url))))
            res))))

(format T "~&;; Parsing default style sheet~%")
(setf r2::*default-style-sheet* 
      (css::parse-style-sheet-from-url (url:parse-url "file://closure/resources/css/default.css")
                                       :name "Closure Default Style Sheet"))

(format T "~&;; Parsing DTD~% ")
(sgml:slurp-catalog (url:parse-url "file://closure/resources/dtd/catalog"))
(setf cl-user::*html-dtd* (sgml:parse-dtd '(:public "-//W3C//DTD HTML 4.0 Frameset//EN")))
(format T "~&;; done~%")



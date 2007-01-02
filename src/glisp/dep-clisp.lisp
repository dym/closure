;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-USER; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: CLISP dependent stuff + fixups
;;;   Created: 1999-05-25 22:32
;;;    Author: Gilbert Baumann <gilbert@base-engineering.com>
;;;   License: MIT style (see below)
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1999 by Gilbert Baumann

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

(setq lisp:*load-paths* '(#P"./"))

(defmacro glisp::with-timeout ((&rest ignore) &body body)
  (declare (ignore ignore))
  `(progn
     ,@body))

(defun glisp::open-inet-socket (hostname port)
  (values
   (lisp:socket-connect port hostname)
   :byte))

(defun glisp::g/make-string (length &rest options)
  (apply #'make-array length 
         :element-type 
         '#.(cond ((stringp (make-array 1 :element-type 'string-char))
                   'string-char)
                  ((stringp (make-array 1 :element-type 'base-char))
                   'base-char)
                  (t
                   (error "What is the string element type of the day?")))
         options))

(defun glisp:run-unix-shell-command (command)
  (lisp:shell command))

(export 'glisp::getenv :glisp)
(defun glisp::getenv (var)
  (sys::getenv var))

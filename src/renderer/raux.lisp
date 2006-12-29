;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: R2; -*-
;;; --------------------------------------------------------------------------------------
;;;     Title: Auxiliary render functions
;;;   Created: 1999-02-24
;;;    Author: Gilbert Baumann <gilbert@base-engineering.com>
;;;   License: MIT style (see below)
;;; --------------------------------------------------------------------------------------
;;;  (c) copyright 1998,1999 by Gilbert Baumann

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

(in-package :R2)

(defun pt-data (x)
  (cond ((text-element-p x)
	 (map 'string #'rune-char (element-text x)))
	((apply 'concatenate 'string
                (mapcar #'pt-data (element-children x))))))

(defun pt-data-iso10646 (x)
  (cond ((text-element-p x)
	 (element-text x))
	((apply 'concatenate 'rod
                (mapcar #'pt-data-iso10646 (element-children x))))))

(defun white-space-rune-p* (rune)
  (or (eq rune 9)
      (eq rune 10)
      (eq rune 12)
      (eq rune 13)
      (eq rune 32)))

(defun white-space-rune-p*/no-nl (rune)
  (or (eq rune 9)
      (eq rune 12)
      (eq rune 13)
      (eq rune 32)))

(define-compiler-macro white-space-rune-p* (rune)
  `((lambda (rune)
      (or (eq rune 9)
          (eq rune 10)
          (eq rune 12)
          (eq rune 13)
          (eq rune 32)))
    ,rune))

(define-compiler-macro white-space-rune-p*/no-nl (rune)
  `((lambda (rune)
      (or (eq rune 9)
          (eq rune 12)
          (eq rune 13)
          (eq rune 32)))
    ,rune))

(defun maybe-resolve-percentage (x total)
  (cond ((css:percentage-p x)   (round (* (cdr x) total) 100))
        ((integerp x)           x)
        ((realp x)              (round x))
        (t                      x)))








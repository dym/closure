;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: R2; -*-
;;; --------------------------------------------------------------------------------------
;;;     Title: Auxiliary render functions
;;;   Created: 1999-02-24
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: GPL (See file COPYING for details).
;;; --------------------------------------------------------------------------------------
;;;  (c) copyright 1998,1999 by Gilbert Baumann

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

(in-package :R2)

(defun pt-data (x)
  (cond ((text-element-p x)
	 (map 'string #'code-char (element-text x)))
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








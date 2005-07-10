;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLUE-GUI2; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Interface for the renderer to input elements
;;;   Created: 1999-05-25 22:27
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

;; Changes
;;
;;  When        Who     What
;; ----------------------------------------------------------------------------
;;  1999-09-19  GB      - implemented OPTION-LIST
;;
;;  1999-08-21  GB      - RO/MAKE-PASSWORD, RO/MAKE-TEXT, MAKE-OPTION-MENU:
;;                        RO/MAKE-TEXT-AREA, RO/MAKE-BUTTON-COMMON
;;                        RO/MAKE-BUTTON, RO/MAKE-RESET-BUTTON, RO/MAKE-SUBMIT-BUTTON:
;;                        new :text-style argument
;;                      - RO/MAKE-TEXT-AREA, RO/MAKE-BUTTON-COMMON, RO/MAKE-BUTTON, 
;;                        RO/MAKE-RESET-BUTTON, RO/MAKE-SUBMIT-BUTTON:
;;                        new :document argument
;;  1999-08-19  GB      - COLLECT-FORM-VALUES: changed due to :%REPLACEMENT 
;;                        attribute change
;;                      - SUBMIT-FORM: takes extra agrument

(in-package :CLUE-GUI2)

;;; Input elements

;; Input elements as such are replaced objects and obey to the robj
;; protocol. Futher moreinput elements should obey to the ro/input
;; protocol:

;; gui:ro/input-reset self
;;   Reset the input element.

;; gui:ro/input-contribution self
;;   Return a list of conses (name . value), which should be submitted
;;   along the <FORM>.


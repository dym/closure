;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLUE-GUI2; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Interface for the renderer to input elements
;;;   Created: 1999-05-25 22:27
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: GPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1999 by Gilbert Baumann

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

(defparameter user::*debug-submit-p* nil
  "Whether to dump the values about to be submit by a <FORM> to the server on the listener.")

;;; Input elements

;; Input elements as such are replaced objects and obey to the robj
;; protocol. Futher moreinput elements should obey to the ro/input
;; protocol:

;; gui:ro/input-reset self
;;   Reset the input element.

;; gui:ro/input-contribution self
;;   Return a list of conses (name . value), which should be submitted
;;   along the <FORM>.


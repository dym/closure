;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: RENDERER; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: The Renderer's Data Structures
;;;   Created: 1999-01-08
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

;;  When        Who     What
;; ----------------------------------------------------------------------------
;;  1999-08-17  GB      - Added sane accessors
;;                        {inner,outer}-{left,right}-edge
;;                        {inner,outer}-{width,height}
;;                        for BBOXen


(in-package :RENDERER)

;;; ---- Data Structures ----------------------------------------------------------------------

(defstruct (text-style 
            (:constructor make-text-style-prim 
                          (font font-family font-weight font-size
                           font-style font-variant
                           letter-spacing word-spacing)))
  font
  font-family
  font-weight
  font-size
  font-style
  font-variant
  letter-spacing
  word-spacing)

;; Note: This rcontext has to leave -- it is currently used by the CSS
;; module to get to the device.

(defvar *rcontext*)                     

(defstruct rc
  device
  y             ;current y coordinate
  x0            ;x-coordinate of current left margin
  x1            ;x-coordinate of current right margin
  vertical-margins
  vertical-margin-callbacks
  first-line-tasks
  left-floating-boxen ;all left floating boxen [list of FBOX-DESC]
  right-floating-boxen                  ;all right floating boxen
  document
  anchors
  container                             ;Die momentane container-box
  abspos                                ;Liste aller absolut positionionieren Boxen
  ;; caches for min/max width
  (min-width-cache (make-hash-table :test #'eq))
  (max-width-cache (make-hash-table :test #'eq))
  )

(declaim (type rc *rcontext*))


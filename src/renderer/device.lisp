;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: RENDERER; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: A Little Device Abstraction
;;;   Created: 2003-03-08
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: GPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1998-2003 by Gilbert Baumann

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
 
(in-package :RENDERER)

(defgeneric device-dpi (device))
(defgeneric device-font-ascent (device font))
(defgeneric device-font-descent (device font))
(defgeneric device-font-underline-position (device font))
(defgeneric device-font-underline-thickness (device font))
(defgeneric device-font-has-glyph-p (device font code-point))
(defgeneric device-font-glyph-width (device font code-point))
(defgeneric scale-font-desc (device font-desc size))
(defgeneric device-realize-font-desc (device font-desc))
(defgeneric device-font-database (device))


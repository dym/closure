;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-USER; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: All DEFPACKAGE forms for the Closure browser
;;;   Created: 1999-05-25 22:33
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

(defpackage :renderer
  (:nicknames :r2)
  (:use :glisp)
  (:import-from :imagelib 
                #:aimage
                #:aimage-width
                #:aimage-height
                #:aimage-data
                #:aimage-alpha-p
                #:aimage-plist
                #:make-aimage
                #:scale-aimage
                #:pnm-stream->aimage)
  (:export
   #:device-font-ascent
   #:device-dpi
   #:device-font-descent
   #:device-font-underline-position
   #:device-font-underline-thickness
   #:device-font-has-glyph-p
   #:device-font-glyph-width
   #:device-realize-font-desc
   #:device-font-database
   #:scale-font-desc

   #:ro/size
   #:ro/resize
   #:ro/intrinsic-size

   #:x11-draw-robj

   #:style-sheet-link-p 
   #:default-style-sheet-link-p 
   #:alternate-style-sheet-link-p 
   #:style-sheet-name-equal-p 
   
   #:document                           ;[class]
   #:document-anchors                   ;[accessor]
   #:document-images                    ;[accessor]
   #:document-location                  ;[accessor]
   #:document-display-list              ;[accessor]
   #:document-pt                        ;[accessor]
   #:document-links                     ;[accessor]
   #:document-selected-author-style     ;[accessor]

   #:link                               ;[class]
   #:link-title                         ;[accessor]
   #:link-rel                           ;[accessor]
   #:link-rev                           ;[accessor]
   #:link-type                          ;[accessor]
   #:link-media                         ;[accessor]
   #:link-target                        ;[accessor]
   
   #:anchor                             ;[structure]
   #:anchor-name                        ;[accessor]
   #:anchor-x                           ;[accessor]
   #:anchor-y                           ;[accessor]

   #:hyper-link                         ;[structure]
   #:make-hyper-link                    ;[constructor]
   #:hyper-link-url                     ;[accessor]
   #:hyper-link-alt                     ;[accessor]
   #:hyper-link-target                  ;[accessor]
   ))

(defpackage :ws/x11
  (:use :glisp)
  (:export
   #:aimage->ximage))

(defpackage :gif
  (:use :glisp)
  (:export #:gif-stream->aimage))

(defpackage :ws/charset
  ;;(:nicknames :charset)
  ;; Arg! CLISP now defines a package called "charset".
  (:use :glisp)
  (:export #:CHARSET
           #:CHARSET-DECODE
           #:CHARSET-ENCODE
           #:CHARSET-NAME
           #:CHARSET-NICKNAMES
           #:FIND-CHARSET
           #:REGISTER-CHARSET
           #:LIST-ALL-CHARSETS))

;;zzz(defpackage :ws/estk (:use :glisp))     ;should die

(defpackage :gui
  (:use :glisp)

  (:import-from #:WS/GRAPHICS-UTILITIES
   #:+nowhere+
   #:+everywhere+
   #:region-intersects-rectangle-p
   #:region-union
   #:make-rectangle*)

  (:export
   #:display-list
   #:display-list-p
   #:display-list-document
   #:display-list-items
   #:make-display-list
   #:button-event
   #:button-press-event
   #:button-release-event
   #:configure-event
   #:enter-event
   #:event
   #:event-button
   #:event-height
   #:event-key-code
   #:event-key-name
   #:event-state
   #:event-width
   #:event-x
   #:event-x-root
   #:event-y
   #:event-y-root
   #:exposure-event
   #:input-event
   #:key-event
   #:key-press-event
   #:key-release-event
   #:leave-event
   #:translate-event
   #:map-notify-event
   #:motion-event
   #:mouse-event
   #:pointer-motion-event
   #:proxy-device
   #:handle-event

   #:RO/INPUT
   #:RO/INPUT-DESTRUCT
   #:RO/INPUT-CONTRIBUTION
   #:RO/INPUT-RESET

   #:make-device-for-display
   #:ro/make-button
   #:ro/make-reset-button
   #:ro/make-submit-button
   #:ro/make-text
   #:ro/make-password
   #:ro/make-text-area
   #:ro/make-check-box
   #:ro/make-radio-box
   #:ro/input-contribution
   #:ro/input-reset

   #:make-option-menu-option
   #:make-option-menu-option-group
   #:make-option-menu
   #:option-menu-option-p
   #:option-menu-option-label
   #:option-menu-option-group-p
   #:option-menu-option-group-label
   #:option-menu-option-group-children
   ))


(defpackage :gtk-gui (:use :glisp))


'(defpackage "WS/POSTSCRIPT"
  (:nicknames "WS/PS")
  (:use "GLISP")
  (:export )
  (:import-from "R2"
                "DEVICE-FONT-ASCENT"
                "DEVICE-FONT-DESCENT"
                "DEVICE-FONT-HAS-GLYPH-P"
                "DEVICE-FONT-GLYPH-WIDTH"
                "DEVICE-REALIZE-FONT-DESC"
                "DEVICE-FONT-DATABASE"
                "SCALE-FONT-DESC"
                "MAKE-FONT-DESC"
                "MAKE-FONT-DATABASE"
                "FONT-DATABASE-RELATE"
                "COPY-FONT-DESC"
                "FONT-DESC-SIZE"
                "FONT-DESC-DDP"
                ;; xxx "CSS-FONT-DESC-GLYPH-INFO"
                "GLYPH-INFO-FID"
                "GLYPH-INFO-INDEX"
                "GLYPH-INFO-WIDTH" 
                "DEVICE-FONT-UNDERLINE-POSITION" 
                "DEVICE-FONT-UNDERLINE-THICKNESS"
                ))

(defpackage :clue-gui2 (:use :glue :clue :glisp))






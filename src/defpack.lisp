;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-USER; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: All DEFPACKAGE forms for the Closure browser
;;;   Created: 1999-05-25 22:33
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

(defpackage :renderer
  (:nicknames :r2)
  (:use :glisp :runes)
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
   #:*tex-mode-p*
   #:*hyphenate-p*
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
  (:use :glisp :runes)
  (:export
   #:aimage->ximage))

(defpackage :gif
  (:use :glisp :runes)
  (:export #:gif-stream->aimage))

(defpackage :ws/charset
  ;;(:nicknames :charset)
  ;; Arg! CLISP now defines a package called "charset".
  (:use :glisp :runes)
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
  (:use :glisp :runes)

  (:import-from #:clim
   #:+nowhere+
   #:+everywhere+
   ;; #:region-intersects-rectangle-p
   #:region-union
   #:make-rectangle*)

  (:export
   #:*home-page*
   #:*user-wants-images-p*
   #:*closure-dpi*
   #:*zoom-factor*
   #:*debug-submit-p*

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


(defpackage :gtk-gui (:use :glisp :runes))

(defpackage :closure
    (:use)
    (:export
     #:*home-page*
     #:*user-wants-images-p*
     #:visit
     #:start
     #:stop))

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

(defpackage :clue-gui2 (:use #||:glue :clue||# :glisp :runes))

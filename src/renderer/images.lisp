;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: RENDERER; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: General image routines
;;;   Created: 1998-11-11
;;;    Author: Gilbert Baumann <gilbert@base-engineering.com>
;;;   License: MIT style (see below)
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1998 by Gilbert Baumann

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
;;  1999-08-18  GB      - Spend a printer for AIMAGE
;;                      - fixed URL->AIMAGE-REAL, 
;;                         - it now returns the condition
;;                         - obeys to `deliver-broken-image-p'
;;                        

(in-package :RENDERER)

;;; ---------------------------------------------------------------------------

(defvar *recursive-broken-image-p* nil)

(defun broken-aimage (document)
  (if *recursive-broken-image-p*
      (error "Recursive broken image")
    (let ((*recursive-broken-image-p* t))
      (url->aimage document "file://closure/resources/icons/broken.png" t))))

(defun url->aimage-real (document url &optional (deliver-broken-image-p t))
  "Attempts to make an AIMAGE from an URL; In case of error return NIL 
   and the condition if known. If `deliver-broken-image-p' is true, return
   broken.png in case of error instead."
  (unless (url:url-p url)
    (setq url (url:parse-url url)))
  (multiple-value-bind (aimage condition)
      (progn ;ignore-errors
       (netlib:with-open-document ((input mime-type) url
                                                     nil ;reload-p
                                                     t ;binary-p
                                                     t ;cache-p
                                                     nil) ;any-p
         (cond ((makeup-image mime-type input))
               (t
                (warn "Image mime type '~A' not understood. -- trying with '~A'." 
                      (netlib::mime-type-name mime-type)
                      ;;XXX
                      (netlib::mime-type-name
                       (setq mime-type (netlib::find-mime-type-from-extension
                                        (url:url-extension url)))))
                (let ((mime-type-2
                       (netlib:find-mime-type 
                        (netlib::mime-type-name 
                         (setq mime-type 
                           (netlib::find-mime-type-from-extension
                            (url:url-extension url)))))))
                  (cond ((makeup-image mime-type-2 input))
                        (t
                         (warn "Auch das hat nix genuetzt.")
                         (cond (deliver-broken-image-p
                                (format *debug-io* "~%;; ~A -> using broken image." url)
                                (broken-aimage document))
                               (t
                                (error "Image mime type `~A' or `~A' not understood."
                                       (netlib::mime-type-name mime-type)
                                       (netlib::mime-type-name mime-type-2) ))))))))))
    (cond ((null aimage)
           (cond (deliver-broken-image-p
                  (progn
                    (format *debug-io* "~%;; ~A -> using broken image. [zweite variante]" url)
                    (broken-aimage document) ))
                 (t
                  (format *debug-io* "~&;; Was unable to read ~S as image, because of:~%;; | ~A."
                          url condition)
                  (values nil condition))))
          (t
           (values aimage nil)))))

(defun url->aimage (document url &optional (deliver-broken-image-p t))
  (let ((res (clue-gui2::aimage-from-url document url)))
    (cond ((eq res :error)
           (if deliver-broken-image-p
               (broken-aimage document)
             nil))
          (t
           res))))

(defun makeup-image (mime-type input)
  (cond
   ((eq mime-type (netlib:find-mime-type "image/png"))
    (png:png-stream->aimage input))
   ((eq mime-type (netlib:find-mime-type "image/gif"))
    (imagelib:gif-stream->aimage input))
   ((eq mime-type (netlib:find-mime-type "image/jpeg"))
    (imagelib:jpeg-stream->aimage input))
   ;; The rest simply goes to the appropriate ->ppm filters.
   ((eq mime-type (netlib:find-mime-type "image/x-xbitmap"))
    (imagelib:any->aimage-by-filter "xbmtopbm" input))
   ((eq mime-type (netlib:find-mime-type "image/x-xpixmap"))
    (imagelib:any->aimage-by-filter "xpmtoppm" input))
   ((eq mime-type (netlib:find-mime-type "image/tiff"))
    (imagelib:any->aimage-by-filter "tifftopnm" input))))


;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: RENDERER; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: General image routines
;;;   Created: 1998-11-11
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: GPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1998 by Gilbert Baumann

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
      (ignore-errors
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
                                (format T "~%;; ~A -> using broken image." url)
                                (broken-aimage document))
                               (t
                                (error "Image mime type `~A' or `~A' not understood."
                                       (netlib::mime-type-name mime-type)
                                       (netlib::mime-type-name mime-type-2) ))))))))))
    (cond ((null aimage)
           (cond (deliver-broken-image-p
                  (progn
                    (format T "~%;; ~A -> using broken image. [zweite variante]" url)
                    (broken-aimage document) ))
                 (t
                  (format T "~&;; Was unable to read ~S as image, because of:~%;; | ~A."
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
    (let ((*print-array* nil))
      (gif-stream->aimage input)))

   ;; The rest simply goes to the appropriate ->ppm filters.
   ((eq mime-type (netlib:find-mime-type "image/jpeg"))
    (any->aimage-by-filter "djpeg" input))
   ((eq mime-type (netlib:find-mime-type "image/x-xbitmap"))
    (any->aimage-by-filter "xbmtopbm" input))
   ((eq mime-type (netlib:find-mime-type "image/x-xpixmap"))
    (any->aimage-by-filter "xpmtoppm" input))
   ((eq mime-type (netlib:find-mime-type "image/tiff"))
    (any->aimage-by-filter "tifftopnm" input))))

(defun gif-stream->aimage (input)
  (with-temporary-file (temp-filename)
    (let ((png-filename (merge-pathnames (make-pathname :type "png")
                                         temp-filename)))
      (with-open-file (sink temp-filename
                       :direction :output
                       :if-exists :overwrite
                       :element-type '(unsigned-byte 8))
        (let ((sink (make-instance 'glisp:cl-byte-stream :cl-stream sink)))
          (let ((tmp (make-array 4096 :element-type '(unsigned-byte 8))))
            (do ((n (g/read-byte-sequence tmp input)
		    (g/read-byte-sequence tmp input)))
                ((= n 0))
              (g/write-byte-sequence tmp sink :end n)))))
      (unwind-protect
          (progn
            (run-unix-shell-command
             (format nil "gif2png -r ~A >/dev/null 2>/dev/null"
                     (namestring (truename temp-filename))))
            (with-open-file (input png-filename
                             :direction :input
                             :element-type '(unsigned-byte 8))
              (let ((i (make-instance 'cl-byte-stream :cl-stream input)))
                (png:png-stream->aimage i))))
        (ignore-errors
         (mapc #'(lambda (x) (ignore-errors (delete-file x)))
               (directory (merge-pathnames (make-pathname :type :wild)
                                           temp-filename)))) ))))

#+NIL
(defun gif-stream->aimage (input)
  (imagelib.gif::read-gif-image input))

(defun any->aimage-by-filter (filter-name input)
  (with-temporary-file (temp-filename)
    (with-temporary-file (pnm-filename)
      (with-open-file (sink temp-filename
                       :direction :output
                       :if-exists :overwrite
                       :element-type '(unsigned-byte 8))
        (let ((sink (make-instance 'glisp:cl-byte-stream :cl-stream sink)))
          (let ((tmp (make-array 4096 :element-type '(unsigned-byte 8))))
            (do ((n (g/read-byte-sequence tmp input)
		    (g/read-byte-sequence tmp input)))
                ((= n 0))
              (g/write-byte-sequence tmp sink :end n)))))
      (let ((cmd (format nil "~A <~A >~A" filter-name
                         (namestring (truename temp-filename))
                         (namestring pnm-filename))))
        (format T "~%;; running: ~A" cmd)
        (run-unix-shell-command cmd))
      (progn                            ;ignore-errors
        (with-open-file (input pnm-filename
                         :direction :input
                         :element-type '(unsigned-byte 8))
          (pnm-stream->aimage
           (make-instance 'cl-byte-stream :cl-stream input)))) )))

;;; Image writers

(defun write-ppm-image (aimage sink)
  ;; We write P3/P6 images
  (let ((binary-p (subtypep (stream-element-type sink) '(unsigned-byte 8))))
    (let ((header
           (with-output-to-string (bag)
             (format bag "~A~%" (if binary-p "P6" "P3"))
             (format bag "~D ~D ~D" (aimage-width aimage) (aimage-height aimage) 255))))
      (if binary-p
          (write-sequence (map '(array (unsigned-byte 8) (*)) #'char-code header) sink)
        (write-string header sink))
      (cond (binary-p
             (write-byte 10 sink)
             (let ((buffer (make-array (* 3 (aimage-width aimage)) :element-type '(unsigned-byte 8)))
                   (width (aimage-width aimage))
                   (data (aimage-data aimage))
                   (i 0))
               (declare (type (simple-array (unsigned-byte 8) (*)) buffer)
                        (type (array (unsigned-byte 32) (* *)) data)
                        (type fixnum width)
                        (type fixnum i))
               (dotimes (y (aimage-height aimage))
                 (setf i 0)
                 (do ((x 0 (the fixnum (+ x 1))))
                     ((= x width))
                   (declare (type fixnum x))
                   (let ((byte (aref data y x)))
                     (declare (type (unsigned-byte 8) byte))
                     (setf (aref buffer i) (ldb (byte 8 0) byte))
                     (setf i (the fixnum (+ i 1)))
                     (setf (aref buffer i) (ldb (byte 8 8) byte))
                     (setf i (the fixnum (+ i 1)))
                     (setf (aref buffer i) (ldb (byte 8 16) byte))
                     (setf i (the fixnum (+ i 1)))))
                 (write-sequence buffer sink))))
            (t
             (dotimes (y (aimage-height aimage))
               (dotimes (x (aimage-width aimage))
                 (when (= (mod x 4) 0)
                   (terpri sink))
                 (let ((byte (aref (aimage-data aimage) y x)))
                   (format sink " ~D ~D ~D"
                           (ldb (byte 8 0) byte)
                           (ldb (byte 8 8) byte)
                           (ldb (byte 8 16) byte)) )))
             (terpri sink))))))

(defun blu (aimage)
  (with-open-file (sink "/tmp/a.ppm" 
                   :direction :output
                   :if-exists :new-version
                   :element-type '(unsigned-byte 8))
    (write-ppm-image aimage sink)))
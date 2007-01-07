;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-
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

(in-package :imagelib)

;;; AIMAGE has been moved into McCLIM under the name RGB-IMAGE, but
;;; without a plist and with different slot accessors.  Here's a wrapper
;;; class for now:
(defclass aimage ()
    ((rgb-image :initarg :rgb-image :accessor aimage-rgb-image)
     (plist :initarg :plist :accessor aimage-plist)))

(defun aimage-width (ai) (climi::image-width (aimage-rgb-image ai)))
(defun aimage-height (ai) (climi::image-height (aimage-rgb-image ai)))
(defun aimage-data (ai) (climi::image-data (aimage-rgb-image ai)))
(defun aimage-alpha-p (ai) (climi::image-alpha-p (aimage-rgb-image ai)))

(defun make-aimage/low (&key width height data alphap plist)
  (make-instance 'aimage
    :rgb-image (make-instance 'climi::rgb-image
		 :width width
		 :height height
		 :data data
		 :alphap alphap)
    :plist plist))

(defmethod print-object ((self aimage) sink)
  (format sink "<~S ~D x ~D from ~S>" 'aimage 
          (aimage-width self) (aimage-height self)
          (getf (aimage-plist self) :url)))

(defun make-aimage (width height &key (alpha-p nil))
  (make-aimage/low :width width
                   :height height
                   :data (make-array (list height width) 
                                     :element-type '(unsigned-byte 32))
                   :alphap alpha-p))

(defun scale-aimage (source new-width new-height)
  (when (or (zerop new-width) (zerop new-height))
    (warn "You can't scale an image to zero width or height"))
  (setf new-height (max 1 new-height))
  (setf new-width (max 1 new-width))
  (cond ((and (= new-width (aimage-width source))
              (= new-height (aimage-height source)))
         source)
        (t
         (let ((res (make-aimage new-width new-height
                                 :alpha-p (aimage-alpha-p source)))
               (w   (aimage-width source))
               (h   (aimage-height source)))
           (dotimes (x new-width)
             (dotimes (y new-height)
               (let ((x2 (floor (* x w) new-width))
                     (y2 (floor (* y h) new-height)))
                 (setf (aref (aimage-data res) y x)
                   (aref (aimage-data source) y2 x2)))))
           res)) ))

;;;; --------------------------------------------------------------------------

(deftype octet () '(unsigned-byte 8))

(defconstant c/raute #o43)

(defconstant c/lf #o12)
(defconstant c/cr #o15)

(defconstant c/0 #o60)
(defconstant c/9 #o71)

(defconstant c/P #o120)

(defun full-read-byte-sequence (sequence input
                                &key (start 0) (end (length sequence)))
  (unless (<= end start)
    (do ((i 0 n)
         (n (g/read-byte-sequence sequence input :start 0)
            (g/read-byte-sequence sequence input :start n)))
        ((or (= i n)
             (>= n end))
         (when (= i n)
           (error "EOF during ~S." 'full-read-byte-sequence))))))

;; BUG in P4,5,6 nach maxval genau ein _white-space_

(defun read-pnm-file (input)
  (let ((lookahead 0))
    (labels
        ((consume ()
           (setf lookahead (g/read-byte input nil :eof)))
         
         (white-p ()
           (or (eql lookahead 8)
               (eql lookahead 10)
               (eql lookahead 13)
               (eql lookahead 32)))
         
         (wsp ()
           (cond ((white-p)
                  (consume)
                  (do () ((not (white-p)))
                    (consume))
                  (when (eql lookahead c/raute)
                    (cmt)))
                 ((eql lookahead c/raute)
                  (cmt))
                 (t
                  (error "[~D] White space expected. -- got `~A'" 
                         (file-position input) 
                         (code-char lookahead)))) )
         
         (cmt ()
           (consume)
           (do () ((or (eql lookahead c/lf) 
                       (eql lookahead c/cr)))
             (consume))
           (wsp))
         
         (int ()
           (let ((r 0))
             (cond ((<= c/0 lookahead c/9)
                    (do () ((not (<= c/0 lookahead c/9)))
                      (setf r (+ (* r 10) (- lookahead c/0)))
                      (consume))
                    r)
                   (t
                    (error "Integer expected.")) )))
         
         (dimensions ()
           (values (progn (wsp) (int))
                   (progn (wsp) (int))
                   (progn 
                     (wsp) 
                     (let ((maxval (int)))
                       (cond ((zerop maxval)
                              (warn "Bogus maxval ~D, resetting to 255." maxval)
                              (setf maxval 55)))
                       maxval))))

         (p1 ()
           (error "Sorry, P1 pnm format not understood."))
         (p4 ()
           (error "Sorry, P4 pnm format not understood."))
         
         (p2 ()
           (multiple-value-bind (width height maxval) (dimensions)
             (let (res dis)
               (setf res (make-array (list height width)
                                     :element-type '(unsigned-byte 32)))
               (setf dis (make-array (* width height)
                                     :displaced-to res
                                     :element-type '(unsigned-byte 32)))
               (dotimes (i (* width height))
                 (setf (aref dis i) 
                   (progn
                     (wsp)
                     (let ((v (int)))
                       (setq v (ldb (byte 8 0) (floor (* v 255) maxval)))
                       (logior v (ash v 8) (ash v 16))))))
               res)))
         
         (p3 ()
           (multiple-value-bind (width height maxval) (dimensions)
             (let (res dis)
               (setf res (make-array (list height width)
                                     :element-type '(unsigned-byte 32)))
               (setf dis (make-array (* width height)
                                     :displaced-to res
                                     :element-type '(unsigned-byte 32)))
               (dotimes (i (* width height))
                 (setf (aref dis i) 
                   (progn
                     (let ((r (progn (wsp) (int)))
                           (g (progn (wsp) (int)))
                           (b (progn (wsp) (int))))
                       (setq r (ldb (byte 8 0) (floor (* r 255) maxval)))
                       (setq g (ldb (byte 8 0) (floor (* g 255) maxval)))
                       (setq b (ldb (byte 8 0) (floor (* b 255) maxval)))
                       (logior r (ash g 8) (ash b 16))))))
               res)))
         
         (p5 ()
           (multiple-value-bind (width height maxval) (dimensions)
             (let (res dis)
               (setf res (make-array (list height width)
                                     :element-type '(unsigned-byte 32)))
               (setf dis (make-array (* width height)
                                     :displaced-to res
                                     :element-type '(unsigned-byte 32)))
               (unless (or (eql lookahead c/cr)
                           (eql lookahead c/lf))
                 (error "Expected exactly one Linefeed."))
               (let ((buffer (make-array 4096 :element-type 'octet))
                     (m (* width height))
                     (i 0)
                     (n 0))
                 (loop
                   (cond ((= i m) (return)))
                   (setf n (g/read-byte-sequence buffer input
                                                 :end (min (length buffer)
                                                           (- m i))))
                   (cond ((= n 0) 
                          (error "Unexpected EOF.")))
                   (cond ((= maxval 255)
                          (dotimes (j n)
                            (setf (aref dis i) 
                              (let ((v (aref buffer j)))
                                (logior v (ash v 8) (ash v 16))))
                            (incf i)))
                         (t
                          (dotimes (j n)
                            (setf (aref dis i) 
                              (let ((v (aref buffer j)))
                                (setq v (ldb (byte 8 0) (floor (* v 255) maxval)))
                                (logior v (ash v 8) (ash v 16))))
                            (incf i)))))
                 res))))
         
         (p6 ()
           (multiple-value-bind (width height maxval) (dimensions)
             (let (res dis)
               (setf res (make-array (list height width)
                                     :element-type '(unsigned-byte 32)))
               (setf dis (make-array (* width height)
                                     :displaced-to res
                                     :element-type '(unsigned-byte 32)))
               (unless (or (eql lookahead c/cr)
                           (eql lookahead c/lf))
                 (error "Expected exactly one Linefeed."))
               (let ((buffer (make-array (* 3 1024) :element-type 'octet))
                     (m (* width height))
                     (i 0)
                     (n 0))
                 (loop
                   (cond ((= i m) (return)))
                   (setq n (min (/ (length buffer) 3) (- m i)))
                   (full-read-byte-sequence buffer input :end (* 3 n))
                   (dotimes (j n)
                     (setf (aref dis i) 
                       (let ((r (aref buffer (* j 3)))
                             (g (aref buffer (+ 1 (* j 3))))
                             (b (aref buffer (+ 2 (* j 3)))))
                         (setq r (ldb (byte 8 0) (floor (* r 255) maxval)))
                         (setq g (ldb (byte 8 0) (floor (* g 255) maxval)))
                         (setq b (ldb (byte 8 0) (floor (* b 255) maxval)))
                         (logior r (ash g 8) (ash b 16))))
                     (incf i))))
               res)))
         
         (pnm ()
           (unless (eql (consume) c/P)
             (error "pnm magic number expected."))
           (consume)
           (cond ((eql lookahead (+ c/0 1)) (consume) (p1))
                 ((eql lookahead (+ c/0 2)) (consume) (p2))
                 ((eql lookahead (+ c/0 3)) (consume) (p3))
                 ((eql lookahead (+ c/0 4)) (consume) (p4))
                 ((eql lookahead (+ c/0 5)) (consume) (p5))
                 ((eql lookahead (+ c/0 6)) (consume) (p6))

                 (t
                  (error "pnm magic number expected. -- '~A'"
                         (code-char c/0)))) ) )
      (pnm) )))

(defun pnm-stream->aimage (input)
  (let ((arr (read-pnm-file input)))
    (make-aimage/low :width (array-dimension arr 1)
                     :height (array-dimension arr 0)
                     :data arr
                     :alphap nil)))


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
        (format *debug-io* "~%;; running: ~A" cmd)
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

;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PNG; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Reading .png Files
;;;   Created: 1997-04-24
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: GPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1997-1998,2001 by Gilbert Baumann

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

(in-package :PNG)

;;; When        Who     What
;;; --------------------------------------------------------------------------------
;;; 2001-12-30  gb      implemented tRNS chunk for color model 0 and 2
;;;

;;;; TODO

;; we want to go with more speed still.

;;; Image formats to support:
;;;
;;;   Color    Allowed    Interpretation
;;;   Type    Bit Depths
;;;
;;;   0       1,2,4,8,16  Each pixel is a grayscale sample.
;;;   2       8,16        Each pixel is an R,G,B triple.
;;;   3       1,2,4,8     Each pixel is a palette index; a PLTE chunk must appear.
;;;   4       8,16        Each pixel is a grayscale sample, followed by an alpha sample.
;;;   6       8,16        Each pixel is an R,G,B triple, followed by an alpha sample.
;;;

(defconstant *png-magic* '#(137 80 78 71 13 10 26 10)
  "The first eight bytes of a png file.")

(defstruct png-image 
  ihdr
  idat
  plte
  transparent-color     ;either nil or
                        ; color model 0: a gray scale sample
                        ; color model 2: #(r g b)
  )

(defstruct ihdr
  width height bit-depth color-type compression-method filter-method interlace-method)

;; CODE DUPLICATION ALERT! killed+yanked from images.lisp
(defun full-read-byte-sequence (sequence input &key (start 0) (end (length sequence)))
  (unless (<= end start)
    (do ((i 0 n)
         (n (g/read-byte-sequence sequence input :start 0)
            (g/read-byte-sequence sequence input :start n)))
        ((or (= i n)
             (>= n end))
         (when (= i n)
           (error "EOF during ~S." 'full-read-byte-sequence))))))

(defun read-unsigned-byte-32 (source &optional (eof-error-p t) eof-value)
  "Reads unsigned byte 32 from an byte stream in network order"
  (let (b1 b2 b3 b4)
    (if (and (setq b1 (g/read-byte source eof-error-p nil))
             (setq b2 (g/read-byte source eof-error-p nil))
             (setq b3 (g/read-byte source eof-error-p nil))
             (setq b4 (g/read-byte source eof-error-p nil)))
        (logior (ash b1 24) (ash b2 16) (ash b3 8) (ash b4 0))
      eof-value)))

(defun read-unsigned-byte-16 (source &optional (eof-error-p t) eof-value)
  "Reads unsigned byte 16 from an byte stream in network order"
  (let (b1 b2)
    (if (and (setq b1 (g/read-byte source eof-error-p nil))
             (setq b2 (g/read-byte source eof-error-p nil)) )
        (logior (ash b1 8) (ash b2 0))
        eof-value)))

(defun read-png-signature-p (source)
  "Checks for PNG signature."
  ;; Returns non-NIL if the first eight bytes read from 'source' is the valid PNG header, NIL otherwise.
  ;; If eof occurs while reading from source NIL is returned
  (dotimes (i (length *png-magic*) t)
    (when (not (eql (g/read-byte source nil 256) (aref *png-magic* i)))
      (return nil))))

(defun read-fixed-string (source n)
 "Read exactly 'n' bytes from 'source' and makeup an iso-latin-1 string."
  ;;Bug: iso-latin-1 encoding of chars expected
  (let ((temp (make-array n :element-type '(unsigned-byte 8))))
    (full-read-byte-sequence temp source)
    (map 'string #'code-char temp) ))

(defun read-chunk (source)
 "Read a PNG chunk from 'source' and return the chunk type, a four
  character string, and a vector containing the data bytes. The CRC is
  not included into the data bytes.
  If eof occurs return NIL."
  ;;TODO: check for CRC errors
  (let ((length (read-unsigned-byte-32 source nil nil)))
    (cond (length
           (let* ((type (read-fixed-string source 4))
                 (data (make-array length :element-type '(unsigned-byte 8) :initial-element 0)))
             (full-read-byte-sequence data source)
             (read-unsigned-byte-32 source);the crc
             (values type data) ))
          (t
           nil) )))

(defun decode-unsigned-byte-32 (data offset) 
 "Decode an (unsigned-byte 32) from the byte vector 'data' starting at 'offset' in
  network order."
  (declare (type (vector (unsigned-byte 8)) data)
           (type fixnum offset))
  (logior (ash (aref data (+ offset 0)) 24)
          (ash (aref data (+ offset 1)) 16)
          (ash (aref data (+ offset 2))  8)
          (ash (aref data (+ offset 3))  0)))

(defun decode-unsigned-byte-16 (data offset) 
 "Decode an (unsigned-byte 16) from the byte vector 'data' starting at 'offset' in
  network order."
  (declare (type (vector (unsigned-byte 8)) data)
           (type fixnum offset))
  (logior (ash (aref data (+ offset 0))  8)
          (ash (aref data (+ offset 1))  0)))

(defun decode-ihdr (data)
 "Decode an IHDR chunk from data."
  (declare (type (vector (unsigned-byte 8)) data))
  (make-ihdr :width              (decode-unsigned-byte-32 data 0)
             :height             (decode-unsigned-byte-32 data 4)
             :bit-depth          (aref data 8)
             :color-type         (aref data 9)
             :compression-method (aref data 10)
             :filter-method      (aref data 11)
             :interlace-method   (aref data 12)))

(defun decode-plte (data)
 "Decode a PLTE chunk from the byte vector 'data'."
  (declare (type (vector (unsigned-byte 8)) data))
  (assert (zerop (mod (length data) 3)))
  (let* ((len (floor (length data) 3))
         (palette (make-array len)))
    (loop
        for i from 0 to (1- len)
        do
          (setf (aref palette i) 
            (vector (aref data (+ (* i 3) 0))
                    (aref data (+ (* i 3) 1))
                    (aref data (+ (* i 3) 2))
                    255)) )
    palette))

(defun decode-trns (palette data)
  (when palette
    (dotimes (i (length data))
      (setf (svref (aref palette i) 3) (aref data i))))
  (let ((*print-array* t))
    (print palette)))

(defun read-png-image (input)
  (unless (read-png-signature-p input)
    (error "~A is probably no PNG file." input))
  (let ((idat '#())
        (plte nil)
        (ihdr nil)
        (transparent-color nil))
    (do ((x (multiple-value-list (read-chunk input))
            (multiple-value-list (read-chunk input))))
        ((or (null (car x)) (string= (car x) "IEND"))
         (cond ((null (car x))
                (error "png file lacks an IEND chunk"))))
      (let ((data (cadr x))
            (type (car x)))
        (let ((*print-array* nil))
          (cond ((string= type "IHDR")
                 (setq ihdr (decode-ihdr data)) )
                ((string= type "PLTE")
                 (setq plte (decode-plte data)) )
                ;;
                ((string= type "tRNS")
                 (cond ((null ihdr)
                        (warn "tRNS chunk without IHDR ignored"))
                       ((case (ihdr-color-type ihdr)
                          (3
                           (if plte
                               (decode-trns plte data)
                               (warn "tRNS chunk without PLTE.")))
                          (2
                           (setf transparent-color
                                 (vector (decode-unsigned-byte-16 data 0)
                                         (decode-unsigned-byte-16 data 2)
                                         (decode-unsigned-byte-16 data 4))))
                          (0
                           (setf transparent-color (decode-unsigned-byte-16 data 0)))
                          (otherwise
                           (warn "tRNS chunk with color model ~D ignored."
                                 (ihdr-color-type ihdr))) ))))
                ;;
                ((string= type "tEXt")
                 (let ((p (position 0 data)))
                   (format nil "~%;Text: `~A' = `~A'."
                           (map 'string #'code-char (subseq data 0 p))
                           (map 'string #'code-char (subseq data (+ p 1))))))
                ((string= type "zTXt")
                 (let ((p (position 0 data)))
                   (format nil "~%;zText: `~A' = `~A'."
                           (map 'string #'code-char (subseq data 0 p)) 
                           (map 'string #'code-char 
                                (png::rfc1951-uncompress-octects (subseq data (+ p 4))) ))))
                ((string= type "IDAT")
                 (setf idat (concatenate '(simple-array (unsigned-byte 8)) idat data)))
                (t
                 ) ))))
    (make-png-image :plte plte
                    :idat (png::rfc1951-uncompress-octects idat :start 2)
                    :ihdr ihdr
                    :transparent-color transparent-color) ))


(defun png-image-row-length (im)
  (let ((width (ihdr-width (png-image-ihdr im)))
        (bit-depth (ihdr-bit-depth (png-image-ihdr im)))
        (color-type (ihdr-color-type (png-image-ihdr im))))
    (+ 1 
       (ceiling
        (* width (ecase color-type
                   (0 bit-depth)
                   (2 (* 3 bit-depth))
                   (3 bit-depth)
                   (4 (* 2 bit-depth))
                   (6 (* 4 bit-depth))))
        8)) ))

(defun paeth-predictor (a b c)
  (let* ((p  (- (+ a b) c))             ;initial estimate
         (pa (abs (- p a)))             ;distances to a, b, c
         (pb (abs (- p b)))
         (pc (abs (- p c))))
    ;; return nearest of a,b,c,
    ;; breaking ties in order a,b,c.
    (cond ((and (<= pa pb) (<= pa pc)) a)
          ((<= pb pc) b)
          (t c) ) ))

(defun apply-png-filter (filter data j j0 len bpp)
  (dotimes (x len)
    (let ((raw (aref data (+ j x)))
          (above (if j0 (aref data (+ j0 x)) 0))
          (left  (if (>= (- x bpp) 0) (aref data (+ j x (- bpp))) 0))
          (left-above (if (and j0 (>= (- x bpp) 0)) (aref data (+ j0 x (- bpp))) 0)))
      (setf (aref data (+ j x))
        (ecase filter
          (0 raw)
          (1 (logand #xFF (+ raw left)))
          (2 (logand #xFF (+ raw above)))
          (3 (logand #xFF (+ raw (floor (+ left above) 2))))
          (4 (logand #xFF (+ raw (paeth-predictor left above left-above)) )))))))

(defun png-image-bits-per-pixel (im)
  (let ((bit-depth (ihdr-bit-depth (png-image-ihdr im)))
        (color-type (ihdr-color-type (png-image-ihdr im))))
    (ecase color-type
      (0 bit-depth)
      (2 (* 3 bit-depth))
      (3 bit-depth)
      (4 (* 2 bit-depth))
      (6 (* 4 bit-depth)))))

(defun png-image-bytes-per-pixel (im)
  (ceiling (png-image-bits-per-pixel im) 8))

(defun get-sample (data i j bit-depth)
  (ecase bit-depth
    (1 (ldb (byte 1 (- 7 (mod i 8))) (aref data (+ (floor i 8) j))))
    (2 (ldb (byte 2 (* 2 (- 3 (mod i 4)))) (aref data (+ (floor i 4) j))))
    (4 (ldb (byte 4 (* 4 (- 1 (mod i 2)))) (aref data (+ (floor i 2) j))))
    (8 (aref data (+ i j)))
    (16 (logior (ash (aref data (+ (* 2 i) j)) 8)
                (aref data (+ (* 2 i) 1 j)))) ))

(defun get-sample* (data i j bit-depth)
  (ecase bit-depth
    (1 (* 255 (get-sample data i j bit-depth)))
    (2 (* 85 (get-sample data i j bit-depth)))
    (4 (* 17 (get-sample data i j bit-depth)))
    (8 (get-sample data i j bit-depth))
    (16 (ldb (byte 8 8) (get-sample data i j bit-depth))) ))

(defun render-filtered-row (im bit-depth color-type transparent-color data j y x0 dx width pw ph put-pixel)
  (do ((x x0 (+ x dx))
       (i 0 (+ i 1)))
      ((>= x width))
    (ecase color-type
      (0
       (if (and transparent-color (= transparent-color (get-sample data i (+ j 1) bit-depth)))
           (funcall put-pixel x y 0 0 0 0 pw ph)
           (let ((v (get-sample* data i (+ j 1) bit-depth)))
             (funcall put-pixel x y v v v 255 pw ph))))
      (2
       (if (and (not (null transparent-color))
                (let ((rsample (get-sample data (+ 0 (* 3 i)) (+ j 1) bit-depth))
                      (gsample (get-sample data (+ 1 (* 3 i)) (+ j 1) bit-depth))
                      (bsample (get-sample data (+ 2 (* 3 i)) (+ j 1) bit-depth)))
                  (and (eql rsample (aref transparent-color 0))
                       (eql gsample (aref transparent-color 1))
                       (eql bsample (aref transparent-color 2)))))
           (funcall put-pixel x y 0 0 0 0 pw ph)
           (let ((r (get-sample* data (+ 0 (* 3 i)) (+ j 1) bit-depth))
                 (g (get-sample* data (+ 1 (* 3 i)) (+ j 1) bit-depth))
                 (b (get-sample* data (+ 2 (* 3 i)) (+ j 1) bit-depth)))
             (funcall put-pixel x y r g b 255 pw ph))))
      (3
       (let* ((i (get-sample data i (+ j 1) bit-depth))
              (p (aref (png-image-plte im) i)))
         (funcall put-pixel x y (aref p 0) (aref p 1) (aref p 2) (aref p 3) pw ph)))
      (4
       (let ((v (get-sample* data (+ 0 (* i 2)) (+ j 1) bit-depth))
             (a (get-sample* data (+ 1 (* i 2)) (+ j 1) bit-depth)))
         (funcall put-pixel x y v v v a pw ph)))
      (6
       (let ((r (get-sample* data (+ 0 (* 4 i)) (+ j 1) bit-depth))
             (g (get-sample* data (+ 1 (* 4 i)) (+ j 1) bit-depth))
             (b (get-sample* data (+ 2 (* 4 i)) (+ j 1) bit-depth))
             (a (get-sample* data (+ 3 (* 4 i)) (+ j 1) bit-depth)))
         (funcall put-pixel x y r g b a pw ph))) ) ))

(defun render-png-image-to-aimage (im)
  (let* ((bpp (png-image-bytes-per-pixel im))
         (data (png-image-idat im))
         (bit-depth (ihdr-bit-depth (png-image-ihdr im)))
         (width (ihdr-width (png-image-ihdr im)))
         (height (ihdr-height (png-image-ihdr im)))
         (color-type (ihdr-color-type (png-image-ihdr im)))
         (res (make-aimage width height :alpha-p t))
         (res-data (aimage-data res))
         (transparent-color (png-image-transparent-color im)))
    (labels ((put-pixel (x y r g b a pw ph)
               pw ph a
               (setf (aref res-data y x) 
                 (dpb r (byte 8 0)
                      (dpb g (byte 8 8) 
                           (dpb b (byte 8 16)
                                (dpb (- 255 a) (byte 8 24) 0) ))))))
      
      (case (ihdr-interlace-method (png-image-ihdr im))
        (0
         (let ((row-len (png-image-row-length im)))
           (do ((y 0 (+ y 1))
                (j 0 (+ j row-len))
                (j0 nil j))
               ((>= j (length data)))
             (apply-png-filter (aref data j) data (+ j 1) (if j0 (+ j0 1) nil) (1- row-len) bpp)
             (render-filtered-row im bit-depth color-type transparent-color data j y 0 1 width 1 1 #'put-pixel))))
        (1
         (let (j0 (j 0))
           (do ((pass 7 (- pass 1)))
               ((< pass 1))
             (let* ((y0 (aref '#(0 1 0 2 0 4 0 0) pass))
                    (x0 (aref '#(0 0 1 0 2 0 4 0) pass))
                    (dy (aref '#(1 2 2 4 4 8 8 8) pass))
                    (ph (aref '#(1 1 2 2 4 4 8 8) pass))
                    (dx (aref '#(1 1 2 2 4 4 8 8) pass)) 
                    (pw (aref '#(1 1 1 2 2 4 4 8) pass)) )
               (let ((row-len (+ 1 (ceiling (* (png-image-bits-per-pixel im) (ceiling (- width x0) dx))
                                            8))))
                 (setf j0 nil)
                 (when (> row-len 1)
                   (do ((y y0 (+ y dy)))
                       ((>= y height))
                     (apply-png-filter (aref data j) data (+ j 1) (if j0 (+ j0 1) nil) (1- row-len) bpp)
                     (render-filtered-row im bit-depth color-type transparent-color data j y x0 dx width pw ph #'put-pixel)
                     (psetf j (+ j row-len) j0 j))))))
           (assert (= j (length data))) ))
        (t
         (error "Unknown interlace method: ~D." (ihdr-interlace-method (png-image-ihdr im)))) ))
    res))

(defun png-stream->aimage (stream)
  (render-png-image-to-aimage (read-png-image stream)))

#||
;;; Testing

(defvar *display* nil)
(defvar *window* nil)
(defvar *gcontext*)
(defvar *black*)
(defvar *white*)

(defun init-x11 ()
  (setq *display* (CLUE-GUI2::THE-DISPLAY))
  (setq *black* (xlib:screen-white-pixel (car (xlib:display-roots *display*))))
  (setq *white* (xlib:screen-black-pixel (car (xlib:display-roots *display*))))
  (setq *window* (xlib:create-window 
                  :parent (xlib:screen-root (car (xlib:display-roots *display*)))
                  :x 0 :y 0
                  :width 259
                  :height 315
                  :background *white*
                  :backing-store :always
                  :border *black*))
  (setq *gcontext* (xlib:create-gcontext :drawable *window*
					 :foreground *black*
					 :background *white*))
  (xlib:map-window *window*)
  (xlib:display-finish-output *display*))


(defun foob (filename)
  (with-open-file (input filename :direction :input :element-type '(unsigned-byte 8))
    (let ((input (cl-byte-stream->gstream input)))
      (let ((aim (png-stream->aimage input)))
        (let ((xim (ws/x11::aimage->ximage *window* aim)))
          (xlib:clear-area *window*)
          (xlib:put-image *window* *gcontext* xim 
                          :src-x 0 :src-y 0 
                          :x 20 :y 20 
                          :width (xlib:image-width xim) :height (xlib:image-height xim))
          (xlib:display-finish-output *display*))))))

(defun qq ()
  (dolist (k (directory "png-fodder/*.png"))
    (unless (char= (char (pathname-name k) 0) #\0)
      (foob k)
      (sleep 1))))

(defun qq ()
  (dolist (k (directory "~/doc/png-suite/*.png"))
    (handler-case
        (foob k)
      (error (condition)
        (warn "Error occured in `~A': ~A." k condition)))
    (sleep 1)))
||#

;;; CRC

(declaim (type (simple-array (unsigned-byte 32) (256)) *crc-table*))

(defvar *crc-table*
    (let ((res (make-array 256 :element-type '(unsigned-byte 32))))
      (dotimes (n 256)
        (let ((c n))
          (dotimes (k 8)
            (if (logbitp 0 c)
                (setf c (logxor #xEDB88320 (ash c -1)))
              (setf c (ash c -1))))
          (setf (aref res n) c)))
      res)
  "Table of CRCs of all 8-bit messages")

;; Update a running CRC with the bytes buf[0..len-1]--the CRC
;; should be initialized to all 1's, and the transmitted value
;; is the 1's complement of the final running CRC (see the
;; crc() routine below)).

(defun update-crc (crc buf &optional (start 0) (end (length buf)))
  (declare (type fixnum start end)
           (type (unsigned-byte 32) crc)
           (type (simple-array (unsigned-byte 8) (*)) buf)
           #.cl-user:+optimize-very-fast+)
  (let ((table *crc-table*))
    (declare (type (simple-array (unsigned-byte 32) (256)) *crc-table*))
    (setf crc (logxor #xFFFFFFFF crc))
    (do ((i start (+ i 1)))
        ((>= i end)
          (logxor #xFFFFFFFF crc))
      (declare (type fixnum i))
      (setf crc (logxor (aref table (logand #xFF (logxor crc (aref buf i))))
                        (ash crc -8))))))

(defun write-unsigned-byte-32 (byte output)
  (write-byte (ldb (byte 8 24) byte) output)
  (write-byte (ldb (byte 8 16) byte) output)
  (write-byte (ldb (byte 8  8) byte) output)
  (write-byte (ldb (byte 8  0) byte) output))

(defun write-chunk (output tag &rest datas)
  ;; First calculate the total length
  (let ((n (reduce #'+ (mapcar #'length datas)))
        (chunk-type (map '(simple-array (unsigned-byte 8) (4)) #'char-code tag)))
    ;; Write the length
    (write-unsigned-byte-32 n output)
    ;; Write the Chunk type
    (write-sequence chunk-type output)
    ;; Write data and compute crc, while doing so
    (let ((crc 0))
      (setf crc (update-crc crc chunk-type))
      (dolist (data datas)
        (write-sequence data output)
        (setf crc (update-crc crc data)))
      ;; finally write the CRC
      (write-unsigned-byte-32 crc output))))

(defun write-signature (output)
  (write-sequence *png-magic* output))

(defun deposit-unsigned-byte-32 (byte buffer offset)
  (setf (aref buffer (+ offset 0)) (ldb (byte 8 24) byte))
  (setf (aref buffer (+ offset 1)) (ldb (byte 8 16) byte))
  (setf (aref buffer (+ offset 2)) (ldb (byte 8  8) byte))  
  (setf (aref buffer (+ offset 3)) (ldb (byte 8  0) byte)))

(defun write-ihdr (output ihdr)
  (let ((buffer (make-array 13 :element-type '(unsigned-byte 8))))
    (deposit-unsigned-byte-32 (ihdr-width ihdr) buffer 0)
    (deposit-unsigned-byte-32 (ihdr-height ihdr) buffer 4)
    (setf (aref buffer 8) (ihdr-bit-depth ihdr))
    (setf (aref buffer 9) (ihdr-color-type ihdr))
    (setf (aref buffer 10) (ihdr-compression-method ihdr))
    (setf (aref buffer 11) (ihdr-filter-method ihdr))
    (setf (aref buffer 12) (ihdr-interlace-method ihdr))
    (write-chunk output "IHDR" buffer)))

(defun write-idat (output data)
  (write-chunk output "IDAT" data))

(defun write-iend (output)
  (write-chunk output "IEND"))

;;;

;; API
;; ===

;; MAKE-ZLIB-STREAM cont
;; STUFF-ZLIB-STREAM octets &key start end
;; END-ZLIB-STREAM

(defstruct (zlib-stream 
            (:constructor make-zlib-stream/low)
            (:copier nil))
  continuation
  adler
  buffer
  bptr
  inflate-state)

(defun make-zlib-stream (cont)
  (let ((zs
         (make-zlib-stream/low
          :continuation cont
          :adler 1
          :buffer (make-array (* 32 1024) :element-type '(unsigned-byte 8))
          :bptr 0
          :inflate-state nil)))
    (let ((cmf 
           (dpb 8 (byte 4 0)            ;compression method = 8
                (dpb 7 (byte 4 4)       ;compression info = 7 = 32K window size
                     0)))
          (flags
           (dpb 0 (byte 5 0)            ;FCHECK; calculated below
                (dpb 0 (byte 1 5)       ;FDICT (present dictionary) = false
                     (dpb 2 (byte 2 6)  ;FLEVEL (compression level) = 2 = compressor used default algorithm
                          0)))))
      ;; calculate FCHECK
      (setf flags (dpb (mod (- (+ (* 256 cmf) flags)) 31) (byte 5 0) flags))
      ;; emit this header
      (let ((buffer (make-array 2 :element-type '(unsigned-byte 8))))
        (setf (aref buffer 0) cmf
              (aref buffer 1) flags)
        (funcall cont buffer 0 2))
      ;;
      zs)))

(defun stuff-zlib-stream (zlib-stream octets &key (start 0) (end (length octets)))
  ;;
  (do ((i start (+ i 1)))
      ((>= i end))
    (cond ((= (zlib-stream-bptr zlib-stream)
              (length (zlib-stream-buffer zlib-stream)))
           ;; buffer full
           (zlib-stream-flush zlib-stream)) )
    (let ((octet (aref octets i)))
      (setf (aref (zlib-stream-buffer zlib-stream) (zlib-stream-bptr zlib-stream)) octet)
      (incf (zlib-stream-bptr zlib-stream))
      (setf (zlib-stream-adler zlib-stream) (update-adler32-one (zlib-stream-adler zlib-stream) octet))) ))

(defun stuff-zlib-stream (zs octets &key (start 0) (end (length octets)))
  (let* ((buffer (zlib-stream-buffer zs))
         (bptr   (zlib-stream-bptr   zs))
         (bend   (length buffer)))
    (do ((i start (+ i 1)))
        ((>= i end))
      (when (= bptr bend)
        (princ "#")(finish-output)
        (setf (zlib-stream-bptr zs) bptr)
        (zlib-stream-flush zs)
        (setf bptr (zlib-stream-bptr zs)))
      (let ((byte (aref octets i)))
        (setf (aref buffer bptr) byte)
        (incf bptr)
        (setf (zlib-stream-adler zs) (update-adler32-one (zlib-stream-adler zs) byte))))
    (setf (zlib-stream-bptr zs) bptr)))

(defun stuff-zlib-stream (zlib-stream octets &key (start 0) (end (length octets)))
  (declare #.cl-user:+optimize-very-fast+)
  (declare (type fixnum start end)
           (type (simple-array (unsigned-byte 8) (*)) octets))
  (let ((buffer (zlib-stream-buffer zlib-stream))
        (bptr   (zlib-stream-bptr   zlib-stream))
        (bend   (length (zlib-stream-buffer zlib-stream)))
        (s1     (ldb (byte 16 0) (the (unsigned-byte 32) (zlib-stream-adler zlib-stream))))
        (s2     (ldb (byte 16 16) (the (unsigned-byte 32) (zlib-stream-adler zlib-stream)))))
    (declare (type fixnum bptr bend)
             (type fixnum s1 s2)
             (type (simple-array (unsigned-byte 8) (*)) buffer))
    (do ((i start (+ i 1)))
        ((>= i end))
      (declare (type fixnum i))
      (cond ((= bptr bend)
             ;; buffer full
             (setf (zlib-stream-bptr zlib-stream) bptr)
             (zlib-stream-flush zlib-stream)
             (setf bptr (zlib-stream-bptr zlib-stream)) ) )
      (let ((octet (aref octets i)))
        (declare (type (unsigned-byte 8) octet))
        (setf (aref buffer bptr) octet)
        (setf bptr (the fixnum (+ bptr 1)))
        (setf s1 (the fixnum (mod (the fixnum (+ s1 octet)) *adler-base*))
              s2 (the fixnum (mod (the fixnum (+ s1 s2)) *adler-base*)))))
    (setf (zlib-stream-adler zlib-stream) (dpb s2 (byte 16 16) s1))
    (setf (zlib-stream-bptr zlib-stream) bptr)))

(defun end-zlib-stream (zlib-stream)
  (zlib-stream-flush zlib-stream t)
  ;; write ADLER-32 check sum
  (let ((adler (zlib-stream-adler zlib-stream)))
    (let ((buf (make-array 4 :element-type '(unsigned-byte 8))))
      (setf (aref buf 0) (ldb (byte 8 24) adler)
            (aref buf 1) (ldb (byte 8 16) adler)
            (aref buf 2) (ldb (byte 8  8) adler)
            (aref buf 3) (ldb (byte 8  0) adler))
      (funcall (zlib-stream-continuation zlib-stream) buf 0 4))))

(defun zlib-stream-flush (zlib-stream &optional (finalp nil))
  (let ((buf (zlib-stream-buffer zlib-stream))
        (blen (zlib-stream-bptr zlib-stream))
        (cont (zlib-stream-continuation zlib-stream)))
    ;;
    (cond (nil
           (multiple-value-bind (o new-state) (compress (zlib-stream-inflate-state zlib-stream) buf 0 blen finalp)
             (setf (zlib-stream-inflate-state zlib-stream) new-state)
             (funcall cont o 0 (length o))))
          (t
           (let ((head (make-array 5 :element-type '(unsigned-byte 8))))
             (setf (aref head 0)
               (dpb (if finalp 1 0) (byte 1 0) ;BFINAL
                    (dpb 0 (byte 2 1)   ;BTYPE
                         0)))
             (setf (aref head 1) (ldb (byte 8 0) blen)
                   (aref head 2) (ldb (byte 8 8) blen)
                   (aref head 3) (ldb (byte 8 0) (logxor #xFFFF blen))
                   (aref head 4) (ldb (byte 8 8) (logxor #xFFFF blen)))
             (funcall cont head 0 5)
             ;; 
             (funcall cont buf 0 blen))) )
    (setf (zlib-stream-bptr zlib-stream) 0)))

;; largest prime smaller than 65536
(defconstant *adler-base* 65521)

(defun update-adler32-one (adler byte)
  (let ((s1 (ldb (byte 16 0) adler))
        (s2 (ldb (byte 16 16) adler)))
    (setf s1 (mod (+ s1 byte) *adler-base*)
          s2 (mod (+ s1 s2) *adler-base*))
    (dpb s2 (byte 16 16) s1)))

(defun write-png-image (aimage output)
  (let ((data (aimage-data aimage))
        (width (aimage-width aimage))
        (height (aimage-height aimage)))
    (write-signature output)
    (write-ihdr output
                (make-ihdr
                 :width width
                 :height height
                 :bit-depth 8
                 :color-type 2          ;bzw 6 wenn mit alpha
                 :compression-method 0
                 :filter-method 0
                 :interlace-method 0))
    (let ((zs (make-zlib-stream (lambda (buffer start end)
                                  ;; arg!
                                  (write-idat output (subseq buffer start end))))))
      (let ((row-buffer (make-array (+ 1 (* 3 width)) :element-type '(unsigned-byte 8))))
        (dotimes (y height)
          (setf (aref row-buffer 0) 0)  ;no filtering
          (let ((p 1))
            (dotimes (x width)
              (let ((pixel (aref data y x)))
                (setf (aref row-buffer p) (ldb (byte 8 0) pixel)) (incf p)
                (setf (aref row-buffer p) (ldb (byte 8 8) pixel)) (incf p)
                (setf (aref row-buffer p) (ldb (byte 8 16) pixel)) (incf p))))
          (stuff-zlib-stream zs row-buffer)))
      (end-zlib-stream zs))
    (write-iend output)))

(defun bar (im)
  (with-open-file (output "/tmp/test.png"
                   :direction :output
                   :if-exists :new-version
                   :element-type '(unsigned-byte 8))
    (write-png-image im output)))

'(defparameter *q*
    (literal-aimage
     :colors ((#\# 0 0 0)
              (#\  255 255 255))
     :pixels ("                    "
              "                    "
              "                    "
              "                    "
              "                    "
              "             #      "
              "             ##     "
              "   #############    "
              "   ##############   "
              "   #############    "
              "             ##     "
              "             #      "
              "                    "
              "                    "
              "                    "
              "                    "
              "                    "
              "                    "
              "                    "
              "                    ")))

(defmacro literal-aimage (&key colors pixels)
  `(literal-aimage-fun :colors ',colors :pixels ',pixels))

(defun literal-aimage-fun (&key colors pixels)
  (let ((w (length (elt pixels 0)))
        (h (length pixels)))
    (labels ((pixel-value (x)
               (dolist (c colors
                         (error "Undefined color ~S." x))
                 (when (eql x (car c))
                   (return (dpb (second c) (byte 8 0)
                                (dpb (third c) (byte 8 8)
                                     (dpb (fourth c) (byte 8 16)
                                          0))))))))
      (let* ((res (make-aimage w h :alpha-p t))
             (res-data (aimage-data res)))
        (dotimes (y h)
          (let ((row (elt pixels y)))
            (dotimes (x w)
              (let ((pixel (elt row x)))
                (setf (aref res-data y x) (pixel-value pixel))))))
        res))))

;;;;;;;;;


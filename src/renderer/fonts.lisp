;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: RENDERER; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Abstract Font Database Interface 
;;;   Created: 1998-09-11
;;;    Author: Gilbert Baumann <gilbert@base-engineering.com>
;;;   License: MIT style (see below)
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1998-2003 by Gilbert Baumann

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

(in-package :RENDERER)

(defparameter *font-names/serif*      '("times new roman" "times" "symbol"))
(defparameter *font-names/sans-serif* '("verdana" "arial" "helvetica" "lucida" "symbol"))
(defparameter *font-names/monospace*  '("courier" "courier new" "monotype.com"))
(defparameter *font-names/cursive*    '("matura capitals"
                                        "times" "symbol"))
(defparameter *font-names/fantasy*    '("desdemona"
                                        "playbill"
                                        "times" "symbol" ))

(unless (boundp '+null-width-table+)
  (defvar +null-width-table+ (make-array 256 :initial-element -1)))

;;;
;;; CAUTION: There is a subtle difference between 'font-desc' and 'css-font-desc'
;;;

(defstruct (font-desc (:print-function print-font-desc))
  family
  weight		; 100 .. 900
  style			; :normal | :italic
  size			; in pixel (hugh?) ; 0 = any size (scalable font)
  ddp			; device depend object
  charset		; character set the font uses (type WS/CHARSET:CHARSET)
  widthen)

(defstruct font-database
  cache                 ;
  device                ; device
  font-list)

(defparameter *frob-weight* 1)
(defparameter *frob-size* 1)

(defun print-font-desc (object sink depth)
  (declare (ignore depth))
  (format sink "#<~S ~S ~S ~S ~S ...>"
          (type-of object)
          (font-desc-family object)
          (font-desc-weight object)
          (font-desc-size object)
          (font-desc-style object)))

(defun font-desc-init-widthen (device fd)
  (unless (font-desc-widthen fd)
    (format T "~&;; Fetching character width info for ~S ... " fd)
    (finish-output)
    (let ((wds (make-array #x10000 :initial-element -1)))
      (dotimes (rune #x10000)
        (let ((code (ws/charset:charset-encode (font-desc-charset fd) rune)))
          (when code
            (let ((fid (device-realize-font-desc device fd)))
              (when (device-font-has-glyph-p device fid code)
                (setf (aref wds rune)
                      (device-font-glyph-width device fid code)))))))
      (setf (font-desc-widthen fd) wds))
    (princ "done.")
    (finish-output)))


;;;; -----------------------------------------------------------------------------------------
;;;;  The Font Database
;;;;

(defun describe-font-database (db)
  (format T "~%~A~30T~A~40T~A~50T~A~60T~A~80T~A"
	  "Family" "Weight" "Style" "Size" "Encoding" "ddp Name")
  (format T "~%~100,,,'-<~>")
  (dolist (k (font-database-font-list db))
    (let ((family (font-desc-family k))
	  (weight (font-desc-weight k))
	  (style  (font-desc-style k))
	  (size   (font-desc-size k))
	  (ddp    (font-desc-ddp k))
	  (enc    (ws/charset:charset-name (font-desc-charset k))))
      (format T "~%~:(~A~)~30T~(~A~)~40T~(~A~)~50T~(~A~)~60T~(~A~)~80T~A"
	      family weight style (if (zerop size) "*" size) enc ddp) )))

(defun font-database-relate (self desc)
  (setf (font-database-font-list self) (nconc (font-database-font-list self) (list desc))))

;;; ---- Matching -----------------------------------------------------------------------------

(defun font-family-matches-p (device-family css-family)
  (or (string-equal device-family css-family)
      (and (string-equal css-family "serif")
	   (member device-family *font-names/serif* :test #'string-equal))
      (and (string-equal css-family "sans-serif")
	   (member device-family *font-names/sans-serif* :test #'string-equal))
      (and (string-equal css-family "monospace")
	   (member device-family *font-names/monospace* :test #'string-equal))
      (and (string-equal css-family "cursive")
	   (member device-family *font-names/cursive* :test #'string-equal))
      (and (string-equal css-family "fantasy")
	   (member device-family *font-names/fantasy* :test #'string-equal)) ))

(defun font-desc-matches-p (desc family weight style size)
  (and (font-family-matches-p (font-desc-family desc) family)
       (or (eql (font-desc-style desc) style)
	   (and (member (font-desc-style desc) '(:italic :oblique))
		(member style '(:italic :oblique))))
       (let ((weight-penality (abs (- (font-desc-weight desc) weight)))
	     (size-penality (cond ((eql (font-desc-size desc) 0) 0)
				  (t (abs (- (font-desc-size desc) size))))))
	 (+ (* *frob-weight* weight-penality)
	    (* *frob-size* size-penality)) )))

(defun font-database-select (self families weight style size)
  (let ((res nil))
    (dolist (family families)
      (setq res (nconc res (font-database-select-one self family weight style size))))
    (when (null res)
      (setq res (font-database-select-one self "times" weight style size)))
    (when (null res)
      (error "Giving up on font ~S, ~S, ~S, ~S."
             families weight style size))
    (setq res (stable-sort res (lambda (x y) (< (car x) (car y)))))
    (mapcar (lambda (x) (scale-font-desc (font-database-device self) x size))
            (mapcar #'cdr res))))

(defun font-database-select-one (self family weight style size)
  (let ((res nil))
    (dolist (desc (font-database-font-list self))
      (let ((p (font-desc-matches-p desc family weight style size)))
	(when p
	  (push (cons p desc) res))))
    (reverse res) ))

;;;; -------------------------------------------------------------------------------------------
;;;;  CSS Font Descriptor and Unicode Handling
;;;;

(defstruct (css-font-desc (:print-function print-css-font-desc))
  font-desc-list                        ;List of font-desc objects
  fid-table
  index-table
  width-table
  
  device                                ;the device
  fake-small-caps-p                     ;whether to fake small-caps
  fake-small-caps-font                  ;font to use for small-caps
  ascent
  descent
  x-height)

(defun print-css-font-desc (object sink depth)
  (declare (ignore depth))
  (format sink "#<~S ...>"
          (type-of object)))

(defstruct glyph-info
  (fid   nil)                           ;device dependent font object
  (index 0   :type (unsigned-byte 16))
  (width 0   :type fixnum))

(defun find-css-font-desc (database families weight style size variant)
  (let ((cache (font-database-cache database)))
    (or (gethash (list families weight style size variant) cache)
        (setf (gethash (list families weight style size variant) cache)
          (generate-css-font-desc database families weight style size variant)))))

(defsubst css-font-desc-ensure-glyph-info (font-desc glyph)
  (declare (type (unsigned-byte 16) glyph))
  ;;(assert (not (null (css-font-desc-fid-table font-desc))))
  (let ((r (svref (css-font-desc-fid-table font-desc) (ldb (byte 8 8) glyph))))
    (unless (and r (svref r (ldb (byte 8 0) glyph)))
      (css-font-desc-generate-glyph-info font-desc glyph)
      ;;(assert (not (null (svref (svref (css-font-desc-fid-table font-desc) (ldb (byte 8 8) glyph)) (ldb (byte 8 0) glyph)))))
      )))

(defsubst css-font-desc-glyph-width (font-desc glyph)
  (declare (type (unsigned-byte 16) glyph))
  (css-font-desc-ensure-glyph-info font-desc glyph)
  (svref (svref (css-font-desc-width-table font-desc) (ldb (byte 8 8) glyph))
         (ldb (byte 8 0) glyph)))

(defun css-font-desc-glyph-fid (font-desc glyph)
  (declare (type (unsigned-byte 16) glyph))
  (css-font-desc-ensure-glyph-info font-desc glyph)
  (svref (svref (css-font-desc-fid-table font-desc) (ldb (byte 8 8) glyph))
         (ldb (byte 8 0) glyph)))

(defun css-font-desc-glyph-index (font-desc glyph)
  (declare (type (unsigned-byte 16) glyph))
  (css-font-desc-ensure-glyph-info font-desc glyph)
  (aref (svref (css-font-desc-index-table font-desc) (ldb (byte 8 8) glyph))
         (ldb (byte 8 0) glyph)))

(defun css-font-desc-generate-glyph-info (font glyph)
  (cond ((and (css-font-desc-fake-small-caps-p font)
              (fat-char-lower-case-p glyph))
         (real-generate-glyph-info font glyph
                                   (fat-char-upcase glyph) (css-font-desc-fake-small-caps-font font)))
        (t
         (real-generate-glyph-info font glyph)) ))

(defun css-font-desc-find-glyph-info (font-desc glyph)
  ;;xxx
  (when (eql glyph #o240) (setq glyph #o40)) ;subst. #\space \ #\nbsp
  (let ((device (css-font-desc-device font-desc)))
    (dolist (k (css-font-desc-font-desc-list font-desc) ;how desc'ed!
              ;;default:
              (let ((fid (device-realize-font-desc 
                          device
                          (car (css-font-desc-font-desc-list font-desc)))))
                (values
                 fid
                 (char-code #\?)
                 (device-font-glyph-width device fid (char-code #\?)))))
      (let ((index (ws/charset:charset-encode (font-desc-charset k) glyph)))
        (when (not (null index))
          (let ((fid (device-realize-font-desc device k)))
            (when (device-font-has-glyph-p device fid index)
              (return (values fid index  (device-font-glyph-width device fid index))))))) )))

(defun real-generate-glyph-info (font-desc glyph
                                 &optional (src-glyph glyph) (src-fd font-desc))
  (multiple-value-bind (fid index width) (css-font-desc-find-glyph-info src-fd src-glyph)
    (let ((r (svref (css-font-desc-fid-table font-desc) (ldb (byte 8 8) glyph))))
      (unless r
        (setf (svref (css-font-desc-fid-table font-desc) (ldb (byte 8 8) glyph))
              (make-array 256 :initial-element nil)
              (svref (css-font-desc-index-table font-desc) (ldb (byte 8 8) glyph)) 
              (make-array 256 :initial-element 0 :element-type '(unsigned-byte 16)))
        (when (eq (svref (css-font-desc-width-table font-desc) (ldb (byte 8 8) glyph))
                  +null-width-table+)
          (setf (svref (css-font-desc-width-table font-desc) (ldb (byte 8 8) glyph)) 
                (make-array 256 :initial-element -1)))))
    (setf (svref (svref (css-font-desc-fid-table font-desc) (ldb (byte 8 8) glyph))
                 (ldb (byte 8 0) glyph))
      fid)
    (setf (aref (svref (css-font-desc-index-table font-desc) (ldb (byte 8 8) glyph))
                 (ldb (byte 8 0) glyph))
      index)
    (setf (svref (svref (css-font-desc-width-table font-desc) (ldb (byte 8 8) glyph))
                 (ldb (byte 8 0) glyph))
      width)))
              
(defun generate-css-font-desc (font-database families weight style size variant)
  (let ((font-desc-list (font-database-select font-database families weight style size)))
    '(dolist (k font-desc-list)
      (font-desc-init-widthen (font-database-device font-database) k))
    (let ((fd 
           (make-css-font-desc :font-desc-list font-desc-list
                               :device (font-database-device font-database)
                               :fid-table (make-array 256 :initial-element nil)
                               :width-table (make-array 256 :initial-element +null-width-table+)
                               :index-table (make-array 256 :initial-element nil)
                               :fake-small-caps-p (eql variant :small-caps)
                               :fake-small-caps-font
                               (if (eql variant :small-caps)
                                   (progn
                                     (generate-css-font-desc font-database families weight style
                                                             (* 2/3 size) ;xxx
                                                             :normal)) ) )))
      (setf (css-font-desc-ascent fd) (device-font-ascent (css-font-desc-device fd) 
                                                          (css-font-desc-glyph-fid fd 65))
            (css-font-desc-descent fd) (device-font-descent (css-font-desc-device fd) 
                                                            (css-font-desc-glyph-fid fd 65))
            (css-font-desc-x-height fd) (css-font-desc-glyph-width fd (char-code #\x)) ; Wrong!
            )
      fd)))

(defun font-desc-ascent (fd)
  (css-font-desc-ascent fd))

(defun font-desc-descent (fd)
  (css-font-desc-descent fd))

(defun font-desc-x-height (fd)
  (css-font-desc-x-height fd))

(defun font-underline-position (fd)
  ;;xxx
  (device-font-underline-position (css-font-desc-device fd)
                                  (css-font-desc-glyph-fid fd 65)))

(defun font-underline-thickness (fd)
  ;;xxx
  (device-font-underline-thickness (css-font-desc-device fd)
                                   (css-font-desc-glyph-fid fd 65)))

(defun glyph-representable-p (font code)
  (let ((device (css-font-desc-device font)))
    (some (lambda (k)
            (let ((index (ws/charset:charset-encode (font-desc-charset k) code)))
              (and (not (null index))
                   (device-font-has-glyph-p device (device-realize-font-desc device k) index))))
          (css-font-desc-font-desc-list font))))

;;; Needed methods on devices

;; DEVICE-FONT-ASCENT device font
;; DEVICE-FONT-DESCENT device font
;; DEVICE-FONT-HAS-GLYPH-P device font index
;; DEVICE-FONT-GLYPH-WIDTH device font index
;; DEVICE-REALIZE-FONT-DESC device font-desc
;; DEVICE-FONT-DATABASE device
;; SCALE-FONT-DESC device fd size
;; DEVICE-FONT-UNDERLINE-POSITION device font
;; DEVICE-FONT-UNDERLINE-THICKNESS device font


(defun fat-char-lower-case-p (char)
  (<= (char-code #\a) char (char-code #\z)))

(defun fat-char-upcase (char)
  (cond ((<= (char-code #\a) char (char-code #\z))
         (- char 32))
        (t
         char)))




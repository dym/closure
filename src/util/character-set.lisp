;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: ws/Charset; -*-
;;; --------------------------------------------------------------------------------------
;;;     Title: Character Sets (considered harmful)
;;;   Created: 1998-05-31
;;;    Author: Gilbert Baumann <gilbert@base-engineering.com>
;;;   License: MIT style (see below)
;;; --------------------------------------------------------------------------------------
;;;  (c) copyright 1998,1999 by Gilbert Baumann

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

(in-package :ws/Charset)

#|

ws/character-set						       [Package]

    Nicknames:	ws/charset, ws/cs.
    Exported:	decode, encode, charset-name, charset-nicknames, find-charset, 
		register-charset.


ws/charset:decode self code &optional (default #xFFFF)			[Method]

    Decode the native character code `code' into an unicode glyph index.
    `self' is the character set to use. If the code given is unassigned in the
    character set `self' return `default'.

      The default value for `default' is #xFFFF, which is the unicode glyph
    "NOT A CHARACTER".


ws/charset:encode self code &optional (default nil)			[Method]

    Given the character set `self' attempt to encode the unicode glyph index
    `code'. When the character set `self' has no equivalent for the given
    glyph `default' is returned.


ws/charset:charset-name self						[Method]
ws/charset:charset-nicknames self					[Method]

    The method `charset-name' returns a character set's primary name. There
    could additionally been defined any number of nicknames, which are
    returned by `charset-nicknames'. All these names are either a string or a
    symbol. It is assumed that these names are compared by the `string-equal'
    function.


ws/charset:find-charset name					      [Function]

    The charset package maintains a repertoire of all character sets
    registered by the 'register-charset' function.

ws/charset:register-charset charset				      [Function]

    Registers a charset.

ws/charset:list-all-charsets					      [Function]

    Returns a list of all character sets.


How to define new character sets
--------------------------------

  Simply define a new subclass of the class `ws/charset:charset' and provide for
the methods `encode' and `decode'.

Signatures
----------

	decode: charset x (unsigned-byte 16) [ x S ] -> (or (unsigned-byte 16) S)
	encode: charset x (unsigned-byte 16) [ x S ] -> (or (unsigned-byte 16) S)
	charset-name: charset -> stringable
	charset-nicknames: charset -> (list-of stringable)
	find-charset : stringable -> (or charset null)
	register-charset : charset ->
|#

;;; Package: ws/character-set nn: ws/charset, ws/cs
;;; ws/charset:find-charset name -> charset | NIL
;;; ws/charset:list-all-charsets -> list-of (charset)
;;; ws/charset:register-charset charset ->
;;;

;;;; ==============================================================================================
;;;;  Character Sets (considered harmful)
;;;; ----------------------------------------------------------------------------------------------
;;;;

(defclass charset ()
  ((name      :initarg :name      :initform nil :reader charset-name)
   (nicknames :initarg :nicknames :initform nil :reader charset-nicknames)) )

;; (charset-encode self unicode) -> native-code
;; (charset-decode self native)  -> unicode
;; (charset-relate self unicode native)

;;; ---- 16-bit-identity-mapping --------------------

(defclass 16-bit-identity-mapping (charset) ())

(defmethod charset-encode ((self 16-bit-identity-mapping) glyph &optional default)
  (declare (ignore default))
  glyph)

(defmethod charset-decode ((self 16-bit-identity-mapping) code &optional default)
  (declare (ignore default))
  code)

;;; ---- 8-bit-simple-charset --------------------

(defclass 8-bit-simple-charset (charset)
  ((decode-vector 
    :initform (make-array 256 :element-type '(unsigned-byte 16) :initial-element #xFFFF)
    :reader charset-decode-vector)
   (encode-table
    :initform (make-hash-table :test #'eql)
    :reader charset-encode-table)))

(defmethod charset-decode ((self 8-bit-simple-charset) code &optional (default #xFFFF))
  (let ((r (aref (charset-decode-vector self) code)))
    (if (eql r #xFFFF) default r)))

(defmethod charset-encode ((self 8-bit-simple-charset) glyph &optional (default nil))
  (gethash glyph (charset-encode-table self) default))

(defmethod charset-relate ((self 8-bit-simple-charset) glyph code)
  (setf (aref (charset-decode-vector self) code) glyph)
  (setf (gethash glyph (charset-encode-table self)) code))


;;; ---- lazy-charset --------------------

(defclass lazy-charset (charset)
  ((encoding-table-url :initarg :encoding-table-url :reader charset-encoding-table-url)
   (initialized-p :initform nil)))

(defmethod ensure-charset-inited ((self lazy-charset))
  (with-slots (initialized-p) self
    (unless initialized-p
      (init-charset-from-encoding-table self (charset-encoding-table-url self))
      (setf initialized-p t))))

(defmethod charset-decode :before ((self lazy-charset) code &optional (default #xFFFF))
  (declare (ignore code default))
  (ensure-charset-inited self))

(defmethod charset-encode :before ((self lazy-charset) glyph &optional (default #xFFFF))
  (declare (ignore glyph default))
  (ensure-charset-inited self))

;;; ---- lazy-8-bit-simple-charset --------------------

(defclass lazy-8-bit-simple-charset (lazy-charset 8-bit-simple-charset) ())

;;(defmethod ensure-charset-inited :after ((self lazy-8-bit-simple-charset))
;;  (change-class self '8-bit-simple-charset))

;;; ------------------------------------------------------------

(defmethod init-charset-from-encoding-table ((self charset) url)
  (netlib:with-open-document ((input mime-type) (maybe-parse-url url))
    ;;(declare (ignore mime-type))
    (do ((line (g/read-line input nil nil) (g/read-line input nil nil)))
	((null line) self)
      (multiple-value-bind (glyph code) (parse-encoding-table-line line)
	(when (and glyph code)
	  (charset-relate self glyph code)))) ))

;;;; ==== Registry of charsets ====================================================================

(defvar *all-charsets* nil)

(defun find-charset (name &optional (default nil))
  (dolist (k *all-charsets* default)
    (when (or (string-equal name (charset-name k))
	      (member name (charset-nicknames k) :test #'string-equal))
      (return k))))

(defun register-charset (charset)
  (unless (not (null (charset-name charset)))
    (error "You cannot register the charset ~S, since it is lacking a name." charset))
  (pushnew charset *all-charsets*)
  charset)

(progn
  (setq *all-charsets* nil)
  
  (register-charset (make-instance 'lazy-8-bit-simple-charset 
		      :name :koi8-r
		      :nicknames '(:koi-8 :koi-8-r :koi8)
		      :encoding-table-url "file://closure/resources/encodings/koi8-r"))

  (register-charset (make-instance 'lazy-8-bit-simple-charset 
		      :name :iso-8859-1
		      :nicknames '(:iso-latin-1 :8859-1 :iso8859-1 :latin-1)
		      :encoding-table-url "file://closure/resources/encodings/iso-8859-1"))

  (register-charset (make-instance 'lazy-8-bit-simple-charset 
		      :name :iso-8859-2
		      :nicknames '(:iso-latin-2 :8859-2 :iso8859-2 :latin-2)
		      :encoding-table-url "file://closure/resources/encodings/iso-8859-2"))

  (register-charset (make-instance 'lazy-8-bit-simple-charset 
		      :name :iso-8859-3
		      :nicknames '(:iso-latin-3 :8859-3 :iso8859-3 :latin-3)
		      :encoding-table-url "file://closure/resources/encodings/iso-8859-3"))

  (register-charset (make-instance 'lazy-8-bit-simple-charset 
		      :name :iso-8859-4
		      :nicknames '(:iso-latin-4 :8859-4 :iso8859-4 :latin-4)
		      :encoding-table-url "file://closure/resources/encodings/iso-8859-4"))

  (register-charset (make-instance 'lazy-8-bit-simple-charset 
		      :name :iso-8859-5
		      :nicknames '(:iso-latin-5 :8859-5 :iso8859-5 :latin-5)
		      :encoding-table-url "file://closure/resources/encodings/iso-8859-5"))

  (register-charset (make-instance 'lazy-8-bit-simple-charset 
		      :name :iso-8859-6
		      :nicknames '(:iso-latin-6 :8859-6 :iso8859-6 :latin-6)
		      :encoding-table-url "file://closure/resources/encodings/iso-8859-6"))

  (register-charset (make-instance 'lazy-8-bit-simple-charset 
		      :name :iso-8859-7
		      :nicknames '(:iso-latin-7 :8859-7 :iso8859-7 :latin-7)
		      :encoding-table-url "file://closure/resources/encodings/iso-8859-7"))

  (register-charset (make-instance 'lazy-8-bit-simple-charset 
		      :name :iso-8859-8
		      :nicknames '(:iso-latin-8 :8859-8 :iso8859-8 :latin-8)
		      :encoding-table-url "file://closure/resources/encodings/iso-8859-8"))

  (register-charset (make-instance 'lazy-8-bit-simple-charset 
		      :name :iso-8859-9
		      :nicknames '(:iso-latin-9 :8859-9 :iso8859-9 :latin-9)
		      :encoding-table-url "file://closure/resources/encodings/iso-8859-9"))

  ;; Codepages
  (register-charset (make-instance 'lazy-8-bit-simple-charset 
		      :name :cp-1250
		      :nicknames '(:codepage-1250 :cp1250 :windows-1250)
		      :encoding-table-url "file://closure/resources/encodings/ms-codepage-1250"))
  (register-charset (make-instance 'lazy-8-bit-simple-charset 
		      :name :cp-1251
		      :nicknames '(:codepage-1251 :cp1251 :windows-1251)
		      :encoding-table-url "file://closure/resources/encodings/ms-codepage-1251"))
  (register-charset (make-instance 'lazy-8-bit-simple-charset 
		      :name :cp-1252
		      :nicknames '(:codepage-1252 :cp1252 :windows-1252)
		      :encoding-table-url "file://closure/resources/encodings/ms-codepage-1252"))
  (register-charset (make-instance 'lazy-8-bit-simple-charset 
		      :name :cp-1253
		      :nicknames '(:codepage-1253 :cp1253 :windows-1253)
		      :encoding-table-url "file://closure/resources/encodings/ms-codepage-1253"))
  (register-charset (make-instance 'lazy-8-bit-simple-charset 
		      :name :cp-1254
		      :nicknames '(:codepage-1254 :cp1254 :windows-1254)
		      :encoding-table-url "file://closure/resources/encodings/ms-codepage-1254"))
  (register-charset (make-instance 'lazy-8-bit-simple-charset 
		      :name :cp-1255
		      :nicknames '(:codepage-1255 :cp1255 :windows-1255)
		      :encoding-table-url "file://closure/resources/encodings/ms-codepage-1255"))
  (register-charset (make-instance 'lazy-8-bit-simple-charset 
		      :name :cp-1256
		      :nicknames '(:codepage-1256 :cp1256 :windows-1256)
		      :encoding-table-url "file://closure/resources/encodings/ms-codepage-1256"))
  (register-charset (make-instance 'lazy-8-bit-simple-charset 
		      :name :cp-1257
		      :nicknames '(:codepage-1257 :cp1257 :windows-1257)
		      :encoding-table-url "file://closure/resources/encodings/ms-codepage-1257"))
  (register-charset (make-instance 'lazy-8-bit-simple-charset 
		      :name :cp-1258
		      :nicknames '(:codepage-1258 :cp1258 :windows-1258)
		      :encoding-table-url "file://closure/resources/encodings/ms-codepage-1258"))
  (register-charset (make-instance 'lazy-8-bit-simple-charset 
		      :name :cp-437
		      :nicknames '(:codepage-437 :cp437 :windows-437)
		      :encoding-table-url "file://closure/resources/encodings/ms-codepage-437"))
  (register-charset (make-instance 'lazy-8-bit-simple-charset 
		      :name :cp-737
		      :nicknames '(:codepage-737 :cp737 :windows-737)
		      :encoding-table-url "file://closure/resources/encodings/ms-codepage-737"))
  (register-charset (make-instance 'lazy-8-bit-simple-charset 
		      :name :cp-775
		      :nicknames '(:codepage-775 :cp775 :windows-775)
		      :encoding-table-url "file://closure/resources/encodings/ms-codepage-775"))
  (register-charset (make-instance 'lazy-8-bit-simple-charset 
		      :name :cp-850
		      :nicknames '(:codepage-850 :cp850 :windows-850)
		      :encoding-table-url "file://closure/resources/encodings/ms-codepage-850"))
  (register-charset (make-instance 'lazy-8-bit-simple-charset 
		      :name :cp-852
		      :nicknames '(:codepage-852 :cp852 :windows-852)
		      :encoding-table-url "file://closure/resources/encodings/ms-codepage-852"))
  (register-charset (make-instance 'lazy-8-bit-simple-charset 
		      :name :cp-855
		      :nicknames '(:codepage-855 :cp855 :windows-855)
		      :encoding-table-url "file://closure/resources/encodings/ms-codepage-855"))
  (register-charset (make-instance 'lazy-8-bit-simple-charset 
		      :name :cp-857
		      :nicknames '(:codepage-857 :cp857 :windows-857)
		      :encoding-table-url "file://closure/resources/encodings/ms-codepage-857"))
  (register-charset (make-instance 'lazy-8-bit-simple-charset 
		      :name :cp-860
		      :nicknames '(:codepage-860 :cp860 :windows-860)
		      :encoding-table-url "file://closure/resources/encodings/ms-codepage-860"))
  (register-charset (make-instance 'lazy-8-bit-simple-charset 
		      :name :cp-861
		      :nicknames '(:codepage-861 :cp861 :windows-861)
		      :encoding-table-url "file://closure/resources/encodings/ms-codepage-861"))
  (register-charset (make-instance 'lazy-8-bit-simple-charset 
		      :name :cp-862
		      :nicknames '(:codepage-862 :cp862 :windows-862)
		      :encoding-table-url "file://closure/resources/encodings/ms-codepage-862"))
  (register-charset (make-instance 'lazy-8-bit-simple-charset 
		      :name :cp-863
		      :nicknames '(:codepage-863 :cp863 :windows-863)
		      :encoding-table-url "file://closure/resources/encodings/ms-codepage-863"))
  (register-charset (make-instance 'lazy-8-bit-simple-charset 
		      :name :cp-864
		      :nicknames '(:codepage-864 :cp864 :windows-864)
		      :encoding-table-url "file://closure/resources/encodings/ms-codepage-864"))
  (register-charset (make-instance 'lazy-8-bit-simple-charset 
		      :name :cp-865
		      :nicknames '(:codepage-865 :cp865 :windows-865)
		      :encoding-table-url "file://closure/resources/encodings/ms-codepage-865"))
  (register-charset (make-instance 'lazy-8-bit-simple-charset 
		      :name :cp-866
		      :nicknames '(:codepage-866 :cp866 :windows-866)
		      :encoding-table-url "file://closure/resources/encodings/ms-codepage-866"))
  (register-charset (make-instance 'lazy-8-bit-simple-charset 
		      :name :cp-869
		      :nicknames '(:codepage-869 :cp869 :windows-869)
		      :encoding-table-url "file://closure/resources/encodings/ms-codepage-869"))
  (register-charset (make-instance 'lazy-8-bit-simple-charset 
		      :name :cp-874
		      :nicknames '(:codepage-874 :cp874 :windows-874)
		      :encoding-table-url "file://closure/resources/encodings/ms-codepage-874"))
  ;;
  
  (register-charset (make-instance 'lazy-8-bit-simple-charset 
		      :name :adobe-symbol
		      :nicknames '()
		      :encoding-table-url "file://closure/resources/encodings/adobe-symbol"))

  (register-charset (make-instance 'lazy-8-bit-simple-charset 
		      :name :zapf-dingbats
		      :nicknames '(:dingbats)
		      :encoding-table-url "file://closure/resources/encodings/zapf-dingbats"))

  (register-charset (make-instance 'lazy-8-bit-simple-charset 
		      :name :adobe-standard
		      :nicknames '()
		      :encoding-table-url "file://closure/resources/encodings/adobe-standard"))

  (register-charset (make-instance '16-bit-identity-mapping
		      :name :unicode
		      :nicknames '(:unicode-1 :unicode-2 :iso-10646-1)))

  (register-charset (let ((cs (make-instance '8-bit-simple-charset
				:name :us-ascii
				:nicknames '(:ascii :ascii-7 :iso646 :iso-646))))
		      (do ((i 32 (+ i 1)))
			  ((= i 127))
			(charset-relate cs i i))
		      cs)) )



(defun maybe-parse-url (url)
  (if (url:url-p url) url (url:parse-url url)))

(defun parse-encoding-table-line (line)
  ;; Remove any NL or CR character, which may be left.
  (setf line (remove #\newline (remove #\return line)))
  ;; First strip any comment field
  (when (position #\# line)
    (setf line (subseq line 0 (position #\# line))))
  ;; Trim the string
  (setf line (string-trim '(#\space #\tab) line))
  (let ((p (position-if (lambda (c) (member c '(#\space #\tab))) line)))
    (and p
	 (let ((left (string-trim '(#\space #\tab) (subseq line 0 p)))
	       (right (string-trim '(#\space #\tab) (subseq line p))))
	   (when (and (>= (length left) 2) (string= left "0x" :end1 2))
	     (setf left (subseq left 2)))
	   (when (and (>= (length right) 2) (string= right "0x" :end1 2))
	     (setf right (subseq right 2)))
	   (cond ((and (= (length left) 4) (= (length right) 2)
		       (maybe-parse-integer left :radix 16)
		       (maybe-parse-integer right :radix 16))
		  (values (parse-integer left :radix 16)
			  (parse-integer right :radix 16)))
		 ((and (= (length left) 2) (= (length right) 4)
		       (maybe-parse-integer left :radix 16)
		       (maybe-parse-integer right :radix 16))
                  (values (parse-integer right :radix 16)
			  (parse-integer left :radix 16)))
		 (t
		  (warn "Somebody is confused here: '~A' <-> '~A'?" left right)
		  nil))))))








(defclass full-double-table-charset (charset)
  ((table :initarg :table)))

(defmethod charset-encode ((self full-double-table-charset) rune &optional (default nil))
  (with-slots (table) self
    (or (aref (aref table (ldb (byte 8 8) rune))
              (ldb (byte 8 0) rune))
        default)))

;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PNG; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: The DEFLATE Compression (rfc-1951)
;;;   Created: 1997-04-24
;;;    Author: Gilbert Baumann <gilbert@base-engineering.com>
;;;   License: MIT style (see below)
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1997,1998 by Gilbert Baumann

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

(in-package :PNG)

(defpackage :deflate
  (:use :glisp)
  (:export
   #:make-deflator
   #:stuff-deflator
   #:end-deflator
   #:make-inflator
   #:stuff-inflator
   #:end-inflator))

;; This code should really move into another package

;; Note: This implementation is inherently sloooow.
;; On the other hand it is safe and complete and easily verify-able.
;;

(defvar *length-encoding*
  '#((0    3)     (0    4)     (0    5)     (0    6)     (0    7)     (0    8)
     (0    9)     (0   10)     (1   11)     (1   13)     (1   15)     (1   17)
     (2   19)     (2   23)     (2   27)     (2   31)     (3   35)     (3   43)
     (3   51)     (3   59)     (4   67)     (4   83)     (4   99)     (4  115)
     (5  131)     (5  163)     (5  195)     (5  227)     (0  258) ))

(defvar *dist-encoding*
  '#( (0     1)      (0     2)      (0     3)      (0     4)      (1     5)      (1     7)
      (2     9)      (2    13)      (3    17)      (3    25)      (4    33)      (4    49)  
      (5    65)      (5    97)      (6   129)      (6   193)      (7   257)      (7   385)
      (8   513)      (8   769)      (9  1025)      (9  1537)     (10  2049)     (10  3073)
     (11  4097)     (11  6145)     (12  8193)     (12 12289)     (13 16385)     (13 24577)))

(defvar *fixed-huffman-code-lengths*
    (let ((res (make-array 288)))
      (loop for i from   0 to 143 do (setf (aref res i) 8))
      (loop for i from 144 to 255 do (setf (aref res i) 9))
      (loop for i from 256 to 279 do (setf (aref res i) 7))
      (loop for i from 280 to 287 do (setf (aref res i) 8))
      res))

(defvar *code-length-code-lengths-order*
    '(16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15)
  "Order in which code lengths for the code length alphabet are written.")

(defstruct bit-stream
  (octets (error "missing-argument")
	  :type (vector (unsigned-byte 8))) ;a vector of octets
  (pos 0 :type fixnum))                         ;bit position within octet stream

(declaim (inline bit-stream-read-bit))
(declaim (inline bit-stream-read-byte))

(defun bit-stream-read-bit (source)
  (prog1
      (the fixnum
        (logand (the fixnum #x1)
                (the fixnum
                  (ash (the fixnum
                         (aref (the (array (unsigned-byte 8) (*)) (bit-stream-octets source))
                               (the fixnum (ash (the fixnum (bit-stream-pos source)) -3))))
                       (the fixnum (- (the fixnum (logand (the fixnum (bit-stream-pos source)) (the fixnum #x7)))))))))
    (incf (the fixnum (bit-stream-pos source)))))

(defun bit-stream-read-byte (source n)
  "Read one unsigned byte of width 'n' from the bit stream 'source'."
  (let* ((data (bit-stream-octets source))
         (pos  (bit-stream-pos source))
         (i (ash pos -3)))
    (declare (type fixnum i)
             (type fixnum pos))
    (prog1
        (logand 
         (the fixnum (1- (the fixnum (ash 1 (the fixnum n)))))
         (the fixnum
           (logior 
            (the fixnum (ash (aref (the (array (unsigned-byte 8) (*)) data) i) (- (logand pos #x7))))
            (the fixnum (ash (aref (the (array (unsigned-byte 8) (*)) data) (+ i 1)) (+ (- 8 (logand pos #x7)))))
            (the fixnum (ash (aref (the (array (unsigned-byte 8) (*)) data) (+ i 2)) (+ (- 16 (logand pos #x7)))))
            #|(the fixnum (ash (aref (the (simple-array (unsigned-byte 8) (*)) data) (+ i 3)) (+ (- 24 (logand pos #x7)))))|#
            )))
      (incf (the fixnum (bit-stream-pos source)) (the fixnum n)) )))

(defun bit-stream-read-reversed-byte (source n)
  "Read one unsigned byte of width 'n' from the bit stream 'source'."
  (let ((res 0))
    (dotimes (k n res)
      (setf res (logior res (ash (bit-stream-read-bit source) (1- (- n k))))) )))

(defun bit-stream-skip-to-byte-boundary (bs)
  (setf (bit-stream-pos bs) (* 8 (floor (+ 7 (bit-stream-pos bs)) 8))))

(defun bit-stream-read-symbol (source tree)
  "Read one symbol (code) from the bit-stream source using the huffman code provided by 'tree'."
  (do ()
      ((atom tree) tree)
    (let ((bit (bit-stream-read-bit source)))
      (setf tree (if (zerop bit) (car tree) (cdr tree))))))

(defun build-huffman-tree (lengthen)
  "Build up a huffman tree given a vector of code lengthen as described in RFC1951."
  (let* ((max-bits (reduce #'max (map 'list #'identity lengthen)))
         (max-symbol (1- (length lengthen)))
         (bl-count (make-array (+ 1 max-bits) :initial-element 0))
         (next-code (make-array (+ 1 max-bits) :initial-element 0))
         (ht nil))
    (dotimes (i (Length lengthen))
      (let ((x (aref lengthen i)))
        (unless (zerop x)
          (incf (aref bl-count x)))))
    (let ((code 0))
      (loop 
          for bits from 1 to max-bits 
          do
            (progn
              (setf code (ash (+ code (aref bl-count (1- bits))) 1))
              (setf (aref next-code bits) code))))
    (loop 
        for n from 0 to max-symbol 
        do
          (let ((len (aref lengthen n)))
            (unless (zerop len)
              (setf ht (huffman-insert ht len (aref next-code len) n))
              (incf (aref next-code len)) )))
    ht ))

(defun huffman-insert (ht len code sym)
  (cond ((= 0 len) 
         (assert (null ht))
         sym)
        ((logbitp (- len 1) code)
         (unless (consp ht) (setq ht (cons nil nil)))
         (setf (cdr ht) (huffman-insert (cdr ht) (1- len) code sym))
         ht)
        (t
         (unless (consp ht) (setq ht (cons nil nil)))
         (setf (car ht) (huffman-insert (car ht) (1- len) code sym))
         ht) ))

(defun rfc1951-read-huffman-code-lengthen (source code-length-huffman-tree number)
  (let ((res (make-array number :initial-element 0))
        (i 0))
    (do ()
        ((= i number))
      (let ((qux (bit-stream-read-symbol source code-length-huffman-tree)))
        (case qux
          (16
           (let ((cnt (+ 3 (bit-stream-read-byte source 2))))
             (dotimes (k cnt) 
               (setf (aref res (+ i k)) (aref res (- i 1))))
             (incf i cnt)))
          (17
           (let ((cnt (+ 3 (bit-stream-read-byte source 3))))
             (dotimes (k cnt)
               (setf (aref res (+ i k)) 0))
             (incf i cnt)))
          (18
           (let ((cnt (+ 11 (bit-stream-read-byte source 7))))
             (dotimes (k cnt)
               (setf (aref res (+ i k)) 0))
             (incf i cnt)))
          (otherwise
           (setf (aref res i) qux)
           (incf i)) )))
    res))

(defun rfc1951-read-length-dist (source code hdists-ht)
  (values 
   (+ (cadr (aref *length-encoding* (- code 257)))
      (bit-stream-read-byte source (car (aref *length-encoding* (- code 257)))))
   (let ((dist-sym (if hdists-ht
                       (bit-stream-read-symbol source hdists-ht)
                     (bit-stream-read-reversed-byte source 5) )))
         (+ (cadr (aref *dist-encoding* dist-sym))
            (bit-stream-read-byte source (car (aref *dist-encoding* dist-sym)))) ) ))

(defun rfc1951-uncompress-octects (octects &key (start 0))
  (let ((res (make-array 0 :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t))
        (ptr 0))
    (rfc1951-uncompress-bit-stream (make-bit-stream :octets octects :pos (* 8 start))
                                     #'(lambda (buf n)
                                         (progn
                                           (adjust-array res (list (+ ptr n)))
                                           (setf (fill-pointer res) (+ ptr n))
                                           (replace res buf 
                                                    :start1 ptr :end1 (+ ptr n)
                                                    :start2 0 :end2 n)
                                           (incf ptr n))))
    res))

(defun rfc1951-uncompress-bit-stream (bs cb)
  (let (final? ctype
        (buffer (make-array #x10000 :element-type '(unsigned-byte 8)))
        (bptr 0))
    (declare (type (simple-array (unsigned-byte 8) (#x10000)) buffer)
             (type fixnum bptr))
    (macrolet ((put-byte (byte)
                 `(let ((val ,byte))
                    (setf (aref buffer bptr) (the (unsigned-byte 8) val))
                    (setf bptr (the fixnum (logand #xffff (the fixnum (+ bptr 1)))))
                    (when (zerop bptr)
                      (funcall cb buffer #x10000) ))))
      (loop
        (setf final? (= (bit-stream-read-bit bs) 1)
              ctype (bit-stream-read-byte bs 2))
        (ecase ctype
          (0
           ;; no compression
           (bit-stream-skip-to-byte-boundary bs)
           (let ((len (bit-stream-read-byte bs 16))
                 (nlen (bit-stream-read-byte bs 16)))
             (assert (= (logand #xFFFF (lognot nlen)) len))
             (dotimes (k len)
               (put-byte (bit-stream-read-byte bs 8)))))
      
          (1 
           ;; compressed with fixed Huffman code
           (let ((literal-ht (build-huffman-tree *fixed-huffman-code-lengths*)))
             (do ((x (bit-stream-read-symbol bs literal-ht) (bit-stream-read-symbol bs literal-ht)))
                 ((= x 256))
               (cond ((<= 0 x 255)
                      (put-byte x)) 
                     (t
                      (multiple-value-bind (length dist) (rfc1951-read-length-dist bs x nil)
                        (dotimes (k length)
                          (put-byte (aref buffer (logand (- bptr dist) #xffff)))))) )) ))
      
          (2
           ;; compressed with dynamic Huffman codes
           (let* ((hlit  (+ 257 (bit-stream-read-byte bs 5))) ;number of literal code lengths
                  (hdist (+ 1 (bit-stream-read-byte bs 5))) ;number of distance code lengths
                  (hclen (+ 4 (bit-stream-read-byte bs 4))) ;number of code lengths for code 
                  (hclens (make-array 19 :initial-element 0)) ; length huffman tree
                  literal-ht distance-ht code-len-ht)
             ;; slurp the code lengths code lengths
             (loop 
                 for i from 1 to hclen 
                 for j in *code-length-code-lengths-order*
                 do (setf (aref hclens j) (bit-stream-read-byte bs 3)))
             ;; slurp the huffman trees for literals and distances
             (setf code-len-ht (build-huffman-tree hclens))
             (setf literal-ht  (build-huffman-tree (rfc1951-read-huffman-code-lengthen bs code-len-ht hlit))
                   distance-ht (build-huffman-tree (rfc1951-read-huffman-code-lengthen bs code-len-ht hdist)))
         
             ;; actually slurp the contents
             (do ((x (bit-stream-read-symbol bs literal-ht) (bit-stream-read-symbol bs literal-ht)))
                 ((= x 256))
               (cond ((<= 0 x 255)
                      (put-byte x))
                     (t
                      (multiple-value-bind (length dist) (rfc1951-read-length-dist bs x distance-ht)
                        (dotimes (k length)
                          (put-byte (aref buffer (logand (- bptr dist) #xffff)))))) )) )) ) 
        (when final?
          (funcall cb buffer bptr)
          (return-from rfc1951-uncompress-bit-stream 'foo)) ))))



;;;

(defun build-huffman-tree-2 (lengthen)
  "Build up a huffman tree given a vector of code lengthen as described in RFC1951."
  (let* ((max-bits      (reduce #'max (map 'list #'identity lengthen)))
         (max-symbol    (1- (length lengthen)))
         (bl-count      (make-array (+ 1 max-bits) :initial-element 0))
         (next-code     (make-array (+ 1 max-bits) :initial-element 0))
         (table         (make-array (+ 1 max-symbol))))
    (dotimes (i (Length lengthen))
      (let ((x (aref lengthen i)))
        (unless (zerop x)
          (incf (aref bl-count x)))))
    (let ((code 0))
      (loop 
          for bits from 1 to max-bits 
          do
            (progn
              (setf code (ash (+ code (aref bl-count (1- bits))) 1))
              (setf (aref next-code bits) code))))
    (loop 
        for n from 0 to max-symbol 
        do
          (let ((len (aref lengthen n)))
            (unless (zerop len)
              (setf (aref table n) (cons len (aref next-code len)))
              (incf (aref next-code len)) )))
    table ))

;; a first attempt on compression

(defstruct inflate-state 
  cur-byte
  bit-ptr)

(defun longest-match (octets p1 p2 end &optional (max-len 10))
  (declare #.cl-user:+optimize-very-fast+
           (type (simple-array (unsigned-byte 8) (*)) octets)
           (type fixnum p1 p2 max-len end))
  (let ((nmax (min (- end p2) max-len)))
    (declare (type fixnum nmax))
    (do ((n 0 (the fixnum (+ n 1))))
        ((or (>= n nmax)
             (not (= (aref octets (the fixnum (+ p1 n)))
                     (aref octets (the fixnum (+ p2 n))))))
         n)
      (declare (type fixnum n)))))

#||
(defun compress (state octets &optional (start 0) (end (length octets)) (finalp t))
  (let ((output-buffer (make-array (* 2 (- end start)) :element-type '(unsigned-byte 8))))
    (let ((cur-byte (if state (inflate-state-cur-byte state) 0))
          (bit-ptr  (if state (inflate-state-bit-ptr state) 0))
          (byte-ptr 0)
          ;; cons pairs of (len . position)
          )
      (labels ((flush-byte ()
                 (setf (aref output-buffer byte-ptr) cur-byte)
                 (setf cur-byte 0)
                 (setf byte-ptr (+ byte-ptr 1))
                 (setf bit-ptr 0))
               (write-bit (bit)
                 (cond ((= bit-ptr 8)
                        (flush-byte)))
                 (setf cur-byte (dpb bit (byte 1 bit-ptr) cur-byte))
                 (setf bit-ptr (+ bit-ptr 1)))
               (write-int (len x)
                 (dotimes (n len)
                   (write-bit (ldb (byte 1 n) x))))
               (write-int-2 (len x)
                 (loop for n from (1- len) downto 0 do
                   (write-bit (ldb (byte 1 n) x))))
               )
        ;;
        (write-int 1 (if finalp 1 0))   ;BFINAL = 0 = false
        (write-int 2 1)                 ;BTYPE = 1 = "compressed with fixed Huffman codes"
        ;;(flush-byte)
        ;;
        (let* ((table (build-huffman-tree-2 *fixed-huffman-code-lengths*))
               (nhash 1009)
               (nentry 20)
               (hash-table
                (make-array (list nhash nentry) :initial-element nil)))
          (declare (type (simple-array T (* *)) hash-table))
          ;;
          (labels ((write-codon (codon)
                     (let ((q (aref table codon)))
                       (write-int-2 (car q) (cdr q))))
                   ;;
                   (write-dist (dist)
                     (multiple-value-bind (code n-bits extra-byte) (encode-distance dist)
                       ;; dist huffman table?!
                       (write-int-2 5 code)
                       (write-int n-bits extra-byte)))
                   ;;
                   (write-length (length)
                     (multiple-value-bind (code n-bits extra-byte) (encode-length length)
                       (write-codon (+ 257 code))
                       (write-int n-bits extra-byte)))
                   ;;
                   (write-len/dist (len dist)
                     (assert (<= 3 len 258))
                     (write-length len)
                     (write-dist dist) )
                   
                   ;; hashtable
                   (hash-fn (c1 c2 c3)
                     (mod (+ c1 c2 c3) nhash)) )
            (do ((i start i))
                ((>= i end))
              (let ((donep nil)
                    (longest-n nil)
                    (longest-j nil))
                (when (< (+ i 2) end)
                  (let ((y (hash-fn (aref octets (+ i 0))
                                    (aref octets (+ i 1))
                                    (aref octets (+ i 2)))))
                    ;;
                    (dotimes (x nentry)
                      (let ((p1 (aref hash-table y x)))
                        (when p1
                          (let ((n (longest-match octets p1 i end 258)))
                            (when (>= n 3)
                              (when (or (null longest-n) (>= n longest-n))
                                (psetf longest-n n
                                       longest-j (if (and longest-n (= n longest-n))
                                                     (max p1 (or longest-j 0))
                                                   p1)) ))))))
                    ;; make entry
                    (let ((smallest-x 0)
                          (smallest-p (aref hash-table y 0)))
                      (dotimes (x nentry
                                 ;; no empty entry, find smallest and replace that
                                 (progn
                                   (setf (aref hash-table y smallest-x) i)) )
                        (let ((p (aref hash-table y x)))
                          (when (null p)
                            (setf (aref hash-table y x) i)
                            (return))
                          (and p
                               (when (< p smallest-p)
                                 (setf smallest-x x
                                       smallest-p p)))))) ))
                ;;
                (when longest-n
                  (let ((dist (- i longest-j 0)))
                    (setf donep t)
                    (write-len/dist longest-n dist)
                    (setf i (+ i longest-n))))
                ;;
                (unless donep
                  (write-codon (aref octets i))
                  (incf i))))
            (write-codon 256)           ;end of block
            ))
        (when finalp
          (flush-byte))
        (setq output-buffer
          (subseq output-buffer 0 byte-ptr))
        output-buffer
        (values
         output-buffer
         (make-inflate-state :cur-byte cur-byte :bit-ptr bit-ptr)) ))))
||#

(defun encode-distance (dist)
  ;; -> code n-bits extra-byte
  (let* ((j
          (dotimes (j (length *dist-encoding*) (1- j))
            (when (> (second (aref *dist-encoding* j)) dist)
              (return (1- j)))))
         (q (aref *dist-encoding* j)))
    (let ((n-extra-bits (first q))
          (low-bound (second q)))
      (values j n-extra-bits (- dist low-bound)))))

(defun encode-length (length)
  (let* ((j
          (dotimes (j (length *length-encoding*) (1- j))
            (when (> (second (aref *length-encoding* j)) length)
              (return (1- j)))))
         (q (aref *length-encoding* j)))
    (let ((n-extra-bits (first q))
          (low-bound (second q)))
      (values j n-extra-bits (- length low-bound)))))

;;; Stuffing Huffman trees
;; 

(defun max-depth (tree)
  (cond ((atom tree) 0)
        ((+ 1 (max (max-depth (car tree))
                   (max-depth (cdr tree)))))))

(defun cons-huffman-code-lengthen (probabilities &optional (max-depth 15))
  ;; 'probabilities' is a vector of probabilities
  ;; 'max-depth' is the maximum bit length of a token
  (let ((sigma (sort (loop for i from (1- (length probabilities)) downto 0
                         when (not (zerop (aref probabilities i)))
                         collect (cons i (aref probabilities i)))
                     #'< :key #'cdr)))
    ;; 'sigma' is a sorted list of (<char> . <prob>) pairs
    (do ()
        ((null (cdr sigma))                     ;only one left
         sigma)

      ;; combine the two least-probable characters into a new one
      (let ((z1 (pop sigma))
            (z2 (pop sigma)))
        ;;
        ;; Reuse z1 cons cell
        (setf (car z1) (cons (car z1) (car z2))
              (cdr z1) (+ (cdr z1) (cdr z2)))
        ;; Insert this new character into sigma
        (cond
         ;; catch special case:
         ;; We need to prepend, either because:
         ((or (null sigma)                      ; i. sigma empty
              (< (cdr z1) (cdr (car sigma))))   ;ii. z1 has lowest probability
          ;; reuse the cons cell z2
          (setf (car z2) z1 (cdr z2) sigma)
          (setf sigma z2) )
         ;; regular case
         (t
          (do ((q sigma (cdr q)))
              ((null q))
            (cond ((or (null (cdr q))
                       (<= (cdr z1) (cdr (cadr q))))
                   ;; Reuse the cons cell z2
                   (setf (car z2) z1
                         (cdr z2) (cdr q)
                         (cdr q)  z2)
                   (return))))) )))
    ;; Finally, we are only interested into the code lengthen
    (let ((codelen (make-array (length probabilities) :initial-element 0))
          (overflow nil))
      (labels ((walk (tree n)
                 (cond ((null tree)
                        nil)
                       ((atom tree)
                        (if (> n max-depth)
                            (push tree overflow)
                          (setf (aref codelen tree) n)))
                       (t
                        (walk (car tree) (+ n 1))
                        (walk (cdr tree) (+ n 1))))))
        (walk (caar sigma) 0))
      ;; For every overflow item, find one, whose bit-length could be extended
      (dolist (o overflow)
        (let ((node nil) (node-len 0))
          (dotimes (i (length codelen))
            (when (and (> (aref codelen i) 0)
                       (> (aref codelen i) node-len)
                       (< (aref codelen i) max-depth))
              (setf node i node-len (aref codelen i))))
          (unless node
            (error "Barf!"))
          (incf (aref codelen node))
          (setf (aref codelen o) (aref codelen node))))
      codelen)))

;; --------------------------------------------------------------------------------------------
;;  This is for debugging:
;; 

;; (:line x0 y0 x1 y1)
;; 

(defvar *dx* 0)
(defvar *dy* 0)

(defun trx (x) (+ *dx* x))
(defun try (y) (+ *dy* y))

(defun dump-figure (figure sink)
  (ecase (car figure)
    ((:line)
     (destructuring-bind (x1 y1 x2 y2) (cdr figure)
       (format sink "~&2 1 0 1 0 7 100 0 -1 0.000 0 0 -1 0 0 2")
       (format sink "~&    ~D ~D ~D ~D"
               (trx x1) (try y1)
               (trx x2) (try y2))))
    ((:circle)
     (destructuring-bind (x y r) (cdr figure)
       (format sink "~&1 3 0 1 0 7 100 0 -1 0.000 1 0.0000 ~D ~D ~D ~D ~D ~D ~D ~D"
               (trx x) (try y)
               (abs (- (trx (+ x r)) (trx (- x r))))
               (abs (- (trx (+ x r)) (trx (- x r))))
               (trx x) (try y)
               (trx (+ x r))
               (try (+ y r)))))
    ((:text)
     (destructuring-bind (x y text) (cdr figure)
       (format sink "~&4 1 0 100 0 4 12 0.0000 0 0 0 ~D ~D ~A\\001"
               (trx x) (try y) 
               (with-output-to-string (bag)
                 (map nil (lambda (c) 
                            (format bag "\\~3,'0O" (char-code c)))
                      text))
               (code-char 1))))
    ((:translate)
     (destructuring-bind (dx dy &rest more) (cdr figure)
       (let ((*dx* (+ *dx* dx))
             (*dy* (+ *dy* dy)))
         (mapc (lambda (f) (dump-figure f sink)) more)))) ))

(defun dump-figure-to-file (filename figure)
  (with-open-file (sink filename :direction :output :if-exists :new-version)
    ;; preamble
    (format sink "#FIG 3.2~%")
    (format sink "Landscape~%")
    (format sink "Center~%")
    (format sink "Inches~%")
    (format sink "Letter  ~%")
    (format sink "100.00~%")
    (format sink "Single~%")
    (format sink "-2~%")
    (format sink "1200 2~%")
    (dump-figure figure sink)))

(defun dump-figure-to-ps-file (filename figure)
  (dump-figure-to-file "/tmp/temp.fig" figure)
  (run-unix-shell-command (format nil "LD_LIBRARY_PATH=;fig2dev -L ps -m 1  /tmp/temp.fig /tmp/temp.ps"))
  ;; fix the broken PS generated by fig2dev
  (run-unix-shell-command (format nil "LD_LIBRARY_PATH=;gs -sDEVICE=pswrite -dNOPAUSE -dBATCH -q -sOutputFile=/tmp/temp2.ps /tmp/temp.ps"))
  (run-unix-shell-command (format nil "LD_LIBRARY_PATH=;mv /tmp/temp2.ps ~A" filename)))

(defun dump-tree-aux (tree)
  (let ((y-raport 600))
    (cond ((atom tree)
           (values 200 200 200
                   `(:text 0 200 ,(prin1-to-string 
                                   (or (code-char tree)
                                       tree)))))
          (t
           (multiple-value-bind (L1 R1 H1 G1) (dump-tree-aux (car tree))
             (multiple-value-bind (L2 R2 H2 G2) (dump-tree-aux (cdr tree))
               (values (+ 20 L1 R1)
                       (+ 20 L2 R2)
                       (+ y-raport (max H1 H2))
                       `(:translate 0 0
                                    (:translate ,(- (+ R1 10)) ,y-raport ,G1)
                                    (:translate ,(+ L2 10) ,y-raport ,G2)
                                    (:line 0 0 ,(- (+ R1 10)) ,y-raport)
                                    (:line 0 0 ,(+ L2 10) ,y-raport)))))))))

(defun dump-tree (tree)
  (multiple-value-bind (L R H G) (dump-tree-aux tree)
    (declare (ignore R H))
    `(:translate ,(+ 300 L) ,(+ 300 0)
                 ,G)))
     
;; --------------------------------------------------------------------------------------------

(defun encode-code-lengths (code-lengths)
  ;; The alphabet for code lengths is as follows:
  ;; 
  ;;  0 - 15: Represent code lengths of 0 - 15
  ;;      16: Copy the previous code length 3 - 6 times.
  ;;          The next 2 bits indicate repeat length
  ;;                (0 = 3, ... , 3 = 6)
  ;;             Example:  Codes 8, 16 (+2 bits 11),
  ;;                       16 (+2 bits 10) will expand to
  ;;                       12 code lengths of 8 (1 + 6 + 5)
  ;;      17: Repeat a code length of 0 for 3 - 10 times.
  ;;          (3 bits of length)
  ;;      18: Repeat a code length of 0 for 11 - 138 times
  ;;          (7 bits of length)
  ;; 
  (let ((res nil))
    (labels ((emit (c)
               (push c res)))
      (let ((ncode (length code-lengths)))
        (do ((i 0 i))
            ((= i ncode))
          (cond
           ;; Can we make use of 16 = "copy the previous code length 3 - 6 times." ?
           ((and (< (+ i 3) ncode)
                 (> i 0)
                 (= (aref code-lengths (- i 1))
                    (aref code-lengths i)
                    (aref code-lengths (+ i 1))
                    (aref code-lengths (+ i 2))))
            ;; we have now at least 3 copies, look for more
            (let ((n (do ((n 3 (+ n 1)))
                         ((or (>= (+ i n) ncode) ;end of vector
                              (/= (aref code-lengths (+ i n)) (aref code-lengths i)) ;mismatch
                              (= n 6))          ;maximum number of repetitions
                          n))))
              (emit (cons 16 (- n 3)))
              (incf i n)))
           ;; 17 = "Repeat a code length of 0 for 3 - 10 times."
           ;; or 18 = "Repeat a code length of 0 for 11 - 138 times" useful?
           ((and (< (+ i 3) ncode)
                 (= 0
                    (aref code-lengths i)
                    (aref code-lengths (+ i 1))
                    (aref code-lengths (+ i 2))))
            (let ((n (do ((n 3 (+ n 1)))
                         ((or (>= (+ i n) ncode) ;end of vector
                              (/= 0 (aref code-lengths (+ i n))) ;mismatch
                              (= n 138))        ;maximum number of repetitions
                          n))))
              (cond ((<= 3 n 10)
                     (emit (cons 17 (- n 3))))
                    ((<= 11 n 138)
                     (emit (cons 18 (- n 11)))))
              (incf i n)))
           ;; otherwise just emit code length
           (t
            (emit (aref code-lengths i))
            (incf i)) ))))
    (reverse res) ))

(defstruct bit-sink
  (cur-byte 0)
  (bit-ptr 0)
  (output-buffer (make-array 10 :element-type '(unsigned-byte 8) 
                             :adjustable t
                             :fill-pointer 0)))

(defun bit-sink-flush-byte (sink)
  (vector-push-extend (bit-sink-cur-byte sink) (bit-sink-output-buffer sink))
  (setf (bit-sink-cur-byte sink) 0
        (bit-sink-bit-ptr sink) 0))

(defun bit-sink-write-bit (sink bit)
  (cond ((= (bit-sink-bit-ptr sink) 8)
         (bit-sink-flush-byte sink)))
  (setf (bit-sink-cur-byte sink) (dpb bit (byte 1 (bit-sink-bit-ptr sink)) (bit-sink-cur-byte sink)))
  (incf (bit-sink-bit-ptr sink)))

(defun bit-sink-write-byte (sink size value)
;;  (format T "~&; B ~v,'0B~13T = ~D." size value value)
  (dotimes (j size)
    (bit-sink-write-bit sink (ldb (byte 1 j) value))))

(defun bit-sink-write-reversed-byte (sink size value)
;;  (format T "~&; R ~v,'0B~13T = (~D,~D)." size value size value)
  (loop for j from (1- size) downto 0 do
        (bit-sink-write-bit sink (ldb (byte 1 j) value))))

(defun make-codon-table (lengths)
  (build-huffman-tree-2 lengths))

(defun bit-sink-write-codon (sink table codon)
  (bit-sink-write-reversed-byte sink (car (aref table codon)) (cdr (aref table codon))))

;;;;

(defun write-code-length-code-lengths (sink lengths)
  ;; we do not need to write trailing zeros, but as a minimum, we need to write 4 elements
  (let ((reordered-lengths (make-array 19)))
    (loop 
        for j in *code-length-code-lengths-order*
        for i from 0
        do (setf (aref reordered-lengths i) (aref lengths j)))

    (let ((hclen (max (+ 1 (or (position-if-not #'zerop reordered-lengths :from-end t) 0))
                      4)))
      (bit-sink-write-byte sink 4 (- hclen 4))
      (loop 
          for i from 1 to hclen
          for x across reordered-lengths
          do (bit-sink-write-byte sink 3 x)))))

;;;;

;; make-deflate-stream continuation
;; stuff-deflate-stream octets start end
;; end-deflate-stream
;; 

;;; -------------------------------------------------------------------------------------------
;;;  gzip files
;;;

(defvar *gz-magic*
    #(#x1F #x8B))

;; gzip header flag bits
(defconstant *gz-ascii-flag-bit*       0)       ;file probably ascii text
(defconstant *gz-head-crc-flag-bit*    1)       ;header crc present
(defconstant *gz-extra-field-flag-bit* 2)       ;extra field present
(defconstant *gz-orig-name-flag-bit*   3)       ;original file name present
(defconstant *gz-comment-flag-bit*     4)       ;file comment present

(defconstant +os-code+              #x03)       ;(unix OS code)
                                                ; Has LOS an OS code already?

(defconstant +z-deflated+              8)

(defun minimal-gz-header ()
  (concatenate '(simple-array (unsigned-byte 8) (*))
    *gz-magic*
    (vector +z-deflated+)
    (vector 0)                                  ;flags
    #(0 0 0 0)                                  ;time
    (vector 0)                                  ;xflags
    (vector +os-code+)))                        ;OS code

(defun write-unsigned-byte-32-LE (byte sink)
  (write-byte (ldb (byte 8 0) byte) sink)
  (write-byte (ldb (byte 8 8) byte) sink)
  (write-byte (ldb (byte 8 16) byte) sink)
  (write-byte (ldb (byte 8 24) byte) sink))

(defun write-gz-file (filename data)
  (with-open-file (sink filename
                   :direction :output
                   :if-exists :new-version
                   :element-type '(unsigned-byte 8))
    (write-sequence (minimal-gz-header) sink)
    (let ((crc 0))
      (setf crc (update-crc crc data))
      (write-sequence (compress nil data) sink)
      (write-unsigned-byte-32-LE crc sink)      ;crc32 of uncompressed data
      (write-unsigned-byte-32-LE (length data) sink)        ;total in
      )))

;;; CRC-32 
;; in png-image.lisp

;;;

(defun s2v (s) (map '(simple-array (unsigned-byte 8) (*)) #'char-code s))

(defun compress-block (octets &optional (start 0) (end (length octets)))
  (let* ((output-buffer (make-array 1000 :adjustable t :fill-pointer 0))
         (nhash 10091)
         (nentry 100)
         (hash-table
          (make-array (list nhash nentry) :initial-element nil)))
    (declare (type (simple-array T (* *)) hash-table))
    ;;
    (labels ((hash-fn (c1 c2 c3)
               (mod (+ (* #x10000 c1) (* #x100 c2) c3) nhash)))
      ;;
      (do ((i start i))
          ((>= i end))
        (let ((longest-n nil)
              (longest-j nil))
          ;; search for suitable n,j values
          (when (< (+ i 2) end)
            (let ((y (hash-fn (aref octets (+ i 0))
                              (aref octets (+ i 1))
                              (aref octets (+ i 2)))))
              ;;
              (dotimes (x nentry)
                (let ((p1 (aref hash-table y x)))
                  (when p1
                    (let ((n (longest-match octets p1 i end 258)))
                      (when (>= n 3)
                        (when (or (null longest-n) (>= n longest-n))
                          (psetf longest-n n
                                 longest-j (if (and longest-n (= n longest-n))
                                               (max p1 (or longest-j 0))
                                             p1)) )))))) 
              ;; make hash table entry
              (let ((smallest-x 0)
                    (smallest-p (aref hash-table y 0)))
                (dotimes (x nentry
                           ;; no empty entry, find smallest and replace that
                           (progn
                             (setf (aref hash-table y smallest-x) i)) )
                  (let ((p (aref hash-table y x)))
                    (when (null p)
                      (setf (aref hash-table y x) i)
                      (return))
                    (and p
                         (when (< p smallest-p)
                           (setf smallest-x x
                                 smallest-p p))))))))
          ;;
          (cond (longest-n
                 ;; encode a repetition
                 (let ((dist (- i longest-j)))
                   (multiple-value-bind (code nbits extra) (encode-length longest-n)
                     (vector-push-extend (+ 257 code) output-buffer)
                     (vector-push-extend `(:lit ,nbits ,extra) output-buffer))
                   (multiple-value-bind (code nbits extra) (encode-distance dist)
                     (vector-push-extend `(:dist ,code) output-buffer)
                     (vector-push-extend `(:lit ,nbits ,extra) output-buffer)) )
                 ;; advance read pointer
                 (incf i longest-n))
                (t
                 ;; encode literal
                 (vector-push-extend (aref octets i) output-buffer)
                 (incf i)) ) )))
    output-buffer))

#||
(defun compress (state octets &optional (start 0) (end (length octets)) (finalp t))
  (let ((output (compress-block octets start end)))
    (vector-push-extend 256 output)
    ;; Now calculate the probabilities
    (let ((lit-prob (make-array 286 :initial-element 0))
          (dist-prob (make-array 30 :initial-element 0)))
      (dotimes (i (length output))
        (let ((v (aref output i)))
          (cond ((atom v)
                 (incf (aref lit-prob v)))
                ((eq (car v) :dist)
                 (incf (aref dist-prob (cadr v)))) )))
      ;;
      (let* ((hlit (cons-huffman-code-lengthen lit-prob))
             (hdist (cons-huffman-code-lengthen dist-prob))
             (hclens-encoding (encode-code-lengths hlit))
             (hdlens-encoding (encode-code-lengths hdist)))
        ;;
        (let ((hcprobs (make-array 19 :initial-element 0))
              (hclens))
          (dolist (c hclens-encoding)
            (if (atom c)
                (incf (aref hcprobs c))
              (incf (aref hcprobs (car c)))))
          (dolist (c hdlens-encoding)
            (if (atom c)
                (incf (aref hcprobs c))
              (incf (aref hcprobs (car c)))))
          ;;
          (setf hclens (cons-huffman-code-lengthen hcprobs))
          (let ((bs (make-bit-sink)))
            (bit-sink-write-byte bs 1 (if finalp 1 0))       ;BFINAL = 0 = false
            (bit-sink-write-byte bs 2 2)                     ;BTYPE = 2 = "compressed with dynamic Huffman codes"
            ;;
            (bit-sink-write-byte bs 5 (- (length hlit) 257));number of length for literal ht
            (bit-sink-write-byte bs 5 (- (length hdist) 1));number of length for dist ht
            ;; (bit-sink-write-byte 4 (- (length hclen) 1));number of length for length encoding
            (write-code-length-code-lengths bs hclens)
            ;; Now emit length-ht and dist-ht
            ;;
            (format T "~&;; HLEN = ~S" hclens-encoding)
            (let ((table (make-codon-table hclens)))
              (dolist (c (append hclens-encoding))
                (cond ((atom c)
                       (bit-sink-write-codon bs table c))
                      ((= (car c) 16)
                       (bit-sink-write-codon bs table 16)
                       (bit-sink-write-byte bs 2 (cdr c)))
                      ((= (car c) 17)
                       (bit-sink-write-codon bs table 17)
                       (bit-sink-write-byte bs 3 (cdr c)))
                      ((= (car c) 18)
                       (bit-sink-write-codon bs table 18)
                       (bit-sink-write-byte bs 7 (cdr c))))))
            (format T "~&;; HDIST")
            (let ((table (make-codon-table hclens)))
              (dolist (c (append hdlens-encoding))
                (cond ((atom c)
                       (bit-sink-write-codon bs table c))
                      ((= (car c) 16)
                       (bit-sink-write-codon bs table 16)
                       (bit-sink-write-byte bs 2 (cdr c)))
                      ((= (car c) 17)
                       (bit-sink-write-codon bs table 17)
                       (bit-sink-write-byte bs 3 (cdr c)))
                      ((= (car c) 18)
                       (bit-sink-write-codon bs table 18)
                       (bit-sink-write-byte bs 7 (cdr c))))))
            (format T "~&;; Nutzdaten")
            ;; Now write the output
            (let ((lit-table (make-codon-table hlit))
                  (dist-table (make-codon-table hdist)))
              (map nil (lambda (c)
                         (cond ((atom c)
                                (bit-sink-write-codon bs lit-table c))
                               ((eq (car c) :dist)
                                (bit-sink-write-codon bs dist-table (cadr c)))
                               ((eq (car c) :lit)
                                (bit-sink-write-byte bs (second c) (third c)))))
                   output))

            (bit-sink-flush-byte bs)
            (let ((res (bit-sink-output-buffer bs)))
              (rfc1951-uncompress-octects res)
              res))
          ;;
          )))))
||#

(defun compress (state octets &optional (start 0) (end (length octets)) (finalp t))
  (let ((output-buffer (make-array (* 2 (- end start)) :element-type '(unsigned-byte 8))))
    (let ((cur-byte (if state (inflate-state-cur-byte state) 0))
          (bit-ptr  (if state (inflate-state-bit-ptr state) 0))
          (byte-ptr 0)
          ;; cons pairs of (len . position)
          )
      (labels ((flush-byte ()
                 (setf (aref output-buffer byte-ptr) cur-byte)
                 (setf cur-byte 0)
                 (setf byte-ptr (+ byte-ptr 1))
                 (setf bit-ptr 0))
               (write-bit (bit)
                 (cond ((= bit-ptr 8)
                        (flush-byte)))
                 (setf cur-byte (dpb bit (byte 1 bit-ptr) cur-byte))
                 (setf bit-ptr (+ bit-ptr 1)))
               (write-int (len x)
                 (dotimes (n len)
                   (write-bit (ldb (byte 1 n) x))))
               (write-int-2 (len x)
                 (loop for n from (1- len) downto 0 do
                   (write-bit (ldb (byte 1 n) x))))
               )
        ;;
        (write-int 1 (if finalp 1 0))   ;BFINAL = 0 = false
        (write-int 2 1)                 ;BTYPE = 1 = "compressed with fixed Huffman codes"
        ;;(flush-byte)
        ;;
        (let* ((table (build-huffman-tree-2 *fixed-huffman-code-lengths*))
               (nhash 1009)
               (nentry 20)
               (hash-table
                (make-array (list nhash nentry) :initial-element nil)))
          (declare (type (simple-array T (* *)) hash-table))
          ;;
          (labels ((write-codon (codon)
                     (let ((q (aref table codon)))
                       (write-int-2 (car q) (cdr q))))
                   ;;
                   (write-dist (dist)
                     (multiple-value-bind (code n-bits extra-byte) (encode-distance dist)
                       ;; dist huffman table?!
                       (write-int-2 5 code)
                       (write-int n-bits extra-byte)))
                   ;;
                   (write-length (length)
                     (multiple-value-bind (code n-bits extra-byte) (encode-length length)
                       (write-codon (+ 257 code))
                       (write-int n-bits extra-byte)))
                   ;;
                   (write-len/dist (len dist)
                     (assert (<= 3 len 258))
                     (write-length len)
                     (write-dist dist) )
                   
                   ;; hashtable
                   (hash-fn (c1 c2 c3)
                     (mod (+ c1 c2 c3) nhash)) )
            (do ((i start i))
                ((>= i end))
              (let ((donep nil)
                    (longest-n nil)
                    (longest-j nil))
                (when (< (+ i 2) end)
                  (let ((y (hash-fn (aref octets (+ i 0))
                                    (aref octets (+ i 1))
                                    (aref octets (+ i 2)))))
                    ;;
                    (dotimes (x nentry)
                      (let ((p1 (aref hash-table y x)))
                        (when p1
                          (let ((n (longest-match octets p1 i end 258)))
                            (when (>= n 3)
                              (when (or (null longest-n) (>= n longest-n))
                                (psetf longest-n n
                                       longest-j (if (and longest-n (= n longest-n))
                                                     (max p1 (or longest-j 0))
                                                   p1)) ))))))
                    ;; make entry
                    (let ((smallest-x 0)
                          (smallest-p (aref hash-table y 0)))
                      (dotimes (x nentry
                                 ;; no empty entry, find smallest and replace that
                                 (progn
                                   (setf (aref hash-table y smallest-x) i)) )
                        (let ((p (aref hash-table y x)))
                          (when (null p)
                            (setf (aref hash-table y x) i)
                            (return))
                          (and p
                               (when (< p smallest-p)
                                 (setf smallest-x x
                                       smallest-p p)))))) ))
                ;;
                (when longest-n
                  (let ((dist (- i longest-j 0)))
                    (setf donep t)
                    (write-len/dist longest-n dist)
                    (setf i (+ i longest-n))))
                ;;
                (unless donep
                  (write-codon (aref octets i))
                  (incf i))))
            (write-codon 256)           ;end of block
            ))
        (when finalp
          (flush-byte))
        (setq output-buffer
          (subseq output-buffer 0 byte-ptr))
        output-buffer
        (values
         output-buffer
         (make-inflate-state :cur-byte cur-byte :bit-ptr bit-ptr)) ))))


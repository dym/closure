;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: R2; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Hyphenation
;;;   Created: 2003-03-07
;;;    Author: Gilbert Baumann <gilbert@base-engineering.com>
;;;   License: MIT style (see below)
;;;       $Id$
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1999-2003 by Gilbert Baumann

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
 
(in-package :R2)

;;;; TODO

;; - The tree hyphenation take uses a fixed code and can only store
;;   words out of [a-z]*, this obviously needs fixing.



;; We define a class hyphenation-table protocol class, since we
;; somewhere have to stick later special caseing for e.g. German,
;; where for example "klicken" was used to be hyphenated as
;; "klik-ken".

(defclass hyphenation-table ()               
  ())

(defgeneric insert-hyphenation-pattern (hyphenation-table string pattern))

;;;; Simple Hyphenation Table using a hash table

;; slow, but i leave it in.

(defclass simple-hyphenation-table (hyphenation-table)
  ((hash-table :initarg :hash-table)
   (max-pattern-length :initarg :max-pattern-length
                       :documentation "The length for the longest pattern.")
   (filename :initarg :filename
             :initform nil
             :documentation "Filename of file this was read from, for a more informative printer.")))

(defmethod print-object ((object simple-hyphenation-table) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~S" (slot-value object 'filename))))

(defmethod insert-hyphenation-pattern ((hyphenation-table simple-hyphenation-table) string pattern)
  (with-slots (hash-table max-pattern-length) hyphenation-table
    (setf (gethash string hash-table) pattern
          max-pattern-length (max max-pattern-length (length string)))))

(defmethod hyphen-points ((hyphen-table simple-hyphenation-table) string)
  ;;(assert (every #'alpha-char-p string))
  (with-slots (hash-table max-pattern-length) hyphen-table
    (setq string (concatenate 'string
                              "."
                              (map 'string #'(lambda (x)
                                               (if (alpha-char-p x)
                                                   (char-downcase x)
                                                   #\.))
                                   string)
                              "."))
    (let ((values (make-array (+ (length string) 1) :initial-element 0)))
      (dotimes (i (length string))
        (dotimes (j (max max-pattern-length (- (length string) i)))
          (let ((pattern (gethash (subseq string i (min (+ i j 1) (length string)))
                                  hash-table)))
            (dotimes (k (length pattern))
              (setf (aref values (+ i k))
                    (max (aref values (+ i k))
                         (aref pattern k)))))))
      (loop for i below (length values)
            when (and (oddp (aref values i))
                      (>= (- i 1) 2)
                      (>= (- (- (length values) 3)
                             (- i 1))
                          3)
                      (not (char= (char string (- i 1)) #\.)))
            collect (- i 1)))))

;;;; Tree Hyphenation Table

(defclass tree-hyphenation-table (hyphenation-table)
  ((tree :initform nil :initarg :tree)
   (filename :initarg :filename
             :initform nil
             :documentation "Filename of file this was read from, for a more informative printer.")))

(defconstant +tree-hyphenation-table-node-size+ (+ 26 1 1))

(defun ht/char-code (c)
  (if (char= c #\.)
      1
      (+ 2 (- (char-code (char-downcase c))
              (char-code #\a)))))

(defun ht/insert (node string value &optional (start 0) (end (length string)))
  (when (null node)
    (setf node (make-array +tree-hyphenation-table-node-size+ :initial-element nil)))
  (cond ((= start end)
         (setf (aref node 0) value))
        (t
         (let ((c (ht/char-code (char string start))))
           (setf (aref node c)
                 (ht/insert (aref node c)
                            string
                            value
                            (+ start 1) end)))))
  node)

(defmethod insert-hyphenation-pattern ((hyphenation-table tree-hyphenation-table) string pattern)
  (with-slots (tree) hyphenation-table
    (setf tree (ht/insert tree string pattern))))

(clim-sys:defresource zeroed-fixnum-vector (n)
  :constructor (make-array n :initial-element 0 :element-type 'fixnum)
  :initializer (fill zeroed-fixnum-vector 0))

(defmethod hyphen-points ((hyphen-table tree-hyphenation-table) string)
  ;;(assert (every #'alpha-char-p string))
  (declare (optimize (speed 3) (safety 0)))
  (with-slots (tree max-pattern-length) hyphen-table
    (clim-sys:using-resource
     (xs zeroed-fixnum-vector (+ (length string) 2))
     (setf (aref xs 0) (ht/char-code #\.))
     (setf (aref xs (+ (length string) 1)) (ht/char-code #\.))
     (loop for i from 1
           for c across string do
           (setf (aref xs i) (ht/char-code c)))
     (let ((string xs))
       (clim-sys:using-resource
        (values zeroed-fixnum-vector (+ (length string) 1))
        (let ((end (length string)))
          (declare (type fixnum end)
                   (type (simple-array fixnum (*)) values)
                   (optimize (speed 3) (safety 0)))
          (loop for i of-type fixnum below end do
                (do* ((node  tree  (svref node (svref string start)))
                      (start i     (+ start 1)))
                     ((or (null node) (>= start end)))
                  (let ((p (svref node 0)))
                    (when p
                      (loop for v of-type fixnum across p
                            for k of-type fixnum from i do
                            (setf (aref values k)
                                  (max (aref values k) v)))))))
          ;;
          (loop for i below (length values)
                when (and (oddp (aref values i))
                          (>= (- i 1) 2)
                          (>= (- (- (length values) 3)
                                 (- i 1))
                              3)
                          (not (= (aref string (- i 1)) (ht/char-code #\.))))
                collect (- i 1))))))))

;;;; Reading Tables

(defun read-hyphen-table (url &key (class 'tree-hyphenation-table))
  (let ((url (if (url:url-p url) url (url:parse-url url))))
    (netlib:with-open-document ((input mime-type) url)
      (declare (ignore mime-type))
      (let ((res (make-instance class :filename url)))
        (g/read-line input nil nil)
        (do ((line (g/read-line input nil nil) (g/read-line input nil nil)))
            ((null line))
          (let* ((j (position #\space line))
                 (pattern (map 'vector #'digit-char-p (subseq line (+ j 1)))))
            (insert-hyphenation-pattern res (subseq line 0 j) pattern)))
        res))))


;; 100,000x hyphenation of "argument":
;; old: 14.35s  327,555,472 bytes
;; new:   .57s    2.400,000 bytes [technically zero]

;; $Log$
;; Revision 1.4  2005/07/17 09:30:51  emarsden
;; The hyphenation table can be referenced using an URL, rather than by
;; an absolute filename.
;;
;; Revision 1.3  2005/03/13 18:03:24  gbaumann
;; Gross license change
;;
;; Revision 1.2  2003/06/15 16:54:30  gilbert
;; Patches by Christophe Rhodes to get it going with SBCL.
;;
;; Revision 1.1  2003/03/13 19:30:56  gilbert
;; imported
;;

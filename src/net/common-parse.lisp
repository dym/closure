;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: NETLIB; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Common Parsing Routines to Parse off Header Fields et al
;;;   Created: 2001-05-16
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: LGPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2001 by Gilbert Baumann

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the 
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, 
;;; Boston, MA  02111-1307  USA.

(in-package :NETLIB)

;;; Basic parsing

(defun p/if (predicate string start end)
  (and (< start end)
       (funcall predicate (char string start))
       (+ start 1)))

(defun p/expect (c string start end)
  (p/if (lambda (d) (char= c d)) string start end))

(defun p/string (match string start end)
  (let ((p2 (+ start (length match))))
    (and (<= p2 end)
         (string= match string :start2 start :end2 p2)
         p2)))

(defun p/skip-opt-white-space (string start end)
  (do ()
      ((or (>= start end)
           (not (white-space-p (char string start))))
       start)
    (incf start)))

;;; Meta parsers

(defun p/concat (funs string start end)
  (let ((res nil))
    (dolist (f funs
              ;; fall thru return value
              (values start (reverse res)))
      (setq start (p/skip-opt-white-space string start end))
      (multiple-value-bind (new-start semantic) (funcall f string start end)
        (unless new-start
          (return nil))
        (push semantic res)
        (setq start new-start) ))))

(defun p/star (fun string start end)
  (let ((res nil))
    (loop
      (setq start (p/skip-opt-white-space string start end))
      (multiple-value-bind (new-start semantic) (funcall fun string start end)
        (cond (new-start
               (push semantic res)
               (setf start new-start))
              (t
               (return)))))
    (values
     start
     (reverse res))))

(defun p/mungle (mungler subfun string start end)
  (multiple-value-bind (p v) (funcall subfun string start end)
    (when p
      (values p (funcall mungler v)))))

(defun p/or (funs string start end)
  (dolist (f funs nil)
    (multiple-value-bind (new-start sem) (funcall f string start end)
      (when new-start
        (return (values new-start sem))))))

(defun p/whole (subfun string start end)
  (multiple-value-bind (start sem) (funcall subfun string start end)
    (when start
      (setf start (p/skip-opt-white-space string start end))
      (when (= start end)
        (values start sem)))))

;;; Toolbox

(defun p/separated-list (separator sub-parser string start end)
  (setf start (p/skip-opt-white-space string start end))
  (multiple-value-bind (p2 semantic) (funcall sub-parser string start end)
    (cond (p2
           (setf p2 (p/skip-opt-white-space string p2 end))
           (cond ((and (< p2 end) (char= (char string p2) separator))
                  (multiple-value-bind (p3 rest) (p/separated-list separator sub-parser string (+ p2 1) end)
                    (if p3
                        (values p3 (cons semantic rest))
                      (values nil nil))))
                 (t
                  (values p2 (list semantic)))))
          (t
           (values nil nil)))))

(defun p/generic-token (initial-predicate more-predicate string start end)
  (and (< start end)
       (funcall initial-predicate (char string start))
       (let ((j (or (position-if-not more-predicate
                                     string :start (+ start 1) :end end)
                    end)))
         (values 
          j
          (subseq string start j)))))

;;; More special parsers in the context of HTTP

(defconstant +token-chars+
    "!#$%&'*+-.0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ^_`abcdefghijklmnopqrstuvwxyz|~")

(defconstant +tspecial+
    (concatenate 'string "()<>@,;:\\\"/[]?={}" (string #\space) (string #\tab)))

(defun http-alpha-char-p (c)
  (find c "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(defun http-digit-char-p (c)
  (find c "0123456789"))

(defun http-token-char-p (c)
  (find c +token-chars+))

(defun p/token (string start end)
  (p/generic-token #'http-token-char-p #'http-token-char-p string start end))

(defun p/quoted-string (string start end)
  (let ((p start))
    (and (< p end)
         (char= (char string p) #\")
         (progn
           (incf p)
           (let ((sem
                  (with-output-to-string (bag)
                    (loop
                      (when (>= p end)
                        (return-from p/quoted-string nil))
                      (let ((c (char string p)))
                        (cond ((char= c #\")
                               (incf p)
                               (return))
                              ((char= c #\\)
                               (incf p)
                               (if (< p end)
                                   (write-char (char string p) bag)
                                 (return-from p/quoted-string nil)))
                              (t
                               (write-char c bag))))
                      (incf p)))))
             (values p sem))))))

;;;; HTTP "Link:" header fields

(defun p/http-link-extension (string start end)
  ;; link-extension = token [ "=" ( token | quoted-string ) ]
  (multiple-value-bind (p2 sem) (p/concat (list #'p/token
                                                (curry #'p/expect #\=)
                                                (curry #'p/or (list #'p/token
                                                                    #'p/quoted-string)))
                                          string start end)
    (and p2 (values p2 (list (first sem) (third sem))))))

(defun p/URI-char-p (c)
  (find c "!#$%&'()*+,-./0123456789:;=?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"))

(defun p/URI-string (string start end)
  (p/generic-token #'p/URI-char-p #'p/URI-char-p string start end))

(defun parse-http-link-field (string &optional (start 0) (end (length string)))
  (multiple-value-bind (start links)
      (p/whole
       (curry #'p/separated-list #\, 
              (curry #'p/mungle
                     (lambda (x)
                       (cons (second x) (fourth x)))
                     (curry #'p/concat (list (curry #'p/expect #\<)
                                             #'p/URI-string
                                             (curry #'p/expect #\>)
                                             (curry #'p/star 
                                                    (curry #'p/mungle
                                                           #'second
                                                           (curry #'p/concat
                                                                  (list (curry #'p/expect #\;)
                                                                        #'p/http-link-extension))))))))
       string start end)
    (when start
      (mapcar (lambda (link)
                ;; Since link header fields work like <LINK> elements,
                ;; we build psuedo elements
                (destructuring-bind (href &rest attributes) link
                  `(:LINK :HREF ,href
                          ,@(mapcan (lambda (attribute)
                                      (destructuring-bind (name value) attribute
                                        (list (intern (string-upcase name) :keyword)
                                              value)))
                                    attributes))))
              links))))

;;;;

(defun apply-p (pfun string &rest args)
  (apply pfun (append args (list string 0 (length string)))))

;;;

;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SGML; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Definition of PT and Misc. PT utilities 
;;;   Created: Somewhen in 1996
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: GPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1996-1999 by Gilbert Baumann

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
;;  1999-08-24  GB      - PT structure: spend PLIST slot
;;  1999-08-24  GB      - PT-ACCESS, PT-PATH: new functions
;;                      - REMOVE-PT, DELETE-PT: new functions
;;                      - ANCESTORP: new function
;;

(in-package :SGML)

;;; -------------------------------------------------------------------------------------------
;;;  The pt Data Type

(defstruct (pt (:constructor make-pt/low) (:print-function print-pt))
  name attrs children parent cache plist)

(defun print-pt (self sink depth)
  (declare (ignore depth))
  (format sink "#<~S ~A~{ ~S~}>" (type-of self) (pt-name self) (pt-children self)))

(defmethod print-object ((object pt) stream)
  (format stream "#<~S ~A ..>" (type-of object) (pt-name object)))

#||
(set-pprint-dispatch 'pt (lambda (stream object)
                           (funcall 'pprint-pt stream object)))

(defun pprint-pt (stream object)
  '(cond ((eql (gi object) :pcdata)
          (loop for c across (rod-string (pt-attrs object)) do
                (cond ((eql c #\newline)
                       (pprint-newline :mandatory stream))
                      (t
                       (princ c stream)))))
    (t
     (format stream "<~A~{ ~A=~S~}~A>"
      (gi object)
      (mapcar (lambda (x)
                (if (glisp::rodp x) (rod-string x) x))
              (pt-attrs object))
      (if (null (pt-children object)) "/" ""))))
  (when (pt-children object)
    (pprint-logical-block (stream (pt-children object)
                                  :prefix (format nil "<~A>" (gi object))
                                  :suffix (format nil "</~A>" (gi object)))
                          (pprint-indent :block 2 stream)
                          (dolist (k (pt-children object))
                            (pprint k stream)))
    ;; (pprint-newline :mandatory stream)
    '(format stream "</~A>" (gi object)) ))
||#

#-CLISP
(defun print-pt (self sink depth)
  (cl-user::with-depth-abbreviation (sink depth)
    (cond ((and (eq (gi self) :pcdata) *print-readably*)
           (prin1 (rod-string (pt-cdata self)) sink))
          ((and (eq (gi self) :pcdata) (not *print-readably*))
           (pprint-newline :fill sink)
           (let ((s (sanify-string (rod-string (pt-cdata self)))))
             (write-char #\" sink)
             (pprint-logical-block (sink (list s))
               (dotimes (i (length s))
                 (let ((c (char s i)))
                   (cond ((char= c #\space)
                          (write-char #\space sink)
                          (pprint-newline :fill sink))
                         ((member c '(#\\ #\"))
                          (write-char #\\ sink)
                          (write-char c sink))
                         (t
                          (write-char c sink)))))
               (write-char #\" sink))))
          (t    
           (let ((atts (mapcar (lambda (x)
                                 (if (sloopy-rod-p x)
                                     (rod-string x)
                                   x))
                               (pt-attrs self))))
             (cond (atts
                    (format sink "~:_#n(~<(~S~<~{~:_ ~S ~S~}~:>)~{ ~S~}~:>)"
                            (list
                             (pt-name self)
                             (list 
                                   atts)
                             (pt-children self))))
                   (t
                    (format sink "~:_#n(~<~S~{ ~S~}~:>)"
                            (list 
                             (pt-name self)
                             (pt-children self))))))))))

(defun print-pt (self sink depth)
  (cl-user::with-depth-abbreviation (sink depth)
    (cond ((eq (gi self) :pcdata)
           (prin1 
            (if *print-readably*
                (rod-string (pt-cdata self))
              (string-trim '(#\space #\newline #\return #\tab #\page)
                           (rod-string (pt-cdata self))))
            sink))
          (t
           (format sink "#n(~A" (pt-name self))
           (cl-user::print-list-with-length-abbreviation 
            (pt-children self)
            #'prin1 
            sink)
           (write-string ")" sink)))))


(defun print-pt (self sink depth)
  (declare (ignore depth))
  (format sink "#<~S ~A ..>" (type-of self) (pt-name self)))

(defun make-pt (&key name attrs children)
  (let ((res (make-pt/low :name name :attrs attrs :children children)))
    (dolist (k children)
      (setf (pt-parent k) res))
    res))

(defun ppt (pt &optional (prefix "") (barp nil))
  (cond ((eq (pt-name pt) :pcdata)
         (let ((s (map 'string #'(lambda (x) (if (eql x 10) #\space (code-char x))) (progn (pt-attrs pt)))) flag)
           (if (and (> (- 120 (length prefix)) 0)
                    (> (length s) (- 120 (length prefix))))
               (setq s (concatenate 'string (subseq s 0 (- 120 (length prefix))))
                     flag t))
           (write-string (format nil "~%~A| ~S ~A" prefix s
                                 (if flag "..." "")))))
        (t
         (write-string (format nil "~%~A| ~A" prefix (pt-name pt)))
         (when (pt-children pt)
           (write-string (format nil "~%~A~A-~A." 
                                 prefix 
                                 (if barp "+" "`")
                                 (make-string (length (symbol-name (pt-name pt))) 
                                             :initial-element #\- )))
           (let ((prefix1 (concatenate 'string 
                            prefix (if barp "|" " ")
                            (make-string (length (symbol-name (pt-name pt))) 
                                        :initial-element #\space)
                            " ")))
             (do ((q (pt-children pt) (cdr q)))
                 ((null q))
               (ppt (car q) prefix1 (if (cdr q) 't 'nil))))))))

;;; -------------------------------------------------------------------------------------------

(defun pt-cdata (pt)
  (assert (or (eq (gi pt) :pcdata)
              (eq (gi pt) :comment)))
  (pt-attrs pt))

(defun pt-attr (pt prop &optional (default nil))
  (let ((value (cond ((eq (pt-name pt) :PCDATA) default)
                     ((getf (pt-attrs pt) prop default)))))
    (cond ((char= (char (symbol-name prop) 0) #\%)
           value)
          (t
           (warn "~S: prop=~S." 'pt-attr prop)
           (map 'string (lambda (x) (or (code-char x) #\?)) value)))))

(defun (setf pt-attr) (value pt prop)
  (setf (getf (pt-attrs pt) prop) value))

(defun pt-root (pt) 
  (cond ((null (pt-parent pt)) pt)
	((pt-root (pt-parent pt)))))

(defun gi (x) (pt-name x))

(defun flat-find-element (gi pt &optional (default nil default-p))
  (or (find gi (pt-children pt) :key #'pt-name)
      (if default-p
	  default
	(error "~A element not found." gi))))

(defun flat-find-elements (gi pt)
  (remove-if-not (lambda (x) (eq (pt-name x) gi)) (pt-children pt)))

(defun pt-full-name-path (pt)
  (cond ((null pt) nil)
        ((cons (pt-name pt) (pt-full-name-path (pt-parent pt))))))

(defun lhtml->pt (tree)
  (cond ((typep tree 'rod)
         (sgml::make-pt :name :pcdata :attrs tree))
        ((stringp tree)
         (sgml::make-pt :name :pcdata :attrs (map '(vector (unsigned-byte 16)) #'char-code tree)))
        ((sgml::pt-p tree) tree)
        ((and (consp tree) (keywordp (car tree)))
         (let ((attrs nil)
               (gi (car tree)))
           (do ((q (cdr tree) (cddr q)))
               ((or (null q)
                    (not (keywordp (car q))))
		(sgml::make-pt :name gi
			       :attrs (nreverse attrs)
			       :children (mapcar #'lhtml->pt q)))
             (push (car q) attrs)
             (push (rod (cadr q)) attrs))))
        (t
         (error "~S does not look like LHTML." tree)) ))

(defun lhtml-reader (stream subchar arg)
  `(lhtml->pt
    ,(funcall (get-macro-character #\`) stream nil)))

(set-dispatch-macro-character #\# #\T 'lhtml-reader)

(defun map-pt (fun pt)
  "Apply 'fun' to all parse tree nodes in 'pt'."
  (funcall fun pt)
  (dolist (k (sgml:pt-children pt))
    (unless (eq (sgml:gi k) :pcdata)
      (map-pt fun k))))

(defun map-pt-with-pcdata (fun pt)
  "Apply `fun' to all parse tree nodes in `pt' including PCDATA nodes."
  (funcall fun pt)
  (dolist (k (sgml:pt-children pt))
    (map-pt-with-pcdata fun k)))

(defun pt-access (root path)
  (cond ((null path) root)
        ((let ((c (elt (pt-children root) (car path))))
           (unless c
             (error "~S: path ~S broken on children list ~S." 'pt-access path (pt-children root)))
           (pt-access (elt (pt-children root) (car path)) (cdr path))))))

(defun pt-path (root element)
  (cond ((eq root element) nil)
        ((append (pt-path root (pt-parent element))
                 (list (position element (pt-children (pt-parent element))))))))

(defun remove-pt (node)
  "Removes the parse tree node `node' (non-destructive wrt children list)."
  (when (pt-parent node)
    (setf (pt-children (pt-parent node))
      (remove node (pt-children (pt-parent node))))))

(defun delete-pt (node)
  "Deletes the parse tree node `node' (destructive wrt children list)."
  (when (pt-parent node)
    (setf (pt-children (pt-parent node))
      (delete node (pt-children (pt-parent node))))))

(defun ancestorp (node putative-ancestor)
  "Is `putative-ancestor' is an ancestor of `node'."
  (or (eq putative-ancestor (pt-parent node))
      (and (not (null (pt-parent node)))
           (ancestorp (pt-parent node) putative-ancestor))))


;;;;; The Basic Closure Element Protocol

(defmethod closure-protocol:element-p ((object pt))
  t)

(defmethod closure-protocol:element-parent ((element pt))
  (pt-parent element))

(defmethod closure-protocol:element-children ((element pt))
  (pt-children element))

(defmethod closure-protocol:element-attribute ((element pt) attribute-name)
  (getf (sgml:pt-attrs element) attribute-name))

(defmethod closure-protocol:element-gi ((element pt))
  (gi element))

(defmethod closure-protocol:text-element-p ((element pt))
  (eql (gi element) :pcdata))

(defmethod closure-protocol:element-text ((element pt))
  (assert (eql (sgml:gi element) :pcdata))
  (pt-attrs element))

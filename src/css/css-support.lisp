;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CSS; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: [temporary] parse tree support for the css stuff
;;;   Created: 2002-08-07
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2002 by Gilbert Baumann

(in-package :css)

(defun element-has-class-p (element class)
  (class-eq class (element-attribute element :class)))

(defun element-has-id-p (element id)
  (id-eq id (element-attribute pt :id)))

;;; hmmm

(defun intern-attribute-name (string)
  ;; XXX hack
  (intern (string-upcase (map 'string (lambda (x) (or (code-char x) #\?)) string)) :keyword))

(defun intern-gi (string)
  (intern-attribute-name string))

(defun css2-class-match-p (string element)
  (attribute-contains-p element #.(map 'vector #'char-code "CLASS") string t))

(defun css2-id-match-p (string element)
  (attribute-equal-p element #.(map 'vector #'char-code "ID") string t))

(defun css2-gi-match-p (string element)
  (eq (sgml::pt-name (the sgml::pt element)) string))

;; We probably want
;;   intern-gi    document-language id-rod
;;   intern-class document-language class-rod
;;   intern-id    document-language class-id
;; and
;;   element-css-gi element
;;   element-css-classes element
;;   element-css-id element
;;

(defun class-eq (x y)
  (equalp x y))

(defun id-eq (x y)
  (equalp x y))

;;;

(defun element-style-cache (element)
  (sgml::pt-cache element))

(defun (setf element-style-cache) (new-value element)
  (setf (sgml::pt-cache element) new-value))

(defun pseudo-class-matches-p (pclass pt)
  (case pclass
    ((:link :visited)
     (and (not (null (r2::pt-attr/latin1 pt :href))))) ;this is of course a bit too lazy!
    ((:hover)
     (and
      (sgml:pt-attr pt :%hover-p nil)))
    ((:first-line)
     (eq pt *first-line-element*))
    ((:before :after :first-letter)
     (eq (sgml::pt-attr pt :%pseudo-class) pclass))
    (otherwise
     nil)))




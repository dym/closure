;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CSS; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Initial setup of css style attributes
;;;   Created: 1998-06-18
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: GPL (See file COPYING for details).
;;;       $Id$
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1998-2002 by Gilbert Baumann

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

(in-package :CSS)

;;;; ------------------------------------------------------------------------------------------

;; This still need some work.

;; . there is too much cruft left

;; . the style fetcher should be in certain ways be ignorant of the
;;   actual style attributes defined.

;; . this whole business of numbered slots is just nuts. we have
;;   defstruct which are just as good.

;; . 

;;;; ------------------------------------------------------------------------------------------

(defvar *device*)
(defvar *style-sheet*)

(defun ensure-style (pt)
  (unless (element-style-cache pt)
    (setf (element-style-cache pt)
      (make-array *n-attrs* :initial-element nil))
    (setup-style/1 *device* *style-sheet* pt)))

(defmacro set-style-attr (pt att value)
  (error "I am obsolete"))

(defmacro style-attr (pt att &optional default)
  (error "I am obsolete"))

(defun create-element-style-cache (pt)
  (setf (element-style-cache pt)
        (make-array *n-attrs* :initial-element nil #|*null*|# ))
  (setup-style/1 *device* *style-sheet* pt))

(defmacro element-style-cache-ref (pt slot)
  `(LOCALLY 
    (DECLARE (OPTIMIZE (SPEED 3) (SAFETY 0)))
    (AREF (THE (SIMPLE-ARRAY T (*))
               (ELEMENT-STYLE-CACHE ,pt))
          (THE FIXNUM ,slot))))

(defmacro %style-attr (pt att &optional default)
  (let ((g (gensym)))
    (cond ((and (consp att) (eq (car att) 'quote))
           `(LET ((,g ,pt))
                 ,(if default
                      `(let ((,g (ELEMENT-STYLE-CACHE-REF ,g ,(symbol-value (cadr att)))))
                         (if ,g ,g ,default))
                    `(ELEMENT-STYLE-CACHE-REF ,g ,(symbol-value (cadr att))))))
          (t
           `(LET ((,g ,pt))
                 (let ((,g (SVREF (ELEMENT-STYLE-CACHE ,g) (SYMBOL-VALUE ,att))))
                   (if ,g ,g ,default)) )))))

(defmacro %set-style-attr (pt att value)
  (let ((g (gensym)))
    (cond ((and (consp att) (eq (car att) 'quote))
           `(LET ((,g ,pt))
                 (SETF (SVREF (ELEMENT-STYLE-CACHE ,g) ,(symbol-value (cadr att)))
                       ,value)))
          (t
           `(LET ((,g ,pt))
                 (SETF (SVREF (ELEMENT-STYLE-CACHE ,g)
                              (SYMBOL-VALUE ,att)) ,value))))))


;;;; ------------------------------------------------------------------------------------------
;;;;  Setting up the Style (part I)
;;;;

(defvar *dpi*)                          ;to be bound in SETUP-STYLE/1

;;;; Todo (excrement from css1 spec)
  
;;; Since  the intent of  the relative keywords  'bolder' and 'lighter' is to
;;; darken or lighten the face within the family and because a family may not
;;; have faces aligned with all  the symbolic weight  values, the matching of
;;; 'bolder'  is to the  next darker face available on  the client within the
;;; family and the matching of  'lighter' is to the  next lighter face within
;;; the family. To be precise, the meaning  of the relative keywords 'bolder'
;;; and 'lighter' is as follows:

;;;  * 'bolder' selects the next weight that is assigned to a font that is
;;;     darker than the inherited one. If there is no such weight, it simply
;;;     results in the next darker numerical value (and the font remains un-
;;;     changed), unless the inherited value was '900' in which case the re-
;;;     sulting weight is also '900'. 

;;;   * 'lighter' is similar, but works in the opposite direction: it selects
;;;     the next lighter keyword with a different font from the inherited
;;;     one, unless there is no such font, in which case it selects the next
;;;     lighter numerical value (and keeps the font unchanged).

;;; There is no  guarantee that there will  be a darker  face for each of the
;;; 'font-weight' values; for example, some fonts may  have only a normal and
;;; a bold  face, others may have eight  different face  weights. There is no
;;; guarantee on how a UA will map font faces within a  family to weight val-
;;; ues. The only guarantee is that a  face of a  given value will be no less
;;; dark than the faces of lighter values.

;;; ------------------------------------------------------------------------------------------
;;;  Setting up the Style (part II)
;;;

(defun kill-style (pt)
  (unless (text-element-p pt)
    (setf (element-style-cache pt) nil)
    (dolist (k (element-children pt))
      (kill-style k))))

(defun setup-style (device style-sheet pt &key (first-line-p nil))
  (setf (element-style-cache pt)
    (make-array *n-attrs* :initial-element nil))  
  (let ((*first-line-p* first-line-p))
    (unless (text-element-p pt)
      (setup-style/1 device style-sheet pt)
      (dolist (k (element-children pt))
        (setup-style device style-sheet k :first-line-p first-line-p)))) )

;;; ---- Some utils --------------------------------------------------------------------------

(defsubst percentage-p (x)
  (and (consp x) (eq (car x) :%)))

;;; -------------------------------------------------------------------------------------------

(defparameter *cookings*
  nil)

(defclass cooking ()
  ((property :initarg :property :reader cooking-property)
   (applicable-if :initarg :applicable-if :reader cooking-applicable-if)
   (value :initarg :value :reader cooking-value)))

(defmacro define-cooking (property &key applicable-if value)
  `(progn
    ,@(mapcar (lambda (property)
                `(progn
                  (setf *cookings* (remove-if (lambda (cooking)
                                                (and (equal (cooking-applicable-if cooking) ',applicable-if)
                                                     (eql   (cooking-property cooking) ',property)))
                                    *cookings*))
                  (push
                   (make-instance 'cooking
                    :property ',property
                    :applicable-if ',applicable-if
                    :value ',value)
                   *cookings*)))
              (if (listp property) property (list property)))))


;;; on how to speed up style look up 

;;; a. make LOOKUP-ALL-STYLE faster:
;;;
;;;    - currently we have a selector for each single assignmnet, 
;;;      which is for from effiecient
;;;    - cache specifity vectors
;;;    - use hashing on GI's et all
;;;    - maybe: compile a selector matcher
;;;
;;;
;;; b. make SETUP-STYLE faster
;;;
;;;    - only interpret style attributes on demand.
;;;      (most attributes are never needed).
;;;    - don't bother to interpret a non-set margin-width et al
;;;

(defun interpret-line-height (device value pt)
  ;; Big caution here: line-height <number> is inherited differently from <percentage>
  (cond ((eq value :normal) 
         (interpret-line-height device *normal-line-height* pt))
	((numberp value)
	 (cons '* value))
	((interpret-length device value pt (r2::pt-font-size pt)))))

(defun cooking-function-dependencies (fn &aux res)
  (labels ((walk (x)
             (cond ((and (consp x) (eq (car x) 'prop))
                    (pushnew (cadr x) res))
                   ((atom x))
                   ((map nil #'walk x)))))
    (walk fn)
    res))

(defun generate-defclass-1 ()
  (let ((props nil))
    (loop for prop being the hash-values of *css-properties* do
          (when (typep prop 'concrete-property-description)
            (push (property-description-name prop) props)))
    `(defclass cooked-style ()
      ((%element :initform nil)
       (r2::computed-margin-left  :initform nil)
       (r2::computed-margin-right :initform nil)
       (r2::computed-width        :initform nil)
       (r2::computed-height       :initform nil)
       (%containing-block
        :initform nil
        :documentation "The style of the containig block")
       ,@(mapcar (lambda (prop)
                   (list prop
                         :initarg (intern (symbol-name prop) :keyword)
                         :reader (intern (format nil "COOKED-STYLE-~A" prop))))
                 props)))))

(defun generate-setup-style-1 ()
  (let ((cookers nil))
    (loop for prop being the hash-values of *css-properties* do
          (when (typep prop 'concrete-property-description)
            (let ((prop (property-description-name prop)))
              (let ((fn (cooking-function prop)))
                (push (list prop fn (cooking-function-dependencies fn))
                      cookers)))))
    (let ((order nil))
      (labels ((foo (x)
                 (map nil #'foo (third (assoc x cookers)))
                 (pushnew x order)))
        (map nil #'foo (mapcar #'first cookers))
        (setf order (reverse order)))

      `(defun setup-style-2 (device style-sheet pt former-cooked)
        (let* ((is (element-implicit-style (r2::rc-document r2::*rcontext*) pt))
               (ss (element-explicit-style (r2::rc-document r2::*rcontext*) pt))
               (style (make-array *n-attrs* :initial-element nil))
               (*dpi* (r2::device-dpi *device*))
               (dpi (r2::device-dpi *device*))
               (res (make-instance 'cooked-style)))
          (lookup-all-style style-sheet pt is ss style)
          (and (element-parent pt) 
               (ensure-style (element-parent pt)))
          (macrolet ((prop (x)
                       `(slot-value res ',x))
                     (parent-prop (x)
                       `(slot-value former-cooked ',x)))
            ,@(mapcar (lambda (prop)
                        (let ((att (intern (format nil "@~A" prop) (find-package :css))))
                          (with-slots (inheritedp default-value) (gethash prop *css-properties*)
                            (let ((cooker (second (assoc prop cookers))))
                              `(let ((value (svref style ,(symbol-value att))))
                                (setf value (or value :inherit))
                                (when (not (eq value :inherit))
                                  (setf value (,cooker value)))
                                (when (eq value :inherit)
                                  (setf value
                                        (if (and ,inheritedp (element-parent pt) former-cooked)
                                            (%style-attr (element-parent pt) ',att)
                                            (,cooker ,default-value))))
                                (setf (slot-value res ',prop) value))))))
                      order)
            res))) )))

(defmacro generate-setup-style ()
  (let ((cookers nil))
    (loop for prop being the hash-values of *css-properties* do
          (when (typep prop 'concrete-property-description)
            (let ((prop (property-description-name prop)))
              (let ((fn (cooking-function prop)))
                (push (list prop fn (cooking-function-dependencies fn))
                      cookers)))))
    (let ((order nil))
      (labels ((foo (x)
                 (map nil #'foo (third (assoc x cookers)))
                 (pushnew x order)))
        (map nil #'foo (mapcar #'first cookers))
        (setf order (reverse order)))

      `(defun setup-style/1 (device style-sheet pt)
          (let* ((is (element-implicit-style (r2::rc-document r2::*rcontext*) pt))
                 (ss (element-explicit-style (r2::rc-document r2::*rcontext*) pt))
                 (cache (element-style-cache pt))
                 (style cache)
                 (*dpi* (r2::device-dpi *device*))
                 (dpi (r2::device-dpi *device*)))
            (lookup-all-style style-sheet pt is ss style)
            (and (element-parent pt) 
                 (ensure-style (element-parent pt)))
            (macrolet ((prop (x)
                         `(svref cache ,(symbol-value (intern (format nil "@~A" x) (find-package :css)))))
                       (parent-prop (x)
                         `(%style-attr (element-parent pt)
                           ',(intern (format nil "@~A" x) (find-package :css)))))
              ,@(mapcar (lambda (prop)
                          (let ((att (intern (format nil "@~A" prop) (find-package :css))))
                            (with-slots (inheritedp default-value) (gethash prop *css-properties*)
                              (let ((cooker (second (assoc prop cookers))))
                                `(let ((value (svref cache ,(symbol-value att))))
                                  (setf value (or value :inherit))
                                  (when (not (eq value :inherit))
                                    (setf value (,cooker value)))
                                  (when (eq value :inherit)
                                    (setf value
                                          (if (and ,inheritedp (element-parent pt))
                                              (%style-attr (element-parent pt) ',att)
                                              (,cooker ,default-value))))
                                  (setf (svref cache ,(symbol-value att)) value))))))
                        order)))) )))

(defmacro generate-setup-style-3 ()
  (let ((cookers nil))
    (loop for prop being the hash-values of *css-properties* do
          (when (typep prop 'concrete-property-description)
            (let ((prop (property-description-name prop)))
              (let ((fn (cooking-function prop)))
                (push (list prop fn (cooking-function-dependencies fn))
                      cookers)))))
    (let ((order nil))
      (labels ((foo (x)
                 (map nil #'foo (third (assoc x cookers)))
                 (pushnew x order)))
        (map nil #'foo (mapcar #'first cookers))
        (setf order (reverse order)))

      `(progn
        ,(generate-defclass-1)
        
        (defun setup-style-3 (device document style-sheet pt
                              former-cooked
                              containing-block-style)
          (let* ((is (element-implicit-style document pt))
                 (ss (element-explicit-style document pt))
                 (style (make-array *n-attrs* :initial-element nil))
                 (*dpi* (r2::device-dpi *device*))
                 (dpi (r2::device-dpi *device*))
                 (res (make-instance 'cooked-style)))
            (setf (slot-value res '%element) pt)
            (setf (slot-value res '%containing-block) containing-block-style)
            (lookup-all-style style-sheet pt is ss style)
            (and (element-parent pt) 
                 (ensure-style (element-parent pt)))
            (macrolet ((prop (x)
                         `(slot-value res ',x))
                       (parent-prop (x)
                         `(slot-value former-cooked ',x)))
              ,@(mapcar (lambda (prop)
                          (let ((att (intern (format nil "@~A" prop) (find-package :css))))
                            (with-slots (inheritedp default-value) (gethash prop *css-properties*)
                              (let ((cooker (second (assoc prop cookers))))
                                `(let ((value (svref style ,(symbol-value att))))
                                  (setf value (or value :inherit))
                                  (when (not (eq value :inherit))
                                    (setf value (,cooker value)))
                                  (when (eq value :inherit)
                                    (setf value
                                          (if (and ,inheritedp (element-parent pt) former-cooked)
                                              (parent-prop ,prop)
                                              (,cooker ,default-value))))
                                  (setf (slot-value res ',prop) value))))))
                        order)
              res)))) )))

(defun new-interpret-length (value device font-size pt dpi)
  (cond ((consp value)
	 (let ((unit (car value))
	       (a (cdr value)))
	   (case unit
	     (:px (* 1 a))
	     (:em 
              (cond ((and pt (not (realp font-size)))
                     (warn "In ~S: font-size not available -- fix your programm."
                           'new-interpret-length)
                     (round (* a dpi 12) 72))
                    (t
                     (* a font-size))
                    (t
                     (round (* a dpi 12) 72))))
	     (:ex 
              (cond ((and pt (not (realp font-size)))
                     (warn "In ~S: font-size not available -- fix your programm."
                           'new-interpret-length)
                     (round (* a dpi 8) 72))
                    (t
                     (* 2/3 a font-size) ))) ;xxx
	     (:in (* dpi a))
	     (:cm (* (round (* a dpi) 2.54))) ;DEVRND
	     (:mm (* (round (* a dpi) 25.4))) ;DEVRND
	     (:pt (* (round (* a dpi) 72))) ;DEVRND
	     (:pc (* (round (* a dpi) 6))) ;DEVRND
             (:canvas-h-percentage
              ;; DEVRND
              (round (* a (r2::device-canvas-width device))
               100))
             (:canvas-v-percentage
              ;; DEVRND
              (round (* a (r2::device-canvas-height device))
               100))
	     (otherwise
	      (error "~S is not a proper css length value." value)) )))
	((eql value 0) 0)
	(t
	 (error "~S is not a proper css length value." value)) ) )

;;;;
;;      (m @font-size              t    #'interpret-font-size           t     :medium)
;;      (m @line-height            t    #'interpret-line-height         nil   '(* . 1.2))
;;      (m @background-position    nil  #'interpret-background-position nil   '((:% . 0) . (:% . 0)))
;;      (m @clip                   nil  #'interpret-clip                nil   :auto)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cooking-function (property)
  `(lambda (value)
    (cond ,@(mapcar (lambda (cooking)
                      (list (cooking-applicable-if cooking)
                            (cooking-value cooking)))
                    (remove-if-not (lambda (x)
                                     (eql (cooking-property x) property))
                                   *cookings*))
          (t
           value))))


(defmacro define-length-cooking (prop)
  `(define-cooking ,prop
    :applicable-if (and (consp value)
                    (member (car value)
                     '(:px :em :ex :in :cm :mm :pt :pc :canvas-h-percentage :canvas-v-percentage)))
    :value         (new-interpret-length value
                    device (prop font-size) pt dpi)))

(defmacro define-percentage-cooking (prop &key base)
  `(define-cooking ,prop
    :applicable-if (and (consp value) (eql (car value) ':%))
    :value         (* 1/100 ,base (cdr value))))


;; $Log$
;; Revision 1.3  2003/03/13 19:29:17  gilbert
;; lots of hacking
;;
;; Revision 1.2  2002/07/29 12:42:30  gilbert
;; - NEW-INTERPRET-LENGTH no actually uses its 'font-size' argument
;;

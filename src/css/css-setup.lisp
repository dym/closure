;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CSS; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Initial setup of css style attributes
;;;   Created: 1998-06-18
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: GPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1998,1999 by Gilbert Baumann

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

(defparameter *minimum-width* 30)

;;;; ------------------------------------------------------------------------------------------

(defvar *device*)
(defvar *style-sheet*)

(defun ensure-style (pt)
  (unless (sgml:pt-cache pt)
    (setf (sgml:pt-cache pt)
      (make-array %%--number-- :initial-element nil))
    (setup-style/1 *device* *style-sheet* pt)))

(defmacro set-style-attr (pt att value)
  (let ((g (gensym)))
    (cond ((and (consp att) (eq (car att) 'quote))
           `(LET ((,g ,pt))
                 (UNLESS (SGML:PT-CACHE (THE SGML::PT ,g))
                         (SETF (SGML:PT-CACHE (THE SGML::PT ,g))
                               (MAKE-ARRAY %%--NUMBER-- :INITIAL-ELEMENT NIL #|*NULL*|# ))
                         (setup-style/1 *device* *style-sheet* ,g))
                 (SETF (SVREF (SGML:PT-CACHE (THE SGML::PT ,g)) ,(symbol-value (cadr att)))
                       ,value)))
          (t
           `(LET ((,g ,pt))
                 (UNLESS (SGML:PT-CACHE (THE SGML::PT ,g))
                         (SETF (SGML:PT-CACHE (THE SGML::PT ,g)) 
                               (MAKE-ARRAY %%--NUMBER-- :INITIAL-ELEMENT NIL #|*NULL*|# ))
                         (setup-style/1 *device* *style-sheet* ,g))
                 (SETF (SVREF (SGML:PT-CACHE (THE SGML::PT ,g))
                              (SYMBOL-VALUE ,att)) ,value))))))

(defun create-pt-cache (pt)
  (setf (sgml:pt-cache (the sgml::pt pt))
        (make-array %%--number-- :initial-element nil #|*null*|# ))
  (setup-style/1 *device* *style-sheet* pt))

(defmacro style-attr (pt att &optional default)
  (let ((g (gensym)))
    (cond ((and (consp att) (eq (car att) 'quote))
           `(LET ((,g ,pt))
                 (UNLESS (SGML:PT-CACHE (THE SGML::PT ,g))
                         (CREATE-PT-CACHE ,g))
                 (let ((,g (SVREF (SGML:PT-CACHE (THE SGML::PT ,g))
                                  ,(symbol-value (cadr att)))))
                   (if ,g ,g ,default)) ))
          (t
           `(LET ((,g ,pt))
                 (UNLESS (SGML:PT-CACHE (THE SGML::PT ,g))
                         (SETF (SGML:PT-CACHE (THE SGML::PT ,g))
                               (MAKE-ARRAY %%--NUMBER-- :INITIAL-ELEMENT NIL #|*NULL*|# ))
                         (setup-style/1 *device* *style-sheet* ,g))
                 (if (SGML:PT-CACHE (THE SGML::PT ,g))
                     (let ((,g (SVREF (SGML:PT-CACHE (THE SGML::PT ,g)) (SYMBOL-VALUE ,att))))
                       (if ,g ,g ,default))
                   ,default))))))

(defmacro pt-cache-ref (pt slot)
  `(LOCALLY 
    (DECLARE (OPTIMIZE (SPEED 3) (SAFETY 0)))
    (AREF (THE (SIMPLE-ARRAY T (,%%--NUMBER--))
               (SGML:PT-CACHE (THE SGML::PT ,pt)))
          (THE (INTEGER 0 (,%%--NUMBER--)) ,slot))))

(defmacro style-attr (pt att &optional default)
  (let ((g (gensym)))
    (cond ((and (consp att) (eq (car att) 'quote))
           `(LET ((,g ,pt))
                 (UNLESS (SGML:PT-CACHE (THE SGML::PT ,g))
                         (CREATE-PT-CACHE ,g))
                 ,(if default
                     `(let ((,g (PT-CACHE-REF ,g ,(symbol-value (cadr att)))))
                        (if ,g ,g ,default))
                    `(PT-CACHE-REF ,g ,(symbol-value (cadr att))))))
          (t
           `(LET ((,g ,pt))
                 (UNLESS (SGML:PT-CACHE (THE SGML::PT ,g))
                         (SETF (SGML:PT-CACHE (THE SGML::PT ,g))
                               (MAKE-ARRAY %%--NUMBER-- :INITIAL-ELEMENT NIL #|*NULL*|# ))
                         (setup-style/1 *device* *style-sheet* ,g))
                 (if (SGML:PT-CACHE (THE SGML::PT ,g))
                     (let ((,g (SVREF (SGML:PT-CACHE (THE SGML::PT ,g)) (SYMBOL-VALUE ,att))))
                       (if ,g ,g ,default))
                   ,default))))))

(defmacro %style-attr (pt att &optional default)
  (let ((g (gensym)))
    (cond ((and (consp att) (eq (car att) 'quote))
           `(LET ((,g ,pt))
                 ,(if default
                      `(let ((,g (PT-CACHE-REF ,g ,(symbol-value (cadr att)))))
                         (if ,g ,g ,default))
                    `(PT-CACHE-REF ,g ,(symbol-value (cadr att))))))
          (t
           `(LET ((,g ,pt))
                 (let ((,g (SVREF (SGML:PT-CACHE (THE SGML::PT ,g)) (SYMBOL-VALUE ,att))))
                   (if ,g ,g ,default)) )))))

(defmacro %set-style-attr (pt att value)
  (let ((g (gensym)))
    (cond ((and (consp att) (eq (car att) 'quote))
           `(LET ((,g ,pt))
                 (SETF (SVREF (SGML:PT-CACHE (THE SGML::PT ,g)) ,(symbol-value (cadr att)))
                       ,value)))
          (t
           `(LET ((,g ,pt))
                 (SETF (SVREF (SGML:PT-CACHE (THE SGML::PT ,g))
                              (SYMBOL-VALUE ,att)) ,value))))))


;;;; ------------------------------------------------------------------------------------------
;;;;  Setting up the Style (part I)
;;;;

(defparameter %%minimum-width (+ *n-attr* 0))
(defparameter %%maximum-width (+ *n-attr* 1))

(defvar *dpi*)                          ;to be bound in SETUP-STYLE/1

(eval-when (compile load eval)
  (defparameter %%--number-- (+ *n-attr* 2)))

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
  (unless (eq (sgml:gi pt) :pcdata)
    (setf (sgml:pt-cache pt) nil)
    (dolist (k (sgml:pt-children pt))
      (kill-style k))))

(defvar *first-line-element*
    nil
  "An element (parse tree node) considered to have the first-line pseudo class.")

(defun setup-style (device style-sheet pt &key (first-line-p nil))
  (setf (sgml:pt-cache pt)
    (make-array %%--number-- :initial-element nil))  
  (let ((*first-line-p* first-line-p))
    (unless (eq (sgml:gi pt) :pcdata)
      (setup-style/1 device style-sheet pt)
      (dolist (k (sgml:pt-children pt))
        (setup-style device style-sheet k :first-line-p first-line-p)))) )

;;; ---- Some utils --------------------------------------------------------------------------

(defsubst percentage-p (x)
  (and (consp x) (eq (car x) :%)))

(defsubst block-element-p (pt)
  (member (display pt) '(:block :list-item)))

(defsubst display (pt)
  (cond ((eq (sgml:gi pt) :pcdata)
	 :pcdata)
	(t
	 (style-attr pt '@display))))

;;; -------------------------------------------------------------------------------------------

(defun class-eq (x y)
  (equalp x y))

(defun id-eq (x y)
  (equalp x y))

(defun collect-gis (as)
  (let ((res nil))
    (dolist (k as)
      (pushnew (singleton-selector-gi (car (assignment-selector k))) res))
    (remove nil res)))

(defun passt-gi? (gi k)
  (or (null (singleton-selector-gi (car (assignment-selector k))))
      (eq (singleton-selector-gi (car (assignment-selector k))) gi)))

(defun collect-cls (as)
  (let ((res nil))
    (dolist (k as)
      (pushnew (singleton-selector-class (car (assignment-selector k)))
               res
               :test #'class-eq))
    (remove nil res)))

(defun passt-class? (class k)
  (or (null (singleton-selector-class (car (assignment-selector k))))
      (class-eq class (singleton-selector-class (car (assignment-selector k))))))

(defun collect-ids (as)
  (let ((res nil))
    (dolist (k as)
      (pushnew (singleton-selector-id (car (assignment-selector k)))
               res
               :test #'id-eq))
    (remove nil res)))

(defun passt-id? (id k)
  (or (null (singleton-selector-id (car (assignment-selector k))))
      (id-eq (singleton-selector-id (car (assignment-selector k))) id)))


(defun make-case-insn (insn test-fn clauses default-clause)
  (let ((q (make-hash-table :test test-fn)))
    (dolist (k clauses)
      (destructuring-bind (key action) k
        (setf (gethash key q) action)))
    `(,insn ,q ,default-clause)))

(defun make-case-gi (clauses default-clause)
  (make-case-insn 'case-gi* #'eq clauses default-clause))

(defun make-case-class (clauses default-clause)
  (make-case-insn 'case-class* #'equalp clauses default-clause))

(defun make-case-id (clauses default-clause)
  (make-case-insn 'case-id* #'equalp clauses default-clause))

#+CMU
;; CMUCL kann (make-hash-table :test #'equalp) nicht ab.
(progn
  #+(OR)
  (defun make-case-gi (clauses default-clause)
    `(case-gi ,@clauses (* ,default-clause)))

  (defun make-case-class (clauses default-clause)
    `(case-class ,@clauses (* ,default-clause)))

  (defun make-case-id (clauses default-clause)
    `(case-id ,@clauses (* ,default-clause)))
  )

(defun select-on-gi (as &optional (next #'identity))
  (let ((gis (collect-gis as)))
    (labels ((select-gi (gi)
               (mapcar (lambda (a)
                         (setf a (copy-assignment a))
                         (setf (assignment-selector a)
                           (mapcar #'copy-singleton-selector (assignment-selector a)))
                         (setf (singleton-selector-gi (car (assignment-selector a))) nil)
                         a)
                       (remove-if-not (lambda (x) (passt-gi? gi x)) as))))
      (make-case-gi
       (mapcar (lambda (gi)
                 (let ((r (select-gi gi)))
                   (list gi 
                         (funcall next r))))
               gis)
       (funcall next (select-gi '*))) )))

(defun select-on-class (as &optional (next #'identity))
  (let ((cls (collect-cls as)))
    (labels ((select-class (class)
               (mapcar (lambda (a)
                         (setf a (copy-assignment a))
                         (setf (assignment-selector a)
                           (mapcar #'copy-singleton-selector (assignment-selector a)))
                         (setf (singleton-selector-class (car (assignment-selector a))) nil)
                         a)
                       (remove-if-not (lambda (x) (passt-class? class x)) as))))
      (if (null cls)
          (funcall next (select-class '*))
        (make-case-class
         (mapcar (lambda (class)
                   (list class (funcall next (select-class class))))
                 cls)
         (funcall next (select-class '*)) )))))

(defun select-on-id (as &optional (next #'identity))
  (let ((ids (collect-ids as)))
    (labels ((select-id (id)
               (mapcar (lambda (a)
                         (setf a (copy-assignment a))
                         (setf (assignment-selector a)
                           (mapcar #'copy-singleton-selector (assignment-selector a)))
                         (setf (singleton-selector-id (car (assignment-selector a))) nil)
                         a)
                       (remove-if-not (lambda (x) (passt-id? id x)) as))))
      (if (null ids)
          (funcall next (select-id '*))
        (make-case-id
         (mapcar (lambda (id)
                   ;; hmm hatten wir das nicht gefixed?!
                   (list id ;;(intern (string-upcase id) :keyword);xxx
                         (funcall next (select-id id))))
                 ids)
         (funcall next (select-id '*))) )) ) )
    
(defun select-on-rest (as)
  `(do .,
       (mapcar (lambda (x)
                 (cond ((and (= (length (assignment-selector x)) 1)
                             (null (singleton-selector-gi (car (assignment-selector x))))
                             (null (singleton-selector-id (car (assignment-selector x))))
                             (null (singleton-selector-class (car (assignment-selector x))))
                             (null (singleton-selector-pseudo-class
                                    (car (assignment-selector x)))))
                        `(IS ,(assignment-slot x)
                             ,(assignment-value x)
                             ,(assignment-importantp x) ))
                       (t
                        `(IF ,(assignment-selector x)
                             ,(assignment-slot x)
                             ,(assignment-value x)
                             ,(assignment-importantp x))) ))
        as)))

(defun make-pcode (s)
  (let ((as (style-sheet->assignments-list s)))
    (let ((res (select-on-gi 
                as (lambda (x) 
                     (select-on-class 
                      x (lambda (x) 
                          (select-on-id 
                           x (lambda (x)
                               (select-on-rest x)))))))))
      res)))

(defun look/aux (style-sheet pt res)
  (when (style-sheet-super-sheet style-sheet) 
    (look/aux (style-sheet-super-sheet style-sheet) pt res))
  (dolist (k (style-sheet-imported-sheets style-sheet))
    (lookup-by-pcode k (or (style-sheet-pcode k) (setf (style-sheet-pcode k) (make-pcode k)))
                     pt
                     res))
  (lookup-by-pcode style-sheet
                   (or (style-sheet-pcode style-sheet)
                       (setf (style-sheet-pcode style-sheet)
                         (make-pcode style-sheet)))
                   pt
                   res))

(defun look/aux (style-sheet pt res)
  (when (style-sheet-super-sheet style-sheet) 
    (look/aux (style-sheet-super-sheet style-sheet) pt res))
  (dolist (k (style-sheet-imported-sheets style-sheet))
    (look/aux k pt res)) 
  (lookup-by-pcode style-sheet
                   (or (style-sheet-pcode style-sheet)
                       (setf (style-sheet-pcode style-sheet)
                         (make-pcode style-sheet)))
                   pt
                   res))

(defun old-lookup-all-style (style-sheet pt is res)
  (let ()
    (declare (type (simple-array t (*)) res))
    (look/aux style-sheet pt res)
    (dolist (k is)
      (let ((slot (car k))
            (value (cdr k)))
        ;; importantp??
        (unless (car (aref res (symbol-value slot)))
          (setf (aref res (symbol-value slot)) (cons nil value)))))
    (dotimes (i (length res))
      (declare (type fixnum i))
      (setf (aref res i) (cdr (aref res i))))
    res))

(defun lookup/assign (sheet res slot value importantp)
  (declare (type (simple-array t (*)) res))
  (setf value
    (cond ((and (or (eq slot 'css:@background-image)
                    (eq slot 'css:@list-style-image))
                (stringp value))
           (url:merge-url (url:parse-url value) (style-sheet-base-url sheet)))
          (t
           value)))
  (setf slot (symbol-value slot))
  (locally
      (declare (type fixnum slot))
    (cond (importantp
           (setf (aref res slot) (cons t value)))
          (t
           (unless (car (aref res slot))
             (setf (aref res slot) (cons nil value)))))))

(defun lookup-by-pcode (sheet pcode pt res)
  (ecase (car pcode)
    ((CASE-GI*)
     (let ((x (gethash (sgml:gi pt) (cadr pcode) :nix)))
       (if (eq x :nix)
           (lookup-by-pcode sheet (caddr pcode) pt res)
         (lookup-by-pcode sheet x pt res))))

    ((CASE-CLASS*)
     (let ((x (gethash (r2::pt-attr* pt :class) (cadr pcode) :nix)))
       (if (eq x :nix)
           (lookup-by-pcode sheet (caddr pcode) pt res)
         (lookup-by-pcode sheet x pt res))))
    
    ((CASE-ID*)
     (let ((x (gethash (r2::pt-attr* pt :id) (cadr pcode) :nix)))
       (if (eq x :nix)
           (lookup-by-pcode sheet (caddr pcode) pt res)
         (lookup-by-pcode sheet x pt res))))

    ((CASE-CLASS)
     (dolist (k (cdr pcode))
       (when (or (eq (car k) '*)
                 (class-eq (r2::pt-attr* pt :class) (car k)))
         (return (lookup-by-pcode sheet (cadr k) pt res)))))

    ((CASE-ID)
     (dolist (k (cdr pcode))
       (when (or (eq (car k) '*)
                 (class-eq (r2::pt-attr* pt :id) (car k)))
         (return (lookup-by-pcode sheet (cadr k) pt res)))))

    ((IF)
     (when (selector-matches-p (second pcode) pt)
       (lookup/assign sheet res (third pcode) (fourth pcode) (fifth pcode))))

    ((IS)
     ;; kludge
     (lookup/assign sheet res (second pcode) (third pcode) (fourth pcode)))

    ((DO)
     (dolist (k (cdr pcode))
       (lookup-by-pcode sheet k pt res))) ))
     

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


;;; length values

#||
(define-cooking border-top-color
    :applicable-if (null @border-top-color)
    :value         @color)


||#

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
          (let* ((is (r2::pt-implicit-style (r2::rc-document r2::*rcontext*) pt))
                 (ss (r2::pt-style-style (r2::rc-document r2::*rcontext*) pt))
                 (cache (sgml:pt-cache pt))
                 (style cache)
                 (*dpi* (r2::device-dpi *device*))
                 (dpi (r2::device-dpi *device*)))
            (lookup-all-style style-sheet pt is ss style)
            (and (sgml:pt-parent pt) 
                 (ensure-style (sgml:pt-parent pt)))
            (macrolet ((prop (x)
                         `(svref cache ,(symbol-value (intern (format nil "@~A" x) (find-package :css)))))
                       (parent-prop (x)
                         `(%style-attr (sgml:pt-parent pt)
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
                                          (if (and ,inheritedp (sgml:pt-parent pt))
                                              (%style-attr (sgml:pt-parent pt) ',att)
                                              (,cooker ,default-value))))
                                  (setf (svref cache ,(symbol-value att)) value))))))
                        order)))) )))

(defun new-interpret-length (value device font-size pt dpi)
  (cond ((consp value)
	 (let ((unit (car value))
	       (a (cdr value)))
	   (case unit
	     (:px (* 1 a))
	     (:em 
              (cond ((and pt (not (style-attr pt '@font-size)))
                     (warn "In ~S: (style-attr pt '@font-size) not available -- fix your programm."
                           'interpret-length/low)
                     (round (* a dpi 12) 72))
                    (pt
                     (* a (style-attr pt '@font-size)))
                    (t
                     (round (* a dpi 12) 72))))
	     (:ex 
              (cond ((null pt)
                     (warn "In ~S: Oops ex without having an pt? -- fix your programm."
                           'interpret-length/low)
                     (round (* a dpi 8) 72))
                    ((not (style-attr pt '@font-size))
                     (warn "In ~S: Oops ex without having a font? -- fix your programm."
                           'interpret-length/low)
                     (round (* a dpi 8) 72))
                    (t
                     (* 1/2 (style-attr pt '@font-size) a) ))) ;xxx
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
                    (member (car value) '(:px :em :ex :in :cm :mm :pt :pc :canvas-h-percentage :canvas-v-percentage)))
    :value         (new-interpret-length value
                    device (prop font-size) pt dpi)))

(defmacro define-percentage-cooking (prop &key base)
  `(define-cooking ,prop
    :applicable-if (and (consp value) (eql (car value) ':%))
    :value         (* 1/100 ,base (cdr value))))


;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: RENDERER; Encoding: utf-8; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Table Renderer
;;;   Created: 1999-05-25
;;;    Author: Gilbert Baumann <gilbert@base-engineering.com>
;;;   License: MIT style (see below)
;;; ---------------------------------------------------------------------------
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

(in-package :RENDERER)

;;;; BUGS

;; When the table cell's width/height got finally adjusted the content
;; (and the surrounding border) are already rendered. => The cells
;; appear to have the wrong size. The solution would be if the table
;; renderer either can modify the appropriate output record or renders
;; the cell border itself, after the cells have their assigned size.

;; Parameters of the table renderer

(defparameter *default-table-cell-spacing* 0)
(defparameter *default-table-cell-padding* 3)
(defparameter *default-table-border*       0)
(defparameter *collapse-p*                 t)
(defparameter *table-border-style*         :solid)
(defparameter *do-not-render-empty-cells-p* nil)

(defparameter *treat-100%-special-p*
    ;;fixme: doc
    nil)

;; We need to distinguish two models:
;;  a. tables with fixed width
;;  b. tables with dynamic width (width = :auto)
;;

;; In principle we can proceed as follows:
;;
;;  1. Assign all absoulte column widths
;;
;;  2. We might have some columns left which not yet have an assigned
;;     width, so assign what is left to these columns.

;; CAPTION is handled as follows: A CAPTION element can be thought as
;; an additional table cell, which has a COLSPAN over all columns, if
;; it is at top/bottom. Or which has a ROWSPAN over all rows, if it is
;; to be typeset left or right of the table.

(defstruct table
  colgroups                     ;List of colgroup definition
  rowgroups                     ;List of rowgroup
  number-of-columns             ;
  column-widthen                ;Allotted column widthen
  ;; Some attribute values      
  attr-frame                    ;one of :void :above :below 
                                ; :hsides :vsides :lhs :rhs :box :border
  attr-rules                    ;one of :none :groups :rows :cols :all
  attr-border                   ;integer
  attr-cell-spacing             ;integer
  attr-cell-padding             ;integer
  %column-minimum-widthen       ;computed on demand
  %column-maximum-widthen       ;dito
  caption                       ;the <CAPTION> element
  pt                            ;the whole <TABLE> element
  100%-hack-p)                  ;wether we applied the width='100%' hack?

(defstruct col
  width                         ;One of (:% . N), (* . N), (:px . N), NIL
  align                         
  valign)                       
                                
(defstruct colgroup             
  cols)                         ;list of col
                                
(defstruct rowgroup             
  kind                          ;One of :THEAD, :TFOOT, :TBODY
  cells                         ;List of CELL
  number-of-rows)               
                                
(defstruct cell                 
  row-index col-index           ;Position (0-origin)
  row-span col-span             ;Span
  content                       ;The TD or TH element
  height                        ;height this cell needs
  minimum-width                 ;
  maximum-width                 ;
  valign
  bbox
  %percentage-width
  or                            ;CLIM output record
  )

(defun cell-percentage-width (cell)
  (or (cell-%percentage-width cell)
      (setf (cell-%percentage-width cell)
        (let ((w (pt-attr/length (cell-content cell) :width nil)))
          (cond ((and (consp w) (eq (car w) :%))
                 (max 1 (cdr w)))
                (t
                 0))))))


;; Notes:
;; cell-height is the iheight plus two times the cellpadding.

;;; Parsing <TABLE> into TABLE structure

(defun parse-table (pt)
  ;;This is not checked below, so do it here.
  (assert (every (rcurry #'member '(:THEAD :TBODY :TFOOT :COL :COLGROUP :CAPTION)) 
                 (mapcar #'sgml:gi (sgml:pt-children pt))))
  (when (> (count :table-caption (element-children pt) :key #'css:display) 1)
    (warn "A TABLE element may only contain one CAPTION element."))
  (when (member :table-caption (cdr (element-children pt)) :key #'css:display)
    (warn "The CAPTION element is only permitted immediately after the TABLE start tag."))
  (let* ((colgroups (parse-colgroups-from-table pt))
         (rowgroups (parse-rowgroups-from-table pt colgroups))
         (table (make-table :pt pt
                            :colgroups colgroups
                            :rowgroups rowgroups)))
    (setf (table-number-of-columns table)
      (reduce #'max (mapcar (lambda (rowgroup)
                              (reduce #'max
                                      (mapcar (lambda (cell)
                                                (+ (cell-col-index cell) (cell-col-span cell)))
                                              (rowgroup-cells rowgroup))
                                      :initial-value 0))
                            (table-rowgroups table))
              :initial-value 0))
    (unless (or (null (table-colgroups table))
                (eql (table-number-of-columns-by-colgroups table)
                     (table-number-of-columns table)))
      (warn "Number of columns in COLGROUP specification does not match ~
             actual number of columns.~%~
             (by COLGROUP gives ~D columns in contrast to ~D actual columns)."
             (table-number-of-columns-by-colgroups table)
             (table-number-of-columns table)))
    
    ;; Assign default colgroup, if the user had not specifed any <COLGROUP> element.
    (when (null (table-colgroups table))
      (setf (table-colgroups table)
        (list (make-colgroup :cols (loop for i from 0 to (1- (table-number-of-columns table))
                                       collect (make-col :width nil))))))

    (fetch-td/th-width-attribute table)
    (fetch-frame/rules/border table pt)
    (fetch-cell-spacing table pt)

    ;; caption
    (dolist (k (sgml:pt-children pt))
      (cond ((eql (css:display k) :table-caption)
             (setf (table-caption table) k))))

    (when (table-caption table)
      (patch-caption-to-table table))
    
    ;; all done
    table))

(defun patch-caption-to-table (table)
  (case (pt-attr/enum (table-caption table) :align :top '(:top :bottom :left :right))
    ((:TOP)
     (patch-caption/top table))
    ((:BOTTOM)
     (patch-caption/bottom table))
    ((:LEFT)
     (patch-caption/left table))
    ((:RIGHT)
     (patch-caption/right table)) ))

(defun patch-caption-to-table (table)
  table
  ;; xxx
  #||
  (case (style-attr (table-caption table) 'css:@caption-side)
    ((:TOP)
     (patch-caption/top table))
    ((:BOTTOM)
     (patch-caption/bottom table))
    ((:LEFT)
     (patch-caption/left table))
    ((:RIGHT)
     (patch-caption/right table)) )
  ||#
  )

(defun patch-caption/top (table)
  (setf (table-rowgroups table)
    (cons (make-rowgroup
           :kind :caption
           :cells (list (make-cell :row-index 0 :col-index 0
                                   :row-span 1  :col-span (table-number-of-columns table)
                                   :content (table-caption table)
                                   :valign :middle))
           :number-of-rows 1)
          (table-rowgroups table))))

(defun patch-caption/bottom (table)
  (setf (table-rowgroups table)
    (nconc (table-rowgroups table)
           (list (make-rowgroup
                  :kind :caption
                  :cells (list (make-cell :row-index 0 :col-index 0
                                          :row-span 1  :col-span (table-number-of-columns table)
                                          :content (table-caption table)
                                          :valign :middle))
                  :number-of-rows 1)))))

(defun patch-caption/left (table)
  ;; first shift all cells right
  (map-table-cells 
   (lambda (cell)
     (incf (cell-col-index cell) 1))
   table)
  (incf (table-number-of-columns table) 1)
  ;; then fix colgroups
  (setf (table-colgroups table)
    (cons (make-colgroup 
           :cols (list (make-col :width nil :align :right :valign :middle)))
          (table-colgroups table)))
  ;; add caption cell
  (dolist (k (table-rowgroups table))
    (when (eq (rowgroup-kind k) :tbody)
      (push (make-cell :row-index 0 
                       :col-index 0
                       :row-span (rowgroup-number-of-rows k)
                       :col-span 1
                       :content (table-caption table)
                       :valign :middle)
            (rowgroup-cells k))
      (return))))

(defun patch-caption/right (table)
  ;; make another COL element;
  (setf (table-colgroups table)
    (nconc (table-colgroups table)
           (list (make-colgroup 
                  :cols (list (make-col :width nil :align :right :valign :middle))))))
  ;; add caption cell
  (dolist (k (table-rowgroups table))
    (when (eq (rowgroup-kind k) :tbody)
      (push (make-cell :row-index 0 
                       :col-index (table-number-of-columns table)
                       :row-span (rowgroup-number-of-rows k)
                       :col-span 1
                       :content (table-caption table)
                       :valign :middle)
            (rowgroup-cells k))
      (return)))
  ;; keep number of columns current
  (incf (table-number-of-columns table) 1) )

(defun fetch-td/th-width-attribute (table)
  ;; We map over all cells and copy the `width' attributes to the corresponding
  ;; COL-definitions. If the COL-definition is in conflict with the `width'
  ;; attribute, emit warning. (This also catches multiple conflicting `width'
  ;; definitions for TD elements -- the first seen wins).
  ;; xxx this is now handled differently
  table
  #+NIL
  (map-table-cells 
    (lambda (cell)
      (let ((w (pt-attr/length (cell-content cell) :width nil)))
        (when (and *treat-100%-special-p* 
                   w
                   (equal w '(:% . 100)))
          (setf (table-100%-hack-p table) t)
          (setf w '(* . 1)))

        '(when (eq (car w) ':%)
          (let (nw)
            (setf nw (cons :px (round (* (cdr w)
                                         (r2::device-canvas-width 
                                          (rc-device *rcontext*)))
                                      100)))
            (setf w nw)
            ))
        (when w
          (let ((span (cell-col-span cell))
                (col (colgroups-col-spec (table-colgroups table) (cell-col-index cell))))
            (cond ((not (eql span 1))
                   (warn "Setting WIDTH on an TD/TH element with COLSPAN=~S is not supported."
                         span))
                  ((equal (col-width col) w))
                  ((col-width col)
                   (warn "Make up your mind: No WIDTH attribute on TD/TH elements with an~%~
                         existing WIDTH attribute on a corresponding COL element or a~%~
                         previous WIDTH definition on a TD/TH element, please.~%~
                         These given values are in conflict: ~S vs. ~S; the former won."
                         (unparse-html-length (col-width col))
                         (unparse-html-length w)))
                  (t 
                   (setf (col-width col) w)))))))
    table))

(defun fetch-cell-spacing (table pt)
  ;; cell-spacing
  (setf (table-attr-cell-spacing table)
    (let ((r (pt-attr/length pt :cellspacing nil)))
      (cond ((eql (car r) :%)
             (pt-attr-warn pt "This implementation lacks percentage values for `CELLSPACING'.")
             *default-table-cell-spacing*)
            ((eql (car r) :px)
             (cdr r))
            (t 
             *default-table-cell-spacing*))))
  ;; Same for cell-padding
  (setf (table-attr-cell-padding table)
    (let ((r (pt-attr/length pt :cellpadding nil)))
      (cond ((eql (car r) :%)
             (pt-attr-warn pt "This implementation lacks percentage values for `CELLPADDING'.")
             *default-table-cell-padding*)
            ((eql (car r) :px)
             (cdr r))
            (t 
             *default-table-cell-padding*)))) )

;; From the HTML-4.0 Spec:

;;    The following settings should be observed by user agents for backwards
;;    compatibility.
;;    
;;       * Setting border="0" implies frame="void" and, unless otherwise
;;         specified, rules="none".
;;       * Other values of border imply frame="border" and, unless otherwise
;;         specified, rules="all".
;;       * The value "border" in the start tag of the TABLE element should be
;;         interpreted as the value of the frame attribute. It implies rules="all"
;;         and some default (non-zero) value for the border attribute.

;; I read (implement) the relevant rules as:
;; 
;;  1 Setting border="0" implies, unless otherwise specified,
;;    frame="void" and rules="none".
;; 
;;  2 Specifing a non-zero value for `border', implies, unless otherwise
;;    specified, frame="border" and rules="all".
;; 
;;  3 frame="border", implies, unless otherwise specified, rules="all".
;; 
;;  4 If `frame' has not yet an assigned value according to the above rules,
;;    it defaults to "void".
;; 
;;  5 If `rules' has not yet an assigned value according to the above rules,
;;    it defaults to "none".
;; 
;;  6 If `frame' is not "void" or `rules' is not "none", and `border' is
;;    not specified, the `border' attribute will be assigned some
;;    (non-zero) default value.

(defun fetch-frame/rules/border (table pt)
  (let ((frame  (pt-attr/table.frame pt :frame nil))
        (rules  (pt-attr/table.rules pt :rules nil))
        (border (pt-attr/pixels pt :border nil)))
    (cond ((eql border 0)
           (setf frame (or frame :void))
           (setf rules (or rules :all)) )
          ((not (null border))
           (setf frame (or frame :border))
           (setf rules (or rules :all)) ))
    (cond ((eq frame :border)
           (setf rules (or rules :all))))
    (setf frame (or frame :void))
    (setf rules (or rules :none))
    (setf border (or border 2))
    (setf (table-attr-border table) border)
    (setf (table-attr-frame table) frame)
    (setf (table-attr-rules table) rules)))

(defun table-number-of-columns-by-colgroups (table)
  (reduce #'+ (mapcar (lambda (colgroup)
                        (length (colgroup-cols colgroup)))
                      (table-colgroups table))))

(defun parse-colgroups-from-table (pt)
  (assert (eq (sgml:gi pt) :TABLE))
  (let ((res nil))
   (dolist (k (sgml:pt-children pt) (nreverse res))
     (case (sgml:gi k)
       ((:COL)
        (push (make-colgroup :cols (parse-col k nil)) res))
       ((:COLGROUP)
        (push (parse-colgroup k) res))))) )

(defun parse-colgroup (pt)
  (assert (every (curry #'eq :COL) (mapcar #'sgml:gi (sgml:pt-children pt))))
  (let ((cols nil))
    (cond ((null (sgml:pt-children pt))
           (let ((span   (pt-attr/integer pt :SPAN 1))
                 (width  (pt-attr/multi-length pt :WIDTH nil))
                 (align  (pt-attr/cell-halign pt :ALIGN nil))
                 (valign (pt-attr/cell-valign pt :VALIGN nil)))
             (unless (>= span 1)
               (warn "SPAN attribute had invalid value ~S, resetting to 1." span)
               (setf span 1))
             (dotimes (i span)
               (push (make-col :width width :align align :valign valign) cols))))
          (t
           (dolist (k (sgml:pt-children pt) cols)
             (setf cols (nconc cols (parse-col k pt))))))
    (make-colgroup :cols cols)))

(defun parse-col (pt parent)
  (let ((span   (pt-attr/integer pt :SPAN 1))

        ;; ???? ZZZ
        (width  (pt-attr/multi-length pt :WIDTH
                                      (and parent (pt-attr/multi-length parent :WIDTH nil))))
        (align  (pt-attr/cell-halign pt :ALIGN 
                                     (and parent (pt-attr/cell-halign parent :ALIGN nil))))
        (valign (pt-attr/cell-valign pt :VALIGN 
                                     (and parent (pt-attr/cell-valign parent :VALIGN nil)))))
    (unless (>= span 1)
      (warn "SPAN attribute had invalid value ~S, resetting to 1." span)
      (setf span 1))
    (let ((res nil))
      (dotimes (i span)
        (push (make-col :width width :align align :valign valign) res))
      res)))

(defun parse-rowgroups-from-table (table colgroups)
  (let ((theads nil) (tbodies nil) (tfoots nil))
    (dolist (k (sgml:pt-children table)
               (append (nreverse theads)
                       (nreverse tbodies)
                       (nreverse tfoots)))
      ;; Q: Are multiple THEAD/TFOOT element allowed?
      (case (sgml:gi k)
        ((:THEAD) 
         (push (parse-rowgroup table colgroups k) theads))
        ((:TFOOT)
         (unless (null tbodies)
           ;; cite
           (warn "TFOOT must appear before TBODY within a TABLE definition~%~
                  so that user agents can render the foot before receiving~%~
                  all of the (potentially numerous) rows of data."))
         (push (parse-rowgroup table colgroups k) tfoots))
        ((:TBODY)
         (push (parse-rowgroup table colgroups k) tbodies)) ))))

(defun td/th-empty-p (pt)
  (if *do-not-render-empty-cells-p*
      (every (lambda (k)
               (and (eq (sgml:gi k) :pcdata)
                    (every (lambda (x) (member x '(9 10 13 32)))
                           (sgml:pt-cdata k))))
             (sgml:pt-children pt))
    nil))

(defmacro my-some/list (fun list)
  (let ((k (gensym))
        (r (gensym))
        (fncll (cond ((and (consp fun) (eq (car fun) 'lambda)) (list fun))
                     ((and (consp fun) (eq (car fun) 'function)) (list (cadr fun)))
                     (t (list 'funcall fun)))))
    `(dolist (,k ,list nil)
       (let ((,r (,@fncll ,k)))
         (when ,r
           (return ,r))))))

(defun parse-rowgroup (table-pt colgroups tbody-pt)
  (let ((cells nil)                                     ;List of cells
        (row-index 0)                                   ;Current row index
        (col-index 0))                                  ;Current col index
    (labels ((cell-free-p (ri ci)
               ;; Is the cell at (ri,ci) free?
               (not (my-some/list
                     (lambda (cell)
                       (and (<= (cell-row-index cell) ri)
                            (< ri (+ (cell-row-index cell) (cell-row-span cell)))
                            (<= (cell-col-index cell) ci)
                            (< ci (+ (cell-col-index cell) (cell-col-span cell)))))
                     cells))) )
      (dolist (tr (sgml:pt-children tbody-pt))
        (ecase (sgml:gi tr)
          ((:TR)
           (setf col-index 0)
           (dolist (td (sgml:pt-children tr))
             (ecase (sgml:gi td)
               ((:TD :TH)
                (let ((col-span (pt-attr/integer td :colspan 1))
                      (row-span (pt-attr/integer td :rowspan 1)))
                  (unless (>= col-span 1)
                    (warn "COLSPAN was '~S' resetting to 1." col-span) (setf col-span 1))
                  (unless (>= row-span 1)
                    (warn "ROWSPAN was '~S' resetting to 1." row-span) (setf row-span 1))
                  ;;Seek the next free cell -- dumb but correct.
                  (do () ((cell-free-p row-index col-index)) (incf col-index))
                  (let ((cell (make-cell :row-index row-index :row-span row-span
                                   :col-index col-index :col-span col-span
                                   :content td)))
                    (setup-cell-alignment-attrs cell td tr tbody-pt colgroups table-pt)
                    (push cell cells))))))
           (incf row-index))))
      ;; parsing done
      ;; XXX Wir muessen noch rowspans die ueber `row-index' hinausgehen bescheiden.
      (unless (>= row-index 1)
        ;; Ist eigentlich schon durch DTD gesichert.
        (warn "Each row group must contain at least one row, defined by the TR element."))
      (make-rowgroup :kind (sgml:gi tbody-pt)
                     :cells (nreverse cells)
                     :number-of-rows row-index) )))

(defun setup-cell-alignment-attrs (cell td-pt tr-pt tbody-pt colgroups table-pt)
  (let ((align (or (pt-attr/cell-halign TD-pt :align nil)
                   (and colgroups
                        (col-align (colgroups-col-spec colgroups (cell-col-index cell))))
                   (pt-attr/cell-halign TR-pt :align nil)
                   (pt-attr/cell-halign TBODY-pt :align nil)
                   (pt-attr/cell-halign TABLE-pt :align nil)
                   ;;XXX hier sollte man lieber die angabe aus dem style-sheet nehmen
                   (if (eql (sgml:gi td-pt) :TH) :center :left)))
        (valign (or (pt-attr/cell-valign TD-pt :valign nil)
                    (pt-attr/cell-valign TR-pt :valign nil)
                    (pt-attr/cell-valign TBODY-pt :valign nil)
                    (and colgroups
                         (col-valign (colgroups-col-spec colgroups (cell-col-index cell))))
                    (pt-attr/cell-valign TABLE-pt :valign nil)
                    (if (eql (sgml:gi td-pt) :TH) :middle :top))))
    (css:set-style-attr (cell-content cell) 'css:@text-align
                    (case align
                      ((:left :right :center :justify) align)
                      (otherwise :left)))
    (setf (cell-valign cell) valign) ))

;;; Geometry der tabelle selbst ist noch falsch:
;;;  bei 'collapse' ist die tabelle eine rahmenbreite zu breit und hoch
;;;  sonst stimmt der aeussere rahmen nicht ganz.

;;; Horizontal geometry

;; Note that a possible width attribute of the TABLE element is
;; handled by #'pt-implicit-style.

(defun table-wanted-width (table www)
  ;; Das ist anders als CSS die breite normalerweise behandelt.
  (let ((r (if (eql (css:style-attr (table-pt table) 'css:@orig-width) :auto)
               :auto
             www)))
    r))

(defun colgroups-col-spec (colgroups i)
  ;; Return the `COL' structure for the `i'th column.
  ;; sometimes (when COLGROUPS and actual number of columns differ),
  ;; it happens to us, that `i' is beyond the number of columns; we
  ;; issue a warning then.
  (or (car (nthcdr i (reduce #'append (mapcar #'colgroup-cols colgroups))))
      (progn
        (warn "Column index ~S is out of bounds" i)
        (make-col :width nil :align nil :valign NIL))))

(defun table-col-spec (table i)
  ;; Return the `COL' structure for the `i'th column.
  (colgroups-col-spec (table-colgroups table) i))

(defun column-specified-width (table index)
  ;; Returns the specified width of the `i'th column.
  ;; Returns two values: 
  ;;   :percentage ; n     -- percentage value
  ;;   :proportional ; n   -- proportional value
  ;;   :absolute ; n       -- absolute value
  ;;   :minimum            -- take the minimum of the columns width
  ;;   :dynamic            -- dynamic column width
  (let ((w (col-width (table-col-spec table index))))
    (cond ((null w) 
           (values :dynamic))
          ((equal w '(* . 0)) 
           (values :minimum))
          ((eql (car w) :%)
           (values :percentage (cdr w)))
          ((eql (car w) :px)
           (values :absolute (cdr w)))
          ((eql (car w) '*)
           (values :proportional (cdr w)))
          ((error "OOPS -- bad value ~S in ~S slot" w 'col-width)) )) )
  
(defun table-column-minimum-width (table i)
  (aref (or (table-%column-minimum-widthen table)
            (setf (table-%column-minimum-widthen table)
              (calc-table-column-minimum-widthen table)))
        i))

(defun table-column-maximum-width (table i)
  (aref (or (table-%column-maximum-widthen table)
            (setf (table-%column-maximum-widthen table)
              (calc-table-column-maximum-widthen table)))
        i))

(defun map-table-cells (fun table)
  (dolist (rg (table-rowgroups table))
    (dolist (cell (rowgroup-cells rg))
      (funcall fun cell))))

#||
(defun calc-table-column-max/min-widthen (table what)
  (let ((n (table-number-of-columns table)))
    (let ((res (make-array n :initial-element 0)))
      (loop for span from 1 to n do
            (map-table-cells (lambda (cell)
                               (when (eql (cell-col-span cell) span)
                                 (let ((m (/ (ceiling (funcall what cell))
                                             span)))
                                   (dotimes (i span)
                                     (setf (aref res (+ (cell-col-index cell) i))
                                       (max (aref res (+ (cell-col-index cell) i)) m))))))
                             table))
      res)) )
||#

(defun calc-table-column-max/min-widthen (table what)
  (let ((n (table-number-of-columns table)))
    (let ((res (make-array n :initial-element 0)))
      (loop for span from 1 to n do
            (map-table-cells (lambda (cell)
                               (when (eql (cell-col-span cell) span)
                                 (let* ((yet (loop 
                                                 for i 
                                                 from (cell-col-index cell) 
                                                 to (1- (+ (cell-col-index cell)
                                                           (cell-col-span cell)))
                                                 sum
                                                   (aref res i)))
                                        (lack (- (funcall what cell) yet)))
                                   (cond ((> lack 0)
                                          (let ((m (/ (ceiling lack) span)))
                                            (dotimes (i span)
                                              (incf (aref res (+ (cell-col-index cell) i)) m))))))))
                             table))
      res)) )

(defun calc-table-column-minimum-widthen (table)
  (calc-table-column-max/min-widthen table #'cell-minimum-width))

(defun calc-table-column-maximum-widthen (table)
  (calc-table-column-max/min-widthen table #'cell-maximum-width))

(defun column-index-between-colgroups-p (table index)
  (let ((i 0))
    (do ((q (table-colgroups table) (cdr q)))
        ((null (cdr q)) nil)
      (incf i (length (colgroup-cols (car q))))
      (when (and (eql index i))
        (return t)))))

(defun table-setup-min/max-widthen (rc table)
  ;; Calculate minimum/maximum widthen:
  (map-table-cells (lambda (cl)
                     (setf
                         (cell-minimum-width cl) (pt-minimum-width rc (cell-content cl))
                         (cell-maximum-width cl)
                         (max (cell-minimum-width cl) 
                              (+ 0 (pt-maximum-width rc (cell-content cl))))));war +1 woher?
                   table))

(defun allot-dynamic-columns-to-fixed-width (table res width min max)
  (let ((w* (- width min))
        (d* (max 0 (- max min))))
    (cond ((< w* 0)
           ;; just take the minima
           (dotimes (i (length res))
             (when (null (aref res i))
               (setf (aref res i) (table-column-minimum-width table i))))
           res)
          (t
           (dotimes (i (length res))
             (when (null (aref res i))
               (setf (aref res i)
                 (let* ((cmin (table-column-minimum-width table i))
                        (cmax (table-column-maximum-width table i))
                        (d (max 0 (- cmax cmin))))
                   (cond ((= d* 0)
                          (+ cmin (/ w* (length res))))
                         (t
                          (+ cmin (* d (/ w* d*)))))))))
           res) )))

#||
(defun allot-dynamic-columns-to-fixed-width (table res width min max)
  ;; `min' ist Summe aller columnen-minima
  ;; `max' die der maxima
  ;;
  ;; Vorgehen:
  ;; Jede columne muß jetzt eine breite zugewiesen bekommen. Dabei
  ;; wollen ausgehend von den minima noch die fehlende breite im
  ;; verhältnis der differenz der extrema der columnen verteilen. oder
  ;; klarer:
  ;; Löse:
  ;;  λ_1 : λ_2 : .. : λ_n = δ_1 : δ_2 : .. : δ_n
  ;;  λ_1 + λ_2 + .. + λ_n = (width - Σ(min_i))
  ;; mit δ_i := (max_i - min_i)
  ;; dann ist
  ;; res_i := min_i + λ_i;
  ;; 
  ;; Man muß hier aufpassen, da δ_i u.U. verschwinden können.
  ;;
  (let ((w* (- width min))
        (d* (- max min)))
    ;; Bemerkung: da eigentlich immer sein sollte max_i >= min_i,
    ;; gilt Σδ = 0 => δ_i = 0
    )) 
||#
  

(defun allot-dynamic-columns (table width avail res)
  (let* ((n (table-number-of-columns table))
         (min (loop for i from 0 to (1- n) sum
                    (if (null (aref res i))
                        (table-column-minimum-width table i)
                      0)))
         (max (loop for i from 0 to (1- n) sum
                    (if (null (aref res i))
                        (table-column-maximum-width table i)
                      0))))
    ;; Zwei Verfahren
    (cond ((eql width :auto)
           (cond ((> min avail)
                  ;; Just take the minima
                  (dotimes (i (length res))
                    (when (null (aref res i))
                      (setf (aref res i) (table-column-minimum-width table i)))))
                 ((<= max avail)
                  ;; Just take the maxima
                  (dotimes (i (length res))
                    (when (null (aref res i))
                      (setf (aref res i) (table-column-maximum-width table i)))))
                 (t
                  (allot-dynamic-columns-to-fixed-width table res avail min max) ) ))
          (t
           (allot-dynamic-columns-to-fixed-width table res width min max)) ))
  res)

;;; Vertical Geometry

(defun calc-row-heights (rowgroup n)
  (let ((res (make-array n :initial-element 0)))
    (loop for s from 1 to n do
          (loop for c in (rowgroup-cells rowgroup) do
                (when (eql (cell-row-span c) s)
                  (let* ((there (+ (loop for i from (cell-row-index c)
                                         to (1- (+ (cell-row-index c) s)) 
                                         sum (aref res i)) ))
                         (needed (cell-height c)))
                    (when (> needed there)
                      (loop for i from (cell-row-index c) to (1- (+ (cell-row-index c) s))
                            do (incf (aref res i) (/ (- needed there) s))))))))
    res))

(defun calc-rowgroup-number-of-rows (rg)
  (reduce #'max (mapcar (lambda (cell)
                          (+ (cell-row-index cell) (cell-row-span cell)))
                        (rowgroup-cells rg))
          :initial-value 0))

;;; Decoration

(defun set-cell-border (cell bw top right bottom left)
  (let ((pt (cell-content cell)))
    (cond ((eql (css:display pt) :table-caption)
           nil)                         ;nothing to do.
          (t
           (css:set-style-attr pt 'css:@border-top-width    (if top bw 0))
           (css:set-style-attr pt 'css:@border-top-style    (if top *table-border-style* :none))
           (css:set-style-attr pt 'css:@border-right-width  (if right bw 0))
           (css:set-style-attr pt 'css:@border-right-style  (if right *table-border-style* :none))
           (css:set-style-attr pt 'css:@border-bottom-width (if bottom bw 0))
           (css:set-style-attr pt 'css:@border-bottom-style (if bottom *table-border-style* :none))
           (css:set-style-attr pt 'css:@border-left-width   (if left bw 0))
           (css:set-style-attr pt 'css:@border-left-style   (if left *table-border-style* :none) ))) ))

(defun setup-cell-borders (table bw)
  (let ((rules (table-attr-rules table))
        (frame (table-attr-frame table))
        (n (table-number-of-columns table)))
    (dolist (rg (table-rowgroups table))
      (let ((first-rg? (eq rg (first (table-rowgroups table))))
            (last-rg? (eq rg (car (last (table-rowgroups table))))))
        (let ((m (rowgroup-number-of-rows rg)))
          (dolist (cell (rowgroup-cells rg))
            (let* ((first-col? (eql (cell-col-index cell) 0))
                   (last-col?  (eql (+ (cell-col-index cell) (cell-col-span cell)) n))
                   (last-row?  (eql (+ (cell-row-index cell) (cell-row-span cell)) m))
                   (first-row? (eql (cell-row-index cell) 0))
                   ;; frame
                   (top (and (member frame '(:above :hsides :box :border))
                             first-rg? first-row?))
                   (rgt (and (member frame '(:vsides :rhs :box :border)) last-col?))
                   (btm (and (member frame '(:below :hsides :box :border)) last-rg? last-row?))
                   (lft (and (member frame '(:vsides :lhs :box :border)) first-col?)))
              ;; Eigentlich benoetigen wir hier den Frame garnicht, den koennen (muessen) wir
              ;; hinterher drumrum zeichen.
              (ecase rules
                (:none
                 (set-cell-border cell bw top rgt btm lft))
                (:groups
                 ;; rules between groups
                 (set-cell-border cell bw
                    (or top (and first-row? (not first-rg?)))
                    (or rgt (column-index-between-colgroups-p table (+ (cell-col-index cell)
                                                                       (cell-col-span cell))))
                    (or btm (and last-row? (or btm (not last-rg?))))
                    (or lft (column-index-between-colgroups-p table (cell-col-index cell))) ))
                (:rows
                 ;; rules between rows
                 (set-cell-border cell bw t rgt t lft))
                (:cols
                 ;; rules between columns
                 (set-cell-border cell bw top t btm t))
                (:all
                 ;; rules everywhere
                 (set-cell-border cell bw t t t t)) ))))))))

(defun setup-cell-padding (table padding)
  (map-table-cells (lambda (cell)
                     (let ((pt (cell-content cell)))
                       (css:set-style-attr pt 'css:@padding-top padding)
                       (css:set-style-attr pt 'css:@padding-right padding)
                       (css:set-style-attr pt 'css:@padding-bottom padding)
                       (css:set-style-attr pt 'css:@padding-left padding)))
                   table))

(defun setup-cell-spacing (table spacing)
  (map-table-cells (lambda (cell)
                     (let ((pt (cell-content cell)))
                       (css:set-style-attr pt 'css:@margin-top spacing)
                       (css:set-style-attr pt 'css:@margin-right spacing)
                       (css:set-style-attr pt 'css:@margin-bottom spacing)
                       (css:set-style-attr pt 'css:@margin-left spacing)))
                   table))

(defun setup-cell-attrs (table)
  (setup-cell-borders table (table-attr-border table))
  (setup-cell-spacing table (table-attr-cell-spacing table))
  (setup-cell-padding table (table-attr-cell-padding table)))

(defun the-four-table-frames-p (table)
  (ecase (table-attr-frame table)
    ((:void)   (values nil nil nil nil))   ;No sides. This is the default value.
    ((:above)  (values  t  nil nil nil))   ;The top side only.
    ((:below)  (values nil nil  t  nil))   ;The bottom side only.
    ((:hsides) (values  t  nil  t  nil))   ;The top and bottom sides only.
    ((:vsides) (values nil  t  nil  t ))   ;The right and left sides only.
    ((:lhs)    (values nil nil nil  t ))   ;The left-hand side only.
    ((:rhs)    (values nil  t  nil nil))   ;The right-hand side only.
    ((:box)    (values  t   t   t   t ))   ;All four sides.
    ((:border) (values  t   t   t   t )))) ;All four sides.


;;; ---- Tables -------------------------------------------------------------------------------

(defun table-pt-aw (rc pt)
  (let* ((table (parse-table pt)))
    (setup-cell-attrs table)
    (table-setup-min/max-widthen rc table)
    (let* ((res (make-skeleton-bbox-for-pt rc pt (- (rc-x1 rc) (rc-x0 rc)))))
      (declare (type bbox res))
      (setf (bbox-ix res) 0
            (bbox-iy res) 0)
      (let* ((www (bbox-width res))
             (x0 (find-x0-at rc (rc-y rc) (rc-x0 rc)))
             (x1 (find-x1-at rc (rc-y rc) (rc-x1 rc)))
             (avail (- x1 x0))
             (cws (map 'vector #'ceiling 
                       (calc-table-column-widthen table (table-wanted-width table www) avail)))
             (aw  (vreduce* #'+ cws)))    ;actual table width
        aw))))

(defun cell-assigned-height (cell row-heights)
  (let* ((ri (cell-row-index cell))
         (rs (cell-row-span cell))
         (h (loop for i from ri to (1- (+ ri rs))
                sum (aref row-heights i))))
    h))

(defun cell-real-height (cell)
  (let ((box (cell-bbox cell)))
    (and box
         (+ (bbox-iheight box) 
            (abox-margin-top box) 
            (abox-margin-bottom box)
            (bbox-padding-top box)
            (abox-border-top-width box)
            (bbox-padding-bottom box)
            (abox-border-bottom-width box) ))))

(defun fix-cell-height (cell row-heights)
  (when (cell-bbox cell)
    (let ((box (cell-bbox cell)))
      (setf (bbox-iheight box) 
        (- (cell-assigned-height cell row-heights)
           (abox-margin-top box) 
           (abox-margin-bottom box)
           (bbox-padding-top box)
           (abox-border-top-width box)
           (bbox-padding-bottom box)
           (abox-border-bottom-width box) )))))

(defun cell-user-specified-height (cell)
  (pt-attr/pixels (cell-content cell) :height nil))

(defun move-cell (cell dx dy)
  (when (cell-bbox cell)
    (nmove-box (cell-bbox cell) dx dy)
    (multiple-value-bind (x y) (clim:output-record-position (cell-or cell))
      (setf (clim:output-record-position (cell-or cell)) (values (+ x dx) (+ y dy))))))

(defun render-rowgroup (rc cws rowgroup x0 y0)
  ;; Erst mal alle Zellenhöhen berechnen
  (dolist (cell (rowgroup-cells rowgroup))
    ;; Breite zuweisen
    (let ((tw (+ (loop 
                     for i from (cell-col-index cell)
                     to (1- (+ (cell-col-index cell) (cell-col-span cell)))
                     sum (aref cws i)) )))
      (let* ((new-rc (copy-rc rc))
             (*rcontext* new-rc))
        (setf (rc-y new-rc) 0
              (rc-x0 new-rc) 0
              (rc-x1 new-rc) tw
              (rc-vertical-margin-callbacks new-rc) nil
              (rc-vertical-margins new-rc) nil
              (rc-first-line-tasks new-rc) nil
              (rc-left-floating-boxen new-rc) nil
              (rc-right-floating-boxen new-rc) nil)
        (clim:with-new-output-record (clim-user::*pane* 'clim:standard-sequence-output-record record)
         (clim:with-output-recording-options (clim-user::*pane* :record t :draw nil)
          (let* ((fake-parent (make-bbox))
                 (bbox (brender new-rc (cell-content cell) fake-parent)))
            (if bbox
                (copy-floating-boxen-into-bbox new-rc bbox)
                (when (or (rc-left-floating-boxen new-rc) 
                          (rc-right-floating-boxen new-rc))
                  (warn "You lost some floating boxen.")))
            (setf (cell-bbox cell) bbox)
            (setf (cell-or cell) record) )))
        (flush-vertical-margin new-rc)
        (handle-clear new-rc nil :both)
        (setf (cell-height cell) (rc-y new-rc))   ;xxx
        (when (cell-user-specified-height cell)
          (setf (cell-height cell) 
            (max (cell-height cell)
                 (cell-user-specified-height cell)))) )))

  ;; So nun noch um baseline kuemmern:
  (dotimes (y (rowgroup-number-of-rows rowgroup))
    (let ((cells (let ((res nil))
                   (dolist (cell (rowgroup-cells rowgroup) res)
                     (when (= (cell-row-index cell) y)
                       (push cell res))))))
      ;; `cells' now contains all cells in the `y' row
      ;; find the baseline

      (cond ((find :baseline cells :key #'cell-valign)
             (let ((baseline-soll (reduce #'max* (mapcar (lambda (cell)
                                                           (bbox-baseline (cell-bbox cell)))
                                                         cells)
                                          :initial-value nil)))
               (when baseline-soll
                 (dolist (cell cells)
                   (when (eq :baseline (cell-valign cell))
                     (let ((baseline-ist (bbox-baseline (cell-bbox cell))))
                       (when baseline-ist
                         (let ((dy (- baseline-soll baseline-ist)))
                           ;; now finally move the content down
                           (mapc (lambda (x)
                                   (nmove-box x 0 dy))
                                 (bbox-contents (cell-bbox cell)))
                           (incf (bbox-iheight (cell-bbox cell)) dy)
                           (incf (cell-height cell) dy))))))))))))

  ;; Each cell now knows its width as well as its height.
  (let* ((nrows (loop for c in (rowgroup-cells rowgroup) 
                    maximize (+ (cell-row-index c) (cell-row-span c))))
         (rhs   (calc-row-heights rowgroup nrows)))
    ;; well, each cell how has its real height.
    ;; so we are ready to draw everything:
    (dolist (c (rowgroup-cells rowgroup))
      (cond ((not (td/th-empty-p (cell-content c)))
             (let* ((x (+ x0 (loop for i from 0 to (1- (cell-col-index c)) 
                                 sum (aref cws i))))
                    (y1 (+ y0 (loop for i from 0 to (1- (cell-row-index c)) 
                                  sum (aref rhs i))))
                    (ah (cell-assigned-height c rhs))
                    (rh (cell-real-height c))
                    (y2 (+ y1 ah)))
               y2
               ;; care for vertical align:
               (multiple-value-bind (nx ny)
                   (when (cell-bbox c)
                     (case (cell-valign c)
                       (:bottom
                        (incf (bbox-iy (cell-bbox c)) (- (floor (- ah rh) 1)))
                        (fix-cell-height c rhs)
                        (values x (+ y1 (floor (- ah rh) 1))) )
                       (:middle
                        (incf (bbox-iy (cell-bbox c)) (- (floor (- ah rh) 2)))
                        (fix-cell-height c rhs)
                        (values x (+ y1 (floor (- ah rh) 2))) )
                       (otherwise
                        (fix-cell-height c rhs)
                        (values x y1))))
                 (render-block-border-and-background rc (cell-bbox c))
                 (move-cell c nx ny) )))
            (t
             (setf (cell-bbox c) nil))))
    ;; return value: the y increment
    (+ (vreduce* #'+ rhs)) )) 

(defun render-table (rc pt parent-box)
  ;; Now, while we render a table, we unfortunatly have to disable
  ;; drawing.
  (clim:with-output-recording-options (clim-user::*pane* :record t :draw nil)
   ;;; xxx not yet correct
   (funcall (if t ;(clim:stream-drawing-p clim-user::*pane*)
                #'clim:replay-output-record
                #'values)
    (clim:with-new-output-record (clim-user::*pane*)
     ;; why does drawp nest proper?
     (render-table-2 rc pt parent-box))
    clim-user::*pane* clim:+everywhere+ 0 0)))
  
(defun render-table-2 (rc pt parent-box)
  (let ((table (parse-table pt))
        (x0 (find-x0-at rc (rc-y rc) (rc-x0 rc)))
        (x1 (find-x1-at rc (rc-y rc) (rc-x1 rc))))
    (when *debug-tables*
      (describe table))
    ;; alles erstmal aufsetzen:
    (setup-cell-attrs table)
    (table-setup-min/max-widthen rc table)
    (flush-vertical-margin rc)
    ;; um die CSS attribute kümmern wir uns erstmal nicht
    (let ((table.width (pt-attr/length pt :width))
          (table.align (pt-attr/align pt :align)))
      ;; columnenbreiten berechnen:
      (let ((cws (calc-table-column-widthen 
                  table 
                  ;; given width:
                  (ecase (car table.width)
                    ((NIL) :AUTO)
                    ((:PX) (cdr table.width))
                    ((:%)  (* 1/100 (cdr table.width) (- x1 x0))))
                  ;; available width
                  (- x1 x0))))
        ;; nun die bbox bauen
        ;; dazu:
        (let ((b.width (reduce #'+ cws)))
          (css:set-style-attr pt 'css:@width b.width))
        (let ((box (make-skeleton-bbox-for-pt rc pt (- x1 x0))))
          ;;(setf (bbox-ix box) b.x0)
          ;; jetzt alles rendern:
          (with-new-margins (rc (+ x0 (abox-left-leading box))
                                (- x1 (abox-right-leading box)))
            (let ((x0 (+ x0 (abox-left-leading box)))) ;ALERT was: x0 -- change compatible ?
              (setf (bbox-iy box) (rc-y rc)
                    (bbox-ix box) x0)

              (dolist (rowgroup (table-rowgroups table))
                (let ((dy (render-rowgroup rc cws rowgroup (bbox-ix box) (rc-y rc))))
                  (incf (rc-y rc) dy)))
        
              (map-table-cells (lambda (cell)
                                 (when (cell-bbox cell)
                                   (add-content box (cell-bbox cell))))
                               table)
              (setf (bbox-iheight box) (- (rc-y rc) (bbox-iy box) ;xxx
                                          ;; zzz hmm
                                          (bbox-padding-top box)
                                          (abox-border-top-width box)
                                          (bbox-padding-bottom box)
                                          (abox-border-bottom-width box)))
              (setf (bbox-width box)
                    (reduce #'+ cws))
              (add-content parent-box box)
              box)))))))

(defun calc-table-column-widthen (table width avail &aux spyp)
  ;; `width' ist die zugewiesene Breite -- kann auch :auto sein.
  ;; `avail' ist die maximale zur Verfügung stehene Breite
  ;;
  ;; (print table)
  ;;
  ;; Now, when a cell has a specified pixel width, we propagate that
  ;; to the cells max-width attribute
  (ignore-errors (table-column-maximum-width table 0))
  (ignore-errors (table-column-minimum-width table 0))
  (dolist (rg (table-rowgroups table))
    (dolist (cell (rowgroup-cells rg))
      (let ((w (pt-attr/length (cell-content cell) :width nil)))
        (when (and w (eq (car w) :px))
          (setf (cell-maximum-width cell) 
            (max (cell-minimum-width cell) (cdr w)))))))
  ;; nuke a possible table-maximum/mimimum-column-widthen cache
  (setf (table-%column-maximum-widthen table) nil
        (table-%column-minimum-widthen table) nil)
  (ignore-errors (table-column-maximum-width table 0))
  (ignore-errors (table-column-minimum-width table 0))
  ;;(print `(table-column-maximum-widthen = ,(table-%column-maximum-widthen table)))
  ;;(print `(table-column-minimum-widthen = ,(table-%column-minimum-widthen table)))
  '(print `(width = ,width
           attr-width = ,(pt-attr/length (table-pt table) :width nil))
         )
  (let (x)
    (ecase (car (setf x (pt-attr/length (table-pt table) :width nil)))
      ((NIL) (setf width :auto))
      ((:PX) (setf width (cdr x)))
      ((:%)  (setf width width))))
  ;;(print `(table = ,table))
  ;; 
  ;;
  (let* ((n (table-number-of-columns table))
         (res (make-array n :initial-element nil)))

    ;; hack
    (dotimes (i n)
      (multiple-value-bind (kind value) (column-specified-width table i)
        (cond ((eq kind :percentage)
               (setf (col-width (table-col-spec table i))
                 (cons '* value))))))
    
    ;; First all percentage, absolute and minimum-width cells
    (dotimes (i n)
      (multiple-value-bind (kind value) (column-specified-width table i)
        (case kind
          (:absolute
           (let ((min (table-column-minimum-width table i)))
             (if (< value min)
                 (warn "Specified table column width of ~D is smaller than minium ~D."
                       value (ceiling min)))
             (setf (aref res i) (max value min))))
          (:percentage
           ;; Percentages are relative to the total available space
           (let ((min (table-column-minimum-width table i))
                 (value (ceiling (* avail value) 100)));DEVRND
             (if (< value min)
                 (warn "Specified table column width of ~D is smaller than minium ~D."
                       value (ceiling min)))
             (setf (aref res i) (max min value))))
          (:minimum
           (setf (aref res i) (table-column-minimum-width table i))))))

    (multiple-value-bind (col-max new-table-width)
        (percent-width-cells table)
      (do ((x 0 (+ x 1))
           (q col-max (cdr q)))
          ((null q))
        (setf (elt (table-%column-maximum-widthen table) x) (car q)))
      (dotimes (i n)
        (multiple-value-bind (kind) (column-specified-width table i)
          (when (eq kind :proportional)
            (setf (col-width (table-col-spec table i))
              nil)))))
    ;; Only the dynamic columns are left
    (cond ((some #'null res)
           (allot-dynamic-columns 
            table 
            ;; :auto #+(OR)
            (if (eq width :auto) 
                :auto
              (- width (vreduce* #'+ res :key (lambda (x) (or x 0)))))
            (- avail (vreduce* #'+ res :key (lambda (x) (or x 0))))
            res))
          (t
           ;; we are done
           res))
    (dotimes (i n)
      (setf (aref res i) (max (aref res i)
                              (table-column-minimum-width table i))))
    (dotimes (i n)
      (setf (aref res i) (round (aref res i))))
    (when spyp
      (warn "@@@ res = ~S (sum ~S) width = ~S."
            res (vreduce* #'+ res) width))
    (cond ((and nil                     ;hmm
                (not (eq width :auto))
                (> (vreduce* #'+ res) width))
           (let* ((zuviel (- (vreduce* #'+ res) width))
                  (u (map 'vector (lambda (cw cm) 
                                    (max 0 (- cw cm)))
                          res
                          (table-%column-minimum-widthen table)))
                  (us (vreduce* #'+ u)))
             (if (> us 0)
                 (dotimes (i n)
                   (decf (aref res i) (* (/ (aref u i) us) zuviel)))))) )
    res))

(defun table-get-cell (table rg y x)
  (declare (ignore table))
  (dolist (cell (rowgroup-cells rg))
    (when (and (= y (cell-row-index cell))
               (= x (cell-col-index cell)))
      (return cell))))

(defun show-row (table rg y)
  (print
   (loop for x from 0 to (1- (table-number-of-columns table)) collect
         (table-get-cell table rg y x))))

(defun percent-width-cells (table
                            &aux col-max)
  ;; warum kommen wir hier eigentlich zweimal vorbei?!
  ;; wie in Netscape
  (dolist (rg (table-rowgroups table))
    (dolist (c (rowgroup-cells rg))
      (setf (cell-%percentage-width c) nil)))
  (let ((new-table-width 0))
    (dolist (rg (table-rowgroups table))
      (dotimes (y (rowgroup-number-of-rows rg))
        (cond (;; has this row any percentage width columns?
               (dotimes (x (table-number-of-columns table) nil)
                 (cond ((and (table-get-cell table rg y x)
                             (< 0 (cell-percentage-width (table-get-cell table rg y x))))
                        (return t))))
               ;; then ..
               (let ((reserve (- 100 (table-number-of-columns table)))
                     (unknown 0)
                     (unknown-base 0))
                 '(show-row table rg y)
                 (dotimes (x (table-number-of-columns table))
                   (let ((cell (table-get-cell table rg y x)))
                     (when cell
                       (let ((w (cell-percentage-width cell)))
                         (cond ((> w 0)
                                (let ((width))
                                  (incf reserve (cell-col-span cell))
                                  (cond ((> w reserve)
                                         (setf (cell-%percentage-width cell) reserve)
                                         (setf w (cell-percentage-width cell))
                                         (setf reserve 0))
                                        (t
                                         (decf reserve w)))
                                  (setf width (/ (* (cell-maximum-width cell) 100.0)
                                                 w))
                                  '(print `(width = ,width 
                                                 w = ,w 
                                                 (cell-maximum-width cell) = ,(cell-maximum-width cell)))
                                  (setf new-table-width (max width new-table-width))))
                               (t
                                (incf unknown)
                                (incf unknown-base (cell-maximum-width cell))))))))
                 '(print (list reserve unknown unknown-base new-table-width))
                 (ignore-errors
                  (unless (zerop unknown)
                    (dotimes (x (table-number-of-columns table))
                      (let ((cell (table-get-cell table rg y x)))
                        (when (and cell (zerop (cell-percentage-width cell)))
                          (let (width percent)
                            (cond ((= unknown 1)
                                   (setf percent reserve))
                                  (t
                                   (setf percent (* reserve (/ (cell-maximum-width cell) unknown)))))
                            (setf reserve (max 0 (- reserve percent)))
                            (incf percent (cell-col-span cell))
                            (setf (cell-%percentage-width cell) percent)
                            (setf width (/ (* 100 (cell-maximum-width cell)) (cell-percentage-width cell)))
                            (setf new-table-width (max width new-table-width))
                            (decf unknown)))))))
                 '(print (list reserve unknown unknown-base new-table-width))
                 '(show-row table rg y)
                 '(print 'row-done)
                 
                 ;;
                 ))
              (t
               ;; row has no percentage width columns
               (maxf new-table-width 
                     (loop for x from 0 to (1- (table-number-of-columns table))
                         sum (let ((cell (table-get-cell table rg y x)))
                               (if cell (cell-maximum-width cell) 0))))
                ))
               )
      ;;
      ;;(show-row table rg y)
      (setf col-max (loop for x from 0 to (1- (table-number-of-columns table))
                        collect (table-column-maximum-width table x)))
      ;;
      ;; (print `(new-table-width ,(* 1.0 new-table-width)))
      '(dotimes (x (table-number-of-columns table))
        (let ()
          (setf (elt col-max x) 1)
          (dotimes (y (rowgroup-number-of-rows rg))
            (let ((cell (table-get-cell table rg y x)))
              (cond ((and cell 
                          (< 0 (cell-percentage-width cell))
                          (= 1 (cell-col-span cell)))
                     (maxf (elt col-max x)
                       (max (* new-table-width (cell-percentage-width cell) 1/100)
                            (cell-minimum-width cell))))
                    ((and cell (< 1 (cell-col-span cell)))
                     '(setf need-pass-two-p t))
                    (t
                     (and cell (maxf (elt col-max x) (cell-maximum-width cell)))))
              (and cell (= 1 (cell-col-span cell))
                   (and cell (maxf (elt col-max x) (cell-minimum-width cell))))))
                
          )
        '(print `(col-max = ,col-max))
        '(show-row table rg 0)
        
        )

      '(dotimes (x (table-number-of-columns table))
        (let ()
          (setf (elt col-max x) 1)
          (dotimes (y (rowgroup-number-of-rows rg))
            (let ((cell (table-get-cell table rg y x)))
              (when cell
                (cond ((= 1 (cell-col-span cell))
                       (cond ((< 0 (cell-percentage-width cell))
                              (maxf (elt col-max x)
                                    (max (* new-table-width (cell-percentage-width cell) 1/100)
                                         (cell-minimum-width cell))))
                             (t
                              (maxf (elt col-max x) 
                                    (cell-minimum-width cell)
                                    (cell-maximum-width cell)))))
                      ((and (< 1 (cell-col-span cell)))
                       '(setf need-pass-two-p t))
                      (t))
                ))
                
            )))

      (setf col-max (make-list (table-number-of-columns table) :initial-element 1))
      (loop for span from 1 to (table-number-of-columns table)
          do
            (dotimes (x (table-number-of-columns table))
              (dotimes (y (rowgroup-number-of-rows rg))
                (let* ((cell (table-get-cell table rg y x))
                       (s (and cell (cell-col-span cell))))
                  (when (and cell (= span s))
                    (let ((value
                           (cond ((< 0 (cell-percentage-width cell))
                                  (max (* new-table-width (cell-percentage-width cell) 1/100)
                                       (cell-minimum-width cell)))
                                 (t
                                  (max (cell-minimum-width cell)
                                       (cell-maximum-width cell))))))
                      '(setf (elt col-max x) (max (elt col-max x) value))
                      '(let ((lack (- value (elt col-max x))))
                        (when (< 0 lack)
                          (incf (elt col-max x) lack)))
                      (let ((lack (- 
                                    ;; what we need
                                    value
                                    ;; what is there already
                                    (loop for j from x to (1- (+ x s)) sum (elt col-max j)))))
                        (cond ((> lack 0)
                               ;; we need to add something?
                               (let ((amount (/ lack s)))
                                 (loop for j from x to (1- (+ x s)) do
                                       (incf (elt col-max j) amount))))))))))))

      )
    (values col-max new-table-width)))

(defun random-elt (seq)
  (elt seq (random (length seq))))


(defun bbox-baseline (bbox)
  ;; xxx with the new renderer this is now completety obsolete, we
  ;; have neither bboxen or iboxen any longer.
  (declare (ignore bbox))
  0)

;; Eingabe:
;;   - Menge von Zellen mit ihren
;;      mindest Breite
;;      maximale Breite
;;      width parameter
;;   - ggf die COLS/COLGROUP Specification
;;   - ggf das WIDTH parameter der Tabelle selbst
;;
;; Ausgabe:
;;   - die effektiven Breiten der Spalten
;;

#|
lo_percent_width_cells(lo_TableRec *table, lo_cell_data XP_HUGE *cell_array,
	lo_TableCell *blank_cell, int32 cell_pad, int32 table_pad,
	int32 *table_width, int32 *min_table_width,
	int32 *base_table_width, int32 *min_base_table_width)
{
	int32 x, y;
	int32 indx;
	int32 new_table_width;
	int32 new_min_table_width;
	int32 new_base_table_width;
	int32 new_min_base_table_width;
	Bool need_pass_two;
	lo_table_span *row_max;
	lo_table_span *col_max;
	lo_TableRow *row_ptr;
	lo_TableCell *cell_ptr;

	new_table_width = 0;
	row_ptr = table->row_list;
	row_max = table->height_spans;
	for (y=0; y < table->rows; y++)
	{
	    if (row_ptr->has_percent_width_cells != FALSE)
	    {
			int32 reserve;
			int32 unknown, unknown_base;

			unknown = 0;
			unknown_base = 0;
			reserve = 100 - table->cols;
			col_max = table->width_spans;
			for (x=0; x < table->cols; x++)
			{
				indx = (y * table->cols) + x;
				cell_ptr = cell_array[indx].cell;
				if ((cell_ptr == blank_cell)||
					(cell_ptr == NULL))
				{
					col_max = col_max->next;
					continue;
				}
				if (cell_ptr->percent_width > 0)
				{
					int32 width;

					reserve += cell_ptr->colspan;
					if (cell_ptr->percent_width > reserve)
					{
					    cell_ptr->percent_width = reserve;
					    reserve = 0;
					}
					else
					{
					    reserve -= cell_ptr->percent_width;
					}
					width = cell_ptr->max_width * 100 /
						cell_ptr->percent_width;
					if (width > new_table_width)
					{
						new_table_width = width;
					}
				}
				else
				{
					unknown++;
					unknown_base += cell_ptr->max_width;
				}
				col_max = col_max->next;
			}
			if (unknown)
			{
			    col_max = table->width_spans;
			    for (x=0; x < table->cols; x++)
			    {
				indx = (y * table->cols) + x;
				cell_ptr = cell_array[indx].cell;
				if ((cell_ptr == blank_cell)||
					(cell_ptr == NULL))
				{
					col_max = col_max->next;
					continue;
				}
				if (cell_ptr->percent_width == 0)
				{
					int32 width;
					int32 percent;

					if (unknown == 1)
					{
					    percent = reserve;
					}
					else
					{
					    percent = reserve *
							cell_ptr->max_width /
							unknown_base;
					}
					reserve -= percent;
					if (reserve < 0)
					{
					    reserve = 0;
					}
					percent += cell_ptr->colspan;
					cell_ptr->percent_width = percent;

					width = cell_ptr->max_width * 100 /
						cell_ptr->percent_width;
					if (width > new_table_width)
					{
						new_table_width = width;
					}

					unknown--;
				}
				col_max = col_max->next;
			    }
			}
	    }
	    else
	    {
            |
            (maxf new-table-width 
                  (loop for x from 0 to (1- (table-number-of-columns table))
                        sum (let ((cell (table-get-cell table y x))
                              (if cell (cell-maximum-width cell) 0)))))
            |
			int32 width;

			width = 0;
			col_max = table->width_spans;
			for (x=0; x < table->cols; x++)
			{
				indx = (y * table->cols) + x;
				cell_ptr = cell_array[indx].cell;
				if ((cell_ptr == blank_cell)||
					(cell_ptr == NULL))
				{
					col_max = col_max->next;
					continue;
				}
				width += cell_ptr->max_width;
				col_max = col_max->next;
			}
			if (width > new_table_width)
			{
				new_table_width = width;
			}
	    }
	    row_ptr = row_ptr->next;
	    row_max = row_max->next;
	}

	if (*table_width > new_table_width)
	{
		new_table_width = *table_width;
	}

	/*
	 * If we already know how wide this table must be
	 * Use that width when calculate percentage cell widths.
	 */
	if ((table->width > 0)&&(table->width >= *min_table_width))
	{
		new_table_width = table->width;
	}

	need_pass_two = FALSE;

	col_max = table->width_spans;
	for (x=0; x < table->cols; x++)
	{
		col_max->dim = 1;
		for (y=0; y < table->rows; y++)
		{
			indx = (y * table->cols) + x;
			cell_ptr = cell_array[indx].cell;
			if ((cell_ptr == blank_cell)||
				(cell_ptr == NULL))
			{
				continue;
			}
			if ((cell_ptr->percent_width > 0)&&
				(cell_ptr->colspan == 1))
			{
				int32 p_width;

				p_width = new_table_width *
					cell_ptr->percent_width / 100;
				if (p_width < cell_ptr->min_width)
				{
					p_width = cell_ptr->min_width;
				}
				if (p_width > col_max->dim)
				{
					col_max->dim = p_width;
				}
			}
			else if (cell_ptr->colspan > 1)
			{
				need_pass_two = TRUE;
			}
			else
			{
				if (cell_ptr->max_width > col_max->dim)
				{
					col_max->dim =
						cell_ptr->max_width;
				}
			}
		}
		if (col_max->dim < col_max->min_dim)
		{
			col_max->dim = col_max->min_dim;
		}
		col_max = col_max->next;
	}
	/*
	 * Take care of spanning columns if any
	 */
	if (need_pass_two != FALSE)
	{
	    row_max = table->height_spans;
	    for (y=0; y < table->rows; y++)
	    {
			col_max = table->width_spans;
			for (x=0; x < table->cols; x++)
			{
				indx = (y * table->cols) + x;
				cell_ptr = cell_array[indx].cell;
				if ((cell_ptr == blank_cell)||
					(cell_ptr == NULL))
				{
					col_max = col_max->next;
					continue;
				}
				if (cell_ptr->colspan > 1)
				{
					int32 i;
					int32 width;
					lo_table_span *max_ptr;
					int32 p_width;
					int32 new_width;

					new_width = cell_ptr->max_width;
				    if (cell_ptr->percent_width > 0)
				    {
						p_width = new_table_width *
							cell_ptr->percent_width / 100;
						if (p_width >= cell_ptr->min_width)
						{
							new_width = p_width;
						}
				    }

					max_ptr = col_max;

					width = max_ptr->dim;
					for (i=1; i < cell_ptr->colspan; i++)
					{
						max_ptr = max_ptr->next;
						width = width + cell_pad +
							max_ptr->dim;
					}

					if (width < new_width)
					{
						int32 add_width;
						int32 add;
						lo_table_span *add_ptr;

						add_ptr = col_max;
						add_width = new_width - width;
						add = 0;
						while (add_ptr != max_ptr)
						{
							int32 newWidth;

							newWidth = add_width *
								add_ptr->dim /
								width;
							add_ptr->dim +=newWidth;
							add += newWidth;
							add_ptr = add_ptr->next;
						}
						add_ptr->dim += (add_width -
							add);
					}
				}
				col_max = col_max->next;
			}
			row_max = row_max->next;
	    }
	}

	new_table_width = 0;
	new_min_table_width = 0;
	new_base_table_width = 0;
	new_min_base_table_width = 0;
	col_max = table->width_spans;
	while (col_max != NULL)
	{
		new_base_table_width += col_max->dim;
		new_min_base_table_width += col_max->min_dim;
		new_table_width = new_table_width + cell_pad +
			col_max->dim;
		new_min_table_width = new_min_table_width + cell_pad +
			col_max->min_dim;
		col_max = col_max->next;
	}
	new_table_width += cell_pad;
	new_table_width += (table->table_ele->border_left_width + 
		table->table_ele->border_right_width);
	new_min_table_width += cell_pad;
	new_min_table_width += (table->table_ele->border_left_width + 
		table->table_ele->border_right_width);
	if (table->draw_borders == TABLE_BORDERS_OFF)
	{
		new_table_width += (2 * table_pad);
		new_min_table_width += (2 * table_pad);
	}

	*table_width = new_table_width;
	*min_table_width = new_min_table_width;
	*base_table_width = new_base_table_width;
	*min_base_table_width = new_min_base_table_width;
}

|#


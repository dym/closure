;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: R2; -*-
;;; --------------------------------------------------------------------------------------
;;;     Title: Minimum and Maximum Width of pts
;;;   Created: 1999-02-24 04:40
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: GPL (See file COPYING for details).
;;; --------------------------------------------------------------------------------------
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

(in-package :R2)

;; Changes
;;
;;  When        Who     What
;; ----------------------------------------------------------------------------
;;  1999-09-19  GB      - minimum/maximum width cache move to rcontext
;;
;;  1999-08-20  GB      - TABLE-MINMAX-WIDTH, TABLE-MAXIMUM-WIDTH, 
;;                        TABLE-MINIMUM-WIDTH: got rcontext argument
;;

;;; ---- Minimum & Maximum Width of Elements ----------------------------------

;; TODO
;; - percentage values of margin, padding
;; - beides in einem Rutsch

(defun pt-minimum-width (rc pt)
  (assert (member (css:display pt) '(:block :list-item :marker)))
  (or (gethash pt (rc-min-width-cache rc))
      (setf (gethash pt (rc-min-width-cache rc))
        ;; outer oder nicht outer is hier die frage
        (pt-minimum-width/block-outer rc pt))))

(defun pt-maximum-width (rc pt)
  (assert (member (css:display pt) '(:block :list-item :marker)))
  (or (gethash pt (rc-max-width-cache rc))
      (setf (gethash pt (rc-max-width-cache rc))
        ;; outer oder nicht outer is hier die frage
        (+ 0 (pt-maximum-width/block-outer rc pt)))))

(defun pt-minimum-width/block-outer (rc pt)
  (let ((ml (css:style-attr pt 'css:@margin-left))
        (bl (css:style-attr pt 'css:@border-left-width))
        (pl (css:style-attr pt 'css:@padding-left))
        (pr (css:style-attr pt 'css:@padding-right))
        (br (css:style-attr pt 'css:@border-right-width))
        (mr (css:style-attr pt 'css:@margin-right)))
    ;; todo: percentage values
    (+ (if (realp ml) ml 0)
       (if (realp bl) bl 0)
       (if (realp pl) pl 0)
       (pt-minimum-width/block rc pt)
       (if (realp pr) pr 0)
       (if (realp br) br 0)
       (if (realp mr) mr 0))))

(defun pt-minimum-width/ro (rc pt ro)
  rc
  (let ((wd (css:style-attr pt 'css:@width)))
    (cond ((realp wd)
           wd)
          ((eq wd :auto)
           (ro/intrinsic-size ro))
          ((css::percentage-p wd)
           0)                           ;keinen beitrag
          (t
           (warn "Oops wd = ~S." wd)))))

(defun pt-minimum-width/block (rc pt)
  (labels ((foo (pt)
             (let ((line nil)
                   (res 0))
               (labels ((flush-line ()
                          (when line
                            (setf line (nreverse line))
                            (setf res (max res (minimum-width/line rc line)))
                            (setf line nil))))
                 (dolist (k (sgml:pt-children pt))
                   (ecase (css:display k)
                     (:none)
                     ((:block :list-item :marker)
                      (flush-line)
                      (setf res (max res (pt-minimum-width/block-outer rc k))))
                     ((:inline :pcdata)
                      (push k line))))
                 (flush-line))
               res) ))
    (let ((re (replaced-element-p (rc-document rc) (rc-device rc) pt)))
      (cond (re
             (cond ((sgml::pt-p re)
                    (foo re))
                   (t
                    (pt-minimum-width/ro rc pt re))))
            ((eq (sgml:gi pt) :table)
             (table-minimum-width rc pt))
            (t
             (foo pt) )))))

(defun minimum-width/line (rc pts)
  (let ((w 0)
        (res 0))
    (labels ((walk (pt first-p last-p)
               (declare (type sgml::pt pt))
               (let ((re (replaced-element-p (rc-document rc) (rc-device rc) pt)))
                 (cond (re
                        (cond ((sgml::pt-p re)
                               (walk/aux re first-p last-p))
                              (t
                               (incf w (minmax-width/horizontal-left-leading pt))
                               (incf w (pt-minimum-width/ro rc pt re))
                               (incf w (minmax-width/horizontal-right-leading pt)))))
                       ((eq (sgml:gi pt) :table)
                        (incf w (table-minimum-width rc pt)))
                       ((eq (sgml:gi pt) :br)
                        (setf res (max res w))
                        (setf w 0))
                       (t
                        (walk/aux pt first-p last-p)))))
             ;;
             (walk-list (pts)
               (do ((q pts (cdr q)))
                   ((null q))
                 (walk (car q) (eq q pts) (null (cdr q)))))
             ;;
             (walk/aux (pt first-p last-p)
               (ecase (css:display pt)
                        ((:inline)
                         (incf w (minmax-width/horizontal-left-leading pt))
                         (walk-list (sgml:pt-children pt))
                         (incf w (minmax-width/horizontal-right-leading pt)))
                        ((:pcdata)
                         (case (css:style-attr (sgml:pt-parent pt) 'css:@white-space)
                           (:nowrap
                            (let ((text-style (pt-text-style (rc-device rc) 
                                                             (sgml:pt-parent pt))))
                              (iterate-over-runes (lambda (rune index x cw)
                                                    (declare (ignore index x rune))
                                                    (incf w cw))
                                                  (sgml:pt-cdata pt)
                                                  0
                                                  (length (sgml:pt-cdata pt))
                                                  text-style
                                                  (css:style-attr (sgml:pt-parent pt)
                                                                  'css:@white-space))) )
                           (t
                            (walk-pcdata-normal pt first-p last-p))))
                        ((:none))
                        ((:block :marker)
                         (setf res (max w res))
                         (setf w 0)
                         '(warn "No ~S block elements within inline elements please."
                           (sgml:gi pt) )) ))
             (walk-pcdata-normal (pt first-p last-p)
               (multiple-value-bind (start end) (nuke-white-space pt first-p last-p)
                 (let ((text-style (pt-text-style (rc-device rc) (sgml:pt-parent pt))))
                   (iterate-over-runes (lambda (rune index x cw)
                                         index x
                                         (cond ((white-space-rune-p* rune)
                                                (setf res (max res w))
                                                (setf w 0))
                                               (t
                                                (incf w cw))))
                                       (sgml:pt-cdata pt)
                                       start
                                       end
                                       text-style
                                       (css:style-attr (sgml:pt-parent pt) 'css:@white-space))) )) )
      (walk-list pts))
    (setf res (max res w))
    res))

(defun maximum-width/line (rc pts)
  (let ((w 0)
        (res 0))
    (labels ((walk (pt)
               (let ((re (replaced-element-p (rc-document rc) (rc-device rc) pt)))
                 (cond (re
                        (cond ((sgml::pt-p re)
                               (walk/aux re))
                              (t
                               (let ((wd (css:style-attr pt 'css:@width)))
                                 (cond ((realp wd) wd)
                                       ((eq wd :auto)
                                        (incf w (minmax-width/horizontal-left-leading pt))
                                        (incf w (ro/intrinsic-size re))
                                        (incf w (minmax-width/horizontal-right-leading pt)))
                                       ((css::percentage-p wd)
                                        0) ;keinen beitrag
                                       (t
                                        (warn "Oops wd = ~S in ~S." wd 'pt-minimum-width/line)))))))
                     ((eql (sgml:gi pt) :table)
                      (table-maximum-width rc pt))
                     (t
                      (walk/aux pt)))))
             (walk/aux (pt)
               (ecase (css:display pt)
                        ((:none))
                        ((:inline)
                         (incf w (minmax-width/horizontal-left-leading pt))
                         (mapc* #'walk (sgml:pt-children pt))
                         (incf w (minmax-width/horizontal-right-leading pt)))
                        ((:pcdata)
                         (let ((text-style (pt-text-style (rc-device rc) 
                                                          (sgml:pt-parent pt))))
                           (incf w (iterate-over-runes
                                    (lambda (a b c d)
                                      (declare (ignore a b c d)))
                                    (sgml:pt-cdata pt)
                                    0
                                    (length (sgml:pt-cdata pt))
                                    text-style
                                    (css:style-attr (sgml:pt-parent pt)
                                                    'css:@white-space)))))
                        ((:block :marker)
                         (setf res (max w res))
                         (setf w 0)
                         '(warn "No block elements within inline elements please.")) ) ) )
      (mapc* #'walk pts))
    (setf res (max res w))
    res))

(defun maximum-width/line (rc pts)
  (let ((w 0)
        (res 0)
        (start-of-line-p t))
    (labels ((walk (pt first-p last-p)
               (declare (type sgml::pt pt))
               (let ((re (replaced-element-p (rc-document rc) (rc-device rc) pt)))
                 (cond (re
                        (cond ((sgml::pt-p re)
                               (walk/aux re first-p last-p))
                              (t
                               (setf start-of-line-p nil)
                               (incf w (minmax-width/horizontal-left-leading pt))
                               (incf w (pt-minimum-width/ro rc pt re))
                               (incf w (minmax-width/horizontal-right-leading pt)))))
                       ((eq (sgml:gi pt) :table)
                        (incf w (table-minimum-width rc pt)))
                       ((eq (sgml:gi pt) :br)
                        (setf res (max res w))
                        (setf w 0)
                        (setf start-of-line-p t))
                       (t
                        (walk/aux pt first-p last-p)))))
             ;;
             (walk-list (pts)
               (do ((q pts (cdr q)))
                   ((null q))
                 (walk (car q) (eq q pts) (null (cdr q)))))
             ;;
             (walk/aux (pt first-p last-p)
               (ecase (css:display pt)
                        ((:inline)
                         (incf w (minmax-width/horizontal-left-leading pt))
                         (walk-list (sgml:pt-children pt))
                         (incf w (minmax-width/horizontal-right-leading pt)))
                        ((:pcdata)
                         (case (css:style-attr (sgml:pt-parent pt) 'css:@white-space)
                           (:nowrap
                            ;; hmm first/last start-of-line-p handling!
                            (let ((text-style (pt-text-style (rc-device rc) 
                                                             (sgml:pt-parent pt))))
                              (iterate-over-runes (lambda (rune index x cw)
                                                    (declare (ignore index x rune))
                                                    (incf w cw))
                                                  (sgml:pt-cdata pt)
                                                  0
                                                  (length (sgml:pt-cdata pt))
                                                  text-style
                                                  (css:style-attr (sgml:pt-parent pt)
                                                                  'css:@white-space))) )
                           (t
                            (walk-pcdata-normal pt first-p last-p))))
                        ((:none))
                        ((:block :marker)
                         (setf res (max w res))
                         (setf w 0)
                         '(warn "No ~S block elements within inline elements please."
                           (sgml:gi pt) )) ))
             (walk-pcdata-normal (pt first-p last-p)
               (multiple-value-bind (start end) (nuke-white-space pt (or start-of-line-p first-p) last-p)
                 (unless (= start end) (setf start-of-line-p nil))
                 (let ((text-style (pt-text-style (rc-device rc) (sgml:pt-parent pt))))
                   (iterate-over-runes (lambda (rune index x cw)
                                         index x rune
                                         (incf w cw))
                                       (sgml:pt-cdata pt)
                                       start
                                       end
                                       text-style
                                       (css:style-attr (sgml:pt-parent pt) 'css:@white-space))) )) )
      (walk-list pts))
    (setf res (max res w))
    res))



;;; Maximum:

(defun pt-maximum-width/block-outer (rc pt)
  (let ((ml (css:style-attr pt 'css:@margin-left))
        (bl (css:style-attr pt 'css:@border-left-width))
        (pl (css:style-attr pt 'css:@padding-left))
        (pr (css:style-attr pt 'css:@padding-right))
        (br (css:style-attr pt 'css:@border-right-width))
        (mr (css:style-attr pt 'css:@margin-right)))
    ;; todo: percentage values
    (+ (if (realp ml) ml 0)
       (if (realp bl) bl 0)
       (if (realp pl) pl 0)
       (pt-maximum-width/block rc pt)
       (if (realp pr) pr 0)
       (if (realp br) br 0)
       (if (realp mr) mr 0))))

(defun pt-maximum-width/block (rc pt)
  (let ((re (replaced-element-p (rc-document rc) (rc-device rc) pt)))
    (labels ((foo (pt)
               (let ((line nil)
                     (res 0))
                 (labels ((flush-line ()
                            (when line
                              (setf line (nreverse line))
                              (setf res (max res (maximum-width/line rc line)))
                              (setf line nil))))
                   (dolist (k (sgml:pt-children pt))
                     (ecase (css:display k)
                       (:none)
                       ((:block :list-item :marker)
                        (flush-line)
                        (setf res (max res (pt-maximum-width/block-outer rc k))))
                       ((:inline :pcdata)
                        (push k line))))
                   (flush-line))
                 res)))
      (cond (re
             (cond ((sgml::pt-p re)
                    (foo re) )
                   (t
                    (let ((wd (css:style-attr pt 'css:@width)))
                      (cond ((realp wd)
                             wd)
                            ((eq wd :auto)
                             (ro/intrinsic-size re))
                            ((css::percentage-p wd)
                             0)         ;keinen beitrag
                            (t
                             (warn "Oops wd = ~S in ~S." wd 'pt-maximum-width/block)))))))
            ((eql (sgml:gi pt) :table)
             (table-maximum-width rc pt))
            (t
             (foo pt)) ))))

(defun minmax-width/horizontal-left-leading (pt)
  (let ((ml (css:style-attr pt 'css:@margin-left))
        (bl (css:style-attr pt 'css:@border-left-width))
        (pl (css:style-attr pt 'css:@padding-left)))
    (+ (if (realp ml) ml 0)
       (if (realp bl) bl 0)
       (if (realp pl) pl 0))))

(defun minmax-width/horizontal-right-leading (pt)
  (let ((mr (css:style-attr pt 'css:@margin-right))
        (br (css:style-attr pt 'css:@border-right-width))
        (pr (css:style-attr pt 'css:@padding-right)))
    (+ (if (realp mr) mr 0)
       (if (realp br) br 0)
       (if (realp pr) pr 0))))

(defun table-maximum-width (rc pt)
  (nth-value 0 (table-minmax-width rc pt)))

(defun table-minimum-width (rc pt)
  (nth-value 1 (table-minmax-width rc pt)))

(defun table-minmax-width (rc pt)
  (let* ((table (parse-table pt)))
    (setup-cell-attrs table)
    (table-setup-min/max-widthen rc table)
    (let* ((wanted
            (let ((w (css:style-attr (table-pt table) 'css:@orig-width)))
              (cond ((realp w)
                     w)
                    (t
                     :auto)))))
      (cond #|| #+(OR)
            ((not (eql wanted :auto))
             (values wanted wanted))
            ||#
            (t
             (values
              (min (if (eql wanted :auto) most-positive-fixnum wanted) 
                   (loop for i from 0 to (1- (table-number-of-columns table)) 
                       sum (table-column-maximum-width table i)))
              (max (if (eql wanted :auto) 0 wanted)
                   (loop for i from 0 to (1- (table-number-of-columns table)) 
                       sum (table-column-minimum-width table i))))))) ))

(defun table-minmax-width (rc pt)
  (let* ((table (parse-table pt)))
    (setup-cell-attrs table)
    (table-setup-min/max-widthen rc table)
    (let* ((wanted
            (let ((w (css:style-attr (table-pt table) 'css:@orig-width)))
              (cond ((realp w)
                     w)
                    (t
                     :auto)))))
      (setf wanted :auto)
      (let ((cws1 (calc-table-column-widthen table wanted 1))
            (cws2 (calc-table-column-widthen table wanted 100000)))
        (multiple-value-bind (max min)
            (cond #+(OR)
                  ((not (eql wanted :auto))
                   (values wanted wanted))
                  (t
                   (values
                    (min (if (eql wanted :auto) most-positive-fixnum wanted) 
                         (loop for i from 0 to (1- (table-number-of-columns table)) 
                             sum (table-column-maximum-width table i)))
                    (max (if (eql wanted :auto) 0 wanted)
                         (loop for i from 0 to (1- (table-number-of-columns table)) 
                             sum (table-column-minimum-width table i))))))
          (setf min (reduce #'+ cws1))
          (setf max (reduce #'+ cws2))
          ;;(setf max (reduce #'+ cws1))
          (values max min))))))


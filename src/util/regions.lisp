;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: WS/GRAPHICS-UTILITIES; -*-
;;; --------------------------------------------------------------------------------------
;;;     Title: Rectangle Lists
;;;   Created: 1999-05-15 02:51
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: GPL (See file COPYING for details).
;;; --------------------------------------------------------------------------------------
;;;  (c) copyright 1999 by Gilbert Baumann

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
;;  2001-11-17  GB      - we switched to use CLIM regions
;;
;;  1999-08-18  GB      - REGION-BOUNDING-RECTANGLE* -- new function
;;                      - REGION-FROM-X11-RECTANGLE-LIST -- new function
;;                      - GU:POLYGON-CONTAINS-POINT-P -- new function
;;

(in-package :WS/GRAPHICS-UTILITIES)

(defun region-intersects-rectangle-p (region x1 y1 x2 y2)
  (clim:region-intersects-region-p region (clim:make-rectangle* x1 y1 x2 y2)))

(defun map-region-rectangles (fun region)
  (clim:map-over-region-set-regions
   (lambda (r)
     (apply fun (mapcar #'round
                        (multiple-value-list (clim:rectangle-edges* r)))))
   region
   :normalize :y-banding))

(defun region-to-x11-rectangle-list (region)
  (let ((res nil))
    (map-region-rectangles (lambda (x1 y1 x2 y2)
                             (push (- y2 y1) res)
                             (push (- x2 x1) res)
                             (push y1 res)
                             (push x1 res))
                           region)
    (mapcar #'round res)))

(defun region-intersections-region-p (region1 region2)
  (clim:region-intersects-region-p region1 region2))

;; can somebody tell me how to make these constants without eval-when?

(defparameter +nowhere+ clim:+nowhere+)

(defparameter +everywhere+ clim:+everywhere+)

(defun translate-region (region dx dy)
  (clim:transform-region (clim:make-translation-transformation dx dy) region))
               
(defun region-bounding-rectangle* (region)
  "Returns the bounding rectangle of a region as four values: x1 y1 x2 y2."
  (values-list (mapcar #'round (multiple-value-list (clim:bounding-rectangle* region)))))

(defun region-from-x11-rectangle-list (rectangle-sequence)
  (let ((res +nowhere+))
    (do ((q rectangle-sequence (nthcdr 4 q)))
        ((endp q))
      (setf res (region-union res
                              (make-rectangle* (first q) (second q)
                                               (+ (first q) (third q))
                                               (+ (second q) (fourth q))))))
    res))

;;;;;

(defun polygon-contains-point-p (coord-seq x y)
  (let ((n (floor (length coord-seq) 2))
        (k 0))
    (macrolet ((px (i) `(elt coord-seq (* 2 (mod ,i n))))
               (py (i) `(elt coord-seq (+ 1 (* 2 (mod ,i n))))))
      (dotimes (i n)
        (let ((x0 (px i)) (y0 (py i))
              (x1 (px (+ i 1))) (y1 (py (+ i 1))))
          (cond ((or (< y0 y y1) 
                     (> y0 y y1))
                 (let ((xi
                        (cond ((= (- x1 x0) 0) x0)
                              (t 
                               (* (+ (* (/ (- y1 y0) (- x1 x0)) x0)
                                     (- y y0))
                                  (/ (- x1 x0) (- y1 y0)))))))
                   (cond ((< xi x) (incf k))
                         ((= xi x) (return-from polygon-contains-point-p t)))))
                ((and (/= y0 y) (= y1 y))
                 (let ((j (do ((j (+ i 1) (+ j 1)))
                              ((= j (+ i n)) j)
                            (when (/= (py j) y)
                              (return j)))))
                   (when (/= (signum (- y y0)) (signum (- y (py j))))
                     (cond ((< x1 x) (incf k))
                           ((= x1 x) (return-from polygon-contains-point-p t))))))
                ((and (= y0 y) (= y 1 y))
                 (when (or (<= x0 x x1) (>= x0 x x1))
                   (return-from polygon-contains-point-p t))))))
      (oddp k))))

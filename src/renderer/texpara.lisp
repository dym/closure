;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: TEXPARA; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: TeX like paragraph formatting
;;;   Created: ???
;;;    Author: Gilbert Baumann <gilbert@base-engineering.com>
;;;   License: MIT style (see below)
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2005 by Gilbert Baumann

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

(defpackage :texpara (:use #:cl))

(in-package :texpara)

;;;; Parameters

(defparameter *line-penality* 10)
(defparameter *hyphen-penality* 50)
(defparameter *exhyphen-penality* 50)

(defparameter *pre-tolerance* 100)
(defparameter *tolerance* 200)

(defparameter *tolerance* 4000)

(defparameter *precision* 1
  "Resolution of your device.")
(eval-when (:compile-toplevel :load-toplevel :execute)
	   (defconstant +badness-infinite+ 10000))

(eval-when (compile eval load)

  (defstruct node
    demerit)

  (defstruct (glue (:include node))
    width
    shrink
    shrink-unit
    stretch
    stretch-unit
    assigned)

  (defstruct (box (:include node))
    width
    data)

  (defstruct (discretionary (:include node))
    pre
    post
    no)

  )

(defvar +hfil-glue+
    ;; a \hfil fake
    (make-glue :width 0
               :shrink 0
               :shrink-unit 0
               :stretch 1e6
               :stretch-unit 0))


(defstruct break-point
  position
  delta-width
  cache)                          ;from this to next

(defun break-points (boxen)
  (let* ((res (cons (make-break-point :position -1) nil))
         (bpf res)
         (n (length boxen))
         (delta 0))
    (do ((i 0 (+ i 1)))
        ((= (the fixnum i) (the fixnum n))
         (setf (break-point-delta-width (car bpf)) delta)
         res)
      (let ((box (svref boxen i)))
        (cond ((or (glue-p box) (discretionary-p box))
               (setf (break-point-delta-width (car bpf)) delta
                     (cdr bpf) (cons (make-break-point :position i) nil)
                     bpf (cdr bpf)
                     delta 0))
              (t
               (incf delta (box-width box))))))))

(defmacro map-split-points (boxen bp cont)
  ;; bp is the break point just where the last break occurred
  `(let ((i (break-point-position (car ,bp)))
         (box)
         (ddw 0)
         (nw 0)                         ;natural width
         (w+ 0)                         ;stretchability
         (w- 0))                        ;shrinkability
     ;; The break just before the current line might have been a \discretionary
     ;; node; in case of that consider the post material
     (cond ((and (>= i 0) (discretionary-p (svref ,boxen i)))
            (mapc (lambda (box)
                    (cond ((box-p box)
                           (incf nw (box-width box)))
                          (t
                           (error "Oops"))))
                  (discretionary-post (svref ,boxen i)))))
     ;; Now consider all other break points
     (loop
       (when (null (cdr ,bp))
         (return))
       ;; go to next break point
       (incf nw (break-point-delta-width (car ,bp)))
       (setf ,bp (cdr ,bp))
       (setf i (break-point-position (car ,bp)))
       (setf box (svref ,boxen i))
       ;; pretend we would break here
       (setf ddw 0)
       (cond ((discretionary-p box)
              (mapc (lambda (box) (incf ddw (box-width box)))
                    (discretionary-pre box))))
       (incf nw ddw)

       (,cont ,bp nw w+ w-)

       ;; now pretend we would not break here
       (cond ((discretionary-p box)
              (decf nw ddw)             ;cancel effect of pre
              (mapc (lambda (box)
                      (cond ((box-p box)
                             (incf ddw (box-width box)))
                            ((glue-p box)
                             (incf nw (glue-width box))
                             (incf w+ (glue-stretch box))
                             (incf w- (glue-shrink box)))
                            (t
                             (error "Barf! no ~S in discretionary-no please." box) )))
                    (discretionary-no box))))
       (cond ((glue-p box)
              (incf nw (glue-width box))
              (incf w+ (glue-stretch box))
              (incf w- (glue-shrink box)))) )))

(defmacro map-feasible-split-points (boxen bp width cont)
  `(block raus
     (map-split-points ,boxen ,bp
                       (lambda (bp nw w+ w-)
                          (let ((badness (badness2 nw w+ w- ,width)))
                            (when (> badness +badness-infinite+)
                              ;; overful box already
                              (return-from raus))
                            (when (< badness *tolerance*)
                              (,cont bp badness)))))))

(defun map-line (fun boxen start end)
  "Map function to all elements, which would make up the line between start
   and end."
  (let ((end end))
    ;; forget leading glue
    (do ()
        ((not (and (glue-p (elt boxen start))
                   (< (1+ start) end))))
      (incf start))
    ;; forget dangling glue
    ;; don't do that when at end of paragraph though
    (unless (= end (length boxen))
      (do ()
          ((not (and (glue-p (elt boxen (1- end)))
                     (< start (1- end)))))
        (decf end)))
    ;; loop
    (do ((i start (+ i 1)))
        ((>= i end))
      (let ((box (elt boxen i)))
        (cond ((discretionary-p box)
               (cond ((= i start)
                      (mapc fun (discretionary-post box)))
                     ((= i (1- end))
                      (mapc fun (discretionary-pre box)))
                     (t
                      (mapc fun (discretionary-no box)))))
              ((funcall fun box))))))
  ;; Special case:
  ;; when at end of boxen think yourself a \hfil glue
  '(cond ((= end (length boxen))
         (funcall fun +hfil-glue+)) ))

(defun badness2 (nw w+ w- width)
  (let ((delta (- width nw)))
    (cond ((= delta 0)
           ;; perfect fit!
           0)
          ((< delta 0)
           (cond ((= w- 0)
                  +badness-infinite+)
                 ((> (- delta) w-)
                  ;; overful box
                  (* 2 +badness-infinite+))
                 (t
                  (min +badness-infinite+ (badness3 (- delta) w-)))))
          ((> delta 0)
           (cond ((= w+ 0)
                  +badness-infinite+)
                 (t
                  (min +badness-infinite+ (badness3 delta w+))))) )))

(defun badness3 (a b)
  (floor (* a (floor (* a (floor (* 100 a) b)) b)) b))

(defun assign-glue (boxen width)
  (let ((nw 0)
        (sha 0)
        (sta 0))
    (dolist (k boxen)
      (etypecase k
        (BOX (incf nw (box-width k)))
        (GLUE 
         (incf nw (glue-width k))
         (incf sha (glue-shrink k))
         (incf sta (glue-stretch k)))))
    (let ((delta (- width nw)))
      (cond ((= delta 0)
             (dolist (k boxen)
               (and (glue-p k)
                    (setf (glue-assigned k) 0))))
            ((< delta 0)
             ;; shrink
             (dolist (k boxen)
               (and (glue-p k)
                    (multiple-value-bind (a lack) 
                        (round (if (zerop sha)
                                   (glue-width k)
                                   (* delta (/ (glue-shrink k) sha)))
                               *precision*)
                      (setf a (* *precision* a))
                      (decf delta a)
                      (decf sha (glue-shrink k))
                      (setf (glue-assigned k) a)))))
            ((> delta 0)
             ;; shrink
             (dolist (k boxen)
               (and (glue-p k)
                    (multiple-value-bind (a lack) 
                        (round (if (zerop sta)
                                   (glue-width k)
                                   (* delta (/ (glue-stretch k) sta)))
                               *precision*)
                      (setf a (* *precision* a))
                      (decf delta a)
                      (decf sta (glue-stretch k))
                      (setf (glue-assigned k) a)))) )) )))

(defun make-white-space-glue (w)
  (make-glue :width w
             :shrink (* 1/2 w)
             :shrink-unit 0
             :stretch (* 2/3 w)
             :stretch-unit 0))

(defun format-paragraph (boxen width)
  (let ((res nil))
    (setq boxen
      (append
       boxen
       (list
        ;; a hfil fake
        (make-glue :width 0
                   :shrink 0
                   :shrink-unit 0
                   :stretch 1e6
                   :stretch-unit 0) )))
    (let ((sps ))
      (setf boxen (coerce boxen 'vector))
      (setf sps (minimum-split boxen width))
      (when r2::*debug-tex-p*
        (format *trace-output* "==== sps = ~S.~%" sps))
      #+NIL
      (when (null sps)
        ;; ### don't know why this happens.
        (setf sps (list (length boxen))))
      (do ((p0 0 p1)
           (p1 (pop sps) (pop sps)))
          ((null p1))
        (let ((ln (line-subseq boxen p0 (min (length boxen) (+ p1 1)))))
          (assign-glue ln width)
          (push ln res)) ))
    (reverse res)))

(defun minimum-split (boxen width)
  (mapcar (lambda (x)
            (break-point-position (caar x)))
          (minimum-split* boxen (break-points boxen) width)))

(defun minimum-split* (boxen bp width)
  (let (res)
    (cond ((null (cdr bp))
           (values nil 0))
          ((setf res (break-point-cache (car bp)));muessen wir noch modifizieren
           (values-list res))
          (t
           (values-list
            (setf (break-point-cache (car bp))
              (multiple-value-list
               (minimum-split** boxen bp width))))))))

(defun minimum-split** (line3 bp3 width3)
  (cond ((null bp3)
         (values nil 0))
        (t
         ;;(princ "@") (finish-output)
         (let ((best-bp nil)
               (best-bad nil)
               (best-pts nil)
               (best-d #.(expt +badness-infinite+ 2)))
           (map-feasible-split-points line3 bp3 width3
                                      (lambda (bp badness)
                                        (multiple-value-bind (points demerit)
                                            (minimum-split* line3 bp width3)
                                          (setf demerit (+ (expt (+ *line-penality* badness) 2)
                                                           demerit))
                                          (when (or (null best-d) (< demerit best-d))
                                            (setf best-d demerit
                                                  best-bp bp
                                                  best-bad badness
                                                  best-pts points)))))
           (values (and best-bad
                        (cons (cons best-bp best-bad) best-pts))
                   best-d)))))

(defun line-subseq (boxen start end)
  (let ((res nil))
    (map-line (lambda (x) (push x res)) boxen start end)
    (reverse res)))

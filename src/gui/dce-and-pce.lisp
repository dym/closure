;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLUE-GUI2; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Various Caches for Images et al
;;;   Created: 1999-05-25
;;;    Author: Gilbert Baumann <gilbert@base-engineering.com>
;;;   License: MIT style (see below)
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1999,2001 by Gilbert Baumann

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

(in-package :CLUE-GUI2)

;;  2001-11-17  GB      - restructured DCACHE

;;  1999-08-21  GB      - REDRAW-NEW-LAZY-IMAGE: changed according to new 
;;                        display list layout

;;; ---------------------------------------------------------------------------

(defstruct dce
  url
  presentation
  data
  lock)

;;;

(defun dcache-get (document url presentation
                            &key lazy-p callback)
  (let ((url (if (url:url-p url) (url:unparse-url url) url)))
    (let* ((dce
            (bordeaux-threads:with-recursive-lock-held (*dcache-lock*)
              (or
               (find-if (lambda (el)
                          (and (equal (dce-url el) url)
                               (eq (dce-presentation el) presentation)))
                        *dcache*)
               (let* ((lock (bordeaux-threads:make-lock "dce lock"))
		      (new-dce (make-dce :url url
					 :presentation presentation
					 :data :work-in-progress
					 :lock lock))
		      (flag (bordeaux-threads:make-condition-variable)))
                 (bordeaux-threads:with-recursive-lock-held (lock)
		   (r2::run-process-on-behalf-of-document
		    document
		    (lambda ()
		      (bordeaux-threads:with-recursive-lock-held (lock)
			(bordeaux-threads:condition-notify flag)
			(setf (dce-data new-dce)
			      (dcache-generate-presentation presentation document url)) )))
		   (bordeaux-threads:condition-wait flag lock)
		   (push new-dce *dcache*))
                 new-dce)))))
      (if lazy-p
          (progn
            (r2::run-process-on-behalf-of-document
             document
             (lambda ()
               (funcall callback
                        (bordeaux-threads:with-recursive-lock-held ((dce-lock dce))
                          (dce-data dce)))))
            nil)
          (bordeaux-threads:with-recursive-lock-held ((dce-lock dce))
            (dce-data dce)) ))))

(defmethod dcache-generate-presentation ((presentation (eql :aimage)) document url)
  (handler-case (aimage-from-url-real document url)
                (error ()
                       :error)))

(defun aimage-from-url-real (document url)
  (let ((res (r2::url->aimage-real document url nil)))
    (when res
      (setf (getf (imagelib:aimage-plist res) :url)
        url))
    (or res :error)))

(defun aimage-from-url (document url)
  (dcache-get document url :aimage))

(defun aimage-from-url-lazy (document url callback)
  (dcache-get document url :aimage :lazy-p t :callback callback))

(defun my-ignore-errors-fn (continuation)
  (multiple-value-bind (res condition)
      (ignore-errors (funcall continuation))
    (when condition
      (format T "~&Error in background process:~%~A" condition)
      (describe condition *standard-output*))
    res))

(defmacro my-ignore-errors (&rest body)
  `(my-ignore-errors-fn (lambda ()
                          ,@body)))

(defstruct pce
  aimage
  width
  height
  design
  refcount)

(defun make-design-from-aimage (medium aimage width height)
  (dolist (k *pixmap-cache*
            (let ((res  (really-make-design-from-aimage
                         medium aimage width height)))
              (when *debug-pixmap-cache-p*
                (format T "~&;; ++ [init] ~A ~Dx~D "
                        (getf (imagelib:aimage-plist aimage) :url)
                        width 
                        height))
              (push (make-pce :aimage aimage
                              :width width
                              :height height
                              :design res
                              :refcount 1)
                    *pixmap-cache*)
              res))
    (when (and (eq (pce-aimage k) aimage)
               (eql (pce-width k) width)
               (eql (pce-height k) height))
      (when *debug-pixmap-cache-p*
        (format T "~&;; ++ ~A ~Dx~D "
                (getf (imagelib:aimage-plist aimage) :url)
                width 
                height))
      (incf (pce-refcount k))
      (return (pce-design k)))))

(defun really-make-design-from-aimage (medium aimage width height)
  (climi::make-rgb-image-design medium
				(imagelib::aimage-rgb-image
				 (imagelib:scale-aimage aimage width height))))

(defun reset-caches ()
  (setf *dcache* nil
        *pixmap-cache* nil))

;; apparently unused  --dfl
;;;(defun ws/x11::aimage->pixmap+mask (drawable aimage)
;;;  (make-design-from-aimage drawable aimage 
;;;                           (imagelib:aimage-width aimage)
;;;                           (imagelib:aimage-height aimage)))

(defclass r2::ro/img ()
  ((url                    :initarg :url)
   (aim-orig :initform nil :initarg :aim-orig)
   (aim      :initform nil)
   (width    :initform nil)
   (height   :initform nil)
   (design   :initform nil)))

(defmethod print-object ((self r2::ro/img) sink)
  (format sink "#<~S url=~S>" (type-of self) 
          (if (slot-boundp self 'url)
              (slot-value self 'url)
            :unbound)))

(defmethod deconstruct-robj ((self r2::ro/img))
  (with-slots ((aim-orig aim-orig) (design design)) self
    (when design
      (deref-aimage-design aim-orig design)
      (setf design nil))))

(defun deref-aimage-design (aimage design)
  (declare (ignore aimage))
  (let ((pce (find design *design-cache* :key #'pce-design :test #'equal)))
    (assert (not (null pce)))
    (assert (> (pce-refcount pce) 0))
    (when *debug-pixmap-cache-p*
      (format T "~&;; -- ~A ~Dx~D "
              (getf (imagelib:aimage-plist (pce-aimage pce)) :url)
              (pce-width pce)
              (pce-height pce)))
    (decf (pce-refcount pce))))

(defun flush-pixmap-cache ()
  (let ((n 0))
    (setf *pixmap-cache*
      (mapcan (lambda (pce)
		(cond 
		  ((and (eql (pce-refcount pce) 0) (pce-design pce))
		    (incf n (* (pce-width pce) (pce-height pce)))
		    (climi::free-image-design (pce-design pce))
		    nil)
		  (t
		    (list pce))))
              *pixmap-cache*))
    n))

;;;;;

(defmethod r2::ro/intrinsic-size ((self r2::ro/img)) ;; -> width; height; depth
  (with-slots (aim-orig) self
    (values (r2::aimage-width aim-orig) (r2::aimage-height aim-orig) 0)))

(defmethod r2::ro/size ((self r2::ro/img))      ;; -> width; height; depth
  (with-slots (width height) self
    (assert (and width height))
    (values width height 0)))

(defmethod r2::ro/resize ((self r2::ro/img) new-width new-height)
  (with-slots (width height aim aim-orig design) self
    (cond ((and new-width new-height)
           (setf width (round new-width)
                 height (round new-height)) )
          (t
           (let ((ow (r2::aimage-width aim-orig))
                 (oh (r2::aimage-height aim-orig)))
             ;;
             (when (and (not new-width) (not new-height))
               (setf new-width ow
                     new-height oh))
             (when (not new-width)
               (setf new-width (* new-height (/ ow oh))))
             (when (not new-height)
               (setf new-height (* new-width (/ oh ow))))
             ;; damit da mal nix passiert:
             (setf new-width (max 1 (round new-width)))
             (setf new-height (max 1 (round new-height)))
             (unless (and (eql new-width width) (eql new-height height))
               (setf width new-width
                     height new-height
                     design nil
                     aim nil #+(OR) (if aim (imagelib:scale-aimage aim-orig new-width new-height) nil) ))))) ))

;; apparently unused  --dfl
;;;(defun ensure-ro/img-pixmap (drawable self)
;;;  (with-slots (aim-orig width height design mask) self
;;;    (when (and aim-orig (not design))
;;;      (setf design (make-design-from-aimage drawable aim-orig width height)))))

;; apparently unused  --dfl
;;;(defmethod r2::x11-draw-robj (drawable gcontext (self r2::ro/img) box x y)
;;;  (declare (ignore box))
;;;  (setq x (round x))
;;;  (setq y (round y))
;;;  (with-slots ((aim-orig aim-orig) (width width) (height height)
;;;               (design design)) 
;;;      self
;;;    (ensure-ro/img-pixmap drawable self)
;;;    (when aim-orig
;;;      (cond ((not (null mask))
;;;             (xlib:with-gcontext (gcontext :clip-mask mask
;;;                                           :clip-x x
;;;                                           :clip-y (- y height))
;;;               (xlib:copy-area pixmap gcontext 0 0 width height
;;;                               drawable x (- y height))) )
;;;            (t
;;;             (xlib:copy-area pixmap gcontext 0 0 width height
;;;                             drawable x (- y height) ))))))


;;; ----------------------------------------------------------------------------------------------------




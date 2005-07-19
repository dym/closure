;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: RENDERER; -*-
;;; --------------------------------------------------------------------------------------
;;;     Title: The Document Class
;;;   Created: 1999-05-07 01:56
;;;    Author: Gilbert Baumann <gilbert@base-engineering.com>
;;;   License: MIT style (see below)
;;; --------------------------------------------------------------------------------------
;;;  (c) copyright 1999 by Gilbert Baumann

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

(defclass document ()
  ((pt           :initarg :pt           :initform nil :accessor document-pt)
   (links        :initarg :links        :initform nil :accessor document-links)
   (title        :initarg :title        :initform nil :accessor document-title)
   (display-list :initarg :display-list :initform nil :accessor document-display-list)
   (location     :initarg :location     :initform nil :accessor document-location
                 :documentation "Location of this document (an URL).")
   (http-header  :initarg :http-header  :initform nil :accessor document-http-header)

   ;; list of all processes working for this document
   (processes      :initform nil :accessor document-processes)
   (processes/lock :initform (mp/make-lock :name "doc-proc-list Lock")
                   :accessor document-processes/lock) ;this needs a lock
   (processes-hooks
    ;; a list of hooks to call when ever the value of processes changes.
    :initform nil
    :accessor document-processes-hooks
    :initarg :processes-hooks)
   
   (dead-p         :initform nil :accessor document-dead-p)

   (source         :initform nil :accessor document-source)

   ;;
   (images
    :initform nil
    :accessor document-images)

   (anchors
    :initform nil
    :accessor document-anchors
    :documentation "A list anchors in this document.")

   (selected-author-style
    :initform :default
    :initarg  :selected-author-style
    :accessor document-selected-author-style)
   ))

(defclass link ()
  ((title  :initarg :title  :accessor link-title)
   (rel    :initarg :rel    :accessor link-rel)
   (rev    :initarg :rev    :accessor link-rev)
   (type   :initarg :type   :accessor link-type)
   (media  :initarg :media  :accessor link-media)
   (target :initarg :target :accessor link-target)
   (href   :initarg :href   :accessor link-href)))

(defstruct anchor 
  name                                  ;name of anchor (a string)
  x y)                                  ;coordinates of anchor

(defun document-base-url (document)
  (with-slots (location pt) document
    (or (closure-protocol:element-base-url closure-protocol:*document-language* pt)
        location)))

(defun run-process-on-behalf-of-document (document continuation &key (name "anonymous process"))
  ;; Runs a process on behalf of a document, `continuation' is the
  ;; function to be run within the new process.
  ;; Returns the new process created.
  (mp/with-lock ((document-processes/lock document))
    (let (new-process)
      (setf new-process
        (mp/process-run-function
         name
         ;; << child
         (lambda ()
           (unwind-protect
               (funcall continuation)
             ;; remove myself from the list of processes
             (progn
               (mp/with-lock ((document-processes/lock document))
                 (setf (document-processes document)
                   (delete new-process (document-processes document)))) )))
         ;; >>
         ))
      ;; add new process to list of process
      (push new-process (document-processes document))
      new-process)))
            
(defun kill-all-document-processes (document)
  (setf (document-dead-p document) t)
  (mp/with-lock ((document-processes/lock document))
    (mapc #'mp/process-kill (document-processes document)))
  (mp/process-wait "Waiting for documents processes dying."
                   (lambda ()
                     (null (document-processes document))))
  (values))

(defstruct image-entry
  url
  aimage
  objects)

(defun document-fetch-image (document object url)
  (let ((x (or (find (url:unparse-url url) (document-images document)
                     :test #'equalp
                     :key #'image-entry-url)
               (car (push (make-image-entry :url (url:unparse-url url)) (document-images document))))))
    (pushnew object (image-entry-objects x))
    (image-entry-aimage x)))

(defun document-add-anchor (document name x y)
  (push (make-anchor :name name :x x :y y)
        (document-anchors document)))

(defun document-style-sheet (doc &key (selected-style :default))
  "Compute the documents style. This could be either a <STYLE> element
  in the header or an external style sheet defined via <LINK>."
  ;; It isn't exactly specified, what one should do, when multiple
  ;; STYLE nodes are present.
  ;; We take the route to parse all styles present by either LINK or
  ;; STYLE and combine, as if they occured via @import.
  (let ((sheets nil)
        (pt (document-pt doc)))

    (setq cl-user::pt pt)
    
    ;;
    (dolist (link (document-links doc))
      (when (and (style-sheet-link-p link)
                 (style-link-does-apply-p link selected-style)
                 (link-href link))
        (let* ((media-type 
                (or (ignore-errors
                     (css::parse-media-type (link-media link)))
                    :all))
               (href (link-href link))
               (sheet (maybe-parse-style-sheet-from-url 
                       href
                       :name "Document style via LINK"
                       :media-type media-type)))
          (when sheet
            (push sheet sheets)))))
    ;;
    (let ((style (closure-protocol:root-element-embedded-style
                  closure-protocol:*document-language*
                  pt)))
      (when style
        (multiple-value-bind (res condition)
            (ignore-errors
             (css:parse-style-sheet (cl-char-stream->gstream
                                     (make-string-input-stream
                                      style))
                                    nil
                                    :name "Document Style via STYLE"
                                    :base-url (document-base-url doc)))
          (cond ((null res)
                 (warn "Error while parsing embedded style sheet in ~S:~% ~A"
                       (r2::document-location doc)
                       condition))
                (t
                 (push res sheets))))))
    (setf sheets (nreverse sheets))
    (let ((s (css:create-style-sheet *default-style-sheet*
                                     :name "Document style"
                                     :base-url (document-base-url doc))))
      (setf (css::style-sheet-imported-sheets s)
        sheets)
      s)))

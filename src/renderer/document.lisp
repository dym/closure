;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: RENDERER; -*-
;;; --------------------------------------------------------------------------------------
;;;     Title: The Document Class
;;;   Created: 1999-05-07 01:56
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
    (or (pt-base-url pt)
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
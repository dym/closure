;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLUE-GUI2; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: A GUI based on the CLUE library
;;;   Created: 1999-02-02 07:42
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: GPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
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
;; =======
;;
;;  When        Who     What
;; ----------------------------------------------------------------------------
;;  1999-09-15  GB      - (GET-DRAWABLE+GCONTEXT HT-VIEW) now returns a per 
;;                        process gcontext
;;                      - DESTROY callback on HT-VIEW adjusted accordingly
;;  1999-09-12  GB      - twix from NEW-[HV]BOX to [HV]BOX
;;  1999-08-19  GB      - SUBMIT-FORM-BY-GET, SUBMIT-FORM-BY-POST: 
;;                        changed signature (caller now passes values)

(in-package :clue-gui2)

(defvar *top*)
(defvar *history*)

(defparameter *dpis*
  '(60 76 88 100 120 160))

(defparameter *dpy* nil)
(defparameter *gui-device* nil)

(defparameter *dcache* nil)

(defparameter *dcache-lock* (mp/make-lock :name "dcache"))

(defparameter *pixmap-cache*
    nil)

(defparameter *debug-pixmap-cache-p*
    t)

(defparameter *multiple-selection-title* "(Multiple Selection)")

(defparameter *gui-element-font* "-*-lucida-medium-r-*-*-*-120-*-*-*-*-iso8859-1"
  "Default font for GUI elements.")

;; some widgets^H^H^H^H^H^H^Hcontacts (these should be bundled into some application object)

(defvar *application-shell*)
(defvar *wholine-label*)
(defvar *url-label*)

(defvar *style-menu-button-foo*)
(defvar *style-menu-pane*)


;;;
;;; Requests
;;;

(defstruct (request (:constructor make-request/low))
  (method :get :type (member :get :post))
  url
  post-data                             ;for post requests
  cache-file-head
  post-header
  cache-file-data)

(defun make-request (&rest options)
  (let ((res (apply #'make-request/low options)))
    #+EXCL
    (excl:schedule-finalization 
     res
     (lambda (x)
       (format T "~&********* shadow of ~A deleted." (request-url x))
       (glisp:delete-temporary-file (request-cache-file-head x))
       (glisp:delete-temporary-file (request-cache-file-data x)) ))
    res))

;;; ---------------------------------------------------------------------------
;; we refine GUI:PRIM-HT-VIEW

(defcontact ht-view (gui::prim-ht-view glue::fake-to-fool-clue composite)
  ((gcontexten 
    ;; this now is an alist mapping from processes to GCONTEXTs
    :initform nil
    )
   (pt       :initform nil :initarg :pt)
   (awidth   :initform nil)
   (frame    :initarg :frame
             :documentation "an q/frame")
   (document :initarg :document :initform nil)
   (double-buffering-p :initarg :double-buffering-p :initform nil)
   (pixmap :initform nil)
   
   ;; clue contact class option:
   (compress-exposures :initform :on) ))

(defmethod cluei::initialize-contact :after ((self ht-view))
  (clue:add-callback self :destroy
                     (lambda ()
                       (with-slots (gcontexten pt document 
                                    frame
                                    callbacks
                                    (display-list gui::display-list)
                                    (active-pt gui::active-pt)
                                    (active-link gui::active-link)) 
                           self
                         (setf callbacks nil)
                         (mapc #'xlib:free-gcontext (mapcar #'cdr gcontexten))
                         (setf frame nil)
                         (setf pt nil)
                         (setf document nil)
                         (setf display-list nil)
                         (setf active-pt nil)
                         (setf active-link nil)))))
                         

;; bind some events
(defevent ht-view :motion-notify motion-notify)
(defevent ht-view :button-press  button-press)
(defevent ht-view :key-press     key-press)

(defun frame-warp-to (frame x y)
  (with-slots (sw htv document-height) frame
    (setf (glue:thumb-pos (glue:scrolled-window-vertical-scrollbar sw))
      (if (zerop document-height)
          0
        (/ y document-height)))
    (xlib:warp-pointer htv (round x) (round y)) ))

(defmethod set-new-width ((self ht-view) new-width)
  (with-slots (awidth) self
    (unless (eql awidth new-width)
      (setf awidth new-width)
      (reformat self))))

(defmethod motion-notify ((self ht-view))
  (with-event (x y state)
    (gui:handle-event self (make-instance 'gui:pointer-motion-event
                             :x x
                             :y y
                             :state state))))

(defmethod button-press ((self ht-view))
  (with-event (x y state)
    (gui:handle-event self (make-instance 'gui:button-press-event
                             :x x
                             :y y
                             :state state))))

(defmethod gui::get-drawable+gcontext ((self ht-view))
  (with-slots (gcontexten double-buffering-p pixmap) self
    (cond ((realized-p self)
           (let ((gcontext (cdr (assoc (mp/current-process) gcontexten))))
             (unless gcontext
               (setf gcontext (xlib:create-gcontext :drawable self))
               (push (cons (mp/current-process) gcontext) gcontexten))
             (values (if double-buffering-p pixmap self)
                     gcontext)))
          (t
           (values nil nil)))))

(defmethod gui::set-mouse-documentation ((self ht-view) string)
  (declare (ignorable string))
  ;;  (describe self)
  (let ((str (if (stringp string)
                 string
               (map 'string (lambda (x) (or (code-char x) #\?)) string))))
    (xlib:change-window-documentation self str)
    '(balloon self str))
  ;;
  (setf (label-string *wholine-label*) string))

(defmethod gui::handle-activated-link ((self ht-view) (link r2::hyper-link) x y)
  (declare (ignore x y))
  (with-slots (document) self
    ;; hmm was ist mit same?!
    (when (r2::hyper-link-url link)
      (goto-url (r2::hyper-link-url link)
                (or (r2::hyper-link-target link)
                    (r2::pt-base-target (r2::document-pt document)) ;zzz
                    "_top") ))
    nil))

(defmethod gui::handle-activated-link ((self ht-view) (link r2::server-side-image-map) x y)
  (with-slots (document) self
    (with-slots ((url r2::url) (target r2::target)) link
      (let ((new-url (url:copy-url url)))
        (setf (url:url-query new-url)
          (format nil "~D,~D" x y))
        (goto-url new-url target)))))

(defmethod gui::handle-activated-link ((self ht-view) (link r2::graphical-submit) x y)
  (with-slots (document) self
    (with-slots ((name r2::name) (pt r2::pt)) link
      (let ((form (gui::find-form-element pt)))
        (cond ((not (null form))
               (labels ((aux (s)
                          (if (and name (> (length name) 0))
                              (concatenate 'rod name (string-rod ".") (string-rod s))
                            (string-rod s))))
                 (submit-form document form
                              (list (cons (aux "x") (string-rod (format nil "~D" x)))
                                    (cons (aux "y") (string-rod (format nil "~D" y)))))))
              (t
               (warn "Graphical submit button has no <FORM> element.")))))))

(defmethod gui::handle-activated-link ((self ht-view) (link t) x y)
  (declare (ignore x y))
  (warn "Do not know how to handle ~S link^wcommand." link))

(defmethod r2::canvas-physical-origin ((self ht-view))
  (values (- (xlib:drawable-x self))
          (- (xlib:drawable-y self))))

(defmethod r2::canvas-physical-origin ((self xlib:pixmap))
  (values 0 0))


(defmethod cluei::display-region ((self ht-view) region)
  (gui:handle-event self (make-instance 'gui:exposure-event :region region)))

#+(OR)
(defmethod display ((self ht-view) 
                    &optional (x 0) (y 0)
                              (width (- (contact-width self) x))
                              (height (- (contact-height self) y))
                    &key)
  (setf x 0 y (- (xlib:drawable-y self)) 
        width (contact-width (clue:contact-parent self))
        height (contact-height (clue:contact-parent self)))
  (with-slots (double-buffering-p pixmap gcontext) self
    (let ((w (contact-width (clue:contact-parent self)))
          (h (contact-height (clue:contact-parent self))))
      (setf (slot-value self 'double-buffering-p) t)
      (setf (slot-value self 'pixmap)
        (xlib:create-pixmap :drawable self
                            :width w
                            :height h
                            :depth (xlib:drawable-depth self)))
      (setf (getf (xlib:pixmap-plist pixmap) :colormap)
        (xlib:window-colormap self))
      (let ((r2::*xoff* 0)
            (r2::*yoff* (+ (xlib:drawable-y self))))
        ;;(xlib:draw-rectangle pixmap gcontext 0 0 #x7FFF #x7FFF t)
        
        (gui::get-drawable+gcontext self)
        (setf (xlib:gcontext-clip-x gcontext) 0
              (xlib:gcontext-clip-y gcontext) r2::*yoff*
              )
        (setf (xlib:window-background self)
          1)
        (gui:handle-event self (make-instance 'gui:exposure-event 
                                 :region 
                                 (gu:make-rectangle* 
                                  x y ;;(+ y r2::*yoff*)
                                  (+ x width) 
                                  (+ y height 1) )))
        (setf (xlib:gcontext-clip-mask gcontext)
          :none)
        (xlib:copy-area pixmap gcontext x (+ y r2::*yoff*) width (1+ height)
                        self x y))
      (xlib:free-pixmap pixmap)
      (setf pixmap nil)
      (setf double-buffering-p nil))))

;; hack
(defvar *orig-window-colormap* #'xlib:window-colormap)
(defun xlib:window-colormap (window)
  (cond ((xlib:pixmap-p window)
         (getf (xlib:pixmap-plist window) :colormap))
        ((funcall *orig-window-colormap* window))))

(defmethod gui:handle-event ((self gui::prim-ht-view) (event gui::exposure-event))
  (with-slots (gui::display-list) self
    ;; wir sollten die region in dem event noch an unserem fenster clippen...
    (multiple-value-bind (drawable gcontext) (gui::get-drawable+gcontext self)
      (setf (xlib:gcontext-clip-mask gcontext) (gu:region-to-x11-rectangle-list (gui::event-region event))
            (xlib:gcontext-clip-x gcontext) 0
            (xlib:gcontext-clip-y gcontext) 0
            )
      (gui::paint-display-list drawable gcontext 0 0 gui::display-list (gui::event-region event) nil))))

#||
;; Ah jetzt geht es los:

       (#<event NIL> :NO-EXPOSURE NIL 26909 #<XLIB:DRAWABLE :0 41944845>)
||#

(defmethod cluei::dispatch-event (event (event-key (eql :no-exposure)) 
                                  send-event-p sequence (drawable xlib:drawable))
  '(print `(,event-key :event :for ,drawable :into :/dev/null))
  )


;;;; ==========================================================================

(defclass q/frame ()  ;; beschreibungsstruktur fuer einen frame
  ((name :initarg :name)                ;sein name
   (app  :initarg :app)                 ;die app
   
   ;; widgets
   (sw)                                 ;das scrolled-window
   (htv)                                ;das ht-view widget
   (pt :initform nil)                   ;der pt?!
   (parent :initarg :parent)            ;was auch immer
   (owidth :initform nil)
   (document-height :initform 0)
   (document-width :initform 0)
   (rendering-process :initform nil)
   (reformat-height :initform nil)
   (reformat-width  :initform nil)
   ))

(defun create-frame (&key parent-widget app form-options parent)
  ;; creates a new frame.
  (let ((self (make-instance 'q/frame 
                :parent parent
                :app app)))
    (with-slots (sw htv) self
      (setf sw (make-contact 'scrolled-window
                             ;;:width 600
                             ;;:height 600
                             :parent parent-widget
                             :background *default-background*
                             :padding-height 0 :padding-width 0
                             :shadow-width 0 :margin-height 0 :margin-width 0
                             :scrolling-policy :application-defined
                             :expand-p t) ;??
            htv (make-contact 'ht-view
                              :parent (scrolled-window-viewport sw)
                              :frame self
                              :width #x7FFF :height #x7FFF
                              :background :none
                              :pt nil))
      (let ((dx 0)
            (dy 0))
        (add-callback (scrolled-window-vertical-scrollbar sw) :value-changed
                      (lambda (v)
                        (setf dy (round (* -1 v (slot-value self 'document-height))))
                        (clue:move htv dx dy)))
        (add-callback (scrolled-window-horizontal-scrollbar sw) :value-changed
                      (lambda (v)
                        (setf dx (round (* -1 v (slot-value self 'document-width))))
                        (clue:move htv dx dy))))
      (add-callback sw :viewport-resize
                    (lambda (width height)
                      height
                      (frame-reformat self width)))
      #+NIL
      (add-callback sw :available-width-changed 
                    (lambda (n) (frame-reformat self n)))
      self)))

(defmethod deconstruct-robj ((self t))
  )

(defun deconstruct-display-list (dl)
  (error "Do not call me"))

(defmethod frame-install-display-list ((self q/frame) display-list)
  (with-slots (htv) self
    '(with-slots ((dl gui::display-list)) htv
      (when dl
        (deconstruct-display-list dl)))
    (gui::set-display-list htv display-list)
    ;;xxx
    (when (clue:realized-p htv)
      (glue::display-region htv (gu:make-rectangle* 0 0 1000 1000)))
    (when (realized-p htv)
      '(display htv))))

(defmethod frame-reformat ((self q/frame) width &optional (forcep nil))
  (with-slots (pt htv owidth) self
    (when (or (not (eql width owidth)) forcep)
      (setf owidth width)
      (when pt
        (frame-reformat-aux self width))
        nil)))

(defmethod frame-reformat-really ((self q/frame))
  (with-slots (pt htv owidth) self
    (when pt 
      (frame-reformat-aux self owidth) )))

(defun deconstruct-document (doc)
  (let ((dl (r2::document-display-list doc)))
    ;; first kill all processes
    (r2::kill-all-document-processes doc)
    ;; after that was done clean the todo queue from all events
    ;; corresponding to this document
    (mp/with-lock ((queue-lock *todo-queue*))
      (let ((contents (loop 
                          while (queue-listen *todo-queue*)
                          collect (queue-get *todo-queue*))))
        (setf contents
          (remove-if (lambda (item)
                       (and (todo-entry-document item)
                            (eq (todo-entry-document item) doc)))
                     contents))
        (dolist (k contents)
          (queue-put *todo-queue* k))))
    ;; then destroy all robjs'
    (dolist (k (and dl (gui:display-list-items dl)))
      (r2::map-boxen (lambda (box)
                       (when (r2::rbox-p box)
                         (deconstruct-robj (renderer::rbox-obj box))))
                     k))
    ;; finally nuke the parse tree, links et al.
    (setf (r2::document-pt doc) nil
          (r2::document-links doc) nil
          (r2::document-display-list doc) nil) ))

(defmethod frame-install-doc ((self q/frame) doc)
  (with-slots (pt owidth htv) self
    (with-slots (document) htv
      (when document (deconstruct-document document))
      (setf pt (and doc (r2::document-pt doc))
            document doc)
      (when (clue:realized-p (slot-value (slot-value self 'sw) 'glue::viewhole))
        (unless owidth 
          (setf owidth (clue:contact-width (slot-value (slot-value self 'sw)
                                                       'glue::viewhole))))
        (when owidth
          (let ((oowidth owidth))
            (setf owidth nil)
            (frame-reformat self oowidth)))))))

;;; ------------------------------------------------------------

(defclass q/frameset ()
  ((app :initarg :app)
   (parent :initarg :parent)
   ;; widgets
   (grid)
   (children) ))

(defstruct frameset-child
  container                             ;the container widget
  view)                                 ;the view object ??!

(defun create-frameset (&key app cols rows parent-widget paned-options parent)
  (let ((self (make-instance 'q/frameset :parent parent :app app)))
    (with-slots (grid children) self
      (setf grid (apply #'make-contact 'grid
                        :parent parent-widget 
                        :cols (make-grid-pans cols) 
                        :rows (make-grid-pans rows)
                        :expand-p t
                        ;;:separator-size 0
                        ;;:background :black;zzz
                        paned-options))
      (setf children
        (mapcar (lambda (widget)
                  (make-frameset-child :container widget :view nil))
                (loop for i from 0 to (1- (* (length cols) (length rows)))
                    collect (make-simple-frame grid))))
      self)))

;;; ------------------------------------------------------------

(defun make-pt-from-input (input mime-type url)
  (let* ((charset :iso-8859-1)
         (mime-type (multiple-value-bind (type subtype parameters)
                        (netlib::parse-mime-content-type mime-type)
                      (let ((cs (assoc :charset parameters :test #'string-equal)))
                        (when cs
                          (setf charset (cdr cs))))
                      (netlib::find-mime-type (format nil "~A/~A" type subtype)))))
    (let ((pt (progn 
                (cond ((member mime-type (list (netlib:find-mime-type "image/png")
                                               (netlib:find-mime-type "image/gif")
                                               (netlib:find-mime-type "image/jpeg")))
                       (sgml:lhtml->pt
                        `(:HTML
                          (:BODY
                           (:IMG :SRC ,(url:unparse-url url))))))
                      ((member mime-type (list (netlib:find-mime-type "text/lml")))
                       (sgml:lhtml->pt (read-from-string (with-output-to-string (bag)
                                                           (do ((x (glisp:g/read-byte input nil nil)
                                                                   (glisp:g/read-byte input nil nil)))
                                                               ((null x))
                                                             (write-char (code-char x) bag))))))
                      ((member mime-type (list (netlib:find-mime-type "text/html")))
                       (sgml::parse-html input charset))
                      ((or t (member mime-type (list (netlib:find-mime-type "text/plain")
                                                     (netlib:find-mime-type "text/css"))))
                       (sgml:lhtml->pt
                        `(:HTML
                          (:BODY
                           (:PRE
                            ,(gstream-as-string input))))))

                      #||
                      (t
                       (download input charset url))
                      ||#
                                 
                      ))))
      pt)))
   

(defun next-dpi (dpi)
  (dolist (k *dpis* dpi)
     (when (> k dpi) (return k))))

(defun prev-dpi (dpi)
  (dolist (k (reverse *dpis*) dpi)
     (when (< k dpi) (return k))))

(defun cmd/enlarge ()
  (setf (slot-value (getf (xlib:display-plist (the-display))
			  'gui::device)
		    'ws/x11::dpi)
	(next-dpi (slot-value (getf (xlib:display-plist (the-display))
				    'gui::device)
			      'ws/x11::dpi)))
  (reformat-all))

(defun cmd/shrink ()
  (setf (slot-value (getf (xlib:display-plist (the-display))
			  'gui::device)
		    'ws/x11::dpi)
	(prev-dpi (slot-value (getf (xlib:display-plist (the-display))
				    'gui::device)
			      'ws/x11::dpi)))
  (reformat-all))

(defun reformat-all ())

;;; ---------------------------------------------------------------------------

(defclass clue-device (gui:proxy-device)
  ((htview :initarg :htview)))

(defmethod r2::device-canvas-height ((self clue-device))
  (with-slots (htview) self
    (contact-height (contact-parent htview))))

(defmethod r2::device-canvas-width ((self clue-device))
  (with-slots (htview) self
    (contact-width (contact-parent htview))))

;;; ---------------------------------------------------------------------------

(defvar user::*n*)

(defmethod r2::lazy-object-region (display-list (self null))
  display-list
  gu:+everywhere+)

(defmethod r2::lazy-object-region (display-list (ro r2::ro/image))
  (let ((res gu:+nowhere+))
    (dolist (k (gui:display-list-items display-list))
      ;; grrf. find the parent of this robj
      (r2::map-boxen (lambda (box)
                       (when (and (r2::abox-p box) 
                                  (find-if (lambda (x)
                                             (and (r2::rbox-p x)
                                                  (eq (r2::rbox-obj x) ro)))
                                           (r2::abox-contents box)) )
                         (multiple-value-bind (x0 y0 x1 y1)
                             (values (r2::abox-bx0 box) (r2::abox-by0 box) 
                                     (r2::abox-bx1 box) (r2::abox-by1 box))
                           (cond ((and x0 y0 x1 y1)
                                  (setf res (gu:region-union res (gu:make-rectangle* x0 y0 x1 y1))))
                                 (t
                                  (warn "Hugh?! a box without a bounding box?")) ))))
                     k))
    res))

(defmethod frame-reformat-aux ((self q/frame) width)
  (with-slots (htv sw pt document-height parent rendering-process reformat-width) self
    (cond
     (rendering-process
      (setf reformat-width width))
     (t
      (setf reformat-width nil)
      (with-slots (document) htv
        (let* ((doc document))
          (setf rendering-process
            (r2::run-process-on-behalf-of-document
             doc
             (lambda (&aux dl)
               (unwind-protect
                   (progn
                     (inc-worker)
                     (setf dl 
                       (r2::render-pt (make-device-for-frame self) doc pt width t 0
                                      :selected-style (r2:document-selected-author-style doc)))
                     (gui-post doc
                               'frame-reformat-part-2
                               self dl width)
                     ;;
                     ;; care for images
                     ;;
                     (let (x need-reformat-p)
                       (while (setq x (find nil (r2:document-images document) 
                                            :key #'r2::image-entry-aimage
                                            :from-end t))
                         (setf need-reformat-p nil)
                         (setf (r2::image-entry-aimage x)
                           (r2::url->aimage document (r2::image-entry-url x)))
                         (dolist (obj (r2::image-entry-objects x))
                           (let ((? (r2::update-lazy-object document obj)))
                             (setf need-reformat-p (or need-reformat-p ?))))
                         (cond (need-reformat-p
                                (setf dl (r2::render-pt (make-device-for-frame self)
                                                        doc pt width t 0
                                                        :selected-style
                                                        (r2:document-selected-author-style doc) ))
                                (gui-post doc
                                          'frame-reformat-part-2
                                          self dl width))
                               (t
                                ;; only redraw
                                ;; XXXX **hack**
                                (gui::set-display-list htv dl)
                                ;; find the region to expose
                                (let ((region +nowhere+))
                                  (dolist (ro (r2::image-entry-objects x))
                                    (setf region (gu:region-union region
                                                                  (r2::lazy-object-region dl ro))))
                                  ;; limit to sane X coordinates
                                  (setf region (gu:region-intersection
                                                region
                                                (gu:make-rectangle* 0 0 #x7FFF #x7FFF)))
                                  (when (clue:realized-p htv)
                                    (glue::display-region htv region))) )))) )
                 ;; unwind
                 (dec-worker)))
             :name "reformat"))) )))))

(defmethod frame-reformat-part-2 ((self q/frame) dl width)
  (with-slots (htv sw pt document-height parent rendering-process reformat-width document-width)
      self
    (setf rendering-process nil)
    (cond (reformat-width
           (frame-reformat-aux self reformat-width))
          (t
           (with-slots (document) htv
             ;; is is only a rough approximation
             (setf (clue:wm-title *application-shell*)
               (format nil "Closure: ~A" (r2::document-title document)))
             ;;
             (let ((alternative-author-style-sheets nil))
               (dolist (k (r2:document-links document))
                 (when (and (r2:alternate-style-sheet-link-p k)
                            (not (null (r2:link-title k))))
                   (pushnew (r2:link-title k) alternative-author-style-sheets
                            :test #'r2:style-sheet-name-equal-p)))
               (when alternative-author-style-sheets
                 (push :default alternative-author-style-sheets))
               (foo-update-author-style (slot-value self 'htv) alternative-author-style-sheets))
             ;;
             '(print (gui:display-list-items dl))
             ;;
             (let ((box (car (gui:display-list-items dl))))
               (setf (r2::bbox-by1 box) 
                 (min 32000 (r2::bbox-by1 box)))
               (frame-install-display-list self dl)
               (setf document-height (r2::bbox-by1 box))
               (setf document-width (r2::bbox-bx1 box))
               (let ((x (min 1 (/ width (r2::bbox-bx1 box)))))
                 (cond (nil ;;(= x 1)
                        (setf (glue::scrolled-window-hbar-visible-p sw) nil) )
                       (t
                        '(setf (glue::scrolled-window-hbar-visible-p sw) t)
                        (setf (thumb-size (scrolled-window-horizontal-scrollbar sw)) x))))
               (let ((x (min 1 (/ (contact-height (scrolled-window-viewport sw)) 
                                  (r2::bbox-by1 box)))))
                 (cond (nil ;;(= x 1)
                        (setf (glue::scrolled-window-vbar-visible-p sw) nil))
                       (t
                        '(setf (glue::scrolled-window-vbar-visible-p sw) t)
                        (setf (thumb-size (scrolled-window-vertical-scrollbar sw)) x))))))))))

;;; --------------------------------------------------------------------------------

(defstruct zview
  ;; union of ZFRAME and ZFRAMESET
  serial        ;an unique serial number
  request       ;the request, which leads to this
  gui-hook)     ;a hook for the GUI to include data

(defstruct (zframe (:include zview))
  ;; a simple frame
  document                              ;document displayed
  plist                                 ;yet another hook
  )

(defstruct (zframeset (:include zview))
  ;; a frameset
  rows
  cols
  children)     ;list of ZFRAMESET-CHILDs

(defstruct zframeset-child
  ;; a single child of a ZFRAMESET 
  name          ;name as found in <FRAME> element; (a rod)
  content)      ;ZVIEW object

(defvar *pb* nil)

(defclass pb-stream (use-byte-for-char-stream-flavour gstream)
  ((nread        :initarg :nread        :initform 0 )
   (ntotal       :initarg :ntotal       :initform 0)
   (progress-bar :initarg :progress-bar :initform *pb*  :accessor pb-stream-progress-bar)
   (old-value                           :initform nil)
   (proxee       :initarg :proxee)
   (serial       :initarg :serial)
   (dumpee       :initarg :dumpee       :initform nil) ) )

(defmethod g/read-byte ((stream pb-stream)  &optional (eof-error-p t) eof-value)
  (with-slots (nread ntotal proxee dumpee) stream
    (let ((res (g/read-byte proxee eof-error-p eof-value)))
      (when (and (not (eq res eof-value)) dumpee)
        (g/write-byte res dumpee))
      (incf nread)
      (pb-stream-update stream)
      res)))

(defmethod g/read-byte-sequence (sequence (stream pb-stream) &key (start 0) (end (length sequence)))
  (with-slots (nread ntotal proxee dumpee) stream
    (let ((res (g/read-byte-sequence sequence proxee :start start :end end)))
      (incf nread (- res start))
      (when dumpee
        (g/write-byte-sequence sequence dumpee :start start :end res))
      (pb-stream-update stream)
      res)))

(defmethod g/close ((stream pb-stream) &key abort)
  (with-slots (proxee dumpee) stream
    (when dumpee
      (g/close dumpee))
    (g/close proxee :abort abort)))

(defmethod pb-stream-update ((self pb-stream))
  (with-slots (nread ntotal old-value progress-bar) self
    (and progress-bar
         (let ((a (/ (floor (* 100 (/ nread ntotal))) 100)))
           (unless (eql a old-value)
             (setf old-value a)
             (setf (glue:progress-bar-value progress-bar) a)
             (xlib:display-finish-output (xlib:window-display progress-bar)) )))))

(defun open-document-3 (url serial)
  (error "Don't call me"))

(let ((serial 1000)
      (lock (mp/make-lock :name "serial number")))
  (defun get-serial ()
    (mp/with-lock (lock)
      (incf serial)
      serial)))

(defun zview-from-url (url)
  (zview-from-request (make-request :url url :method :get)))
  
(defun zview-from-request (request)
  ;; Given an URL return a zview object.
  ;; May return NIL, if something unexpected occurs.
  (let ((serial (get-serial)))
    (multiple-value-bind (io header) (ignore-errors (open-document-4 request))
      (unwind-protect
          (let ((do-not-close-p nil))
            (cond ((null io)
                   ;; some error occurred
                   (warn "An error occurred, while retreiving ~S:~%~A" (request-url request) header)
                   nil)
                  (t
                   (unwind-protect
                       (multiple-value-bind (type subtype parameters)
                           (netlib::parse-mime-content-type 
                            (netlib::get-header-field header :content-type))
                         (declare (ignore parameters))
                         (cond ((and (string-equal type "text")
                                     (string-equal subtype "html"))
                                (let ((res (zview-from-url/html (request-url request) io header serial)))
                                  (setf (zview-request res) request)
                                  res))
                               (t
                                (warn "~S has unsupported mime type ~A/~A."
                                      (request-url request) type subtype)
                                (setf do-not-close-p t)
                                (download io (request-url request) type subtype)
                                nil)))
                     (unless do-not-close-p
                       (g/close io))) ))) ))))

(defun zview-from-url/html (url io header serial)
  (let ((pt (make-pt-from-input io (netlib::get-header-field header :content-type) url)))
    (cond ((sgml:flat-find-element :frameset pt nil)
           (zview-from-url/frameset url pt header serial))
          (t
           (zview-from-url/normal url pt header serial)) )))

(defun zview-from-url/normal (url pt header serial)
  (make-zframe
   :serial serial
   :document (make-instance 'r2::document
               :processes-hooks (list 'pl-hook)
               :location
               (r2::parse-url* (or (netlib::get-header-field header :location)
                                   url))
               :http-header header
               :pt pt) ))

(defun zview-from-url/frameset (url pt header serial)
  serial
  (let ((doc (make-instance 'r2::document
               :processes-hooks (list 'pl-hook)
               :location (r2::parse-url*
                          (or (netlib::get-header-field header :location)
                              url))
               :http-header header
               :pt pt)))
    (parse-elm->zf doc (sgml:flat-find-element :frameset pt))))

(defun parse-elm->zf (doc elm)
  (ecase (sgml:gi elm)
    (:FRAMESET
     (let ((set (make-zframeset
                 :serial (gensym)
                 :cols (gui::parse-html-frameset-length-list 
                        (r2::pt-attr/latin1 elm :cols "100%"))
                 :rows (gui::parse-html-frameset-length-list 
                        (r2::pt-attr/latin1 elm :rows "100%")))))
       (dolist (k (sgml:pt-children elm))
         (when (member (sgml:gi k) '(:frame :frameset))
           (multiple-value-bind (res name) (parse-elm->zf doc k)
             (setf (zframeset-children set)
               (nconc (zframeset-children set)
                      (list (make-zframeset-child
                             :name name 
                             :content res)))))))
       (values set nil)))
    (:FRAME
     (values
      (zview-from-url (r2::pt-effective-url-attr doc elm :src))
      (r2::pt-attr/latin1 elm :name))) ))

;;;; --------------------------------------------------------------------------------

(defun copy-zview (zview)
  (etypecase zview
    (zframe
     (make-zframe 
      :serial (zframe-serial zview)
      :gui-hook nil                     ;intentionally
      :document (copy-zview-document (zframe-document zview))))
    (zframeset
     (make-zframeset
      :serial (zframeset-serial zview)
      :gui-hook nil                     ;intentionally
      :rows (zframeset-rows zview)
      :cols (zframeset-cols zview)
      :children (mapcar (lambda (child)
                          (make-zframeset-child
                           :name (zframeset-child-name child)
                           :content (copy-zview (zframeset-child-content child))))
                        (zframeset-children zview)))) ))

(defun copy-zview-document (document)
  document)

#||
(make-instance 'r2::document
    :title (r2::document-title document)
    :location (r2::document-location document)
    ))||#

;;;

(defun initialise-zview-data (zview)
  (etypecase zview
    (zframe
     ;; uff, was wenn jetzt wieder zur Abwechslung ein FRAMESET rauskommt?!
     (let ((doc (zframe-document zview)))
       (unless (r2::document-pt doc)
         (let ((url (r2::document-location doc)))
           (multiple-value-bind (io header)
               (open-document-4 (zframe-request zview))
             (setf (r2::document-pt doc)
               (make-pt-from-input io 
                                   (netlib::get-header-field header :content-type)
                                   url))))))
     zview)
    (zframeset
     (dolist (k (zframeset-children zview))
       (setf (zframeset-child-content k)
         (initialise-zview-data (zframeset-child-content k))))
     zview)))

;;;;

(defstruct ghd 
  container
  contact)

;;;;

(defun update-zview (ist soll)
  (cond ((and (zframe-p ist) (zframe-p soll)
              (eql (zframe-serial ist) (zframe-serial soll)))
         ;; nichts zu tun
         ist)
        ((and (zframeset-p ist) (zframeset-p soll)
              (eql (zframeset-serial ist) (zframeset-serial soll)))
         ;; nur durch alle kinder gehen
         (let ((res (copy-zframeset ist)))
           (setf (zframeset-children res)
             (mapcar (lambda (child-ist child-soll)
                       (make-zframeset-child
                        :name (zframeset-child-name child-ist)
                        :content (update-zview 
                                  (zframeset-child-content child-ist)
                                  (zframeset-child-content child-soll))))
                     (zframeset-children ist)
                     (zframeset-children soll)))
           res))
        (t
         ;; jetzt wird es komplizierter:
         ;; alten contact abbauen und neuen dafuer einsetzen
         (let ((ghd (zview-gui-hook ist)))
           (ghd-contact-destroy (ghd-contact ghd))
           (cond ((zframe-p ist)
                  (deconstruct-document (zframe-document ist)))
                 ((zframeset-p ist)
                  ;; noch uber die kinder gehen ...
                  ))
           (realize-zview soll (ghd-container ghd)))) ))

(defmethod ghd-contact-destroy ((self q/frame))
  (clue:destroy (slot-value self 'sw)))

(defmethod ghd-contact-destroy ((self q/frameset))
  (clue:destroy (slot-value self 'grid)))

(defun realize-zview (zview container)
  (etypecase zview
    (zframe
     (let ((fr (create-frame :parent-widget container)))
       (frame-install-doc fr (zframe-document zview))
       (setf (zview-gui-hook zview)
         (make-ghd :contact fr
                   :container container)))
     zview)
    (zframeset
     (let ((fs (create-frameset
                :cols (zframeset-cols zview)
                :rows (zframeset-rows zview)
                :parent-widget container)))
       (setf (zframeset-children zview)
         (mapcar (lambda (zfsc gdc)
                   (make-zframeset-child
                    :name (zframeset-child-name zfsc)
                    :content (realize-zview (zframeset-child-content zfsc)
                                            (frameset-child-container gdc))))
                 (zframeset-children zview)
                 (slot-value fs 'children)))
       (setf (zview-gui-hook zview)
         (make-ghd :contact fs
                   :container container))
       zview)) ))

;;;; --------------------------------------------------------------------------------

(defun g (url)
  (setf url (url:parse-url url))
  (visit-zview (zview-from-url url)))

;;;; --------------------------------------------------------------------------------

(defun inc-worker ()
  #+ALLEGRO (mp::without-scheduling (incf clue-gui2::*n-workers*))
  #-ALLEGRO (incf clue-gui2::*n-workers*)
  (worker-hook))

(defun dec-worker ()
  #+ALLEGRO (mp::without-scheduling (decf clue-gui2::*n-workers*))
  #-ALLEGRO (decf clue-gui2::*n-workers*)
  (worker-hook))

(defun worker-hook ()
  (gui-post nil
            (lambda (value)
              (setf (blinker-on-p *blinker*) value))
            (not (zerop *n-workers*))))

(defun goto-url (url target)
  (unless (url:url-p url)
    (setf url (url:parse-url url)))
  ;; xxx
  (when (string-equal target "_blank")
    (setf target "_top"))
  ;; 
  (mp/process-run-function
   ;; oops -- hier haben wir noch kein document ...?!
   (format nil "~S" url)
   (lambda ()
     (unwind-protect
         (progn
           (inc-worker)
           (format T "~&;; Starting to render ~S on ~S." url target)
           (gui-post nil 'visit-zview (zview-from-url url) target))
       (format T "~&;; Finished ~S." url)
       (dec-worker) ) )))

(defun goto-zview (zv &optional (target "_top"))
  (when zv
    (initialise-zview-data zv)
    (setf *top*
      (update-zview *top* (subst-target-zv *top* zv target)))
    (reflect-zview-in-url-edit-field *top*)
    (reflect-zview-style-sheets *top*) ))

(defun visit-zview (zv &optional (target "_top"))
  (and zv
       (progn
         (setf zv (subst-target-zv *top* zv target))
         (history-add zv)
         (goto-zview zv "_top"))))

(defmethod reflect-zview-in-url-edit-field ((zv zview))
  (setf (glue:sle-string *url-label*)
    (unparse-zview-location zv)))

(defmethod unparse-zview-location ((zv zframeset) &optional base-url)
  (format nil "frameset:~A[~{ ~{~A=~A~}~}]"
          (if (zview-request zv)
              (let ((url (request-url (zview-request zv))))
                (url:unparse-url (if base-url (unmerge-urls url base-url) url)))
            "*")
          (mapcar (lambda (child)
                    (list (zframeset-child-name child)
                          (unparse-zview-location (zframeset-child-content child)
                                                  (if (zview-request zv)
                                                      (request-url (zview-request zv))
                                                    base-url))))
                  (zframeset-children zv))))

(defmethod unparse-zview-location ((zv zframe) &optional base-url)
  (let ((url (request-url (zview-request zv))))
    (url:unparse-url (if base-url (unmerge-urls url base-url) url))))

(defun reflect-zview-style-sheets (top)
  (cond ((zframeset-p top)
         '(foo-update-author-style nil))
        ((zframe-p top)
         '(setq user::x top)
         '(break))
        ))


(defun find-target-zv (zv name)
  (cond ((string-equal name "_top") *top*)
        ((zframe-p zv) nil)
        ((zframeset-p zv)
         (let (x)
           (dolist (k (zframeset-children zv))
             (cond ((and (zframeset-child-name k)
                         (string-equal name (zframeset-child-name k)))
                    (return (zframeset-child-content k)))
                   ((setq x (find-target-zv (zframeset-child-content k) name))
                    (return x))))) )))
         
(defun subst-target-zv (zv new name)
  ;; TODO: do this exactly once.
  (cond ((and (eq zv *top*)
              (string-equal name "_top"))
         new)
        ((zframe-p zv) 
         zv)
        ((zframeset-p zv)
         (make-zframeset 
          :serial (zframeset-serial zv) ;???
          :gui-hook (zframeset-gui-hook zv)
          :rows (zframeset-rows zv)
          :cols (zframeset-cols zv)
          :children
          (loop for k in (zframeset-children zv)
              collect
                (progn 'print
                 (cond ((and (zframeset-child-name k)
                             (string-equal name (zframeset-child-name k)))
                        (make-zframeset-child
                         :name (zframeset-child-name k)
                         :content new))
                       (t
                        (make-zframeset-child
                         :name (zframeset-child-name k)
                         :content (subst-target-zv (zframeset-child-content k) new name))))))
          ))
        (t
         (error "oops")) ))


(defun url-label-callback (new-value)
  (goto-url (url:parse-url new-value) "_top"))

(defvar *blinker*)

(defcontact blinker (glue:3d)
  ((on-p :initform nil :reader blinker-on-p)))

(defmethod clue:preferred-size ((self blinker) &key width height border-width)
  (declare (ignore border-width))
  (unless (zerop (clue:contact-width self))
    (setf width (or width (clue:contact-width self))))
  (unless (zerop (clue:contact-height self))
    (setf height (or height (clue:contact-height self))))
  (when (and (null width) (null height))
    (setf width 20))
  (when (null width)
    (setf width height))
  (when (null height)
    (setf height width))
  (values
   width 
   height
   0))

(defmethod clue:display ((self blinker) &optional x y width height &key)
  (declare (ignore x y width height))
  (with-slots (on-p) self
    (multiple-value-bind (x0 y0 x1 y1) (glue:interior-rectangle* self)
      (glue::old-draw-rectangle* self x0 y0 x1 y1
                                 :foreground (if on-p :red :black)
                                 :filled-p t))))

(defmethod (setf blinker-on-p) (new-value (self blinker))
  (with-slots (on-p) self
    (unless (eq (if new-value 1 0)
                (if on-p 1 0))
      (setf on-p new-value)
      (when (clue:realized-p self)
        (clue:display self)))))

(defvar *links-button*)
(defvar *links-pane*)

(defvar *closure-default-width* 600)
(defvar *closure-default-height* 800)

(defvar *download-dialog* nil)

(defvar *stop-button* nil)
(defvar *back-button* nil)
(defvar *forw-button* nil)

(defun closure ()
  (setf *download-dialog* nil)
  (setf *pb* nil)
  (reset-caches)
  (setq *todo-queue* (make-queue))
  (setq *history* nil)
  (setf *bg-process* nil)
  (setf *gui-device*
    (gui:make-device-for-display (the-display)))
  (setf (xlib:display-after-function (the-display))
    nil #|#'xlib:display-finish-output|# )
  (unwind-protect
      (progn
        (setq *application-shell*
          (make-contact 'top-level-shell
                        :width 600 :height 800
                        :parent (the-display)
                        :background :none))

        (setf (wm-title *application-shell*) "Closure")
        (setf (wm-icon-title *application-shell*) "Closure")

        ;; The overall vertical box
        (let ((vb (make-contact 'glue:vbox
                                :name :overall
                                :margin-height 0 :margin-width 0
                                :shadow-width 0 :shadow-style :none
                                :padding-height 0 :padding-width 0
                                :parent *application-shell*
                                :background *default-background*)))
          ;; The menu-bar
          (let ((menu-bar (make-contact 'glue:hbox
                                        :name :menu-bar
                                        :margin-height 0 :margin-width 0
                                        :shadow-width 2 :shadow-style :groove
                                        :padding-width 0 :padding-height 0 
                                        :parent vb)))
            ;; The View menu
            (multiple-value-bind (shell pane) (make-menu-shell (the-display))
              ;;(make-simple-menu-button pane "View" #'cmd/open-url)
              (make-simple-menu-button pane "Enlarge Fonts" #'nop)
              (make-simple-menu-button pane "Shrink Fonts"  #'nop :sensitive :off)
              '(make-simple-menu-toggle-button pane "No colors" #'nop
                :sensitive :off)
              '(make-simple-menu-toggle-button pane "No author style sheets"  #'nop
                :indicator-type :n-of-many)
              (make-contact 'popup-button 
                            :popee shell
                            :parent menu-bar
                            :label-string "View") )
            ;; The Links menu
            (multiple-value-bind (shell pane) (make-menu-shell (the-display))
              (setf *links-pane* pane)
              (setf *links-button*
                (make-contact 'popup-button 
                              :popee shell
                              :parent menu-bar
                              :sensitive :off
                              :label-string "Links")
                ))

            ;; The Alternative Style Menu
            (multiple-value-bind (shell pane) (make-menu-shell (the-display))
              (setf *style-menu-pane* pane)
              (make-contact 'popup-button 
                            :popee shell
                            :parent menu-bar
                            ;;:sensitive :off
                            :label-string "Style")
              (make-simple-menu-button pane "Default Style" #'nop)
              (make-simple-menu-button pane "User Style: Fancy" #'nop)
              (make-simple-menu-button pane "User Style: Bluish" #'nop)
              #||
              (make-instance 'glue:horizontal-separator :parent pane)
              (setf *style-menu-button-foo*
                (make-simple-menu-button pane "Author Style: A" #'nop))
              (make-simple-menu-button pane "Author Style: B" #'nop)
              ||#
              ))
          ;; Toolbar
          (let ((tb (make-contact 'glue:hbox :parent vb
                                  :shadow-style :groove
                                  :shadow-width 2)))
            ;; Toolbar buttons
            (setf *back-button*
              (make-simple-button tb "Back" 'cmd/back :margin-width 2
                                  :documentation "Back, halt."))
            (setf *forw-button*
              (make-simple-button tb "Forward" 'cmd/forward :margin-width 2))
            (make-simple-button tb "Zoom In" #'cmd/enlarge :margin-width 2)
            (make-simple-button tb "Zoom Out" #'cmd/shrink :margin-width 2)
            (setf *stop-button*
              (make-simple-button tb "Stop" 
                                  'cmd/stop
                                  :margin-width 2
                                  :sensitive :off
                                  :documentation "Stop loading of the current document."))
            ;; Label for the URL text field
            (make-contact 'label :parent tb
                          :label-string "URL:"
                          :padding-height 2
                          :padding-width 2)
            ;; The URL text field itself
            (progn
              (setq *url-label*
                (make-contact 'sle
                              :parent tb
                              :expand-p t
                              :padding-height 2
                              :padding-width 2
                              :shadow-width 2
                              :shadow-style :inset
                              :font "-*-courier-medium-r-*-*-*-120-*-*-*-*-iso8859-1"
                              :string "Hallo"
                              :documentation 
                              (format nil "The currently visited URL. ~
                                       Or type-in an URL to visit.")))
              (clue:add-callback *url-label* :value-entered 'url-label-callback) ))
          
          ;; Frame
          (let ((fr   (make-contact 'glue:vbox
                                    :name :top-frame-container
                                    :parent vb
                                    :margin-width 0 :margin-height 0
                                    :padding-width 2 :padding-height 2
                                    :expand-p t
                                    :shadow-width 0)))

            (setf (contact-constraint fr 'expand-p) t)
            (realize-zview
             (setq *top* 
               (zview-from-url (url:parse-url user::*start-url*)))
             fr)
            (history-add *top*)
            )

          ;; Wholine
          (let ((hb (make-contact 'glue:hbox
                                  :parent vb
                                  :margin-height 0
                                  :margin-width 0
                                  :padding-height 2
                                  :padding-width 2
                                  :shadow-width 0
                                  :shadow-style :inset)))
            (let ((pb
                   (make-contact 'glue:progress-bar
                                 :parent hb
                                 :width 200
                                 :margin-height 0 :margin-width 0
                                 :padding-height 0 :padding-width 0
                                 :shadow-width 1 :shadow-style :inset
                                 :foreground "#319acf"
                                 )))
              (setq *pb* pb)
              (setf (glue:progress-bar-value pb)
                1/3))
            (setq *wholine-label*
              (make-contact 'label
                            :font
                            (r2::make-text-style
                             *gui-device*
                             :font-family (list "helvetica" "arial" "verdana" "symbol" "sans")
                             :font-weight 400
                             :font-size 12
                             :font-style :normal
                             :font-variant :normal
                             :letter-spacing 0
                             :word-spacing 0)
                            :parent hb
                            :expand-p t ;constraint
                            :margin-height 0
                            :margin-width 0
                            :padding-height 2
                            :padding-width 2
                            :shadow-width 1
                            :shadow-style :inset
                            :label-string "Welcome"
                            :alignment :left
                            :background *default-dark-background*
                            ;;:sensitive :off
                            ))
            (setq *blinker*
              (make-contact 'blinker :parent hb
                            :shadow-width 1
                            :shadow-style :inset))
            ))
        ;;(change-geometry *application-shell* :width 600 :height 800)
        (clue:resize *application-shell* *closure-default-width* *closure-default-height* 0)
        (clue:update-state (the-display))
        (event-loop (the-display)) )
    (setf *closure-default-width* 
      (clue:contact-width *application-shell*))
    (setf *closure-default-height* 
      (clue:contact-height *application-shell*))
    (clue:destroy *application-shell*)
    (nimm-den-besen) ))

(defmethod key-press ((self ht-view))
  (with-event (character)
    (case character
      (#\m (cmd/mark))
      (#\r (cmd/recall))
      (#\b (cmd/back))
      (#\f (cmd/forward))
      (#\+ (cmd/enlarge))
      (#\- (cmd/shrink))
      (#\g (cmd/tty-goto))
      (#\h (cmd/home))
      (#\a (cmd/add))
      ;; (#\M (magic self))
      ;; (#\? (cmd/where))
      (#\space (cmd/scroll-down self))
      (#\T (cmd/textarea-hack))

      (#\!
       (cmd/hack))

      )))

(defun cmd/hack ()
  (clue:destroy *style-menu-button-foo*)
  ' (setf *style-menu-button-foo*
    (make-simple-menu-button *style-menu-pane* "Author Style: C" #'nop))
  (clue:change-layout *style-menu-pane*)
  )

(defvar *author-style-buttons*
    nil)

(defun foo-update-author-style (ht-view list-of-style-names)
  ;; Destroy old buttons
  (map nil #'clue:destroy *author-style-buttons*)
  (setf *author-style-buttons* nil)
  (when list-of-style-names
    (let ((pane *style-menu-pane*))
      (push (make-instance 'glue:horizontal-separator :parent pane)
            *author-style-buttons*)
      (dolist (style-name list-of-style-names)
        (let ((style-name style-name))
          (push (make-simple-menu-button pane (format nil "Author Style: ~A" 
                                                      (if (eq style-name :default)
                                                          "(default style)"
                                                        style-name))
                                         #'(lambda ()
                                             (cmd/select-author-style ht-view style-name)))
                *author-style-buttons*)))))
  (clue:change-layout *style-menu-pane*))

(defun cmd/select-author-style (ht-view style-name)
  (let ((q/frame (slot-value ht-view 'frame))
        (doc     (slot-value ht-view 'document)))
    (unless (r2:style-sheet-name-equal-p
             (r2:document-selected-author-style doc)
             style-name)
      (setf (r2:document-selected-author-style doc)
        style-name)
      (let ((width
             (clue:contact-width (slot-value (slot-value q/frame 'sw) 'glue::viewhole))))
        (frame-reformat q/frame width t)))))

(defun cmd/home ()
  (goto-url user::*start-url* "_top"))

(defun cmd/tty-goto ()
  (fresh-line)
  (write-string "URL? ")
  (goto-url (read-line) "_top"))

(defun submit-form-by-get (document form-elm values)
  (let ((new-url (url:copy-url 
                  (r2::pt-effective-url-attr document form-elm :action))))
    (let ()
      (setf values 
        ;; XXX
        (mapcar (lambda (x)
                  (labels ((foo (y)
                             (cond ((and y (typep y 'rod))
                                    (map 'string #'code-char y))
                                   (t y))))
                    (cons (foo (car x)) (foo (cdr x)))))
                values))
      (setf (url:url-query new-url) values))
    (gui-post nil 'goto-url new-url "_top"
              #||(or (r2::pt-attr/latin1 form-elm :target) 
                     (r2::pt-base-target form-elm)
                     "_top")||#
              )))


;;;;
(defun submit-form-by-post (document form-elm values)
  (let ((new-url (url:copy-url (r2::pt-effective-url-attr document form-elm :action))))
    (let ()
      (gui-post nil 'goto-url 
                (make-request :method :post
                              :url new-url
                              :post-data (url-encode-values values)
                              :post-header
                              (list (cons "Content-Type" "application/x-www-form-urlencoded")))
                "_top"))))

(defun utf8-encode (sequence &key (start nil) (end nil) (result-type '(simple-array (unsigned-byte 8) (*))))
  ;; use for small data only
  (setf start (or start 0))
  (setf end (or end (length sequence)))
  (let (res)
    (labels ((put (byte)
               (push byte res) ))
      (do ((i start (+ i 1)))
          ((= i end))
        (let ((c (elt sequence i)))
          (cond ((< c #x80)
                 (put c))
                ((< c #x800)
                 (put (logior #b11000000 (ldb (byte 5 6) c)))
                 (put (logior #b10000000 (ldb (byte 6 0) c))) )
                ((< c #x10000)
                 (put (logior #b11100000 (ldb (byte 4 12) c)))
                 (put (logior #b10000000 (ldb (byte 6 6) c)))
                 (put (logior #b10000000 (ldb (byte 6 0) c))) )
                (t
                 (error "Uff: ~S saw ~D." 'utf8-encode c))))))
    (cond ((subtypep result-type 'string)
           (map result-type #'code-char (nreverse res)))
          (t
           (coerce (nreverse res) result-type)))))

(defun url-encode-values (alist)
  (with-output-to-string (sink)
    (labels ((safe-char-p (code)
               (or (<= #.(char-code #\a) code #.(char-code #\z))
                   (<= #.(char-code #\A) code #.(char-code #\Z))
                   (<= #.(char-code #\0) code #.(char-code #\9))
                   (member code '#.(map 'list #'char-code "$-_.!*'(),"))))
             (put (rod)
               (dolist (code (utf8-encode (rod rod) :result-type 'list))
                 (cond ((= code 10)     ;newline
                        (write-string "%0d%0a" sink))
                       ((= code 32)     ;space
                        (write-char #\+ sink))
                       ((safe-char-p code)
                        (write-char (code-char code) sink))
                       (t
                        (format sink "%~2,'0X" code))))))
      (do ((q alist (cdr q)))
          ((null q))
        (put (caar q))
        (write-char #\= sink)
        (put (cdar q))
        (when (cdr q)
          (write-char #\& sink))))))
        


;;;;



(defun cmd/mark ()
  (setf *mark* *top*))

(defun cmd/recall ()
  (goto-zview *mark*))

(defparameter *download-directory* 
    (merge-pathnames (make-pathname :directory '(:relative "download")) 
                     (user-homedir-pathname)))

(defun download (input url type subtype)
  (when (and (typep input 'pb-stream)
             (typep (slot-value input 'proxee) 'netlib::http-stream))
    (setf (slot-value (slot-value input 'proxee) 'netlib::cache-p) nil)) ;zzz
  (mp/process-run-function
   "download"
   (lambda ()
     (let ((start (get-universal-time))
           (end nil))
       (unwind-protect
           (progn
             (with-open-file (sink 
                              (merge-pathnames 
                               (make-pathname :name ".manifest")
                               *download-directory*)
                              :direction :output
                              :if-does-not-exist :create
                              :if-exists :append)
               (format sink "~A ~D ~A/~A~%" 
                       (url:unparse-url url)
                       (get-universal-time)
                       type
                       subtype))
             (with-open-file (sink
                              (merge-pathnames
                               (make-pathname
                                :name (car (last (url:url-path url)))) ;xxx
                               *download-directory*)
                              :if-exists :new-version
                              :direction :output
                              :element-type '(unsigned-byte 8))
               (netlib::copy-gstream input (cl-byte-stream->gstream sink)
                                     ))
             (setf end (get-universal-time))
             (when (> (- end start) 0)
               (print (* 1.0 (/ (slot-value input 'nread) (- end start))))))
         (g/close input)))) ))

;; A very simple history for now

(defvar *history*)

(defstruct hent
  prev
  next
  zview)

(defun history-add (zview)
  (let ((new (make-hent :next nil :prev *history* :zview zview)))
    (labels ((foo (x)
               (cond ((zframe-p x)
                      )
                     ((zframeset-p x)
                      (mapc #'foo (mapcar #'zframeset-child-content
                                          (zframeset-children x)))) )))
      (foo zview))
    (when *history* (setf (hent-next *history*) new))
    (setf *history* new)))

(defun history-back ()
  (when (hent-prev *history*)
    (setf *history* (hent-prev *history*))))

(defun history-forw ()
  (when (hent-next *history*)
    (setf *history* (hent-next *history*))))

(defun cmd/back ()
  (history-back)
  (goto-zview (hent-zview *history*)))

(defun cmd/forward ()
  (history-forw)
  (goto-zview (hent-zview *history*)))


;;; ---------------------------------------------------------------------------

(defvar *n-workers* 0)

(defun pl-hook (doc))

(defun cmd/add ()
  (with-open-file (sink (merge-pathnames (user-homedir-pathname)
                                         (make-pathname :name ".closure-bookmarks.html"))
                   :direction :output
                   :if-exists :append
                   :if-does-not-exist :create)
    (format sink " <LI><A href=\"~A\">~A</A>~%"
            (netlib::html-escape-string (url:unparse-url (r2::document-location (zframe-document *top*))))
            (netlib::html-escape-string (r2::document-title (zframe-document *top*))))))

(defun cmd/magic ()
  (setq bb
    (make-simple-menu-button *links-pane* "bar"  #'nop :sensitive :off))
  (setf (clue:contact-sensitive *links-button*) :on))


;;;;

(defvar *bg-process*)

(defun bg-on ()
  (setf (clue:contact-sensitive *stop-button*) :on)
  (setf (clue:contact-sensitive *back-button*) :off)
  (setf (clue:contact-sensitive *forw-button*) :off)
  (mapc #'clue:display (list *back-button* *forw-button* *stop-button*)))

(defun bg-off ()
  (setf (clue:contact-sensitive *back-button*) :on)
  (setf (clue:contact-sensitive *forw-button*) :on)
  (setf (clue:contact-sensitive *stop-button*) :off)
  (mapc #'clue:display (list *back-button* *forw-button* *stop-button*))
  )


(defun run-bg-process (name fun)
  (cond (*bg-process*
         (xlib:bell *dpy*))
        (t
         (setf *bg-process*
           (mp/process-run-function name
                                    (lambda ()
                                      (unwind-protect
                                          (funcall fun)
                                        (setf *bg-process* nil)
                                        (gui-post nil 'bg-off)))))
         (bg-on)) ))

(defun find-zview-if (predicate root)
  "Map over all zviews and return the one, which satisfies 'predicate'."
  (cond ((funcall predicate root)
         root)
        ((zframeset-p root)
         (some (compose (curry #'find-zview-if predicate)
                        #'zframeset-child-content)
               (zframeset-children root)))))

(defun goto-url (url target)
  target                                ;XXX - for now we ignore that
  (cond (*bg-process*
         (xlib:bell *dpy*))
        (t
         (let ((req
                (cond ((request-p url) url)
                      ((url:url-p url)
                       (make-request :url url :method :get))
                      ((stringp url)
                       (make-request :url (url:parse-url url) :method :get))
                      (t
                       (error "~S: What to do with ~S?" 'goto-url url)))))
           ;;
           ;; Look out for intra-document links. That is look into
           ;; the frame tree, if there is already a document visited,
           ;; whose source request differs only by the URL's anchor
           ;; component.
           ;;
           (let ((target-zv
                  (find-zview-if (lambda (zv)
                                   (and (zframe-p zv)           ;we cannot go to frame sets.
                                                                ; an warning message would be 
                                                                ; appropriate.
                                        (eql (request-method (zview-request zv))
                                             (request-method req))
                                        (every (lambda (s)
                                                 (equal (funcall s (request-url (zview-request zv)))
                                                        (funcall s (request-url req))))
                                               `(,#'url:url-protocol
                                                 ,#'url:url-host
                                                 ,#'url:url-path
                                                 ,#'url:url-query
                                                 ,#'url:url-parameters))))
                                 *top*)))
             (cond (target-zv
                    ;; This is an intra-document link
                    (goto-anchor target-zv (url:url-anchor (request-url req))) )
                   (t
                    ;; Vanilla link
                    (setf *bg-process*
                      (mp/process-run-function
                       ;; oops -- hier haben wir noch kein document ...?!
                       (format nil "~S" url)
                       (lambda ()
                         (unwind-protect
                             (progn
                               (inc-worker)
                               (format T "~&;; Starting to render ~S on ~S." url target)
                               (gui-post nil 'visit-zview (zview-from-request req) target))
                           (format T "~&;; Finished ~S." url)
                           (dec-worker)
                           (setf *bg-process* nil)
                           (gui-post nil 'bg-off) ) )))
                    (bg-on))))))))

(defun goto-anchor (zview anchor)
  "Try to arrange for visiting anchor 'anchor' in the (already fetched) zframe 'zview'."
  (multiple-value-bind (x y)
      (find-anchor-position (zframe-document zview) anchor)
    (cond (x
           (frame-warp-to (ghd-contact (zview-gui-hook zview)) x y))
          (t
           (warn "Anchor ~S cannot be found." anchor)))))

; (defun find-anchor-position (document anchor)
;   "-> x y or NIL
;    Given a display-list, try to find the position of an anchor."
;   (setf anchor (string-rod anchor))
;   ;; We first seek out for the appropriate parse tree node, defining
;   ;; this anchor.
;   (let ((anchor-pt
;          (block nil
;            (sgml:map-pt (lambda (node)
;                           (let ((name (r2::pt-attr* node :name nil)))
;                             (when (and name (equalp anchor name))
;                               (return node)))
;                           (let ((name (r2::pt-attr* node :id nil)))
;                             (when (and name (equalp anchor name))
;                               (return node))))
;                         (r2:document-pt document))
;            nil)))
;     ;;
;     (cond ((not anchor-pt)
;            (warn "Anchor ~S is not defined at all." (rod-string anchor))
;            nil)
;           (t
;            ;; Traverse the display list and seek out for a box
;            ;; representing 'anchor-pt'. A box representing one of its
;            ;; ancestors is also considered a match to also find an
;            ;; approximation for hidden or removed parse tree nodes. 
;            ;; (As this might happen due to e.g. "display:none"). In
;            ;; any case the nearer we are topologically to 'anchor-pt',
;            ;; the better the match.
;            ;;
;            ;; Q: Would it be better to define distance by a
;            ;; "depth-first travesal index"?
;            ;;
;            (let ((best-dist nil)        ;best distance so far
;                  (best-box  nil))       ;best box found so far
;              (labels ((walk (box)
;                         ;; visits a box
;                         (cond ((r2::abox-p box)
;                                (do ((dist 0 (+ dist 1)) ;distance from 'anchor-pt'
;                                     (pt anchor-pt (sgml:pt-parent pt)))
;                                    ((null pt))
;                                  (when (eq (r2::abox-pt box) pt)
;                                    (when (or (null best-dist) (< dist best-dist))
;                                      (setf best-dist dist
;                                            best-box  box))))
;                                ;; Recurse into children
;                                (mapc #'walk (r2::abox-contents box)) )
;                               (t
;                                nil))))
;                ;; traverse
;                (mapc #'walk (gui:display-list-items (r2:document-display-list document)))
;                (cond ((null best-dist)
;                       (warn "Anchor ~S not found, though a matching node exists."
;                             (rod-string anchor))
;                       nil)
;                      (t
;                       (when (> best-dist 0)
;                         (warn "Anchor ~S found at distance ~D."
;                               (rod-string anchor) best-dist))
;                       (values (r2::abox-bx0 best-box)
;                               (r2::abox-by0 best-box)) ))))))))


;;; Second versin of FIND-ANCHOR-POSITION:
 
(defun find-anchor-position (document anchor)
  "-> x y or NIL
   Given a display-list, try to find the position of an anchor."
  (setf anchor (string-rod anchor))
  (let ((anchor-pt)
        (index-table (make-hash-table :test #'eq)))     ;mapping from nodes to their index
    ;; Count all nodes during a depth-first traversal and remember the
    ;; index of the node, which defines the anchor.
    (let ((j 0))
      (sgml:map-pt (lambda (node)
                     (incf j)
                     (setf (gethash node index-table) j)
                     (let ((name (r2::pt-attr* node :name nil)))
                       (when (and name (equalp anchor name))
                         (setf anchor-pt j)))
                     (let ((name (r2::pt-attr* node :id nil)))
                       (when (and name (equalp anchor name))
                         (setf anchor-pt j))))
                   (r2:document-pt document)))
    ;;
    (cond ((not anchor-pt)
           (warn "Anchor ~S is not defined at all." (rod-string anchor))
           nil)
          (t
           (let ((best-dist nil)        ;best distance so far
                 (best-box  nil))       ;best box found so far
             ;; Traverse the display list and seek out for a box
             ;; representing 'anchor-pt'. There might be no such box,
             ;; since not parse trees must actually have a rendered
             ;; representation. To overcome this, we only seek for a
             ;; good match. We find the node, which is topologically
             ;; closest to 'anchor-pt'.
             (labels ((walk (box)
                        ;; visits a box
                        (cond ((r2::abox-p box)
                               (let ((index (gethash (r2::abox-pt box) index-table)))
                                 (when index
                                   (let ((delta (abs (- anchor-pt index))))
                                     (when (or (null best-dist) (< delta best-dist))
                                       (setf best-dist delta
                                             best-box  box)))))
                               ;; Recurse into children
                               (mapc #'walk (r2::abox-contents box)) )
                              (t
                               nil))))
               ;; traverse
               (mapc #'walk (gui:display-list-items (r2:document-display-list document)))
               ;;
               (cond ((null best-dist)
                      (warn "Anchor ~S not found, though a matching node exists."
                            (rod-string anchor))
                      nil)
                     (t
                      (when (> best-dist 0)
                        (warn "Anchor ~S found at distance ~D."
                              (rod-string anchor) best-dist))
                      (values (r2::abox-bx0 best-box)
                              (r2::abox-by0 best-box)) ))))))))

(defun cmd/stop ()
  (when *bg-process*
    (mp/process-kill *bg-process*)))


(defun cmd/back ()
  (history-back)
  (run-bg-process "BACK" (lambda () (goto-zview (hent-zview *history*)))))

(defun cmd/forward ()
  (history-forw)
  (run-bg-process "FORWARD" (lambda () (goto-zview (hent-zview *history*)))))


;;;
;;; very first attempt at an file name completer
;;;

(defun filename-complete (fn)
  (mapcar (lambda (x)
            (if (netlib::is-directory-p x)
                (netlib::mungle-pathname-to-directory x)
              x))
          (ignore-errors (directory (concatenate 'string fn "*")))))
;;;;

#||
(defun invoke-with-profiling (fun)
  (monitor:monitor-all :r2)
  (monitor:monitor-all :css)
  (monitor:reset-all-monitoring)
  (multiple-value-prog1
      (funcall fun)
    (monitor:report-monitoring)
    (monitor:unmonitor *monitored-functions*) ))

||#

(defun open-document-4 (request)
  (check-type request request)
  (cond ((not (null (request-cache-file-head request)))
         (values (cl-byte-stream->gstream
                  (open (request-cache-file-data request)
                        :direction :input
                        :element-type '(unsigned-byte 8)))
                 (with-open-file (input 
                                  (request-cache-file-head request)
                                  :direction :input)
                   (with-standard-io-syntax
                     (read input)))))
        (t
         (let* ((fn-head (glisp:find-temporary-file))
                (fn-data (glisp:find-temporary-file)))
              (setf (request-cache-file-head request) fn-head)
              (setf (request-cache-file-data request) fn-data)
           (ecase (request-method request)
             ((:GET)
              ;; ZZZ we will probably only what to update the cahce-file-name 
              ;; field, if the transfer was complete.
              (multiple-value-bind (io header) (netlib::open-document-2 (request-url request))
                (with-open-file (sink fn-head 
                                 :direction :output
                                 :if-exists :overwrite)
                  (with-standard-io-syntax 
                    (print header sink)))
                (let ((dumpee (cl-byte-stream->gstream 
                               (open fn-data 
                                     :direction :output
                                     :if-exists :overwrite
                                     :element-type '(unsigned-byte 8)))))
                  (let ((ntot (maybe-parse-integer (netlib::get-header-field header :content-length))))
                    (setf ntot (or ntot 100000)) ;xxx
                    (let ((res (make-instance 'pb-stream
                                 :dumpee dumpee
                                 :ntotal ntot
                                 :nread 0
                                 :proxee io)))
                      (pb-stream-update res)
                      (values res header))))))
             ((:POST)
              ;; ZZZ cut + paste alert
              ;; ZZZ we will probably only what to update the cahce-file-name 
              ;; field, if the transfer was complete.
              (unless (string-equal (url:url-protocol (request-url request)) "http")
                (error "I could only POST via HTTP: ~S" (request-url request)))
              (multiple-value-bind (io header)
                  (netlib::http-open-document (request-url request) 
                                              :method :post
                                              :post-header (request-post-header request)
                                              :post-data   (request-post-data request) )
                (with-open-file (sink fn-head :direction :output :if-exists :overwrite)
                  (with-standard-io-syntax 
                    (print header sink)))
                (let ((dumpee (cl-byte-stream->gstream 
                               (open fn-data
                                     :if-exists :overwrite
                                     :direction :output
                                     :element-type '(unsigned-byte 8)))))
                  (let ((ntot (maybe-parse-integer (netlib::get-header-field header :content-length))))
                    (setf ntot (or ntot 100000)) ;xxx
                    (let ((res (make-instance 'pb-stream
                                 :dumpee dumpee
                                 :ntotal ntot
                                 :nread 0
                                 :proxee io)))
                      (pb-stream-update res)
                      (values res header)))))) )))))
             
     
(defun cmd/textarea-hack ()
  ;; enlarges every TEXTAREA to 80x30 !!
  ;; (Nukes the content, so be careful)!
  (when nil ;; disabled
    (labels ((walk (zv)
               (cond ((zframeset-p zv)
                      (mapc #'walk (mapcar #'zframeset-child-content (zframeset-children zv))))
                     ((zframe-p zv)
                      (hack zv))))
             (hack (zv)
               (sgml:map-pt (lambda (pt)
                              (cond ((eq (sgml:gi pt) :textarea)
                                     (setf (sgml:pt-attrs pt)
                                       (list* :rows (rod "30") :cols (rod "80")
                                              (sgml:pt-attrs pt) ))
                                     (clue:destroy (slot-value (getf (sgml:pt-attrs pt) :%replacement) 
                                                               'contact))
                                     (remf (sgml:pt-attrs pt) :%replacement)
                                     )))
                            (r2::document-pt (zframe-document zv))
                            )))
      (walk *top*))))

;;;; 

(defun nimm-den-besen ()
  (setq * nil)
  (setq ** nil)
  (setq *** nil)
  (setq / nil)
  (setq // nil)
  (setq /// nil)
  (setf *top* nil)
  (setf *history* nil)
  (setf *application-shell* nil)
  (setf *wholine-label* nil)
  (setf *url-label* nil)
  (setf *pb* nil)
  (setf *blinker* nil)
  (setf *links-button* nil)
  (setf *links-pane* nil)
  (setf *download-dialog* nil)
  (setf *stop-button* nil)
  (setf *back-button* nil)
  (setf *forw-button* nil)
  (setf *history* nil)
  (setf *download-dialog* nil)
  (setf *download/vbox* nil)
  (setf *bg-process* nil)
  (setf *dpy* nil)
  (setf css::*device* nil)
  (setf XLIB::*TEMP-GCONTEXT-CACHE* nil) ;was auch immer das is.
  #+ALLEGRO
  (progn
    (setf DEBUGGER::*CUR-FD* nil)
    (setf DEBUGGER::*CUR-INTERPRETED-ENVIRONMENT* nil))
  ;; in den processes bleibt manchmal noch
  ;; in (slot-value p 'MULTIPROCESSING::INITIAL-FORM)  was hängen.
  ;; cluei::default-gcontext hält über closure noch display
  (cluei::reinit-default-gcontext)
  (reset-caches)
  )

(defun cmd/scroll-down (self)
  (my-ignore-errors
   (with-slots (frame) self
     (with-slots (sw) frame
       (let ((sb (scrolled-window-vertical-scrollbar sw)))
         (incf (glue:thumb-pos sb)
               (* .8 (glue:thumb-size sb))))))))

;;;;;

(defclass ro/drawn-button (gui::ro/input)
  ((width :initarg :width)
   (height :initarg :height)
   (bbox :initarg :bbox)
   (widget :initform nil)
   
   (gui::disabled-p :initarg :disabled-p)
   (gui::name :initarg :name)
   (gui::initial-value :initarg :initial-value)
   (gui::device :initarg :device)
   
   (type :initarg :type)
   (gcontext :initform nil)
   (pt :initarg :pt) ))

(defmethod r2::ro/size ((self ro/drawn-button))
  (with-slots (width height) self
    (values width height 0)))

(defmethod r2::ro/intrinsic-size ((self ro/drawn-button))
  (with-slots (width height) self
    (values width height 0)))

(defmethod r2::ro/resize ((self ro/drawn-button) w h)
  w h
  )

(defmethod gui::ro/input-reset ((self ro/drawn-button)) )
(defmethod gui::ro/input-contribution ((self ro/drawn-button)))

(defmethod r2::x11-draw-robj (drawable gcontext (self ro/drawn-button) box x y)
  drawable gcontext box 
  (with-slots (widget width height gui::device bbox gui::disabled-p type) self
    (setf x (floor x) y (floor y))      ;DEVRND
    (unless widget
      (setf widget
        (clue:make-contact 'glue::drawn-button
                           :background *default-background*
                           :parent (slot-value gui::device 'htview)
                           :x (1- x) :y (- y height +1)
                           :width width :height height
                           :sensitive (if (not gui::disabled-p) :on :off) ))
      (r2::calc-bounding-box bbox)
      (clue:add-callback widget :expose
                         (curry 'drawn-button-expose self widget))
      (ecase type
        (:submit (clue:add-callback widget :activate (curry #'submit-button-callback self)))
        (:reset  (clue:add-callback widget :activate (curry #'reset-button-callback self)))
        (:button nil)) )
    (clue:move widget (1- x) (- y height +1)) ))

(defmacro with-gcontext-stippled ((drawable gcontext) &body body)
  `(with-gcontext-stippled-fn ,drawable ,gcontext #'(lambda () .,body)))

(defun drawn-button-expose (self widget)
  (with-slots (gcontext bbox) self
    (multiple-value-bind (x1 y1 x2 y2) (glue:interior-rectangle* widget)
      (unless gcontext
        (setf gcontext (xlib:create-gcontext :drawable widget)))
      (cond ((eq :on (clue:contact-sensitive widget))
             (setf (xlib:gcontext-clip-mask gcontext)
               (list x1 y1 (- x2 x1) (- y2 y1)))
             (r2::draw-bbox '--doc-- 
                            widget gcontext bbox +everywhere+))
            (t
             (with-gcontext-stippled (widget gcontext)
               (r2::draw-bbox '--doc-- 
                              widget gcontext bbox +everywhere+)))))))

(defun with-gcontext-stippled-fn (drawable gcontext continuation)
  (let* (stipple-bitmap gc2)
    (unwind-protect
        (progn
          (setf stipple-bitmap (xlib:create-pixmap :drawable drawable :width 2 :height 2 :depth 1))
          (setf gc2 (xlib:create-gcontext :drawable stipple-bitmap))
          ;;
          (setf (xlib:gcontext-foreground gc2) 1)
          (xlib:draw-points stipple-bitmap gc2 '(0 0 1 1))
          (setf (xlib:gcontext-foreground gc2) 0)
          (xlib:draw-points stipple-bitmap gc2 '(0 1 1 0))
          (xlib:with-gcontext (gcontext :stipple stipple-bitmap
                                        :fill-style :stippled)
            (funcall continuation)))
      (progn
        (and gc2 (xlib:free-gcontext gc2))
        (and stipple-bitmap (xlib:free-pixmap stipple-bitmap) )))))
;;;

(defun make-download-dialog (nam stop-fn)
  (let ((*pb)
        (shell
         (clue:make-contact 'top-level-shell
                            :parent (the-display)
                            :background *default-background*)))
    (setf (wm-title shell) "Download")
    (setf (wm-icon-title shell) "Download")
    (let ((vb (clue:make-contact 'glue:vbox :parent shell
                                 :margin-width 2
                                 :margin-height 2)))
      (clue:make-contact 'glue:label :height 1 :parent vb :label-string "" :expand-p t)
      (let ((hb (clue:make-contact 'glue:hbox :parent vb
                                   :padding-width 2 
                                   :padding-height 2
                                   :shadow-style :groove
                                   :shadow-width 2)))
        (setq *pb (clue:make-contact 'glue:progress-bar :parent hb
                                     :shadow-width 2
                                     :shadow-style :inset
                                     :background *default-dark-background*))
        (clue:make-contact 'glue:label :parent hb
                           :margin-width 5
                           :expand-p t
                           :label-string nam))
      (let ((hb (clue:make-contact 'glue:hbox :parent vb)))
        (clue:make-contact 'glue:label :parent hb :label-string "" :expand-p t)
        (make-simple-button hb "Stop" stop-fn :margin-height 2)
        (clue:make-contact 'glue:label :parent hb :label-string "" :expand-p t))
      (clue:make-contact 'glue:label :parent vb :height 1 :label-string "" :expand-p t)
      )
    (values shell *pb)))


(defun download (input url type subtype)
  (when (and (typep input 'pb-stream)
             (typep (slot-value input 'proxee) 'netlib::http-stream))
    (setf (slot-value (slot-value input 'proxee) 'netlib::cache-p) nil)) ;zzz
  (let ((process nil))
    (multiple-value-bind (shell pb) 
        (make-download-dialog (url:unparse-url url)
                              #'(lambda ()
                                  (mp/process-kill process)))
      (and (typep input 'pb-stream)
           (setf (pb-stream-progress-bar input) pb))
      (mp/process-run-function
       "download"
       (lambda ()
         (let ((start (get-universal-time))
               (end nil))
           (unwind-protect
               (progn
                 (with-open-file (sink 
                                  (merge-pathnames 
                                   (make-pathname :name ".manifest")
                                   *download-directory*)
                                  :direction :output
                                  :if-does-not-exist :create
                                  :if-exists :append)
                   (format sink "~A ~D ~A/~A~%" 
                           (url:unparse-url url)
                           (get-universal-time)
                           type
                           subtype))
                 (with-open-file (sink
                                  (merge-pathnames
                                   (make-pathname
                                    :name (car (last (url:url-path url)))) ;xxx
                                   *download-directory*)
                                  :if-exists :new-version
                                  :direction :output
                                  :element-type '(unsigned-byte 8))
                   (netlib::copy-gstream input (cl-byte-stream->gstream sink)
                                         ))
                 (setf end (get-universal-time))
                 (when (> (- end start) 0)
                   (print (* 1.0 (/ (slot-value input 'nread) (- end start))))))
             (progn
               (g/close input)
               (gui-post nil 'clue:destroy shell)))) )))))

;; --------------------------------------------------------------------------------

(defun balloon (on-behalf string &optional (mx 10) (my 10))
  (let () ;; ((mx 10) (my 10))
    (multiple-value-bind (x y) 
        (clue:contact-translate on-behalf mx my (clue:contact-root on-behalf))
      (let ((balloon (or (getf (xlib:window-plist on-behalf) 'balloon)
                         (setf (getf (xlib:window-plist on-behalf) 'balloon)
                           (let* ((shell 
                                   (make-contact 'clue:override-shell
                                                 :parent on-behalf ;; (clue:contact-root on-behalf)
                                                 :state :mapped
                                                 :name :balloon
                                                 :save-under :on))
                                  (box 
                                   (make-contact 'glue:vbox :parent shell
                                                 :background :lemonchiffon2))
                                  (label
                                   (make-contact 'glue:label
                                                 :label-string string
                                                 :parent box
                                                 ;;:background :white
                                                 :margin-width 5
                                                 :margin-height 5)))
                             (list shell box label))))))
        (cond ((= 0 (length string))
               (setf (clue:contact-state (first balloon))
                 :withdrawn)
               )
              (t
               (setf (clue:contact-state (first balloon))
                 :withdrawn)
               (clue:move (first balloon)
                          (+ x 10) (+ y 10))
               (setf (glue:label-string (third balloon))
                 string)
               (multiple-value-bind (w h) (clue:preferred-size (third balloon))
                 (clue:resize (third balloon) w h 0))
               (clue:change-layout (second balloon))
               (setf (clue:contact-state (first balloon))
                 :mapped)

               (setf (xlib:window-priority (first balloon)) :above)
               ))
        ))
    (clue:update-state (the-display))))

;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: GUI; -*-
;;; --------------------------------------------------------------------------
;;;     Title: Toolkit independent GUI stuff (albeit X specific)
;;;   Created: Sun Jan 17 06:45:18 1999
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: GPL (See file COPYING for details).
;;; --------------------------------------------------------------------------
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
;;  1999-08-21  GB      - PAINT-DISPLAY-LIST: use R2::DRAW-BBOX-BACKGROUND.
;;                      - DISPLAY-LIST: new struct
;;                      - ACTIVE-EXTENTS, FIND-ACTIVE-LINK, PAINT-DISPLAY-LIST,
;;                        MAP-BOXEN-FOR-PT: changed accordingly
;;
;;  1999-08-19  GB      - PT-REPLACEMENT: Changed due to :%REPLACEMENT 
;;                        attribute change; still returns the object only.
;;                      - FIND-ACTIVE-IN-ABOX* now also returns coordinates
;;                      - ACTIVE-LINK-MOUSE-DOCUMENTATION uses r2::command protocol
;;

(in-package :GUI)

(defstruct display-list
  document
  items)

;;;
;;;  Events
;;;

(defclass event () ())

(defclass input-event (event) 
  ((x      :initarg :x      :reader event-x)
   (y      :initarg :y      :reader event-y)
   (x-root :initarg :x-root :reader event-x-root)
   (y-root :initarg :y-root :reader event-y-root)
   (state  :initarg :state  :reader event-state)))

(defclass mouse-event (input-event)
  ())

(defclass motion-event (mouse-event) ())

(defclass pointer-motion-event (motion-event) ())
(defclass enter-event (motion-event) ())
(defclass leave-event (motion-event) ())

(defclass button-event (mouse-event)
  ((button :initarg :button :reader event-button)))

(defclass button-press-event (button-event) ())
(defclass button-release-event (button-event) ())

(defclass exposure-event (event)
  ((region :initarg :region :reader event-region)))

(defclass configure-event (event)
  ((x      :initarg :x      :reader event-x)
   (y      :initarg :y      :reader event-y)
   (width  :initarg :width  :reader event-width)
   (height :initarg :height :reader event-height)))

(defclass map-notify-event (event) ())

(defclass key-event (input-event)
  ((key-code :initarg :key-code  :reader event-key-code)
   (key-name :initarg :key-name :reader event-key-name)))

(defclass key-press-event (key-event) ())
(defclass key-release-event (key-event) ())

;;

(defmethod translate-event ((event input-event) dx dy)
  (make-instance (type-of event)
    :x (+ (event-x event) dx)
    :y (+ (event-y event) dy)
    :x-root (event-x-root event)
    :y-root (event-y-root event)
    :state (event-state event)))

(defmethod translate-event ((event button-event) dx dy)
  (make-instance (type-of event)
    :x (+ (event-x event) dx)
    :y (+ (event-y event) dy)
    :x-root (event-x-root event)
    :y-root (event-y-root event)
    :state (event-state event)
    :button (event-button event)))

(defmethod translate-event ((event exposure-event) dx dy)
  (make-instance (type-of event)
    :region (transform-region (make-translation-transformation dx dy)
                              (event-region event))))

(defmethod translate-event ((event configure-event) dx dy)
  (make-instance (type-of event)
    :x (+ (event-x event) dx)
    :y (+ (event-y event) dy)
    :width (event-width event)
    :height (event-height event)))
  
(defmethod translate-event ((event map-notify-event) dx dy)
  (declare (ignore dx dy))
  (make-instance (type-of event)))

(defmethod translate-event ((event key-event) dx dy)
  (make-instance (type-of event)
    :x (+ (event-x event) dx)
    :y (+ (event-y event) dy)
    :x-root (event-x-root event)
    :y-root (event-y-root event)
    :state (event-state event)
    :key-code (event-key-code event)
    :key-name (event-key-name event)))

;;

(defmethod print-object ((self mouse-event) sink)
  (format sink "#<~S ~S ~S ~S ~S ~S ~S>" (type-of self)
          :x (slot-value self 'x)
          :y (slot-value self 'y)
          :state (slot-value self 'state)))

(defmethod print-object ((self button-event) sink)
  (format sink "#<~S ~S ~S ~S ~S ~S ~S ~S ~S>" (type-of self)
          :x (slot-value self 'x)
          :y (slot-value self 'y)
          :state (slot-value self 'state)
          :button (slot-value self 'button)))

(defmethod print-object ((self exposure-event) sink)
  (format sink "#<~S ~S ~S>" (type-of self)
          :region (slot-value self 'region)))

(defmethod print-object ((self configure-event) sink)
  (format sink "#<~S ~S ~S ~S ~S ~S ~S ~S ~S>" (type-of self)
          :x (slot-value self 'x)
          :y (slot-value self 'y)
          :width (slot-value self 'width)
          :height (slot-value self 'height)))


;;;
;;;  Proxy Device
;;;

;; a common vehicle to define GUI specific devices

(defclass proxy-device ()
  ((clonee :initarg :clonee) ))

(defmethod renderer:device-font-ascent ((self proxy-device) font)
  (with-slots (clonee) self
    (renderer:device-font-ascent clonee font)))

(defmethod renderer:device-dpi ((self proxy-device))
  (with-slots (clonee) self
    (renderer:device-dpi clonee)))

(defmethod renderer:device-font-descent ((self proxy-device) font)
  (with-slots (clonee) self
    (renderer:device-font-descent clonee font)))

(defmethod renderer:device-font-underline-position ((self proxy-device) font)
  (with-slots (clonee) self
    (renderer:device-font-underline-position clonee font)))

(defmethod renderer:device-font-underline-thickness ((self proxy-device) font)
  (with-slots (clonee) self
    (renderer:device-font-underline-thickness clonee font)))

(defmethod renderer:device-font-has-glyph-p ((self proxy-device) font index)
  (with-slots (clonee) self
    (renderer:device-font-has-glyph-p clonee font index)))

(defmethod renderer:device-font-glyph-width ((self proxy-device) font index)
  (with-slots (clonee) self
    (renderer:device-font-glyph-width clonee font index)))

(defmethod renderer:device-realize-font-desc ((self proxy-device) font-desc)
  (with-slots (clonee) self
    (renderer:device-realize-font-desc clonee font-desc)))

(defmethod renderer:device-font-database ((self proxy-device))
  (with-slots (clonee) self
    (renderer:device-font-database clonee )))

(defmethod renderer:scale-font-desc ((self proxy-device) fd size)
  (with-slots (clonee) self
    (renderer:scale-font-desc clonee fd size)))

(defmethod gui::make-image-replacement ((self proxy-device) doc &rest args &key url width height)
  (with-slots (clonee) self
    (apply #'gui::make-image-replacement clonee doc args)))

;;; ---- Some CLIM fake ---------------------------------------------------------------------------------

(defconstant +black+ (xlib:make-color :red 0 :green 0 :blue 0))
(defconstant +white+ (xlib:make-color :red 1 :green 1 :blue 1))

;;; ====================================================================================================

;; gui:make-image-replacement device &key url width height -> replacement-object
;; r2:x11-draw-robj drawable gcontext replacement-object box x y -> ;
;; gui:ro/make-submit-button device &key pt label name size disabled-p read-only-p
;;   -> replacement-object (must obey to the ro/input protocol also)
;; gui:ro/make-text device &key pt name initial-value width max-length disabled-p read-only-p)


;; RO/INPUT protocol:
;;    slots: initial-value name disabled-p read-only-p pt
;;    RO/INPUT-DESTRUCT self -> ;
;;    RO/INPUT-CONTRIBUTION self -> alist ;
;;    RO/INPUT-RESET self -> ;
;;

;;; ====================================================================================================

(defun parse-html-frameset-length (string &aux n)
  (cond ((and (>= (length string) 2)
              (char= (char string (1- (length string))) #\%)
              (every #'digit-char-p (subseq string 0 (1- (length string)))))
         (cons :% (parse-integer (subseq string 0 (1- (length string))))))
        ((and (>= (length string) 2)
              (char= (char string (1- (length string))) #\*)
              (every #'digit-char-p (subseq string 0 (1- (length string))))
              (>= (setq n (parse-integer (subseq string 0 (1- (length string))))) 1))
         (cons '* n))
        ((and (>= (length string) 2)
              (char= (char string 0) #\*)
              (every #'digit-char-p (subseq string 1))
              (>= (setq n (parse-integer (subseq string 1))) 1))
         (cons '* n))
        ((string= string "*")
         (cons '* 1))
        ((and (>= (length string) 1)
              (every #'digit-char-p string))
         (cons '1 (parse-integer string))) 
        (t
         (warn "HTML frameset length value `~A' does not parse; using `1*' instead." string)
         (cons '* 1))))

(defun parse-html-frameset-length-list (string)
  (mapcar #'parse-html-frameset-length
          (mapcar (curry #'string-trim '(#\space #\tab #\newline #\return))
                  (split-by #\, string))))

(defun allot-frameset-lengthen (lengthen total)
  (let* ((n (length lengthen))
         (res (make-list n :initial-element 0)))
    ;; First allot the fixed and percentage values
    (dotimes (i n)
      (case (car (elt lengthen i))
        (:% (setf (elt res i) (* 1/100 (cdr (elt lengthen i)) total)))
        (1  (setf (elt res i) (cdr (elt lengthen i))))))
    ;; Now the proportional ones
    (let ((lack (max 0 (- total (reduce #'+ res))))
          (m (count '* lengthen :key #'car)))
      (dotimes (i n)
        (when (eql (car (elt lengthen i)) '*)
          (setf (elt res i) (* (/ (cdr (elt lengthen i)) m) lack))))
      ;; Adjust to given total value
      ;; Das muss noch anders werden:
      ;; Wenn wir genuegt werte haben, die nicht fest sind, sollten
      ;; wir diese nehmen. Das resultat sieht dann besser aus.
      (let ((rtotal (reduce #'+ res)))
        (if (zerop rtotal)
            (dotimes (i n)
              (setf (elt res i) (/ total n)))
          (dotimes (i n)
            (incf (elt res i) (* (- total rtotal) (/ (elt res i) rtotal)))))))
    ;; Finally round all values and adjust the first one according to
    ;; the rounding error.
    (dotimes (i n)
      (setf (elt res i) (round (elt res i))))
    (incf (elt res 0) (- total (reduce #'+ res)))
    ;; all done
    res))

;;;; ----------------------------------------------------------------------------------------------------


(defun pretty-color (x)
  (or (car (rassoc x css::*color-names* :test #'string-equal))
      x))

(defun make-device-for-display (dpy)
  (or (getf (xlib:display-plist dpy) 'device)
      (setf (getf (xlib:display-plist dpy) 'device)
        (make-instance 'ws/x11::x11-device :display dpy))))

(defun dump-source-of (url &optional (filename "/tmp/s"))
  (setq url (if (url:url-p url) url (url:parse-url url)))
  (with-open-file (sink filename 
                   :direction :output
                   :if-exists :new-version
                   :element-type '(unsigned-byte 8))
    (netlib:with-open-document ((input mime-type) url)
      (netlib::copy-gstream input (glisp:cl-byte-stream->gstream sink))))
  (format T "~&;; Source of ~A dumped to ~S." url filename))

;;;; ------------------------------------------------------------------------------------------
;;;;  FORMS
;;;;

(defun input-name-equal-p (name1 name2)
  (and name1 name2
       (string-equal name1 name2)))

(defun find-form-element (pt)
  "Finds the <FORM> element 'pt' belongs to. May return NIL, if no FORM is present."
  (cond ((null pt) nil)
        ((eq (sgml:gi pt) :FORM) pt)
        ((find-form-element (sgml:pt-parent pt)))))

(defun map-input-elements (fun form)
  "Call function `fun' on all input elements within the FORM element `form'."
  (assert (eq (sgml:gi form) :FORM))
  (sgml:map-pt (lambda (x)
                 (when (input-element-p x)
                   (funcall fun x)))
               form))

(defun input-element-p (elm)
  "Is the element `elm' an HTML input element?
   ISINDEX is *not* consided to be an input element."
  (member (sgml:gi elm) '(:input :button :select :textarea)))

(defun pt-replacement (pt)
  (car (r2::pt-%replacement pt nil)))

(defun collect-form-values (form-elm)
  (let ((res nil))
    (map-input-elements (lambda (x)
                          (let ((obj (pt-replacement x)))
                            (when (typep obj 'ro/input)
                              (setf res (append res (ro/input-contribution obj))))))
                        form-elm)
    res))

(defun reset-form (form-elm)
  (map-input-elements (lambda (x)
                        (let ((obj (pt-replacement x)))
                          (when (typep obj 'ro/input)
                            (ro/input-reset obj))))
                      form-elm))

(defun encode-values-via-application/x-www-form-urlencoded (values)
  (url::unparse-query values))

;;;; 

;;;; ------------------------------------------------------------------------------------------
;;;;  profiling
;;;;

(defparameter user::*profile-closure-p* nil)
(defparameter user::*closure-dpi* 88)           ;this doesn't belong here

#+ALLEGRO
(defun invoke-with-profiling (fun)
  (multiple-value-prog1
      (prof:with-profiling () (funcall fun))
    (prof:show-flat-profile)))

#+CMU
(defun invoke-with-profiling (fun)
  (profile:profile-all :package :r2 :callers-p t)
  (profile:profile-all :package :css :callers-p t)
  (eval `(profile:reset-time ,@profile:*timed-functions*))
  (multiple-value-prog1
      (funcall fun)
    (with-open-file (*trace-output* "profile.out" :direction :output :if-exists :new-version)
      (let ((*package* (find-package :renderer)))
        (eval `(profile:report-time ,@profile:*timed-functions*))))
    (format *trace-output* "~&;; profile data dumped to 'profile.out'.")
    (finish-output *trace-output*)
    (eval `(profile:unprofile ,@profile:*timed-functions*))))

#+CLISP
(defun invoke-with-profiling (fun)
    (funcall fun)
#||
  (monitor:monitor-all :r2)
  (monitor:monitor-all :css)
  (monitor:reset-all-monitoring)
  (multiple-value-prog1
      (funcall fun)
    (monitor:report-monitoring)
    (monitor:unmonitor *monitored-functions*) )
||#
)

#-(OR ALLEGRO CMU CLISP)
(defun invoke-with-profiling (fun)
  (warn "Define ~S for your Lisp implementation." 'invoke-with-profiling)
  (funcall fun))

(defun my-time-fn (fun)
  (if user::*profile-closure-p*
      (invoke-with-profiling fun)
    (time (funcall fun))))

(defmacro my-time (&body body)
  `(my-time-fn (lambda () ,@body)))

;;;; ----------------------------------------------------------------------------------------------------

;;;
;;;  Minimal Hypertext View
;;;

;; This is a bare bones implementation of an hyper text view. It
;; displays the renderer's output and reacts to exposure and input
;; events. Any implementation of a toolkit specific hypertext view
;; should mixin this class.

;; Methods which are still to be defined for a PRIM-HT-VIEW:

;;   GET-DRAWABLE+GCONTEXT self -> drawable ; gcontext
;;     Obtain a drawable and a graphics context for painting the
;;     document
  
;;   SET-MOUSE-DOCUMENTATION self string ->
;;     This is called, when ever the mouse hovers over some hyperlink
;;     and should update some wholine or pop-up some balloon help.
  
;;   HANDLE-ACTIVATED-LINK self pt 
;;     When ever the user actually clicks on an hyper link this method
;;     is invoked.

;; A prim-ht-view instance unterstands the following methods:

;;   HANDLE-EVENT self <exposure-event>
;;   HANDLE-EVENT self <enter-event>
;;   HANDLE-EVENT self <leave-event>
;;   HANDLE-EVENT self <pointer-motion-event>
;;   HANDLE-EVENT self <button-press-event>
;;   HANDLE-EVENT self <button-release-event>
;;     This method is to be invoked when ever some event comes in from
;;     the window system.
  
;;   SET-DISPLAY-LIST self new-display-list
;;     Change the display list maintained by the hyper text view.

;; Things, which still need to be implemented:
;;
;;  - Marking some section of the hypertext for yanking from some
;;    other app.
;;  - It may be useful to be able to use the <TAB> key to cycle
;;    thru' all the hyper links. (One would activate these by <RIGHT>,
;;    like within Lynx).
;;  - Jumping to anchors.

(defclass prim-ht-view ()
  ((display-list        :initform nil)
   (active-pt           :initform nil)
   (active-link         :initform nil) ))

(defmethod handle-event ((self prim-ht-view) (event exposure-event))
  (with-slots (display-list) self
    ;; wir sollten die region in dem event noch an unserem fenster clippen...
    (multiple-value-bind (drawable gcontext) (get-drawable+gcontext self)
      (setf (xlib:gcontext-clip-mask gcontext) (gu:region-to-x11-rectangle-list (event-region event))
            (xlib:gcontext-clip-x gcontext) 0
            (xlib:gcontext-clip-y gcontext) 0)
      (paint-display-list drawable gcontext 0 0 display-list (event-region event) nil))))

;;; Maintaining the active region

(defun r2::abox-bounding-region (box)
  (if (and (r2::abox-bx0 box) (r2::abox-by0 box) (r2::abox-bx1 box) (r2::abox-by1 box))
      (make-rectangle* (floor (r2::abox-bx0 box)) (floor (r2::abox-by0 box))
                       (ceiling (r2::abox-bx1 box)) (ceiling (r2::abox-by1 box)))
    +nowhere+))

(defun map-link-boxen (fun display-list link)
  (when link
    (map-display-list-boxen (lambda (box)
                              (when (and (r2::abox-p box) (r2::abox-map box))
                                (dolist (q (r2::abox-map box))
                                  (when (eq link (r2::imap-area-link q))
                                    (funcall fun box)))))
                            display-list)))

(defmethod active-extents ((self prim-ht-view) link)
  "Find the region the active pt spans."
  (let ((res +nowhere+))
    (with-slots (display-list) self
      (map-link-boxen (lambda (box)
                        (setf res (gu:region-union res (r2::abox-bounding-region box))))
                      display-list link))
    res))

(defmethod maintain-active ((self prim-ht-view) x y)
  "Called when we get knowledge of a new mouse position."
  ;; 'x' might as well be NIL. (leave event)
  ;; calls UPDATE-NEW-ACTIVE, if active changed.
  (with-slots (active-link) self
    (let ((a (if (null x) 
                 nil
               (find-active-link self x y))))
      ;;(when a (print (list (first a) (floor (second a)) (floor (third a)))))
      (setf a (first a))
      (unless (eq a active-link)
        (update-new-active self a)))))

(defmethod find-active-link ((self prim-ht-view) x y)
  (with-slots (display-list) self
    (when display-list
      (dolist (k (display-list-items display-list))
        (when (r2::bbox-p k)
          (let ((r (find-active-in-abox* k x y)))
            (when r
              (return r))))))))

(defun find-active-in-abox* (abox x y)
  (let (q)
    (when (point-in-abox-p abox x y)
      (cond ((setq q (some (rcurry #'find-active-in-abox* x y) 
                           (r2::abox-contents abox)))
             q)
            ((and (r2::ibox-p abox) (r2::abox-map abox))
             (dolist (area (r2::abox-map abox))
               (cond ((r2::area-contains-point-p
                       area 
                       (- x (r2::inner-left-edge abox))
                       (- y (r2::inner-top-edge abox))
                       (r2::inner-width abox)
                       (r2::inner-height abox))
                      (return 
                        (list (r2::imap-area-link area)
                              (- x (r2::inner-left-edge abox))
                              (- y (r2::inner-top-edge abox)) ))))))))))

(defun point-in-abox-p (abox x y)
  (cond ((and (r2::abox-p abox)
              (r2::abox-bx0 abox)
              (r2::abox-by0 abox)
              (r2::abox-bx1 abox)
              (r2::abox-by1 abox))
         (and (<= (r2::abox-bx0 abox) x (r2::abox-bx1 abox))
              (<= (r2::abox-by0 abox) y (r2::abox-by1 abox))))
        (t
         nil)))

(defun map-display-list-boxen (fun display-list)
  (when display-list
    (mapc (curry #'r2::map-boxen fun) (display-list-items display-list))))

(defun map-boxen-for-pt (fun display-list pt)
  (map-display-list-boxen (lambda (box)
                            (cond ((and (r2::abox-p box) (eq (r2::abox-pt box) pt))
                                   (funcall fun box))))
                          display-list))

(defmethod update-new-active ((self prim-ht-view) new-active)
  (let ((id (if (and new-active
                     #|(r2::hyper-link-url new-active)|# )
                58 68)))
    (let* ((fn (xlib:open-font (xlib:window-display (get-drawable+gcontext self))
                               "cursor"))
           (cr (xlib:create-glyph-cursor :source-font fn
                                         :source-char id
                                         :mask-font fn
                                         :mask-char (+ id 1)
                                         :foreground (xlib:make-color :red 0 :green 0 :blue 0)
                                         :background (xlib:make-color :red 1 :green 1 :blue 1)
                                         )))
      (setf (xlib:window-cursor (get-drawable+gcontext self)) cr)))
  (with-slots (active-link) self
    (let ((reg (gu:region-union (active-extents self active-link) (active-extents self new-active))))
      (and active-link (unactivate-link self active-link))
      (and new-active (activate-link self new-active))
      (setf active-link new-active)
      (multiple-value-bind (drawable gcontext) (get-drawable+gcontext self)
        (declare (ignore gcontext))
        (gu:map-region-rectangles (lambda (x1 y1 x2 y2)
                                    (xlib:clear-area drawable 
                                                     :x x1 :y y1 
                                                     :width (- x2 x1)
                                                     :height (- y2 y1)
                                                     :exposures-p nil) )
                                  reg)
        #-(AND)
        (cond (active-link
               (let ((mx #x7fff) (my 0))
                 (gu:map-region-rectangles (lambda (x1 y1 x2 y2)
                                             (setf mx (min x1 x2 mx)
                                                   my (max y1 y2 my)))
                                           (active-extents self active-link))
                 (let ((str (active-link-mouse-documentation self active-link)))
                   (unless (stringp str)
                     (setf str (map 'string (lambda(x)(or (code-char x) #\?)) str)))
                   (clue-gui2::balloon drawable str (+ mx 5) (+ my 2)))))
              (t
               (clue-gui2::balloon drawable ""))))
      (handle-event self (make-instance 'exposure-event :region reg))
      (set-mouse-documentation self (active-link-mouse-documentation self active-link)) )))

;; xxx
(defvar *old-background* nil)

(defmethod unactivate-link ((self prim-ht-view) link)
  (with-slots (display-list) self
    (map-link-boxen (lambda (box)
                      (setf (r2::abox-background box) *old-background*) )
                    display-list link)))

(defmethod activate-link ((self prim-ht-view) link)
  (with-slots (display-list) self
    (map-link-boxen (lambda (box)
                      (setf *old-background* (r2::abox-background box))
                      (setf (r2::abox-background box) 
                        (r2::make-background :color "#9cc")));9cc
                    display-list link)))

(defmethod active-link-mouse-documentation ((self prim-ht-view) (command null))
  "")

(defmethod active-link-mouse-documentation ((self prim-ht-view) (command t))
  (r2::command-documentation command))

;;;;

(defmethod set-display-list ((self prim-ht-view) new-display-list)
  (with-slots (display-list) self
    (maintain-active self nil nil)
    (setf display-list new-display-list)
    (multiple-value-bind (window) (get-drawable+gcontext self)
      (when window
        ;;(xlib:clear-area window :exposures-p t)
        '(handle-event self (make-instance 'exposure-event :region +everywhere+))
        (multiple-value-bind (x y same-screen-p) (xlib:query-pointer window)
          ;; handle positions outside of the window
          (maintain-active self (and same-screen-p x) y) )))))

(defun paint-display-list (drawable gcontext xo yo display-list region active-link)
  (declare (ignore xo yo active-link))
  #||
  (xlib:with-gcontext (gcontext :foreground 7)
    (xlib:draw-rectangle drawable gcontext 0 0 1000 1000 t))
  ||#
  (when (not (null display-list))
    (let ((items (display-list-items display-list))
          (doc   (display-list-document display-list)))
      (labels ((aux (box)
                 (multiple-value-bind (x1 y1 x2 y2) (gu:region-bounding-rectangle* region)
                   (r2::draw-bbox-background doc drawable gcontext box x1 y1 x2 y2))))
        (cond ((r2::abox-background (car items))
               (aux (car items)))
              ((and (r2::abox-contents (car items))
                    (r2::abox-background (car (r2::abox-contents (car items)))))
               (aux (car (r2::abox-contents (car items)))))))
      (when r2::*no-color-p*
        (setf (xlib:gcontext-foreground gcontext)
          (ws/x11::x11-find-color drawable "white"))
        (xlib:draw-rectangle drawable gcontext 0 0 #x7FFF #x7FFF t))
      (dolist (k items)
        (cond ((r2::bbox-p k)
               (r2::draw-abox doc drawable gcontext k region)) )))))

;;;; Glue it to the clm-window

(defmethod handle-event ((self prim-ht-view) (event enter-event))
  (maintain-active self (event-x event) (event-y event)))

(defmethod handle-event ((self prim-ht-view) (event leave-event))
  (maintain-active self nil nil))

(defmethod handle-event ((self prim-ht-view) (event pointer-motion-event))
  (maintain-active self (event-x event) (event-y event)))

(defmethod handle-event ((self prim-ht-view) (event button-press-event))
  (let ((q (find-active-link self (event-x event) (event-y event))))
    (when q
      (handle-activated-link self (first q) (second q) (third q)))))

(defmethod handle-event ((self prim-ht-view) (event button-release-event))
  )


;;; --------------------------------------------------------------------------------

(defvar user::*html-dtd* nil)

(defun aimage->pixmap+mask/raw (drawable aim)
  (let* ((width (r2::aimage-width aim))
         (height (r2::aimage-height aim))
         (depth (xlib:drawable-depth drawable))
         (im  (ws/x11::aimage->ximage drawable aim)))
    (setf width (max width 1))
    (setf height (max height 1))
    (values
     (let* ((pixmap (xlib:create-pixmap :drawable drawable
                                        :width width
                                        :height height
                                        :depth depth))
            (gc     (xlib:create-gcontext :drawable pixmap)))
       (xlib:put-image pixmap gc im 
                       :src-x 0 :src-y 0
                       :x 0 :y 0
                       :width width :height height)
       (xlib:free-gcontext gc)
       pixmap)
     (when (imagelib:aimage-alpha-p aim)
       (ws/x11::make-mask-from-aimage drawable aim)))))

(defun init-closure ()
  ;; Init general closure stuff
  #||
  (unless *ht* 
    (format T "~&;; Slurping hyphenation table ...")
    (setf *ht* (slurp-patterns "resources/patterns/english.ptn"))
    (princ " done.") )
  ||#
  (unless user::*html-dtd*
    (cond 
     ;; xxx hack
     ((probe-file (compile-file-pathname "html-dtd.lisp"))
      (format T "~&;; Loading DTD ")
      (setf user::*html-dtd* (sgml::undump-dtd "html-dtd"))
      (princ " done.")
      (finish-output))
     (t
      (format T "~&;; Parsing DTD ")
      (sgml:slurp-catalog (url:parse-url "file://closure/resources/dtd/catalog"))
      (setf user::*html-dtd* (sgml:parse-dtd '(:public "-//W3C//DTD HTML 4.0 Frameset//EN")))
      (princ " done.")
      (finish-output))))
  
  (format T "~&;; Parsing default style sheet ...")
  (setf r2::*default-style-sheet* 
    (css::parse-style-sheet-from-url (url:parse-url "file://closure/resources/css/default.css")
                                     :name "Closure Default Style Sheet"))
  (princ " done.")
  (finish-output)
  (values))  


(defclass ro/input ()
  ((initial-value :initarg :initial-value)
   (name          :initarg :name)
   (disabled-p    :initarg :disabled-p :initform nil)
   (read-only-p   :initarg :read-only-p :initform nil)
   (pt            :initarg :pt)
   (document      :initarg :document)))

;;;;

(defstruct option-menu-option
  label         ;the entries label as in DTD (a ROD)
  selected-p
  disabled-p    ;disabled? Note: DTD says it doesn't apply here?
  value         ;the value 
  content)      ;the entries content as in DTD (a ROD)

(defstruct option-menu-option-group
  disabled-p
  label
  children)



;;;;





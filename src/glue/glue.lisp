;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: GLUE; -*-
;;; --------------------------------------------------------------------------------------
;;;     Title: The Glue toolkit based on CLUE
;;;   Created: 1999-03-24 07:18
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
;; =======
;;
;;  When        Who     What
;; ----------------------------------------------------------------------------
;;  1999-09-21  GB      - added FIND-CONTACT-UNDER-POSITION now watches
;;                        CLUE:SENSITIVE-P.
;;  1999-09-12  GB      - twix from NEW-[HV]BOX to [HV]BOX
;;                      - old HBOX/VBOX implementation is gone.

(in-package :GLUE)

;;; Parameters

(defparameter *default-background*      "#c7c7c7")
(defparameter *default-dark-background* "#aaaaaa")

(defparameter *default-font* 
  ;; "variable"
  "-*-lucida-medium-r-*-*-*-120-*-*-*-*-iso8859-1")

(defparameter *default-italic-font* 
    "-*-lucida-medium-i-*-*-*-120-*-*-*-*-iso8859-1")

(defparameter *default-bold-font* 
    "-*-lucida-bold-r-*-*-*-140-*-*-*-*-iso8859-1")

(defparameter *default-text-input-background*
    "#ffc")

(defparameter *popup-sticky-delay* 200)

(defparameter *trauerrand-p* t)

(defparameter *mono-p* nil)               ;experimental monochrom support.

;;;

(defun redisplay (contact)
  (clue:display-region contact +everywhere+))

;;; ---- Some Utilities ----------------------------------------------------------------------

(defun contact-size (contact)
  "Returns the width and height of a contact as multiple values."
  (values (contact-width contact)
          (contact-height contact)))

(defun move-resize (contact x y width height &optional (border-width 0))
  "Does clue:move and clue:resize in one step."
  (xlib:with-state (contact)
    (clue:move contact x y)
    (clue:resize contact width height border-width)))

;;;;

;;; ---- The 3D subtrate ----------------------------------------------------------------------

;; This contact class takes care of the common 3D look. This class should be
;; mixed into other contact classes, which want to offer a 3D look.

;; INTERIOR-RECTANGLE* then returns the actual rectangle, which is for
;; drawing. There are also the {LEFT,RIGHT,TOP,BOTTOM}-LEADING methods.

;; Drawing of the 3D borders is done in an :after method on
;; clue:display. Maybe I should switch to an :around method, which also sets
;; up clipping, so that one will be unable to paint over the decoration.

(clue:defcontact 3d (clue:contact)
  ((shadow-style) 
   (shadow-width)
   (shadow-color)
   (padding-height)
   (padding-width)
   (margin-height)
   (margin-width))
  (:resources
   ;; The style of shadow to use
   (shadow-style        :type (member :none :inset :outset 
                                      :groove :ridge 
                                      :dotted :dashed 
                                      :solid) 
                        :initform :none)
   ;; Width (or thickness) of shadow
   (shadow-width        :type integer           :initform 0)
   ;; Its colour (currently only used for flat borders).
   (shadow-color        :type xlib:pixel        :initform "black")
   ;; Padding is between the border and the contents
   (padding-height      :type integer           :initform 0)
   (padding-width       :type integer           :initform 0)
   ;; Margin is between the X11 contact border and our border
   (margin-height       :type integer           :initform 0)
   (margin-width        :type integer           :initform 0)))

(defmethod left-leading ((self 3d))
  (with-slots (shadow-width padding-width margin-width) self
    (+ shadow-width padding-width margin-width)))

(defmethod right-leading ((self 3d))
  (with-slots (shadow-width padding-width margin-width) self
    (+ shadow-width padding-width margin-width)))

(defmethod top-leading ((self 3d))
  (with-slots (shadow-width padding-height margin-height) self
    (+ shadow-width padding-height margin-height)))

(defmethod bottom-leading ((self 3d))
  (with-slots (shadow-width padding-height margin-height) self
    (+ shadow-width padding-height margin-height)))

(defmethod interior-rectangle* ((self 3d))
  "Returns the interior region of an 3d look contact as four values:
   x0; y0; x1; y1."
  (with-slots (shadow-width 
               padding-height margin-height
               padding-width margin-width) self
    (values 
     (+ shadow-width padding-width margin-width)
     (+ shadow-width padding-height margin-height)
     (- (contact-width self) 1 shadow-width padding-width margin-width)
     (- (contact-height self) 1 shadow-width padding-height margin-height))))

(defmethod interior-height ((self 3d))
  (multiple-value-bind (x1 y1 x2 y2) (interior-rectangle* self)
    (declare (ignore x1 x2))
    (- y2 y1)))

(defmethod interior-width ((self 3d))
  (multiple-value-bind (x1 y1 x2 y2) (interior-rectangle* self)
    (declare (ignore y1 y2))
    (- x2 x1)))

(defmethod clue:display-region :after ((self 3d) region)
  (declare (ignore region))
  (with-slots (shadow-width shadow-style shadow-color
               margin-height margin-width) self
    (draw-simple-border self 
                        margin-width margin-height
                        (- (contact-width self) (* 2 margin-width))
                        (- (contact-height self) (* 2 margin-height))
                        shadow-style shadow-width shadow-color)))

(defmethod (setf shadow-style) (new-value (self 3d))
  (with-slots (shadow-style) self
    (setf shadow-style new-value))
  ;; Arg! I really want something like queue-redraw
  ;; Or at least something like xlib:with-state for contacts.
  (when (clue:realized-p self)
    (xlib:clear-area self)
    (redisplay self)))

;;;; Abstract fonts

;; (text-width contact font sequence &key start end)
;; (font-ascent contact font)
;; (font-descent contact font)
;; (draw-glyphs contact font x y sequence &key start end foreground background image-p)

#||
(deftype afont ()
  `(or xlib:font r2::text-style))       ;hmmm -- wir haben hier ein echtes problem
||#

(deftype afont ()
  `(satisfies afontp))                   ;so that others can later add foreign types here.

(defmethod afontp ((x t))
  ;; fallback method
  (declare (ignorable x))
  nil)

(defmethod afontp ((x xlib:font))
  (declare (ignorable x))
  t)

(defmethod text-width ((contact clue:contact) (font xlib:font) sequence
                       &key (start 0) (end (length sequence))
                            (white-space :normal))
  (declare (ignorable contact))
  white-space ;xxx
  (xlib:text-width font sequence :start start :end end))

(defmethod font-ascent ((contact clue:contact) (font xlib:font))
  (declare (ignorable contact))
  (xlib:font-ascent font))

(defmethod font-descent ((contact clue:contact) (font xlib:font))
  (declare (ignorable contact))
  (xlib:font-descent font))

(defmethod draw-glyphs ((contact clue:contact) (font xlib:font)
                        x y sequence 
                        &key (start 0) (end (length sequence))
                             foreground background image-p
                             (white-space :normal))
  white-space                           ;fix me
  (using-gcontext (gc 
                   :drawable contact
                   :foreground (and foreground (convert contact foreground 'xlib:pixel))
                   :background (and background (convert contact background 'xlib:pixel))
                   :font       font
                   ;;:size       16
                   )
    (funcall 
     (if image-p #'xlib:draw-image-glyphs #'xlib:draw-glyphs)
     contact gc x y sequence :start start :end end)))

;;;; -- Label -----------------------------------------------------------------

(defcontact label (3d contact)
  ;; Slots
  ((font                :reader label-font)
   (label-string        :reader label-string :initarg :label-string)
   (foreground          :reader label-foreground)
   (alignment)
   )
  (:Resources
   (label-string        :type (or rod string)
                        :initform "")
   (alignment           :type (member :left :center :right)
                        :initform :left)
   (font                :type afont
                        :initform *default-font*)
   (foreground          :type xlib:pixel
                        :initform "black")) )

(defmethod clue:preferred-size ((self label) &key)
  ;; --> width height border-width
  (with-slots (label-string font) self
    ;; Note that we have to use the root window in the USING-GCONTEXT form
    ;; below, because the label itself is probably not yet realized, when
    ;; its preferred size is queried.
    (let ((w (text-width self font label-string))
          (a (font-ascent self font))
          (d (font-descent self font)))
      (values
       (+ (left-leading self) (right-leading self) w)
       (+ (top-leading self) (bottom-leading self) a d)
       0))))

(defmethod clue:display-region ((self label) region)
  (declare (ignore region))
  (with-slots (label-string alignment foreground font) self
    (multiple-value-bind (x0 y0 x1 y1) (interior-rectangle* self)
      (let* ((width (text-width self font label-string))
             (a (font-ascent self font))
             (d (font-descent self font))
             (yo (+ y0 (+ a (round (- (- y1 y0) (+ a d)) 2))))
             (xo (case alignment
                   (:left x0)
                   (:right (- x1 width))
                   (:center (+ x0 (round (- (- x1 x0) width) 2))))))
        (cond ((clue:sensitive-p self)
               (draw-glyphs self font xo yo label-string
                            :foreground (label-foreground self)))
              (t
               (draw-glyphs self font (1+ xo) (1+ yo) 
                            label-string
                            :foreground (3d-light-color self foreground))
               (draw-glyphs self font xo yo 
                            label-string
                            :foreground (3d-dark-color self foreground))))))))

(defmethod (setf label-string) (new-value (self label))
  (setf (slot-value self 'label-string) new-value)
  (when (realized-p self)
    (xlib:clear-area self)
    (redisplay self)))

(defmethod (setf label-alignment) (new-value (self label))
  (setf (slot-value self 'alignment) new-value)
  (when (realized-p self)
    (xlib:clear-area self)
    (redisplay self)))

(defmethod (setf label-foreground) (new-value (self label))
  (setf (slot-value self 'foreground) (convert self new-value 'xlib:pixel))
  (when (realized-p self)
    (xlib:clear-area self)
    (redisplay self)))

(defmethod (setf label-font) (new-value (self label))
  (setf (slot-value self 'font) (convert self new-value 'xlib:font))
  (when (realized-p self)
    (xlib:clear-area self)
    (redisplay self)))

;;; ---- Button ---------------------------------------------------------------

(defcontact button (label)
  ()
  (:resources
   (shadow-style        :initform :outset)
   (shadow-width        :initform 2)
   (padding-height      :initform 2)
   (padding-width       :initform 2)
   (alignment           :initform :center)))

(defevent button :button-press button-press)
(defevent button :button-release button-release)
(defevent button :enter-notify enter-notify)
(defevent button :leave-notify leave-notify)

(defmethod button-press ((self button))
  (setf (shadow-style self) :inset))

(defmethod button-release ((self button))
  (with-event (x y)
    (when (inside-contact-p self x y)
      (apply-callback self :activate))
    (setf (shadow-style self) :outset)))

(defmethod enter-notify ((self button))
  (with-event (state)
    (unless (zerop (logand #xF00 state))
      (setf (shadow-style self) :inset))))

(defmethod leave-notify ((self button))
  (setf (shadow-style self) :outset) )

#|
;;fancy
(defmethod enter-notify ((self button))
  (with-event (state)
    (setf (shadow-style self) :outset)
    (unless (zerop (logand #xF00 state))
      (setf (shadow-style self) :inset))))

(defmethod leave-notify ((self button))
  (setf (shadow-style self) :none) )

;; LISPM style
(defmethod enter-notify ((self button))
  (with-event (state)
    (setf (shadow-style self) :solid)
    (unless (zerop (logand #xF00 state))
      (setf (shadow-style self) :solid))))

(defmethod leave-notify ((self button))
  (setf (shadow-style self) :none) )

|#

(defcontact drawn-button (button)
  ()
  (:resources
   (shadow-style        :initform :outset)
   (shadow-width        :initform 2)
   (padding-height      :initform 2)
   (padding-width       :initform 2)
   (alignment           :initform :center)) )

(defmethod clue:display-region ((self drawn-button) region)
  region
  (clue:apply-callback self :expose))

;;; ---- menu-button ----------------------------------------------------------

;; Menu items do only have to obey to this menu item protocol:

;;  mi-enter menu-item -- The menu item is entered (either by mouse or 
;;                        keyboard).
;;  mi-leave menu-item -- The luser left the item
;;  mi-activate        -- The luser activated the item

;; (There isn't really more what could be done to menu items, with a mouse
;; while dragging; O.k. you could press a key, but try this with c-m-s-%
;; while mb-1 pressed).

;; Menu items, which pop up other menu-shells need some additions here.

;; Further menu-shell are supposed to listen to clue:sensitive-p.


(defclass menu-item () ())
(defclass popup-menu-item (menu-item)
  ((popee :initarg :popee)))

(defevent menu-button :button-press nop)
(defevent menu-button :button-release nop)
(defevent menu-button :enter-notify nop)
(defevent menu-button :leave-notify nop)

(defcontact menu-button (button menu-item)
  ()
  (:resources
   (shadow-style  :initform :none)
   (shadow-width  :initform 2)
   (margin-height :initform 0)
   (margin-width  :initform 0)
   (padding-height :initform 2)
   (padding-width  :initform 2)
   (alignment :initform :left)
   (font :initform *default-font*)))

(defmethod mi-enter ((self menu-button))
  (setf (shadow-style self) :outset) )

(defmethod mi-leave ((self menu-button))
  (setf (shadow-style self) :none) )

#||
(defmethod mi-enter ((self menu-button))
  (setf (shadow-style self) :none)
  (setf (contact-background self) :blue4)
  (setf (slot-value self 'foreground) :white)
  (xlib:clear-area self :exposures-p t))

(defmethod mi-leave ((self menu-button))
  (setf (shadow-style self) :none)
  (setf (contact-background self) *default-background*)
  (setf (slot-value self 'foreground) :black)
  (xlib:clear-area self :exposures-p t))
||#

(defmethod mi-activate ((self menu-button))
  (apply-callback self :activate))

;;; ---- popup-menu-button ----------------------------------------------------

(defevent popup-menu-button :button-press nop)
(defevent popup-menu-button :button-release nop)
(defevent popup-menu-button :enter-notify nop)
(defevent popup-menu-button :leave-notify nop)

(defcontact popup-menu-button (button popup-menu-item)
  ()
  (:resources
   (shadow-style  :initform :none)
   (shadow-width  :initform 2)
   (margin-height :initform 0)
   (margin-width  :initform 0)
   (padding-height :initform 2)
   (padding-width  :initform 2)
   (alignment :initform :left)))

(defmethod mi-enter ((self popup-menu-button))
  ;;(setf (label-font self) *default-bold-font*)
  (setf (shadow-style self) :outset)
  (with-slots (popee) self
    (multiple-value-bind (rx ry) 
        (contact-translate self (+ (contact-width self) 0) 0)
      (popup-popee popee rx ry) )))

(defmethod mi-leave ((self popup-menu-button))
  (setf (shadow-style self) :none)
  (with-slots (popee) self
    (setf (contact-state popee) :withdrawn)))

(defmethod popup-popee ((popee contact) x y)
  ;; well we try our best to avoid that the popup shell is visible only partially.
  (setf x (max 0 (min (- (contact-width (contact-root popee))
                         (contact-width popee))
                      x)))
  (setf y (max 0 (min (- (contact-height (contact-root popee))
                         (contact-height popee))
                      y)))
  (move popee x y)
  (setf (contact-state popee) :mapped)
  ;; we need to do an `update-state' to ensure, that the contact is
  ;; mapped, when we want to raise it. (otherwise we receive an X
  ;; error).
  (update-state (contact-display popee))
  (setf (xlib:window-priority popee) :above))

;;; ---- Pop-up Buttons -------------------------------------------------------

;; A POPUP-BUTTON is a button, which will popup a menu, if pressed.
;; It also then controlls the menu operation.

(defcontact popup-button (button)
  ((popee :initarg :popee)
   (poped-p                :initform nil)
   (grabed-p               :initform nil)
   (active-child           :initform nil)
   (time-last-button-press :initform 0) )
  (:Resources
   (shadow-style   :initform :none)
   (shadow-width   :initform 0)
   (margin-height  :initform 0)
   (margin-width   :initform 0)
   (padding-height :initform 2)
   (padding-width  :initform 2)
   (alignment      :initform :left)))

(defevent popup-button :button-press   pubu-press)
(defevent popup-button :button-release pubu-release)
(defevent popup-button :motion-notify  pubu-motion-notify)

(defmethod pubu-press ((self popup-button))
  (with-slots (popee poped-p grabed-p time-last-button-press) self
    (with-event (time)
      (setf time-last-button-press time)
      (cond (grabed-p
             nil)
            (t
             (setf poped-p t)
             (multiple-value-bind (rx ry) 
                 (contact-translate self 0 (+ 0 (contact-height self)))
               (popup-popee popee rx ry))
             (pubu-motion-notify self))) )))

(defmethod pubu-pop-down ((self popup-button))
  (with-slots (popee poped-p grabed-p time-last-button-press active-child) self
    (setf poped-p nil)
    (setf (contact-state popee) :withdrawn)
    (dolist (k active-child)
      (when (typep k 'menu-button)     ;xxx
        (mi-activate k)))
    (pubu-update-active self nil)))

(defmethod pubu-release ((self popup-button))
  (with-slots (popee poped-p grabed-p time-last-button-press) self
    (with-event (time)
      (cond (grabed-p
             (xlib:ungrab-pointer (contact-display self))
             (setf grabed-p nil)
             (pubu-pop-down self) )
            ((< (- time time-last-button-press) *popup-sticky-delay*)
             (xlib:grab-pointer self 
                                (xlib:make-event-mask :button-press 
                                                      :button-release 
                                                      :pointer-motion))
             (setf grabed-p t))
            (t
             (pubu-pop-down self))  ))))

;;
;; Wir haben immer noch arge Problem mit FIND-CONTACT-UNDER-POSITION:
;; Denn wir sollten auch die stacking order im Auge behalten, dann
;; klappt es auch mit dem Nachbarn.
;;
;; Zusätzlich könnten wir POPUP-POPEE noch eine Region übergeben, die
;; eine no-no area angibt.
;;
;; Und: Die doppelte und dreifache Definition DISPLAY-REGION,
;; PREFERRED-SIZE ist etwas unelegant.
;;

(defmethod find-contact-under-position ((contact composite) x y)
  (or
   (dolist (child (composite-children contact))
     (multiple-value-bind (x y) (contact-translate contact x y child)
       (and (or (typep child 'popup-menu-item)
                (typep child 'popup-button))
            (clue:sensitive-p child)
            (inside-contact-p child x y)
            (return (list child)))))
   (dolist (child (composite-children contact))
     (multiple-value-bind (x y) (contact-translate contact x y child)
       (let ((r (find-contact-under-position child x y)))
         (when r
           (return r)))))))

(defmethod find-contact-under-position ((contact contact) x y)
  (and (inside-contact-p contact x y)
       (clue:sensitive-p contact)
       (list contact)))

(defmethod find-contact-under-position ((self popup-menu-button) x y)
  (with-slots (popee) self
    (or
     (and (inside-contact-p self x y)
          (clue:sensitive-p self)
          (list self))
     (and (eql :mapped (contact-state popee))
          (cons self
                (multiple-value-bind (x y) (contact-translate self x y popee)
                  (find-contact-under-position popee x y)))))))

(defmethod find-contact-under-position ((self popup-button) x y)
  (with-slots (popee) self
    (or
     (and (eql :mapped (contact-state popee))
          (cons self
                (multiple-value-bind (x y) (contact-translate self x y popee)
                  (find-contact-under-position popee x y))))
     (and (inside-contact-p self x y)
          (clue:sensitive-p self)
          (list self)))))

(defmethod pubu-update-active ((self popup-button) new-active)
  (with-slots (popee poped-p active-child) self
    (unless (equal new-active active-child)
      (dolist (k active-child)
        (unless (member k new-active)
          (and (or (typep k 'menu-item) (typep k 'popup-button))
               (mi-leave k))))
      (dolist (k new-active)
        (unless (member k active-child)
          (and (or (typep k 'menu-item) (typep k 'popup-button))
               (mi-enter k))))
      (setf active-child new-active))))

#||
(defmethod pubu-motion-notify ((self popup-button))
  (with-slots (popee poped-p active-child) self
    (with-event (x y)
      (when poped-p
        (multiple-value-bind (x y) 
            (contact-translate self x y (contact-parent self))
          (pubu-update-active self
                              (find-contact-under-position (contact-parent self)
                                                           x y)))))))
||#

(defmethod pubu-motion-notify ((self popup-button))
  (with-slots (popee poped-p active-child) self
    (with-event (x y)
      (when poped-p
        (multiple-value-bind (x y) 
            (contact-translate self x y self)
          (pubu-update-active self (find-contact-under-position self x y)))))))

(defmethod mi-enter ((self popup-button))
  (setf (shadow-style self) :outset)
  (with-slots (popee) self
    (multiple-value-bind (rx ry) 
        (contact-translate self 0 (+ 0 (contact-height self)))
    (popup-popee popee rx ry))))

(defmethod mi-leave ((self popup-button))
  (setf (shadow-style self) :none)
  (with-slots (popee) self
    (setf (contact-state popee) :withdrawn)))

;;;;

(defcontact popup-button-drop (popup-button) ())

(defmethod clue:preferred-size ((self popup-button-drop) &key)
  ;; --> width height border-width
  (with-slots (label-string font) self
    ;; Note that we have to use the root window in the USING-GCONTEXT form
    ;; below, because the label itself is probably not yet realized, when
    ;; its preferred size is queried.
    (let ((w (text-width self font label-string))
          (a (font-ascent self font))
          (d (font-descent self font)))
      (values
       (+ (left-leading self) (right-leading self) w 20)
       (+ (top-leading self) (bottom-leading self) a d)
       0))))

(defmethod clue:display-region ((self popup-button-drop) region)
  (declare (ignore region))
  (with-slots (label-string alignment foreground font) self
    (multiple-value-bind (x0 y0 x1 y1) (interior-rectangle* self)
      (let* ((width (text-width self font label-string))
             (a (font-ascent self font))
             (d (font-descent self font))
             (yo (+ y0 (+ a (round (- (- y1 y0) (+ a d)) 2))))
             (xo (case alignment
                   (:left x0)
                   (:right (- x1 width))
                   (:center (+ x0 (round (- (- x1 x0) width) 2))))))
        (cond ((clue:sensitive-p self)
               (draw-glyphs self font xo yo label-string
                            :foreground (label-foreground self)))
              (t
               (draw-glyphs self font (1+ xo) (1+ yo) 
                            label-string
                            :foreground (3d-light-color self foreground))
               (draw-glyphs self font xo yo 
                            label-string
                            :foreground (3d-dark-color self foreground))))
        (let ((a 5))
          (draw-down-arrow self 
                           (- x1 16) 
                           (- (ceiling (+ y0 y1) 2) a) 
                           (+ (- x1 16) (floor (* 2 (sqrt 2) a)))
                          (+ (ceiling (+ y0 y1) 2) a))) ))))
;;;;;;

(defmethod clue:preferred-size ((self popup-menu-button) &key)
  ;; --> width height border-width
  (with-slots (label-string font) self
    ;; Note that we have to use the root window in the USING-GCONTEXT form
    ;; below, because the label itself is probably not yet realized, when
    ;; its preferred size is queried.
    (let ((w (text-width self font label-string))
          (a (font-ascent self font))
          (d (font-descent self font)))
      (values
       (+ (left-leading self) (right-leading self) w 20)
       (+ (top-leading self) (bottom-leading self) a d)
       0))))

(defmethod clue:display-region ((self popup-menu-button) region)
  (declare (ignore region))
  (with-slots (label-string alignment foreground font) self
    (multiple-value-bind (x0 y0 x1 y1) (interior-rectangle* self)
      (let* ((width (text-width self font label-string))
             (a (font-ascent self font))
             (d (font-descent self font))
             (yo (+ y0 (+ a (round (- (- y1 y0) (+ a d)) 2))))
             (xo (case alignment
                   (:left x0)
                   (:right (- x1 width))
                   (:center (+ x0 (round (- (- x1 x0) width) 2))))))
        (cond ((clue:sensitive-p self)
               (draw-glyphs self font xo yo label-string
                            :foreground (label-foreground self)))
              (t
               (draw-glyphs self font (1+ xo) (1+ yo) 
                            label-string
                            :foreground (3d-light-color self foreground))
               (draw-glyphs self font xo yo 
                            label-string
                            :foreground (3d-dark-color self foreground))))
        (let* ((a 5)
               (b (floor (* 2  a))))
          (draw-right-arrow self 
                           (- x1 b) 
                           (- (ceiling (+ y0 y1) 2) a) 
                           (- x1 0)
                           (+ (ceiling (+ y0 y1) 2) a))) ))))


;;; ---- Boxen ----------------------------------------------------------------

;; Simple vertical or horizontal boxen.

(defun preferred-height (x)
  (nth-value 1 (preferred-size x)))

(defun preferred-width (x)
  (nth-value 0 (preferred-size x)))


;;; ---- separator ------------------------------------------------------------

(defcontact separator (contact)
  ())

(defcontact horizontal-separator (separator)
  ())

(defcontact vertical-separator (separator)
  ())

(defmethod preferred-size ((self horizontal-separator) &key width)
  (values width 4 0))

(defmethod preferred-size ((self vertical-separator) &key height)
  (values 4 height 0))

(defmethod clue:display-region ((self vertical-separator) region)
  (declare (ignore region))
  (draw-complex-border self 1 0 2 (contact-height self)
                       :none 0 "black" :none 0 "black" :none 0 "black"
                       :groove 2 "black"))

(defmethod clue:display-region ((self horizontal-separator) region)
  (declare (ignore region))
  (draw-complex-border self 1 0 (contact-width self) 2
                       :none 0 "black" :none 0 "black"
                       :groove 2 "black"
                       :none 0 "black"))

;;; ---------------------------------------------------------------------------
;;;  Grid
;;;

(defparameter *default-separator-size* 4)

;; Very strange -- If we do not include this 'fake-to-fool-clue' class
;; into the list of super classes for grid, we do not get redraw
;; events on the grid itself. I guess you are not supposed to define
;; #'display for composite contacts.

(defcontact fake-to-fool-clue (contact) ())

(defcontact grid (fake-to-fool-clue composite)
  ((cols     :initarg :cols     :initform nil :reader   grid-cols)
   (rows     :initarg :rows     :initform nil :reader   grid-rows)
   (separator-size :initarg :separator-size
                   :initform *default-separator-size*
                   :accessor grid-separator-size)
   (grabed   :initform nil                    :accessor grid-grabed) ))

;; Each row/column is described by exactly one grid-pan object:

(defstruct grid-pan
  size          ;the current size of this pane in pixels
  ;; The specified size
  spec-value    ;
  spec-unit     ;one of :%, :px or '*
  min-size      ;minimum size
  max-size      ;maximum size (currently unused)
  ;; 
  mirror)       ;while dragging the separator, a temporary CLX window 
                ; to indicate new position

;; NOTE: The spec-value field isn't really needed, because you could
;; always recalculate it on the fly.

;;; event handlers

(defevent grid :button-press button-press)
(defevent grid :motion-notify motion-notify)
(defevent grid :button-release button-release)

(defmethod button-press ((self grid))
  (with-slots (grabed) self
    (with-event (x y)
      (setq grabed nil)
      (grid-map-over-separators (lambda (k orientation x0 y0 x1 y1)
                                 (declare (ignore orientation))
                                 (when (and (<= x0 x x1) (<= y0 y y1))
                                   (push k grabed)))
                               self)
      (grid-update-mirrors self)) ))

(defmethod motion-notify ((self grid))
  (with-slots (grabed) self
    ;; Save the pan sizes. -- Everything we do while dragging 
    ;; is only temporary.
    (let ((saved (mapcar (lambda (s) 
                           (grid-pan-size s)) 
                         (append (grid-rows self) (grid-cols self)))))
      (with-event (x y)
        (adjust-grid-pans (grid-rows self) grabed y)
        (adjust-grid-pans (grid-cols self) grabed x))
      (grid-update-mirrors self)
      ;; Restore the old values
      (mapcar (lambda (x y) 
                (setf (grid-pan-size x) y)) 
              (append (grid-rows self) (grid-cols self)) saved) )))

(defmethod button-release ((self grid))
  (with-slots (grabed) self
    (with-event (x y)
      (adjust-grid-pans (grid-cols self) grabed x)
      (adjust-grid-pans (grid-rows self) grabed y))
    (setf grabed nil)
    (grid-update-mirrors self)
    (respecifiy-grid-pans (grid-rows self))
    (respecifiy-grid-pans (grid-cols self))
    (grid-update-separators self)
    (grid-update-cells self)))

(defmethod resize :after ((self grid) width height border-width)
  (declare (ignore border-width))
  (allot-grid-pans (grid-cols self) width)
  (allot-grid-pans (grid-rows self) height)
  (grid-update-separators self)
  (grid-update-cells self))

;;;; resizing pans

(defun respecifiy-grid-pans (pans)
  ;; Given a list of pans using their current size, adjust the spec-value
  ;; field to match the current size.
  (let* ((wtot (reduce #'+ (mapcar #'grid-pan-size pans)))
         (wrem (- wtot (reduce #'+ 
                               (mapcar #'grid-pan-size 
                                       (remove '* pans 
                                               :key #'grid-pan-spec-unit))))))
    (dolist (s pans)
      (setf (grid-pan-spec-value s)
        (ecase (grid-pan-spec-unit s)
          (:%  (* 100 (/ (grid-pan-size s) wtot)))
          (:px (grid-pan-size s))
          (*   (/ (grid-pan-size s) wrem))))) ))

(defun adjust-grid-pans (pans grabed z)
  (do ((q pans (cdr q))
       (zo 0 (+ zo (grid-pan-size (car q)))))
      ((null q))
    (when (member (car q) grabed)
      (let ((this (car q))
            (next (cadr q)))
        (let ((delta (- (- z zo) (grid-pan-size this))))
          (cond ((< (- (grid-pan-size next) delta) (grid-pan-min-size next))
                 (setf delta (- (grid-pan-size next) (grid-pan-min-size next)))
                 (incf (grid-pan-size this) delta)
                 (decf (grid-pan-size next) delta) )
                ((< (+ (grid-pan-size this) delta) (grid-pan-min-size this))
                 (setf delta (- (grid-pan-min-size this) (grid-pan-size this)))
                 (incf (grid-pan-size this) delta)
                 (decf (grid-pan-size next) delta) )
                (t
                 (incf (grid-pan-size this) delta)
                 (decf (grid-pan-size next) delta)))))
      (return))))

(defun allot-grid-pans (spec tot)
  (dolist (k spec)
    (ecase (grid-pan-spec-unit k)
      (:px (setf (grid-pan-size k) (grid-pan-spec-value k)))
      (:%  (setf (grid-pan-size k) 
             (round (* (grid-pan-spec-value k) tot) 100)))
      (*   (setf (grid-pan-size k) 0)) ))
  (let ((fest (reduce #'+ (mapcar #'grid-pan-size spec))))
    (dolist (k spec)
      (when (eql '* (grid-pan-spec-unit k))
        (setf (grid-pan-size k)
          (round (* (grid-pan-spec-value k) (- tot fest))))))) )

;;;; keeping the display up to date

(defmethod grid-update-mirrors ((self grid))
  (with-slots (grabed) self
    (grid-map-over-separators 
     (lambda (sep orientation x0 y0 x1 y1)
       (declare (ignore orientation))
       (cond (grabed ;;(member sep grabed)
              (cond ((not (grid-pan-mirror sep))
                     (setf (grid-pan-mirror sep)
                       (xlib:create-window :parent self
                                           :x x0 :y y0 
                                           :width (- x1 x0) 
                                           :height (- y1 y0)
                                           :background 128
                                           ;;xxx
                                           #||(gtk-gui::50%-gray-pixmap 
                                                        (xlib:drawable-display 
                                                         (gui::get-drawable+gcontext self)))
                                           ||#
                                           :save-under :on
                                           :border 255 ;xxx
                                           :border-width 0))
                     (setf (xlib:window-priority (grid-pan-mirror sep)) :above)
                     (xlib:map-window (grid-pan-mirror sep))
                     (xlib:display-finish-output (xlib:window-display (grid-pan-mirror sep)))
                     '(break))
                    (t
                     (xlib:with-state ((grid-pan-mirror sep))
                       (setf (xlib:drawable-x (grid-pan-mirror sep))      x0
                             (xlib:drawable-width (grid-pan-mirror sep))  (- x1 x0)
                             (xlib:drawable-y (grid-pan-mirror sep))      y0
                             (xlib:drawable-height (grid-pan-mirror sep)) (- y1 y0))) )))
             (t
              (cond ((grid-pan-mirror sep)
                     (xlib:destroy-window (grid-pan-mirror sep))
                     (setf (grid-pan-mirror sep) nil))))))
     self)))

(defmethod grid-update-separators ((self grid))
  (when (realized-p self)
    (grid-map-over-separators 
     (lambda (k orientation x0 y0 x1 y1)
       k
       (case orientation
         (:horizontal (draw-complex-border self (+ x0 2) 
                                           (- (round (+ y0 y1) 2) 1) 
                                           (- x1 x0 4) 1
                                           :groove 2 :black
                                           :none 0 :black
                                           :none 0 :black
                                           :none 0 :black))
         (:vertical 
          (draw-complex-border self 
                               (- (round (+ x0 x1) 2) 1) 
                               (+ y0 2) 
                               1 
                               (- y1 y0 4)
                               :none 0 :black
                               :none 0 :black
                               :none 0 :black
                               :groove 2 :black)
          '(draw-complex-border self x0 y0 (- x1 x0) (- y1 y0)
            ;;(round (+ x0 x1) 2) (+ y0 2) 1 (- y1 y0 4)
                               :outset 2 :black
                               :outset 2 :black
                               :outset 2 :black
                               :outset 2 :black))))
     self)))

(defmethod clue:display-region ((self grid) region)
  (declare (ignore region))
  (grid-update-separators self))

(defmethod grid-update-cells ((self grid))
  (grid-map-over-cells 
   (lambda (child x0 y0 w h)
     (when child
       (move child x0 y0)
       (resize child (max 1 w) (max 1 h) 0)))
   self))

(defmethod grid-map-over-separators (fun (self grid))
  (with-slots (width height) self
    (let ((x 0))
      (dolist (k (butlast (grid-cols self)))
        (incf x (grid-pan-size k))
        (funcall fun k :vertical 
                 (- x (grid-separator-size self)) 0 
                 (+ x (grid-separator-size self)) height)))
    (let ((y 0))
      (dolist (k (butlast (grid-rows self)))
        (incf y (grid-pan-size k))
        (funcall fun k :horizontal 
                 0 (- y (grid-separator-size self)) 
                 width (+ y (grid-separator-size self))))) ))

(defmethod grid-map-over-cells (fun (self grid))
  (let ((cp (composite-children self)))
    (let ((y 0))
      (dolist (row (grid-rows self))
        (let ((x 0))
          (dolist (col (grid-cols self))
            (funcall fun (pop cp) 
                     (+ x (if (eq col (car (grid-cols self)))
                              0
                            (grid-separator-size self)))
                     (+ y (if (eq row (car (grid-rows self)))
                              0
                            (grid-separator-size self)))
                     (+ 0 (grid-pan-size col))
                     (+ 0 (grid-pan-size row)))
            (incf x (grid-pan-size col))))
        (incf y (grid-pan-size row))))))

;;;

(defmethod preferred-size ((self grid) &key width height border-width)
  (values 700 700 0))

(defmethod initialize-geometry ((self grid))
  '(mapc #'initialize-geometry (composite-children self))
  (multiple-value-bind (w h b) (preferred-size self)
    (resize self w h b))
  '(dolist (k (composite-children self))
    (setf (slot-value k 'width) 1
          (slot-value k 'height) 1))
  '(while-changing-layout (self)
                         (with-slots (width height) self
                           (allot-grid-pans (grid-cols self) width)
                           (allot-grid-pans (grid-rows self) height)
                           (grid-update-separators self)
                           (grid-update-cells self) )))

(defmethod manage-geometry ((self grid) contact
			    x y width height border-width &key)
  (with-slots (width height) self
    (allot-grid-pans (grid-cols self) width)
    (allot-grid-pans (grid-rows self) height)
    (grid-update-separators self)
    (grid-update-cells self) ))

(defmethod change-layout ((self grid) &optional newly-managed)
  (with-slots (width height) self
    (allot-grid-pans (grid-cols self) width)
    (allot-grid-pans (grid-rows self) height)
    (grid-update-separators self)
    (grid-update-cells self) ))

;;;

(defun make-grid-pans (xs)
  (mapcar (lambda (x)
            (when (eql (car x) 1) (setf x (cons :px (cdr x))))
            (assert (and (consp x) 
                         (realp (cdr x)) (> (cdr x) 0)
                         (member (car x) '(:% * :px))))
            (make-grid-pan :spec-value (cdr x)
			   :spec-unit (car x) :min-size 10))
          xs))

;;; ---------------------------------------------------------------------------
;;;  Scrolled Window
;;;

(defcontact scrolled-window (3d fake-to-fool-clue composite)
  ((viewhole)
   (canvas)
   (hbar)
   (vbar)
   (hbar-visible-p :initform t :reader scrolled-window-hbar-visible-p :initarg :hbar-visible-p)
   (vbar-visible-p :initform t :reader scrolled-window-vbar-visible-p :initarg :vbar-visible-p)
   (scrolling-policy
    :initarg :scrolling-policy
    :reader scrolled-window-scrolling-policy)
   )
  (:Resources
   (scrolling-policy
    :type (member :automatic :application-defined)
    :initform :automatic
    )
   ))

(defcontact scrolled-window-viewport (composite)
  ())

(defmethod scrolled-window-viewport ((self scrolled-window))
  (with-slots (viewhole) self
    viewhole))

(defmethod scrolled-window-horizontal-scrollbar ((self scrolled-window))
  (with-slots (hbar) self
    hbar))

(defmethod scrolled-window-vertical-scrollbar ((self scrolled-window))
  (with-slots (vbar) self
    vbar))

(defmethod swvp-update-scrollbars ((self scrolled-window-viewport))
  (let ((sw (contact-parent self)))
    (when (clue:composite-children self)
      (setf (thumb-size (scrolled-window-horizontal-scrollbar sw))
        (/ (contact-width self)
           (contact-width (car (clue:composite-children self)))))
      (setf (thumb-size (scrolled-window-vertical-scrollbar sw))
        (/ (contact-height self)
           (contact-height (car (clue:composite-children self)))))) ))

(defmethod swvp-hbar-callback ((self scrolled-window-viewport) value)
  (when (clue:composite-children self)
    (change-geometry (car (clue:composite-children self))
                     :x (floor (* -1 value (contact-width (car (clue:composite-children self))))))))

(defmethod swvp-vbar-callback ((self scrolled-window-viewport) value)
  (when (clue:composite-children self)
    (change-geometry (car (clue:composite-children self))
                     :y (floor (* -1 value (contact-height (car (clue:composite-children self))))))))

(defmethod clue:resize :after ((self scrolled-window-viewport) width height border-width)
  (declare (ignore border-width))
  (when (eq (scrolled-window-scrolling-policy (contact-parent self)) :automatic)
    (swvp-update-scrollbars self))
  (when (clue:realized-p self)
    (clue:apply-callback (clue:contact-parent self) :viewport-resize width height)))

(defmethod realize :after ((self scrolled-window-viewport))
  (clue:apply-callback (clue:contact-parent self) 
                       :viewport-resize (contact-width self) (contact-height self)))

(defmethod clue:add-child :after 
           ((self scrolled-window-viewport) contact &key)
  )

(defmethod cluei::initialize-contact :after ((self scrolled-window))
  (with-slots (hbar vbar viewhole) self
    (setf hbar         (make-contact 'horizontal-scrollbar :parent self :width 1 :height 1)
          vbar         (make-contact 'vertical-scrollbar :parent self :width 1 :height 1)
          viewhole     (make-contact 'scrolled-window-viewport
                                     :parent self :width 1 :height 1
                                     :border-width 0
                                     :background "white")
          )
    (when (eq (scrolled-window-scrolling-policy self)
              :automatic)
      (add-callback vbar :value-changed (lambda (x) (swvp-vbar-callback viewhole x)))
      (add-callback hbar :value-changed (lambda (x) (swvp-hbar-callback viewhole x))) )))

(defmethod clue:preferred-size ((self scrolled-window) &key width height)
  ;;hhmm?!
  (values (or width
              (if (zerop (slot-value self 'width))
                  100
                (slot-value self 'width)))
          (or height
              (if (zerop (slot-value self 'height))
                  100
                (slot-value self 'height)))
          0))

(defmethod viewport-rectangle* ((self scrolled-window))
  (let ((d 4))
    (values (+ 13 d)
            (+ 1 d)
            (- (contact-width self) 1 d) 
            (- (contact-height self) 13 d))))

(defmethod relayout ((self scrolled-window))
  (with-slots (hbar vbar viewhole hbar-visible-p vbar-visible-p) self
    (while-changing-layout (self)
      (multiple-value-bind (x0 y0 x1 y1) (interior-rectangle* self)
        (let ((padding 0)
              (q 15))
          (cond (vbar-visible-p
                 (setf (contact-state vbar) :mapped)
                 (move-resize vbar x0 y0 q 
                              (max 1 (- (- y1 (if hbar-visible-p q 0)) y0))))
                (t
                 (setf (contact-state vbar) :withdrawn)))
          (cond (hbar-visible-p
                 (setf (contact-state hbar) :mapped)
                 (move-resize hbar (+ x0 (if vbar-visible-p q 0)) 
                              (- y1 q)
                              (max 1 (- (- x1 q) x0))
                              q))
                (t
                 (setf (contact-state hbar) :withdrawn)))
          (move-resize viewhole
                       (+ x0 (if vbar-visible-p q 0) padding) (+ y0 padding)
                       (max 1 (- (- x1 (if vbar-visible-p q 0)) x0 padding padding))
                       (max 1 (- (- y1 (if hbar-visible-p q 0)) y0 padding padding))) )))))

(defmethod resize :after ((self scrolled-window) width height border-width)
  (declare (ignore width height border-width))
  (relayout self))

(defmethod initialize-geometry ((self scrolled-window))
  ;;(print 'foo)
  (resize self (preferred-width self) (preferred-height self) 0))

(defmethod (setf scrolled-window-vbar-visible-p) (new-value (self scrolled-window))
  (setf (slot-value self 'vbar-visible-p) new-value)
  (relayout self))

(defmethod (setf scrolled-window-hbar-visible-p) (new-value (self scrolled-window))
  (setf (slot-value self 'hbar-visible-p) new-value)
  (relayout self))

;;;; ---------------------------------------------------------------------------
;;;;  toggle buttons
;;;;

(defcontact radio-button (3d)
  ((toggle-state :initform nil :initarg :toggle-state)
   (indicator-size :initform 10 :initarg :indicator-size)
   (font                )
   (label-string        )
   (foreground          )
   (indicator-on-background :initarg :indicator-on-background )
   (indicator-type :initarg :indicator-type)
   )
  ;; callbacks
  ;; :value-changed new-value
  ;;
  (:Resources
   (label-string        :type string
                        :initform "")
   (font                :type xlib:font
                        :initform *default-font*)
   (foreground          :type xlib:pixel
                        :initform "black")
   (indicator-type      :type (member :1-of-many :n-of-many)
                        :initform :1-of-many)
   (indicator-on-background :type xlib:pixel :initform *default-dark-background*)) )

(defmethod display-indicator ((self radio-button))
  (with-slots (indicator-size toggle-state padding-width foreground  
               indicator-on-background
               indicator-type) self
    (multiple-value-bind (x0 y0 x1 y1) (interior-rectangle* self)
      x1
      (case indicator-type
        (:1-of-many
         (if *mono-p*
             (progn
               (using-gcontext (gc :drawable self 
                                   :foreground foreground)
                 (unless toggle-state
                   (xlib:clear-area self 
                                    :x x0 :y (floor (+ y0 y1 (- indicator-size)) 2)
                                    :width (1- indicator-size) :height (1- indicator-size)))
                 (xlib:draw-arc self gc 
                                x0 (floor (+ y0 y1 (- indicator-size)) 2)
                                (1- indicator-size) (1- indicator-size)
                                0 (* 2 pi))
                 (when toggle-state
                   (let ((indicator-size2 (- indicator-size 4)))
                     (xlib:draw-arc self gc 
                                    (+ x0 2)
                                    (floor (+ y0 y1 (- indicator-size2)) 2)
                                    (1- indicator-size2) (1- indicator-size2)
                                    0 (* 2 pi) t)))
                 ))
           (draw-diamond self 
                         x0 (floor (+ y0 y1 (- indicator-size)) 2)
                         (+ x0 indicator-size) (floor (+ y0 y1 indicator-size) 2)
                         :shadow-style (if toggle-state :inset :outset)
                         :dark-background indicator-on-background)))
        (:n-of-many
         (cond (toggle-state
                (multiple-value-bind (x0 y0 x1 y1)
                    (values x0 
                            (ceiling (+ y0 y1 (- indicator-size)) 2)
                            (+ x0 indicator-size) 
                            (+ (ceiling (+ y0 y1 (- indicator-size)) 2)
                               indicator-size))
                  (old-draw-rectangle* self 
                                   x0 y0 x1 y1
                                   :foreground indicator-on-background
                                   :filled-p t)
                  (old-draw-line* self x0 y0 (1- x1) (1- y1) :foreground foreground)
                  (old-draw-line* self (1+ x0) y0 x1 (1- y1) :foreground foreground)
                  (old-draw-line* self x0 (1- y1) (1- x1) y0 :foreground foreground)
                  (old-draw-line* self (1- x0) (1- y1) (- x1 2) y0 :foreground foreground)
                  ))
               (t
                (xlib:clear-area self 
                                 :x x0 :y (floor (+ y0 y1 (- indicator-size)) 2)
                                 :width indicator-size :height indicator-size
                                 :exposures-p nil)))
         (draw-simple-border self
                             x0 (ceiling (+ y0 y1 (- indicator-size)) 2)
                             indicator-size indicator-size
                             (if toggle-state :inset :outset)
                             2
                             foreground)) ))))

(defmethod clue:display-region ((self radio-button) region)
  (declare (ignore region))
  (with-slots (indicator-size toggle-state padding-width foreground font label-string 
               indicator-on-background)
      self
    (multiple-value-bind (x0 y0 x1 y1) (interior-rectangle* self)
      (display-indicator self)
      (draw-text-into-box self (+ x0 padding-width indicator-size) y0 x1 y1
                          label-string
                          :foreground foreground :font font))))

(defmethod preferred-size ((self radio-button) &key width height border-width)
  (declare (ignore width height border-width))
  (with-slots (indicator-size font label-string padding-width) self
    (using-gcontext (gc :drawable (contact-root self)
                        :font     font)
      (multiple-value-bind (w) (xlib:text-extents gc label-string)
        (let ((a (xlib:font-ascent font))
              (d (xlib:font-descent font)))
          (values (+ (left-leading self) 
                     indicator-size
                     padding-width
                     w
                     (right-leading self))
                  (+ (max (+ a d) (+ 1 indicator-size))
                     (top-leading self) (bottom-leading self))))))))

(defevent radio-button :button-press nop)
(defevent radio-button :button-release radio-button-toggle-event)

(defmethod radio-button-toggle-event ((self radio-button))
  (with-event (x y) 
    (when (inside-contact-p self x y)
      (setf (radio-button-toggle-state self) (not (radio-button-toggle-state self))))
    (apply-callback self :value-changed (radio-button-toggle-state self)) ))

(defmethod radio-button-toggle-state ((self radio-button))
  (slot-value self 'toggle-state))

(defmethod (setf radio-button-toggle-state) (value (self radio-button))
  (setf (slot-value self 'toggle-state) value)
  (redisplay self))

(defmacro with-temporary-bitmap ((bitmap-var drawable width height) &body body)
  `(let ((,bitmap-var (xlib:create-pixmap :drawable ,drawable
                                          :width ,width
                                          :height ,height
                                          :depth 1)))
     (unwind-protect
         (let () .,body)
       (xlib:free-pixmap ,bitmap-var))))

#|
;;; experimental use of the shape extension for contacts
(defmethod clue:resize :after ((self radio-button) width height border-width)
  (declare (ignore width height border-width))
  (with-slots (indicator-size indicator-type) self
    (when (eq :1-of-many indicator-type)
      (multiple-value-bind (x0 y0 x1 y1) (interior-rectangle* self)
        x0 y0 x1 y1
        (multiple-value-bind (x0 y0 x1 y1)
            (values x0 (floor (+ y0 y1 (- indicator-size)) 2)
                    (+ x0 indicator-size) (floor (+ y0 y1 indicator-size) 2))
          (let ((x2 (floor (+ x0 x1) 2))
                (y2 (floor (+ y0 y1) 2)))
            (with-temporary-bitmap (bitmap self x1 y1)
              (using-gcontext (gc :drawable bitmap :foreground 0)
                (xlib:draw-rectangle bitmap gc 0 0 #x7FFF #x7FFF t))
              (using-gcontext (gc :drawable bitmap :foreground 1)
                (xlib:draw-lines bitmap gc
                                 (list (- x2 x0) 0 
                                       0 (- y2 y0)
                                       (- x2 x0) (- y1 y0)
                                       (- x1 x0) (- y2 y0))
                                 :fill-p t)
                (xlib:draw-lines bitmap gc
                                 (list 0 (- y2 y0)
                                       (1- (- x2 x0)) (1- (- y1 y0))
                                       (- x2 x0) (1- (- y1 y0))
                                       (1- (- x1 x0)) (- y2 y0)
                                       (+ (- x2 x0)) 1)
                                 :fill-p nil)
                )
              
              (xlib:shape-mask self bitmap :x-offset x0 :y-offset y0))))))))
|#

;;;; X11 utilities / fixups

(defun get-selection (requestor &optional (selection :primary))
  (xlib:convert-selection selection :string requestor)
  (xlib:display-finish-output (xlib:window-display requestor))
  (car 
   (xlib:event-case ((xlib:window-display requestor) :peek-p t :timeout 2 :discard-p nil)
     (:selection-notify (sequence window property)
        sequence
        (cond ((and (eq window requestor))
               (xlib:discard-current-event (xlib:window-display requestor))
               (cond ((not (null property))
                      (list (map 'string #'code-char (xlib:get-property requestor property))))
                     (t
                      (list nil))))))
     (t nil))))

;;;; ---------------------------------------------------------------------------

(defvar *keysym-name-table*
  (make-hash-table :test #'eql))

(defun define-keysym (name value)
  (setf (gethash value *keysym-name-table*) name))

(define-keysym :backspace       #xFF08) ;back space, back char
(define-keysym :tab             #xFF09)
(define-keysym :linefeed        #xFF0A) ;Linefeed, LF
(define-keysym :clear           #xFF0B)
(define-keysym :return          #xFF0D) ;Return, enter
(define-keysym :pause           #xFF13) ;Pause, hold 
(define-keysym :scroll-lock     #xFF14)
(define-keysym :sys-req         #xFF15)
(define-keysym :escape          #xFF1B)
(define-keysym :delete          #xFFFF) ;Delete, rubout

(define-keysym :home            #xFF50)
(define-keysym :left            #xFF51) ; Move left, left arrow
(define-keysym :up              #xFF52) ; Move up, up arrow
(define-keysym :right           #xFF53) ; Move right, right arrow
(define-keysym :down            #xFF54) ; Move down, down arrow
(define-keysym :prior           #xFF55) ; Prior, previous
(define-keysym :page-up         #xFF55)
(define-keysym :next            #xFF56) ; Next
(define-keysym :page-down       #xFF56)
(define-keysym :end             #xFF57) ; EOL
(define-keysym :begin           #xFF58) ; BOL

(define-keysym :select          #xFF60) ; Select, mark
(define-keysym :print           #xFF61)
(define-keysym :execute         #xFF62) ; Execute, run, do
(define-keysym :insert          #xFF63) ; Insert, insert here
(define-keysym :undo            #xFF65) ; Undo, oops
(define-keysym :redo            #xFF66) ; redo, again
(define-keysym :menu            #xFF67)
(define-keysym :find            #xFF68) ; Find, search
(define-keysym :cancel          #xFF69) ; Cancel, stop, abort, exit
(define-keysym :help            #xFF6A) ; Help
(define-keysym :break           #xFF6B)
(define-keysym :mode-switch     #xFF7E) ; Character set switch
(define-keysym :num-lock        #xFF7F)

;; Modifiers

(define-keysym :shift-left      #xFFE1) ; Left shift
(define-keysym :shift-right     #xFFE2) ; Right shift
(define-keysym :control-left    #xFFE3) ; Left control
(define-keysym :control-right   #xFFE4) ; Right control
(define-keysym :caps-lock       #xFFE5) ; Caps lock
(define-keysym :shift-lock      #xFFE6) ; Shift lock

(define-keysym :meta-left       #xFFE7) ; Left meta
(define-keysym :meta-right      #xFFE8) ; Right meta
(define-keysym :alt-left        #xFFE9) ; Left alt
(define-keysym :alt-right       #xFFEA) ; Right alt
(define-keysym :super-left      #xFFEB) ; Left super
(define-keysym :super-right     #xFFEC) ; Right super
(define-keysym :hyper-left      #xFFED) ; Left hyper
(define-keysym :hyper-right     #xFFEE) ; Right hyper

(defun modifier-skeysym->modifier-name (sks)
  (case sks
    ((:shift-left :shift-right)
     :shift)
    ((:control-left :control-right)
     :control)
    ((:caps-lock) :caps-lock)
    ((:shift-lock) :shift-lock)
    ((:meta-left :meta-right)
     :meta)
    ((:alt-left :alt-right)
     :alt)
    ((:super-left :super-right)
     :super)
    ((:hyper-left :hyper-right)
     :hyper)
    ((:mode-switch)
     :mode-switch)))

;; State bits:
;;  shift  lock  control  mod1  mod2  mod3  mod4  mod5
;;    0      1      2      3     4     5     6     7


(defun keysym->symbolic-keysym (keysym)
  (gethash keysym *keysym-name-table*))

(defun symbolic-modifier-mapping (display)
  (let ((xs (multiple-value-list (xlib:modifier-mapping display))))
    (mapcar #'car
            (mapcar (lambda (x) 
                      (mapcan (lambda (y)
                                (let ((ks (xlib:keycode->keysym display y 0)))
                                  (setq ks 
                                    (modifier-skeysym->modifier-name
                                     (gethash ks *keysym-name-table*)))
                                  (and ks (list ks))))
                              x))
                    xs))))

(defun keysym->iso10646 (keysym)
  (cond ((or (<= #x20 keysym #x7E) (<= #xA0 keysym #xFF))
         ;; iso-latin-1 mapping is straight
         keysym)
        ;; and for testing the most important greek letter!
        ((= keysym #x7EB) #x03BB)        ;greek small lambda
        (t
         nil)))

(defun state-mask->modifier-list (display mask)
  (let ((res nil)
        (modifiers (symbolic-modifier-mapping display)))
    (dotimes (bit 8)
      (when (logbitp bit mask)
        (when (elt modifiers bit)
          (push (elt modifiers bit) res))))
    res))

(defun translate-key-press-event (display keycode state)
  ;; translates an X11 key press event and returns a cons:
  ;; -> (<code> . <bits>)
  ;;
  ;; Where <code> is either a symbol or an integer representing the
  ;; key.  If it is a symbol it is a key, which is not considered to
  ;; be a character, such as :UP or :F1. If it is an integer, it is
  ;; the iso-10646 code of the character. Shift bits are only included
  ;; with non-character key press events.
  ;;
  ;; <bits> is a list of symbols indicating the active modifier bits,
  ;; such as :meta or :control.
  ;;
  (let ((mods (state-mask->modifier-list display state)))
    (let ((keysym (xlib:keycode->keysym display keycode
                                        (+ (if (member :shift mods) 1 0)
                                           (if (member :mode-switch mods) 2 0)))))
      (let ((rune (or (gethash keysym *keysym-name-table*)
                      (keysym->iso10646 keysym)))
            (bits (remove :mode-switch (remove :caps-lock (remove :shift-lock mods)))))
        (if (integerp rune)
            ;; on actual characters, there is no need to include shift
            (cons rune (remove :shift bits))
          (cons rune bits))))))

(defun key-press-match-p (matchee matcher)
  (let ((rune1 (car matchee))
        (rune2 (car matcher))
        (bits1 (cdr matchee))
        (bits2 (cdr matcher)))
    (when (characterp rune1) (setf rune1 (char-code rune1)))
    (when (characterp rune2) (setf rune2 (char-code rune2)))
    (and (null (set-exclusive-or bits1 bits2))
         (eql rune1 rune2))))

;;;; ---------------------------------------------------------------------------
;;;;  text area
;;;;

(defparameter *default-monowidth-font*
    "fixed")

;;;; PATCHES

(defmethod clue:convert (contact value (type (eql 'xlib:color)))
  (typecase value
    (xlib:stringable
      (ignore-errors
       (xlib:parse-color 
        (xlib:screen-default-colormap (contact-screen contact))
        value)))
    (xlib:color value)
    (otherwise nil)))

;;; hack -- inefficient and slow!
(defun char-width (contact font char)
  (text-width contact font (rod char)))


;;;; ====================================================================================================

(defcontact box (3d composite)
  ()
  (:constraints 
   (expand-p :type boolean :initform nil)))

(defcontact vbox (box) () )
(defcontact hbox (box) () )

;; box

(defmethod clue:preferred-size ((self box) &key width height border-width)
  (declare (ignore width height border-width))
  (multiple-value-bind (w h)
      (compute-preferred-size self)
    (values w h 0)))

(defmethod relayout ((self box))
  (mapcar (lambda (geo child)
            (destructuring-bind (x y w h) geo
              (move child (+ x (left-leading self)) (+ (top-leading self) y))
              (resize child w h 0)))
          (compute-children-layout self)
          (clue:composite-children self)))

(defmethod clue:resize :after ((self box) width height border-width)
  (declare (ignore width height border-width))
  (relayout self))

(defmethod clue:manage-geometry ((self box) contact x y width height border-width &key)
  ;; -> success-p x y width height border-width
  (let ((rd-width (lambda (child)
                    (if (and (eq child contact) (not (null width)))
                        width
                      (contact-width child))))
        (rd-height (lambda (child)
                     (if (and (eq child contact) (not (null height)))
                         height
                       (contact-height child)))))
    (multiple-value-bind (ww hh)
        (compute-preferred-size self 
                                :width-reader rd-width
                                :height-reader rd-height)
      (change-geometry self :width ww :height hh))
    ;;
    (let (nx ny nw nh)
      (mapcar (lambda (geo child)
                (destructuring-bind (x y w h) geo
                  (cond ((eq child contact)
                         (setf nx (+ x (left-leading self))
                               ny (+ y (top-leading self))
                               nw w nh h)))))
              (compute-children-layout self :width-reader rd-width :height-reader rd-height)
              (clue:composite-children self))
      (values
       (and (or (null x) (= x nx))
            (or (null y) (= y ny))
            (or (null width) (= width nw))
            (or (null height) (= height nh))
            (or (null border-width) (= border-width 0)))
       nx ny nw nh 0) )))

(defmethod clue:change-layout ((self box) &optional changed-child)
  (declare (ignore changed-child))
  (multiple-value-bind (ww hh) (compute-preferred-size self)
    (change-geometry self :width ww :height hh))
  (relayout self))

;;;

(defun compute-preferred-size/aux (box width-reader height-reader
                                   my-height
                                   h-leading v-leading)
  ;; This is formulated for an VBOX
  (let ((ww 1)
        (hh 0)
        (fixed-p t))
    (dolist (child (composite-children box))
      (let ((w (funcall width-reader child))
            (h (funcall height-reader child)))
        (cond ((not (integerp w))
               (warn "Bogus width: ~S." w)
               (setf w 42))
              ((zerop w)
               (setf w 42)))
        (cond ((not (integerp h))
               (warn "Bogus height: ~S." w)
               (setf h 42))
              ((zerop h)
               (setf h 42)))
        (setf ww (max ww w))
        (incf hh h))
      (when (contact-constraint child 'expand-p)
        (setf fixed-p nil)))
    (values (+ h-leading ww)
            (+ v-leading
               (if fixed-p
                   hh
                 (max hh (- my-height v-leading)))))))

(defmethod compute-preferred-size ((self vbox)
                                   &key (width-reader #'contact-width)
                                        (height-reader #'contact-height))
  (compute-preferred-size/aux self 
                              (f-zerop-or width-reader (lambda (c) (nth-value 0 (preferred-size c)))) 
                              (f-zerop-or height-reader (lambda (c) (nth-value 1 (preferred-size c)))) 
                              (contact-height self)  
                              (+ (left-leading self) (right-leading self))
                              (+ (top-leading self) (bottom-leading self))))

(defmethod compute-preferred-size ((self hbox)
                                   &key (width-reader #'contact-width)
                                        (height-reader #'contact-height))
  (multiple-value-bind (h w bw)
      (compute-preferred-size/aux self 
                                  (f-zerop-or height-reader (lambda (c) (nth-value 1 (preferred-size c)))) 
                                  (f-zerop-or width-reader (lambda (c) (nth-value 0 (preferred-size c)))) 
                                  (contact-width self)  
                                  (+ (top-leading self) (bottom-leading self))
                                  (+ (left-leading self) (right-leading self)))
    (values w h bw)))

;;; hbox

(defun compute-children-layout/each (available size-reader children)
  (let ((free (- available
                 (reduce #'+ (mapcar size-reader
                                     (remove-if (lambda (child) (contact-constraint child 'expand-p))
                                                children))))))
    (and (remove-if-not (lambda (child) (contact-constraint child 'expand-p))
                        children)
         (max 1 (floor free 
                       (count-if (lambda (child) (contact-constraint child 'expand-p))
                                 children))))))

(defun f-zerop-or (alt-0 alt-1)
  ;; functional zerop-based OR
  (lambda (x)
    (let ((y (funcall alt-0 x)))
      (if (zerop y)
          (funcall alt-1 x)
        y))))

(defmethod compute-children-layout ((self vbox)
                                    &key (width-reader #'contact-width)
                                         (height-reader #'contact-height) )
  (declare (ignore width-reader))
  (let ((height-reader (f-zerop-or height-reader (lambda (c) (nth-value 1 (preferred-size c))))))
    (let ((available-width (- (contact-width self) (left-leading self) (right-leading self)))
          (available-height (- (contact-height self) (top-leading self) (bottom-leading self))))
      (let* ((each (compute-children-layout/each available-height height-reader (composite-children self))))
        (let ((y 0))
          (mapcar (lambda (child)
                    (let ()
                      (let ((h (if (contact-constraint child 'expand-p)
                                   each 
                                 (funcall height-reader child))))
                        (prog1 (list 0 y available-width h)
                          (incf y h)))))
                  (composite-children self)))))))


(defmethod compute-children-layout ((self hbox)
                                    &key (width-reader #'contact-width)
                                         (height-reader #'contact-height) )
  (declare (ignore height-reader))
  (let ((width-reader (f-zerop-or width-reader (lambda (c) (nth-value 0 (preferred-size c))))))
    (let ((available-width (- (contact-width self) (left-leading self) (right-leading self)))
          (available-height (- (contact-height self) (top-leading self) (bottom-leading self))))
      (let* ((each (compute-children-layout/each available-width width-reader (composite-children self))))
        (let ((x 0))
          (mapcar (lambda (child)
                    (let ()
                      (let ((w (if (contact-constraint child 'expand-p)
                                   each 
                                 (funcall width-reader child))))
                        (prog1 (list x 0 (max 1 w) (max 1 available-height))
                          (incf x w)))))
                  (composite-children self)))))))


;;;; ------------------------------------------------------------------------------------------

(defmethod find-contact ((contact contact) name)
  (cond ((eql (contact-name contact) name)
         contact)
        (t
         nil)))

(defmethod find-contact ((contact composite) name)
  (cond ((eql (contact-name contact) name)
         contact)
        (t
         (some (lambda (x) (find-contact x name)) (composite-children contact)))))

(defmethod clear-region ((self glue:canvas) region)
  (with-slots (xo yo) self
    (gu:map-region-rectangles (lambda (x1 y1 x2 y2)
                                (xlib:clear-area self
                                                 :x (+ x1 xo)
                                                 :y (+ y1 yo)
                                                 :width (- x2 x1 -1)
                                                 :height (- y2 y1 -1))
                                )
                              region)))

(defcontact menu-bar (hbox)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;; ---------------------------------------------------------------------------
;;;;  Simple Geometry Management Mixin
;;;;

(defclass simple-geometry-management-mixin ()
  ())
           
(defmethod clue:resize :after ((self simple-geometry-management-mixin) width height border-width)
  (declare (ignore border-width))
  (dolist (k (clue:composite-children self))
    (multiple-value-call #'move-resize k (compute-geometry self k width height))))

(defmethod clue:manage-geometry ((self simple-geometry-management-mixin) 
                                 contact x y width height border-width &key)
  (multiple-value-bind (nx ny nw nh nb)
      (compute-geometry self contact (contact-width self) (contact-height self))
    (values
     (and (or (null x) (= x nx))
          (or (null y) (= y ny))
          (or (null width) (= width nw))
          (or (null height) (= height nh))
          (or (null border-width) (= border-width nb)))
     nx ny nw nh nb)))

;;;;;;;;;;;;;

(defmethod clue:convert ((contact xlib:drawable) value (type (eql 'xlib:font)))
  (typecase value
    (xlib:stringable
     (ignore-errors (xlib:open-font (xlib:drawable-display contact) value)))
    (xlib:font value)
    (otherwise nil)))
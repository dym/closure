;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLUE-GUI2; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Interface for the renderer to input elements
;;;   Created: 1999-05-25 22:27
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
;;
;;  When        Who     What
;; ----------------------------------------------------------------------------
;;  1999-09-19  GB      - implemented OPTION-LIST
;;
;;  1999-08-21  GB      - RO/MAKE-PASSWORD, RO/MAKE-TEXT, MAKE-OPTION-MENU:
;;                        RO/MAKE-TEXT-AREA, RO/MAKE-BUTTON-COMMON
;;                        RO/MAKE-BUTTON, RO/MAKE-RESET-BUTTON, RO/MAKE-SUBMIT-BUTTON:
;;                        new :text-style argument
;;                      - RO/MAKE-TEXT-AREA, RO/MAKE-BUTTON-COMMON, RO/MAKE-BUTTON, 
;;                        RO/MAKE-RESET-BUTTON, RO/MAKE-SUBMIT-BUTTON:
;;                        new :document argument
;;  1999-08-19  GB      - COLLECT-FORM-VALUES: changed due to :%REPLACEMENT 
;;                        attribute change
;;                      - SUBMIT-FORM: takes extra agrument

(in-package :CLUE-GUI2)

(defparameter user::*debug-submit-p* nil
  "Whether to dump the values about to be submit by a <FORM> to the server on the listener.")

;;; Input elements

;; Input elements as such are replaced objects and obey to the robj
;; protocol. Futher moreinput elements should obey to the ro/input
;; protocol:

;; gui:ro/input-reset self
;;   Reset the input element.

;; gui:ro/input-contribution self
;;   Return a list of conses (name . value), which should be submitted
;;   along the <FORM>.

(defclass ro/contact ()
  ;; common class for CLUE input elements
  ((contact  :initarg :contact)
   (width    :initform nil)
   (height   :initform nil)
   (document :initform nil) ))

;;; Buttons

(defclass ro/button (ro/contact gui::ro/input)
  ((value :initarg :value) ))

(defmethod gui:ro/make-button ((self clue-device) &rest options)
  (apply #'ro/make-button-common self options))

(defmethod gui:ro/make-reset-button ((self clue-device) &rest options)
  (let ((q (apply #'ro/make-button-common self options)))
    (add-callback (slot-value q 'contact)
                  :activate
                  (curry #'reset-button-callback q))
    q))

(defmethod gui:ro/make-submit-button ((self clue-device) &rest options)
  (let ((q (apply #'ro/make-button-common self options)))
    (add-callback (slot-value q 'contact)
                  :activate
                  (curry #'submit-button-callback q))
    q))

(defmethod gui:ro/input-contribution ((self ro/button))
  ;; a button does not contribute anything
  nil)

(defmethod gui:ro/input-reset ((self ro/button))
  ;; nothing to do
  nil)

;;; Single Line Text Edits

(defclass ro/text (ro/contact gui::ro/input)
  ((size :initform 20 :initarg :size)))

(defmethod renderer:ro/intrinsic-size ((self ro/text))
  ;; -> width ascent descent
  (with-slots (contact size) self
    (multiple-value-bind (w h bw) (clue:preferred-size contact)
      (declare (ignore w bw))
      (values (* (glue:text-width contact (glue:sle-font contact) "m")
                 size)
              h 0))))

(defmethod gui::ro/make-text-common ((self clue-device)
                                    &key (initial-value "")
                                         (size nil) 
                                         (max-length nil) 
                                         (disabled-p nil)
                                         (read-only-p nil)
                                         password-p
                                         name
                                         text-style
                                         pt)
  (make-instance 'ro/text
    :initial-value initial-value
    :name          name
    :disabled-p    disabled-p
    :read-only-p   read-only-p
    :size          (or size 20)
    :contact (clue:make-contact 
              'sle
              :font text-style
              :string initial-value
              :password-p password-p
              :read-only-p read-only-p
              :padding-height 2
              :padding-width 2
              :shadow-width 2
              :shadow-style :inset
              :sensitive (if disabled-p :off :on)
              :x -100 :y -100
              :width 1 :height 1
              :parent (slot-value self 'htview))))

(defmethod gui:ro/input-reset ((self ro/text))
  (with-slots (contact (initial-value gui::initial-value)) self
    (when contact
      (setf (sle-string contact) initial-value))))

(defmethod gui:ro/input-contribution ((self ro/text))
  (with-slots (contact (name gui::name) (initial-value gui::initial-value)) self
    (cond ((null name)
           (warn "Text input element without a name."))
          (t
           (if contact
               (list (cons name (glue:sle-string contact)))
             (cons name initial-value))))))

;;; toggle buttons

(defclass ro/toggle-button (ro/contact)
  ((initial-state :initarg :initial-state :reader ro/toggle-button-initial-state)))

(defclass ro/check-box (ro/toggle-button gui::ro/input)
  ())

(defclass ro/radio-box (ro/toggle-button gui::ro/input)
  ())

(defclass ro/hidden (gui::ro/input)
  ())

(defmethod renderer:ro/intrinsic-size ((self ro/contact))
  ;; -> width ascent descent
  (with-slots (contact) self
    (multiple-value-bind (w h bw) (clue:preferred-size contact)
      (declare (ignore bw))
      (values w h 0))))

(defmethod renderer:ro/size ((self ro/contact))
  ;; -> width ascent descent
  (with-slots (width height) self
    (assert (not (null width)))
    (assert (not (null height)))
    (values width height 0)))

(defmethod renderer:ro/resize ((self ro/contact) new-width new-height)
  ;; ->
  (with-slots (width height) self
    (setf new-width  (or new-width (nth-value 0 (renderer:ro/intrinsic-size self)))
          new-height (or new-height (nth-value 1 (renderer:ro/intrinsic-size self))))
    (setf width (round new-width)
          height (round new-height))))

(defmethod renderer:x11-draw-robj (drawable gcontext (self ro/contact)
                                   box x y)
  (declare (ignore box gcontext drawable))
  (with-slots (contact width height) self
    (when contact
      (move-resize contact (round x) (round (- y height)) (round width) (round height))) ))


(defmethod gui:ro/make-check-box ((self clue-device)
                                  &key name initial-value checked-p
                                       disabled-p read-only-p size)
  (let ((contact (clue:make-contact 'radio-button
                                    :background *default-background*
                                    :x -100 :y -100 :width 1 :height 1
                                    :indicator-size (or size 16)
                                    :sensitive (if (or disabled-p read-only-p) :off :on)
                                    :toggle-state checked-p
                                    :label-string ""
                                    :parent (slot-value self 'htview)
                                    :indicator-type :n-of-many)))
    (make-instance 'ro/check-box
      :initial-value initial-value
      :initial-state checked-p
      :name name
      :disabled-p disabled-p
      :read-only-p read-only-p
      :contact contact)))

(defmethod gui:ro/make-radio-box ((self clue-device)
                                  &key name initial-value 
                                       checked-p disabled-p read-only-p
                                       size
                                       pt)
  (declare (ignore pt))
  (let ((contact (clue:make-contact 'radio-button
                                    :toggle-state checked-p
                                    :background *default-background*
                                    :x -100 :y -100 :width 1 :height 1
                                    :indicator-size (or size 16)
                                    :label-string ""
                                    :parent (slot-value self 'htview)
                                    :indicator-type :1-of-many))
        (self nil))
    (clue:add-callback 
     contact :value-changed
     (lambda (new-value)
       (and new-value
            (let ((form (gui::find-form-element pt)))
              (if form
                  (gui::map-input-elements 
                   (lambda (x)
                     (let ((obj (gui::pt-replacement x)))
                       (when (and (typep obj 'ro/radio-box)
                                  (not (eql obj self))
                                  (rod-equal (slot-value obj 'gui::name) name))
                         (setf (glue:radio-button-toggle-state 
                                (slot-value obj 'contact))
                           nil))))
                   form))))))
    (setq self (make-instance 'ro/radio-box
      :initial-value initial-value
      :name name
      :disabled-p disabled-p
      :read-only-p read-only-p
      :initial-state checked-p
      :contact contact))))

(defmethod (setf ro/toggle-button-state) (new-value (self ro/radio-box))
  (with-slots (contact) self
    (setf (radio-button-toggle-state contact) new-value)))

(defmethod (setf ro/toggle-button-state) (new-value (self ro/check-box))
  (with-slots (contact) self
    (setf (radio-button-toggle-state contact) new-value)))


(defmethod gui::ro/make-hidden ((self clue-device)
                                &key name value disabled-p)
  (make-instance 'ro/hidden
        :initial-value value
        :name name
        :disabled-p disabled-p
        :read-only-p nil))

(defmethod deconstruct-robj ((self ro/contact))
  (with-slots (contact) self
    (when contact
      ;; das alte problem, dass widget sich nicht selbst destroy'en
      ;; sollten, bevor alle events aus der queue sind, die dieses
      ;; widget betreffen; Arg!!
      (clue:destroy contact)
      (setf contact nil) )))

(defmethod gui::ro/make-password ((self clue-device) &rest options)
  (apply #'gui::ro/make-text-common self :password-p t options))

(defmethod gui::ro/make-text ((self clue-device) &rest options)
  (apply #'gui::ro/make-text-common self :password-p nil options))

;;;; ---------------------------------------------------------------------------
;;;; Option menus
;;;;

;; a basic option menu independent of actual presentation as either a
;; list or a pop-up menu
(defclass ro/basic-option-menu (ro/contact gui:ro/input)
  ((disabled-p         :initarg :disabled-p)
   (name               :initarg :name)
   (device             :initarg :device)
   (options            :initarg :options)
   (size               :initarg :size)
   (multiple-p         :initarg :multiple-p)
   (selected-options   :initarg :selected-options)
   (font               :initarg :font) ))

;; an option menu presented as pop-up menu
(defclass ro/option-menu (ro/basic-option-menu)
  ((top-cascade-button) ))

;; an option menu presented as a list box
(defclass ro/option-list (ro/basic-option-menu)
  ())

;;; ro/basic-option-menu implementation

(defun initial-selected-options (options multiple-p)
  "Compute the set of initially selected options. 
   Returns a list of GUI:OPTION-MENU-OPTION objects"
  (let ((res nil)
        (first-option nil))
    ;; first collect everything
    (labels ((walk (x)
               (etypecase x
                 (GUI::OPTION-MENU-OPTION
                  (unless first-option
                    (setf first-option x))
                  (when (gui::option-menu-option-selected-p x)
                    (push x res)))
                 (GUI::OPTION-MENU-OPTION-GROUP
                  (mapc #'walk (gui::option-menu-option-group-children x))))))
      (mapc #'walk options))
    ;; now check for saneness
    (cond (multiple-p
           res)
          (t
           (cond ((= (length res) 1) res)
                 (t
                  ;; someone on www-html mentioned, that this is quite true.
                  (warn "Exactly one option should be preselected.")
                  (cond (res
                         (list (car res)))
                        (first-option
                         (list first-option))
                        (t
                         (warn "Option-menu even does not have any options.")
                         nil))))))))

(defmethod gui:ro/input-contribution ((self ro/basic-option-menu))
  (with-slots ((name gui::name) selected-options) self
    (cond ((null name)
           (warn "SELECT element has no name.")
           nil)
          (t
           (let ((bag nil))
             (dolist (k selected-options (nreverse bag))
               (let ((value (gui::option-menu-option-value k)))
                 (cond ((null value)
                        (warn "Option menu item '~A' has no value." 
                              (gui::option-menu-option-content k)))
                       (t
                        (push (cons name value) bag))))))))))

(defmethod gui:ro/input-reset ((self ro/basic-option-menu))
  (with-slots (selected-options multiple-p options) self
    ;; xxx alle Unterwidgets muessen ge-updated werden
    (setf selected-options (initial-selected-options options multiple-p))
    (sync-option-menu self)))

(defmethod option-menu/all-content ((self ro/basic-option-menu))
  "Return a list of all the labels of all option menu's entries. 
   This is used to calculate the width of the option menu pop up button."
  (with-slots (options multiple-p) self
    (cond (multiple-p
           (list *multiple-selection-title*))
          (t
           (let ((res nil))
             (labels ((walk (x)
                        (etypecase x
                          (GUI::OPTION-MENU-OPTION
                           (push (gui::option-menu-option-label x) res))
                          (GUI::OPTION-MENU-OPTION-GROUP
                           (mapc #'walk (gui::option-menu-option-group-children x))))))
               (mapc #'walk options))
             res)))))

;;; option menu (as pop-up menu)

(defmethod sync-option-menu ((self ro/option-menu))
  "Take care that GLUE's idea of the set of selected options and the option menu's idea agree."
  (with-slots (multiple-p selected-options contact) self
    (cond (multiple-p
           nil)
          (t
           (setf (label-string contact)
             (gui::option-menu-option-label (car selected-options)) )))))

(defmethod r2:ro/intrinsic-size ((self ro/option-menu))
  ;; -> width, ascent, descent
  ;; why isn't that derived from the contact's intrinsic size?
  (with-slots (options font contact) self
    (let ((fd font))
      (let ((width (+ (reduce #'max (mapcar (curry #'glue:text-width contact fd) 
                                            (option-menu/all-content self))
                              :initial-value 0)
                      16))              ;???
            (ascent (+ (glue:font-ascent contact fd) 8))
            (descent (+ (glue:font-descent contact fd) 8)))
        (values width ascent descent)))))

(defmethod r2:ro/intrinsic-size ((self ro/option-menu))
  ;; -> width, ascent, descent
  ;; We use a hack here.

  ;; We first setup a null label string and compute the preferred
  ;; size, then we add the maximum string length.
  (with-slots (options font contact) self
    (setf (label-string contact) "")
    (multiple-value-bind (pw ph) (clue:preferred-size contact)
      ;; resync
      (sync-option-menu self)
      (let ((max-width
             (reduce #'max (mapcar (curry #'glue:text-width contact font) 
                                            (option-menu/all-content self))
                     :initial-value 0)))
        (values (+ max-width pw)
                ph
                0)))))

(defun option-menu-activate-cb (option-menu item)
  "Callback to be installed on the pop-up menu's items."
  (with-slots (multiple-p selected-options) option-menu
    (cond (multiple-p
           )
          (t
           (setf selected-options (list item))
           (sync-option-menu option-menu)))))

(defmethod finish-construction (device (self ro/option-menu))
  (with-slots (contact font options disabled-p) self
    (multiple-value-bind (popup-shell pane) (make-menu-shell (the-display))
      (setf contact (clue:make-contact 
                     'glue::popup-button-drop
                     :sensitive (if disabled-p :off :on)
                     :popee popup-shell
                     :label-string "foo"
                     :x -100 :y -100 :width 1 :height 1
                     :parent  (slot-value device 'htview)
                     :shadow-width 2
                     :shadow-style :outset
                     :background *default-background*
                     :font font))
      ;; construct the possibly hierarchical menu
      (labels ((aux (pane options)
                 (dolist (k options)
                   (cond 
                    ((gui:option-menu-option-p k)
                     (let ((label (gui:option-menu-option-label k)))
                       (let ((w (clue:make-contact 'menu-button
                                                   :font font
                                                   :parent pane
                                                   :label-string label
                                                   :background :parent-relative
                                                   :shadow-width 2)))
                         (add-callback w :activate (curry #'option-menu-activate-cb self k)) )))
                    ((gui:option-menu-option-group-p k)
                     (let ((label (gui:option-menu-option-group-label k))
                           (children (gui:option-menu-option-group-children k)))
                       (multiple-value-bind (shell2 pane2) (make-menu-shell (the-display))
                         (clue:make-contact 'popup-menu-button
                                            :label-string label
                                            :parent pane
                                            :popee shell2
                                            :font font)
                         (aux pane2 children))))))))
        (aux pane options)))))

;;;; option-list

(defmethod sync-option-menu ((self ro/option-list))
  (with-slots (selected-options contact) self
    (setf (glue::value contact) selected-options) ))

(defmethod r2:ro/intrinsic-size ((self ro/option-list))
  ;; -> width ascent descent
  ;; copied from r2:ro/intrinsic-size <ro/contact>
  (with-slots (contact) self
    (multiple-value-bind (w h bw) (clue:preferred-size contact)
      (declare (ignore bw))
      (values w h 0))))

(defmethod finish-construction (device (self ro/option-list))
  (with-slots (contact selected-options multiple-p font size options) self
    (setf contact
      (clue:make-contact 
       'glue:list-box
       :x -1000 
       :y -1000
       :parent (slot-value device 'htview)
       :selection-mode (if multiple-p :multiple :single)
       :shadow-width 2
       :shadow-style :inset
       :font font
       :nvisible (or size :auto)
       :width 1 :height 1))
    (setf (glue:items contact) options)
    (clue:add-callback contact :value-changed
                       (lambda (new-selection)
                         (setf selected-options new-selection)))))

;; item protocol on options

(defmethod glue:item-size ((self gui::option-menu-option) medium)
  (values (glue::new-text-width medium (gui::option-menu-option-label self))
          (glue::text-ascent medium)
          (glue::text-descent medium)))

(defmethod glue:item-paint ((self gui::option-menu-option) medium x y)
  (glue:draw-text* medium (gui::option-menu-option-label self) x y))

;;;; constructor

(defun gui:make-option-menu (device 
                             &key options name multiple-p disabled-p size 
                                  text-style pt)
  "Create an option menu on the device `device', returns an R2:ROBJ. 
   The created object implements the ROBJ protocol as well as the RO/INPUT
   protocol.
   Arguments:
   `options' is a list of GUI:OPTION-MENU-OPTION or GUI:OPTION-MENU-OPTION-GROUP objects
   `size' is an integer or NIL and denotes the number of visible item.
   `multiple-p' boolean; whether it is possible to select more than one option.
   `disabled-p' boolean; whether this controll should be disabled (not implemented).
   `text-style' R2:TEXT-STYLE object, specifies the text style to use to render the option labels.
   `name', a ROD, the controls name for submission."
  ;;
  (declare (ignore pt))
  (let ((self
         (make-instance
             ;; when either MULTIPLE is present or the user specified
             ;; SIZE, we use an option-list, otherwise an option-menu
             (if (or multiple-p size)
                 'ro/option-list
                 'ro/option-menu)
           ;; fill in the common slots
           :disabled-p disabled-p
           :multiple-p multiple-p
           :size size
           :name name
           :device device               ;what for?
           :font text-style
           :options options
           :selected-options (initial-selected-options options multiple-p) )))
    ;; then let the class decide how to finish this
    (finish-construction device self)
    ;; finally sync
    (sync-option-menu self)
    ;; all done
    self))

;;;;;;;;;;;;;;;;

(defmethod renderer:ro/intrinsic-size ((self ro/hidden))
  ;; why is that ever called?! grr.
  (values 0 0 0))

;; form protocol.

;; A disabled input element does not contribute any values
(defmethod gui:ro/input-contribution :around ((self gui:ro/input))
  (with-slots ((disabled-p gui::disabled-p)) self
    (and (not disabled-p)
         (call-next-method))))

(defmethod gui:ro/input-contribution ((self ro/toggle-button))
  (with-slots ((name gui::name) (initial-value gui::initial-value) contact) 
      self
    (setf initial-value (or initial-value (rod "on")))
    (and name initial-value
         (glue:radio-button-toggle-state contact)
         (list (cons name initial-value)))))
      
(defmethod gui:ro/input-contribution ((self ro/hidden))
  (with-slots ((name gui::name) (initial-value gui::initial-value)) self
    (and name initial-value
         (list (cons name initial-value)))))

(defun reset-button-callback (self)
  (with-slots ((pt gui::pt)) self
    (let ((form (gui::find-form-element pt)))
      (if form
          (gui::reset-form form)
        (warn "Reset button has no <FORM> element."))) ))

(defun submit-button-callback (self)
  (with-slots ((document gui::document) (pt gui::pt)
               (name gui::name) value) 
      self
    (let ((form (gui::find-form-element pt))
          (vals (if name (list (cons name value)) nil)))
      (if form
          (submit-form document form vals)
        (warn "Submit button has no <FORM> element.")))))

;;;;;;

(defun submit-form (document form-elm &optional (additional-values nil))
  "Submit the form given by `form-elm'."
  (let ((method  (r2::pt-attr/enum form-elm :method :get '(:get :post)))
        (enctype (r2::pt-attr/latin1 form-elm :enctype "application/x-www-form-urlencoded"))
        (values  (append (collect-form-values form-elm) additional-values)))
    (when user::*debug-submit-p* 
      (format T "~&;; Submitting form")
      (format T "~&;;  enctype = ~S." enctype)
      (format T "~&;;  action  = ~S." (r2::pt-attr/latin1 form-elm :action))
      (format T "~&;;  values: [")
      (dolist (k values)
        (format T "~&;;    ~S = ~S."
                (if (glisp::rodp (car k)) (rod-string (car k)) (car k))
                (if (glisp::rodp (cdr k)) (rod-string (cdr k)) (cdr k))))
      (format T "~&;;  ]"))
    (cond ((and (eq method :get) 
                (string-equal enctype "application/x-www-form-urlencoded"))
           (submit-form-by-get document form-elm values))
          ;; Bevor wir POST anbieten muessen wir noch einige andere
          ;; Sachen etwas aufräumen.
          ((and (eq method :post) (string-equal enctype "application/x-www-form-urlencoded"))
           (submit-form-by-post document form-elm values))
          (t
           (warn "<FORM method=~A enctype=~S ...> is not supported."
                 method enctype)))))

(defun collect-form-values (form-elm)
  (let ((res nil))
    (gui::map-input-elements 
     (lambda (x)
       (let ((obj (car (r2::pt-%replacement x))))
         (when (and (eq (sgml:gi x) :input)
                    (and (not (r2::pt-%replacement x))))
           (print x))
         (when (typep obj 'gui::ro/input)
           (setf res (append res (gui::ro/input-contribution obj))))))
     form-elm)
    res))

(defmethod gui:ro/input-reset ((self ro/toggle-button))
  (with-slots (initial-state) self
    (setf (ro/toggle-button-state self) initial-state)))
  
(defclass ro/text-area (ro/contact gui::ro/input)
  ())


(defmethod gui:ro/make-text-area ((device clue-device)
                                  &key pt name initial-value cols rows
                                       disabled-p read-only-p
                                       text-style document)
  (let ((res
         (make-instance 'ro/text-area
           :initial-value initial-value
           :name          name
           :disabled-p    disabled-p
           :read-only-p   read-only-p
           :pt            pt
           :document      document
           :contact       (clue:make-contact 
                           'glue:text-area
                           :font         text-style
                           :parent       (slot-value device 'htview)
                           :sensitive    (if disabled-p :off :on)
                           :shadow-style :inset
                           :shadow-width 2
                           :background   :white
                           :x            -10000 
                           :y            -10000
                           :width        1 
                           :height       1
                           :ncols        cols
                           :nrows        rows))))
    (gui:ro/input-reset res)            ;take care of initial value.
    res))

(defmethod gui:ro/input-contribution ((self ro/text-area))
  (with-slots (contact (name gui::name) (initial-value gui::initial-value)) self
    (cond ((null name)
           (warn "Text-area input element without a name."))
          (t
           (if contact
               (list (cons name (glue:text-area-string contact)))
             (cons name initial-value))))))

(defmethod gui:ro/input-reset ((self ro/text-area))
  (with-slots (contact (initial-value gui::initial-value)) self
    (when contact
      (setf (glue:text-area-string contact) initial-value))))


(defun ro/make-button-common (self
                              &key label name size disabled-p read-only-p pt
                                   text-style document)
  (make-instance 'ro/button
    :initial-value nil
    :value label
    :name name
    :disabled-p disabled-p
    :read-only-p read-only-p
    :pt pt
    :document document
    :contact (clue:make-contact 
              'button
              :font text-style
              :sensitive (if disabled-p :off :on)
              :background *default-background*
              :x -100 :y -100
              :width 1 :height 1
              :label-string label
              :parent (slot-value self 'htview))))

(defmethod gui:ro/input-reset ((self ro/hidden)) 
  ;; nothing to do
  nil)

;;;;

(defmethod make-device-for-frame ((self q/frame))
  ;; xxx was macht das hier?
  (with-slots (htv) self
    (make-instance 'clue-device
      :htview htv
      :clonee (gui:make-device-for-display (clue:contact-display htv)))))


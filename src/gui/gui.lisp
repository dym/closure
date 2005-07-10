;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: GUI; -*-
;;; --------------------------------------------------------------------------
;;;     Title: Toolkit independent GUI stuff (albeit X specific)
;;;   Created: Sun Jan 17 06:45:18 1999
;;;    Author: Gilbert Baumann <gilbert@base-engineering.com>
;;;   License: MIT style (see below)
;;; --------------------------------------------------------------------------
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

(defparameter *home-page* "http://common-lisp.net/project/closure/")

(defvar *user-wants-images-p* t)

(defvar *closure-dpi* 96)

(defvar *zoom-factor* 1.0)

(defparameter *debug-submit-p* nil
  "Whether to dump the values about to be submit by a <FORM> to the server on the listener.")

;; experimental code that is not activated by default
(defvar *tex-mode-p* nil)
(defvar *hyphenate-p* nil)



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
  (declare (ignore url width height))
  (with-slots (clonee) self
    (apply #'gui::make-image-replacement clonee doc args)))

;;; ---- Some CLIM fake ---------------------------------------------------------------------------------

(defvar +black+ (xlib:make-color :red 0 :green 0 :blue 0))
(defvar +white+ (xlib:make-color :red 1 :green 1 :blue 1))

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
      (declare (ignore mime-type))
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

(defparameter cl-user::*profile-closure-p* nil)
(defparameter cl-user::*closure-dpi* 88)           ;this doesn't belong here

(defclass prim-ht-view ()
  ((display-list        :initform nil)
   (active-pt           :initform nil)
   (active-link         :initform nil) ))

;;; --------------------------------------------------------------------------------

(defvar cl-user::*html-dtd* nil)

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
       (unless (or (>= width 2048) (>= height 2048)) ;### CLX bug
	 (xlib:put-image pixmap gc im 
			 :src-x 0 :src-y 0
			 :x 0 :y 0
			 :width width :height height))
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
  (unless cl-user::*html-dtd*
    (cond 
     ;; xxx hack
     ((probe-file (compile-file-pathname "html-dtd.lisp"))
      (format T "~&;; Loading DTD ")
      (setf cl-user::*html-dtd* (sgml::undump-dtd "html-dtd"))
      (princ " done.")
      (finish-output))
     (t
      (format T "~&;; Parsing DTD ")
      (sgml:slurp-catalog (url:parse-url "file://closure/resources/dtd/catalog"))
      (setf cl-user::*html-dtd* (sgml:parse-dtd '(:public "-//W3C//DTD HTML 4.0 Frameset//EN")))
      (princ " done.")
      (finish-output))))

  (unless r2::*default-style-sheet* 
    (format T "~&;; Parsing default style sheet ...")
    (setf r2::*default-style-sheet* 
          (css::parse-style-sheet-from-url (url:parse-url "file://closure/resources/css/default.css")
                                           :name "Closure Default Style Sheet")))
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





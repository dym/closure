;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLUE-GUI2; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Some basic GUI infrastructure
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

;;; Changes

;;  When        Who     What
;; ---------------------------------------------------------------------------
;;  1999-09-19  GB      - added RENDERER:TEXT-STYLE support for GLUE-DRAW 
;;                        module
;;  1999-09-12  GB      - twix from NEW-[HV]BOX to [HV]BOX
;;

(in-package :CLUE-GUI2)

(defun get-display-to-open (&optional (display-string NIL))
  ;; finds the X display to connect to.
  ;; returns: host ; display ; screen
  (values-list
   (multiple-value-bind (res cond)
       (ignore-errors 
        (multiple-value-list (parse-display (or display-string 
                                                (glisp:getenv "DISPLAY") 
                                                ":0.0"))))
     (or res
         (progn
           (warn "Error while parsing display string: ~S: ~A; Trying with \":0.0\"."
                 display-string
                 cond)
           (list NIL 0 0))))))

(defun parse-display (string)
  ;; Parses an X display string.
  ;; Syntax:
  ;;  [host]:display[.screen]
  ;; -> host ; display ; screen
  ;; where host is either a string or NIL (meaning local transport mechanism).
  (let ((i (position #\: string :from-end t))
        host display 
        (screen "0"))
    (cond (i
           (setf host (subseq string 0 i)
                 display (subseq string (+ i 1)))
           ;; is there a colon in `display'?
           (let ((j (position #\. display)))
             (when j
               (psetf display (subseq display 0 j)
                      screen  (subseq display (+ j 1)))))
           (values 
            (if (or (string= host "") (string-equal host "localhost"))
                nil
              host)
            (parse-integer display)
            (parse-integer screen)))
          (t
           (error "A display string should feature a colon." string)))))

(defun the-display ()
  (or *dpy*
      (multiple-value-bind (host display screen) (get-display-to-open)
        
        (setf *dpy* (open-contact-display "closure"
                                          :class "Closure"
                                          :host #+CMU (or host "") #-CMU host
                                          :display display
                                          :default-screen screen))
        #+ALLEGRO
        (progn
          (setf user::v (excl:weak-vector 1))
          (setf (aref user::v 0) *dpy*))
        *dpy*
        )))

;;;;  Queue data type

;; A simple MT-safe queue data type.

(defstruct (queue (:print-function print-queue))
  (lock (mp/make-lock :name "A queue lock"))
  head
  tail)

(defun print-queue (self sink depth)
  depth
  (format sink "#<~S>" (type-of self)))

(defvar *todo-queue* (make-queue)
  "Queue of forms to be evaluated within the main GUI process.")

(defstruct todo-entry
  document
  fun
  args)

(defun queue-get (queue)
  "Fetch the next item form the queue, returns NIL, if queue is empty."
  (mp/with-lock ((queue-lock queue))
    (prog1 
        (pop (queue-head queue))
      (when (null (queue-head queue))
        (setf (queue-tail queue) nil)))))

(defun queue-put (queue x)
  "Put an item into the queue."
  (mp/with-lock ((queue-lock queue))
    (cond ((null (queue-tail queue))
           (setf (queue-head queue) (setf (queue-tail queue) (cons x nil))))
          (t
           (setf (queue-tail queue)
             (setf (cdr (queue-tail queue)) (cons x nil)))))))

(defun queue-empty-p (queue)
  "Is the queue empty?"
  (mp/with-lock ((queue-lock queue))
    (null (queue-head queue))))

(defun queue-listen (queue)
  "Is there something in the queue?"
  (not (queue-empty-p queue)))

(defun fast-queue-listen (queue)
  "Fast variant of QUEUE-LISTEN, does not lock the queue and is
   meant for MP:PROCESS-WAIT only."
  (not (null (queue-head queue))))


(defun make-simple-button (parent label action
                           &rest options)
  (let ((x (apply #'make-contact 'button :parent parent
                         :label-string label
                         options)))
    (add-callback x :activate action)
    x))

(defun make-simple-menu-button (parent label action
                                &rest options)
  (let ((x (apply #'make-contact 'menu-button :parent parent
                  :label-string label
                  options)))
    (add-callback x :activate action)
    x))

(defun make-simple-menu-toggle-button (parent label action
                                       &rest options)
  (let ((x (apply #'make-contact 'glue::menu-toggle-button :parent parent
                  :label-string label
                  options)))
    (add-callback x :activate action)))

;;; ---- Menubar --------------------------------------------------------------

(defun make-menu-shell (parent)
  ;; -> shell pane
  ;; the shell is for the popee slot of popup-buttons and the pane is
  ;; a vbox to stuff the menu items into.
  ;; the parent should be the display this menu should live on.
  (let ((popup-shell (make-contact 'override-shell :parent parent :state :withdrawn
                                   :save-under :on
                                   :border-width 0)))
    (let ((vb (make-contact 'glue:vbox :parent popup-shell
                            :border-width 0
                            :shadow-width 2
                            :shadow-style :outset ;;dotted ;;ridge;groove
                            :margin-height 0
                            :margin-width 0
                            :padding-height 2
                            :padding-width 2
                            :background *default-background*
                            )))
      (values popup-shell vb))))
  
(defun make-simple-menu (parent strings)
  (multiple-value-bind (popup-shell vb) (make-menu-shell parent)
    (dolist (k strings)
      (make-contact 'menu-button :parent vb :label-string k
                    :background :parent-relative
                    :shadow-width 2))
    (values popup-shell vb)))

(defun make-simple-frame (parent &key (margin 0)
                                      (padding 0)
                                      (shadow-style :inset)
                                      (shadow-width 0))
  (make-contact 'glue:vbox
                :parent parent
                :margin-width margin :margin-height margin
                :padding-width padding :padding-height padding
                :shadow-style shadow-style :shadow-width shadow-width))

;;; ---------------------------------------------------------------------------

;; Make renderer:text-style available to GLUE.


(defmethod glue::text-width ((contact clue:contact) (text-style r2::text-style)
                             sequence
                             &key (start 0) (end (length sequence))
                                  (white-space :normal))
  (when (stringp sequence)
    (setf sequence (map 'rod #'char-code sequence)))
  (let ((res 0))
    (r2::iterate-over-runes
     (lambda (rune index x cw)
       (declare (ignore rune index x))
       (incf res cw))
     sequence start end text-style white-space)
    res))

(defmethod glue::font-ascent ((contact clue:contact) (text-style r2::text-style))
  (r2::font-desc-ascent (r2::text-style-font text-style)))

(defmethod glue::font-descent ((contact clue:contact) (text-style r2::text-style))
  (r2::font-desc-descent (r2::text-style-font text-style)))

(defmethod glue::draw-glyphs ((contact clue:contact) (text-style r2::text-style)
                              x y sequence 
                              &key (start 0) (end (length sequence))
                                   foreground background image-p
                                   (white-space :normal))
  foreground background image-p
  (when (stringp sequence)
    (setf sequence (map 'rod #'char-code sequence)))
  (when image-p
    (cond ((not (null background))
           (clue:using-gcontext (gc :drawable contact
                                    :foreground (clue:convert contact background 'xlib:pixel))
             (xlib:draw-rectangle contact gc
                                  x (- y (font-ascent contact text-style))
                                  (text-width contact text-style (subseq sequence start end)) ;aua
                                  (+ (font-ascent contact text-style)
                                     (font-descent contact text-style))
                                  t)))))
  ;; ob USING-GCONTEXT hier wohl gut geht?!
  (clue:using-gcontext (gc :drawable contact
                           :foreground (and foreground 
                                            (clue:convert contact foreground 'xlib:pixel)))
    (r2::x11-draw-runes contact gc x y sequence start end text-style white-space)))
                              



;; fake alert:

(defmethod clue:convert ((contact clue:contact) (value r2::text-style) (type (eql 'glue::afont)))
  value)

(defmethod clue:convert ((contact xlib:drawable) (value string) (type (eql 'glue::afont)))
  (clue:convert contact value 'xlib:font))

(defmethod clue:convert ((contact clue:contact) (value string) (type (eql 'rod)))
  (rod value))


;;; ---------------------------------------------------------------------------
;;;  The Event loop

;; Since there are much fewer pitfalls, if we do CLUE stuff only
;; within the main process, there is a queue, where other processes
;; could enqueue forms to be evaluated.

(defun do-todo ()
  (let ((item (queue-get *todo-queue*)))
    (cond (nil
           (print (list (cons (todo-entry-fun item)
                              (todo-entry-args item))
                        :lost)))
          (t
           (apply (todo-entry-fun item)
                  (todo-entry-args item))))))

;; ACL version
;;; XXX redefined below -- see comment.
#+ALLEGRO
(defun event-loop (display)
  ;; The main event loop of our application. Listens to X events and
  ;; for forms queued into *todo-queue*.
  (loop
    ;; flush CLUE state
    (clue:update-state display)
    ;; flush output queue
    (xlib:display-finish-output display)
    (or
     ;; check for events still in the display's input queue
     (xlib:event-listen display 0)
     ;; or wait for input from the X server or the queue
     (mp:wait-for-input-available 
      (list (xlib::display-input-stream display))
      :wait-function (lambda (stream)
                       (or #+:ALLEGRO-V4.3
                           (stream:stream-listen stream)
                           #+:ALLEGRO-V5.0
		           (excl:stream-listen stream)
                           (fast-queue-listen *todo-queue*)))
      :whostate "Waiting for X11 event") )
    (if (queue-listen *todo-queue*)
        (progn
          (do-todo))
      ;; otherwise, it was an X event.
      (process-next-event display))))

#+CMU
;; CMUCL version, busy waiting -- very bad on multi-user systems.
;;; XXX redefined below -- see comment.
(defun event-loop (display)
  ;; The main event loop of our application. Listens to X events and
  ;; for forms queued into *todo-queue*.
  (loop
    ;; flush CLUE state
    (clue:update-state display)
    ;; flush output queue
    (xlib:display-finish-output display)
    ;; busy loop
    (loop
      (when (or 
             (xlib:event-listen display 0)
             (queue-listen *todo-queue*))
        (return t))
      (mp/process-yield))
    (if (queue-listen *todo-queue*)
        (progn
          (do-todo))
      ;; otherwise, it was an X event.
      (process-next-event display))))

#-(OR ALLEGRO CMU)
;;
(defun event-loop (display)
  ;; The main event loop of our application. Listens to X events and
  ;; for forms queued into *todo-queue*.
  (loop
      (process-next-event display)))

(defun gui-post (document fun &rest args)
  (queue-put *todo-queue*
             (make-todo-entry :document document
                              :fun fun
                              :args args)))

;;; Portable Event Loop

;; (Using round trips).

;; The problem is that the event loop has to listen for two input
;; sources: *TODO-QUEUE* and the socket to the X server. Since CMUCL
;; has no MP:WAIT-FOR-INPUT-AVAILABLE and my implemention of
;; EVENT-LOOP does not work reliable under ACL, I use a hack:

;; GUI-POST now issues XLIB:SEND-EVENT sending a harmless nonsens
;; event. This request is then executed by the X server and fed back
;; into our X input stream. CLUE:PROCESS-NEXT-EVENT then reads this
;; event and returns, so that we could check for the queue. This is
;; non-optiminal, since this metho forces a round trip via the X
;; server, which may kind of slow.

(defun event-loop (display)
  (loop
    ;; flush CLUE state
    (clue:update-state display)
    ;; flush output queue
    (xlib:display-finish-output display)
    (clue:process-next-event display)
    (if (queue-listen *todo-queue*)
        (progn '(princ #\Q))
      '(princ #\@))
    (finish-output *standard-output*)
    (while (queue-listen *todo-queue*)
      (do-todo))))

(defun gui-post (document fun &rest args)
  '(princ #\E)
  (finish-output *standard-output*)
  (queue-put *todo-queue* 
             (make-todo-entry :document document
                              :fun fun
                              :args args))
  (when *application-shell*
    '(princ #\!)
    (finish-output *standard-output*)
    (xlib:send-event *application-shell*
                     :gravity-notify #x20000
                     :window *application-shell*
                     :event-window *application-shell*
                     :x 0 :y 0)
    (xlib:display-finish-output (xlib:window-display *application-shell*))
    #+CMU
    (mp:process-yield)) )

;;;; R2 text style

(defmethod glue::medium-text-width ((sink glue::x11-sink) (font r2::text-style) sequence start end)
  (when (stringp sequence)
    (setf sequence (map 'rod #'char-code sequence)))
  (let ((res 0))
    (r2::iterate-over-runes
     (lambda (rune index x cw)
       (declare (ignore rune index x))
       (incf res cw))
     sequence start end font :pre)
    res))

(defmethod glue::medium-draw-text ((sink glue::x11-sink) 
                                   (font r2::text-style) x y sequence start end)
  (when (stringp sequence)
    (setf sequence (map 'rod #'char-code sequence)))
  (with-slots ((drawable glue::drawable)) sink
    (let ((gc (glue::sink-gcontext sink)))
      (glue::sync-gcontext sink)
      (r2::x11-draw-runes drawable gc (floor x) (floor y) sequence start end font :pre))))

(defmethod glue::medium-text-ascent ((sink glue::x11-sink) (font r2::text-style))
  (r2::font-desc-ascent (r2::text-style-font font)))

(defmethod glue::medium-text-descent ((sink glue::x11-sink) (font r2::text-style))
  (r2::font-desc-descent (r2::text-style-font font)))


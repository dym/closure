;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: GLUE; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: GLUE Single-Line-Edit Contact
;;;   Created: 2000-11-28
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: LGPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1999, 2000 by Gilbert Baumann

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the 
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, 
;;; Boston, MA  02111-1307  USA.

;; - added button-1 and button-2 events --GB 2000-11-28
;; - added :FOCUS-IN and :FOCUS-OUT events

(in-package :GLUE)

;;;; ---------------------------------------------------------------------------
;;;;  Single Line Edit (sle)
;;;;

(defcontact sle (3d)
  ((font        :reader sle-font)
   (string      :reader sle-string)
   (point       :initform 0)
   (offset      :initform 0)
   (password-p  :initform nil)
   (read-only-p :initform nil)
   (foreground)
   (mark :initform nil)
   (region-active-p :initform nil)
   (drag-mode :initform nil)
   
   (prefix      :initform nil
                ;; The currently typed prefix.
                )
   )
  ;;Callbacks:
  ;; :value-entered -- called, when the user pressed <RETURN>
  (:Resources
   (password-p :type boolean
               :initform nil
               ;; when true, display each character as #\*
               )
   (read-only-p :type boolean
               :initform nil)
   (string     :type rod
               :initform (rod ""))
   (font       :type afont
               :initform *default-font*)
   (foreground :type xlib:pixel
               :initform "black")
   (background :initform *default-text-input-background*)
   ;; some default values
   (padding-height :initform 2)
   (padding-width :initform 2)
   (shadow-width :initform 2)
   (shadow-style :initform :inset) ))

(defevent sle :key-press key-press)
(defevent sle (:button-press :button-1) button-press-1)
(defevent sle (:button-release :button-1) button-release-1)
(defevent sle (:button-press :button-2) button-press-2)
(defevent sle :focus-in focus-in)
(defevent sle :focus-out focus-out)
(defevent sle :motion-notify motion-notify)

;; focus-in/focus-out do not seem to work ;-(
;; Well under the fvwm window manager, they work.
;; Under ctwm they do not work.

(defmethod focus-in ((self sle))
  (redisplay self) )

(defmethod focus-out ((self sle))
  (redisplay self) )

(defmethod sle-ensure-point-visible ((self sle))
  (with-slots (string point offset font) self
    (setf offset (min offset point))
    (cond ((> (text-width self font 
                          (sle-string-presentation self (subseq string offset point))
                          :white-space :pre)
	      (interior-width self))
	   (incf offset)
	   (sle-ensure-point-visible self)))))

(defmethod preferred-size ((self sle) &key width border-width)
  (let ((a (font-ascent self (sle-font self)))
        (d (font-descent self (sle-font self))))
    (values
     (or width 
         (if (zerop (contact-width self))
             100
           (contact-width self)))
     (+ (top-leading self) (bottom-leading self) a d 2)
     border-width) ))

(defmethod sle-string-presentation ((self sle) string)
  (with-slots (password-p) self
    (let ((res nil))
      (do ((i 0 (+ i 1)))
          ((= i (length string)))
        (let ((ch (rune string i)))
          (cond (password-p
                 (push (char-code #\*) res))
                (t
                 ;;xx
                 (push ch res)
                 #+(OR)
                 (cond ((graphic-char-p ch)
                        (write-char ch sink))
                       (t
                        (format sink "~S" ch)))))))
      (coerce (nreverse res) 'rod)) ))

(defmethod clue:display-region ((self sle) region)
  (declare (ignore region))
  (with-slots (string point offset font password-p mark
               foreground background region-active-p) self
    (multiple-value-bind (x0 y0 x1 y1) (interior-rectangle* self)
      x1 y1
      (xlib:clear-area self 
                       :x (max 0 x0)
                       :y (max 0 y0)
                       :width (max 0 (- x1 x0))
                       :height (max 0 (- y1 y0 -1)))
      ;; just a quick hack
      (cond ((clue:sensitive-p self)
             (cond (mark
                    ;; grrr
                    (setf mark (max 0 (min mark (length string))))
                    (let ((p0 (min mark point))
                          (p1 (max mark point))
                          (x x0))
                      (let (s)
                        (setf s (sle-string-presentation self (subseq string offset p0)))
                        (draw-glyphs self font x (+ y0 (font-ascent self font))
                                     s
                                     :foreground foreground
                                     :white-space :pre)
                        (incf x (text-width self font s :white-space :pre))
                        (setf s (sle-string-presentation self (subseq string p0 p1)))
                        (draw-glyphs self font x (+ y0 (font-ascent self font))
                                     s
                                     :foreground (if region-active-p background foreground)
                                     :background (if region-active-p foreground background)
                                     :image-p t
                                     :white-space :pre)
                        (incf x (text-width self font s :white-space :pre))
                        (setf s (sle-string-presentation self (subseq string p1)))
                        (draw-glyphs self font x (+ y0 (font-ascent self font))
                                     s
                                     :foreground foreground
                                     :white-space :pre) )))
                   (t
                    (draw-glyphs self font x0 (+ y0 (font-ascent self font))
                                 (sle-string-presentation self (subseq string offset))       
                                 :foreground foreground
                                 :white-space :pre))))
            (t
             (draw-glyphs self font (+ 1 x0) (+ y0 1 (font-ascent self font))
                         (sle-string-presentation self (subseq string offset))       
                         :foreground (3d-light-color self foreground)
                         :white-space :pre)
             (draw-glyphs self font x0 (+ y0 (font-ascent self font))
                         (sle-string-presentation self (subseq string offset))       
                         :foreground (3d-dark-color self foreground)
                         :white-space :pre)))
      ;; cursor:
      (when (and (clue:sensitive-p self)
                 ;;(clue:owns-focus-p self)
                 )
        (let ((w (text-width self font 
                             (sle-string-presentation self (subseq string offset point))
                             :white-space :pre)))
          (old-draw-rectangle* self
                           (+ x0 w )
                           (+ y0)
                           (+ 1 x0 w)
                           (+ y0 (font-ascent self font) (font-descent self font))
                           :foreground foreground) )) )))

(defmethod sle-check-editable ((self sle))
  (with-slots (read-only-p) self
    (cond (read-only-p
           (xlib:bell (clue:contact-display self))
           nil)
          (t
           t))))

(defmethod sle-set-string-interactive ((self sle) new-value)
  (with-slots (read-only-p string mark point) self
    (when (sle-check-editable self)
      (setf string new-value)
      (when mark (setf mark (min mark (length string))))
      (setf point (min point (length string))))))

(defmethod sle-insert-string ((self sle) str)
  (with-slots (string point) self
    (setq str (rod str))
    (when (sle-check-editable self)
      (setf string
        (concatenate 'rod
          (subseq string 0 point)
          str
          (subseq string point)))
    (incf point (length str)) )))

(defmethod sle-edit-command ((self sle) (cmd (eql :end-of-line)))
  (with-slots (string point) self
    (setf point (length string))))

(defmethod sle-edit-command ((self sle) (cmd (eql :beginning-of-line)))
  (with-slots (string point) self
    (setf point 0)))

(defmethod sle-edit-command ((self sle) (cmd (eql :backward-delete-char)))
  (sle-edit-command self :backward-char)
  (sle-edit-command self :delete-char))

(defmethod sle-edit-command ((self sle) (cmd (eql :backward-char)))
  (with-slots (string point) self
    (setf point (max 0 (1- point)))))

(defmethod sle-edit-command ((self sle) (cmd (eql :forward-char)))
  (with-slots (string point) self
    (setf point (min (length string) (1+ point)))))

(defmethod sle-edit-command ((self sle) (cmd (eql :delete-char)))
  (with-slots (string point) self
    (sle-set-string-interactive self
                                (concatenate 'rod
                                  (subseq string 0 point) 
                                  (subseq string (min (length string) (+ 1 point)))))))

(defmethod sle-edit-command ((self sle) (cmd (eql :clear-line)))
  (with-slots (string point) self
    (when (sle-check-editable self)
      (setf string (rod ""))
      (setf point 0))))

(defmethod sle-edit-command ((self sle) (cmd (eql :kill-line)))
  (with-slots (string point) self
    (sle-set-string-interactive self (subseq string 0 point))))

(defmethod sle-edit-command ((self sle) (cmd (eql :paste)))
  (let ((a (get-selection self :primary)))
    (and a (sle-insert-string self a))))

(defmethod sle-edit-command ((self sle) (cmd (eql :set-mark)))
  (with-slots (region-active-p point mark) self
    (setf mark point
          region-active-p t)))

(defmethod sle-edit-command ((self sle) (cmd (eql :exchange-point-and-mark)))
  (with-slots (region-active-p point mark) self
    (when mark
      (rotatef mark point)
      (setf region-active-p t))))

(defmethod sle-edit-command ((self sle) (cmd (eql :delete-key)))
  (sle-edit-command 
   self
   (case user::*delete-key-mode*
     (:forward       :delete-char)
     (:backward      :backward-delete-char)
     (t
      (format T "~&")
      (format T "~% ===============================================================")
      (format T "~%  I do not know, how your <DELETE> key should operate. ")
      (format T "~%  Consider setting user::*delete-key-mode* in your config file ")
      (format T "~%  (~~/.closure/config.rc). For now it will operate backwards.")
      (format T "~% ================================================================")
      (finish-output)
      (setf user::*delete-key-mode* :backward)
      :backward-delete-char))))

(defmethod sle-edit-command ((self sle) (cmd (eql :enter)))
  (clue:apply-callback self :value-entered (sle-string self)))

(defmethod (setf sle-string) (value (self sle))
  (with-slots (string point mark) self
    (setf value (rod value))
    (setf string value
          point (max 0 (min (length string) point))
          mark nil)
    (when (clue:realized-p self)
      '(sle-ensure-point-visible self)
      (redisplay self))))


(defparameter *sle-command-table*
    '(( ( (#\a :control) ) . :beginning-of-line)
      ( ( (#\b :control) ) . :backward-char)
      ( ( (#\d :control) ) . :delete-char)
      ( ( (#\e :control) ) . :end-of-line)
      ( ( (#\f :control) ) . :forward-char)
      ( ( (#\h :control) ) . :backward-delete-char)
      ( ( (#\k :control) ) . :kill-line)
      ( ( (#\u :control) ) . :clear-line)
      ( ( (#\y :control) ) . :paste)
      ;; 
      ( ( (:return) )      . :enter)
      ( ( (:enter) )       . :enter)
      ( ( (:home) )        . :beginning-of-line)
      ( ( (:left) )        . :backward-char)
      ( ( (:right) )       . :forward-char)
      ( ( (:begin) )       . :beginning-of-line)
      ( ( (:end) )         . :end-of-line)
      ( ( (:backspace) )   . :backward-delete-char)
      ( ( (:clear) )       . :clear-line)
      ( ( (:delete) )      . :delete-key)
      ( ( (:insert) )      . :paste)
      ;; 
      ( ( (#\space :control) ) . :set-mark)
      ;; 
      ( ( (:tab) )         . :complete)
      ( ( (#\i :control) ) . :complete)
      ;;
      ( ( (#\x :control) ) . :prefix)
      ( ( (#\x :control) (#\x :control ) )  .    :exchange-point-and-mark)
      )
  "The command table is a simple mapping from sequences of key presses to command names.")

(defmethod key-press-to-command ((self sle) kp)
  (declare (ignorable sle))
  (with-slots (prefix) self
    (let ((q (append prefix (list kp))))
      (dolist (k *sle-command-table*)
        (let ((keyseq (car k))
              (cmd    (cdr k)))
          (when (and (= (length keyseq)
                        (length q))
                     (every #'key-press-match-p keyseq q))
            (return cmd)))))))

(defmethod key-press ((self sle))
  (with-slots (region-active-p prefix) self
    (with-event (state code)
      (let ((kp (translate-key-press-event (clue:contact-display self) code state)))
        (format T "~&;; prefix = ~S, kp = ~S.~&"
                prefix kp)
        (let ((cmd (key-press-to-command self kp)))
          (unless (member cmd '(:backward-char
                                :forward-char
                                :beginning-of-line
                                :end-of-line))
            (setf region-active-p nil))
          (cond ((eq cmd :prefix)
                 (setf prefix (append prefix (list kp))) )
                (t
                 (cond
                  (cmd
                   (sle-edit-command self cmd)
                   (sle-ensure-point-visible self)
                   (redisplay self))
                  ((and (null prefix) (null (cdr kp)) (integerp (car kp)))
                   (sle-insert-string self (vector (car kp)))
                   (sle-ensure-point-visible self)
                   (redisplay self))
                  (t
                   nil) )
                 (setf prefix nil) )))))))

(defmethod button-press-2 ((self sle))
  (sle-edit-command self :paste)
  (redisplay self))

(defmethod mouse-x-coordinate-to-point ((self sle) mx)
  (with-slots (string offset font) self
    (multiple-value-bind (x0 y0 x1 y1) (interior-rectangle* self)
      (declare (ignorable x0 y0 x1 y1))
      (do ((j offset (+ j 1)))
          ((> j  (length string))
           (length string))
        ;; Will fail with RTL text
        (when (> (+ x0 (text-width self font
                                   (sle-string-presentation self (subseq string offset j))
                                   :white-space :pre))
                 mx)
          (return j))))))

(defmethod button-press-1 ((self sle))
  ;; This is just a quick & dirty hack.
  (with-slots (point mark drag-mode region-active-p) self
    (setf drag-mode :mark)
    (let ((i (with-event (x) (mouse-x-coordinate-to-point self x))))
      (when i
        (setf point i)
        (setf mark i)
        (setf region-active-p nil)
        (redisplay self) ))))

(defmethod button-release-1 ((self sle))
  ;; This is just a quick & dirty hack.
  (with-slots (point drag-mode) self
    (let ((i (with-event (x) (mouse-x-coordinate-to-point self x))))
      (when i
        (setf point i)
        (redisplay self) ))
    (setf drag-mode nil)))

(defmethod motion-notify ((self sle))
  (with-slots (drag-mode) self
    (case drag-mode
      (:mark
       (with-slots (point mark region-active-p) self
         (let ((i (with-event (x) (mouse-x-coordinate-to-point self x))))
           (setf point i)
           (setf region-active-p t)
           (redisplay self)))) ) ))


;;;

#|
(defun pretty-print-key-press (kp &key longp)
  "Pretty prints an the abstract key press 'kp'"
  )
|#
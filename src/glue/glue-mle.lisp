;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: glue-mle; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Simple Emacs-like Editor
;;;   Created: 1999-04-03
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

(in-package :glue-mle)

;; This is an Emacs-like editor; it is thought as a multiple purpose
;; implementation for:

;; - single-line edit widget
;; - multiple-line edit widget
;; - full featured multi-buffer/multi-window emacs
;; - some kind of text-sink widget

;;; TODO

;; - undo
;; - maybe we could make use for dirty bits for lines
;; - maybe we should conser fewer MARK objects?
;; - deal with TABs
;; - kill-ring instead of simple kill-buffer
;; - Somehow the kill-ring must interact with the X selection
;; - mark-ring
;; - maybe we should offer a motif compatible mode.
;; - TABs
;; - weniger consen bei dyn-deco (cursor evlt. extra)
;; - Support für SHY und NBSP
;; - Support für nicht anzeigbare Zeichen
;; - Argumente (sowohl universal-argument und auch einfache strings etc).
;;   (sowas wie rekursives editieren). Obwohl so etwas mit einem
;;   zweiten Thread einfach zu erschlagen waere, will ich erstmal
;;   versuchen ohne auszukommen.
;;   Bei universal argumenten dann so wie in libreadline:
;;   Das bisher gelesene dann davor. z.b. (Arg 7): blabla
;; - ein m-q wäre cool. (Eine häufige Anwendung für MLE wäre ja gerade
;;   Texte zu schreiben).
;; - evlt. etwas Unterstützung für Lisp? (So viel, daß man daran denken
;;   kann dieses MLE als Ersatz-Emacs bei Programentwicklung
;;   zu benutzen).
;; - bring TTY driver back to live.

;; Wir koennen ja erstmal den Funktionsumfang von uEmacs erfassen. (Fuer ein
;; MLE reicht das ja).

(defconstant +c/newline+ 10)
(defconstant +escape+ (code-char 27))
(defconstant +tilde-rod+ (map 'rod #'char-code "~"))

;;; Buffers

;; Basically a buffer is a doubly linked list of lines.

;; Line Numbers

;; To overcome some of the limitations of a line oriented representation,
;; each line also has a line number (the `n' slot). Since renumbering all
;; lines with each editing command would be too expensive, this line number
;; is a rational number. When a new line is inserted between two other
;; lines, the new line's number is the "arithmetrisches Mittel" [ (a+b)/2 ]
;; of the two lines. Hence line numbers can become true rational numbers.

;; The benefit of these line numbers is that we could decide in O(1) whether
;; one line is before or after another line in the buffer. This is needed to
;; offer Zmacs-like region highlighting with satisfying speed.

;; Marks

;; Each buffer has a list of marks, which are to be updated upon insertion
;; or deletion of characters. There are two kind of marks.

(defstruct line
  data                  ;the contents of the line (a ROD)
  next                  ;next line or NIL
  prev                  ;previous line or NIL
  deco                  ;possible decoration
  n)                    ;line number (see above for explanation).

(defstruct buffer
  top                   ;the very first line of the buffer
  display-start         ;the first line on the display
  marks                 ;a list of marks to update (list of MARK-MARK objects).
  point                 ;the point
  mark)                 ;the mark

(defstruct mark
  line                  ;the line this mark points at. (a LINE)
  offset)               ;the character offset within that line

(defstruct mark-mark
  reader                ;a function to read the mark
  writer                ;a procedure to update the mark
  (compare #'>=))

;; To reduce the number of global/special variables, we introduce a
;; so-called 'editing-context', which carries all necessary
;; information. The special variable then is `*ec*'.

(defvar *ec*)

(defstruct (editing-context (:conc-name "EC-"))
  buffer        ;the current buffer
  window        ;the current (emacs) window
  screen        ;the current screen
  vars          ;
  )

(defstruct screen
  display-cache
  nrows
  ncols)

(defstruct window
  nrows          ;number of rows in this window
  (hoff 0))

;;;; Random utilities

(defun word-rune-p (rune)
  (or (<= (char-code #\a) rune (char-code #\z))
      (<= (char-code #\A) rune (char-code #\Z))
      (<= #x00C0 rune #x00D6)
      (<= #x00D8 rune #x00F6)
      (<= #x00F8 rune #x00FF)
      (<= #x0100 rune #x017F)
      (<= #x0400 rune #x045F) ))

;;;; Primitive Buffer Operations

(defun create-buffer ()
  (let ((ln (make-line :data (coerce '() 'rod)
                       :next nil
                       :prev nil
                       :n 1)))
    (let (self)
      (setf self
        (make-buffer :top ln
                     :display-start (make-mark :line ln :offset 0)
                     :point (make-mark :line ln :offset 0)
                     :marks
                     (list (make-mark-mark
                            :reader (lambda () (buffer-point self))
                            :writer (lambda (x) (setf (buffer-point self) x)))
                           (make-mark-mark
                            :reader (lambda () (buffer-display-start self))
                            :writer (lambda (x) (setf (buffer-display-start self) x))
                            :compare #'>)
                           (make-mark-mark
                            :reader (lambda () (buffer-mark self))
                            :writer (lambda (x) (setf (buffer-mark self) x))
                            :compare #'>)) ))
      self)))



(defun buffer-insert (buf line offset string)
  (setf string (rod string))
  (buffer-insert/aux buf line offset string 0 (length string)))

(defun buffer-insert/aux (buf line offset string start end)
  ;; This needs improvement
  (let ((p (position +c/newline+ string :start start :end end)))
    (cond ((null p)
           (setf (line-data line)
             (concatenate 'rod
               (subseq (line-data line) 0 offset)
               (subseq string start end)
               (subseq (line-data line) offset)))
           (dolist (k (buffer-marks buf))
             (let ((m (funcall (mark-mark-reader k))))
               (when m
                 (let ((ln (mark-line m))
                       (of (mark-offset m)))
                   (when (and (eq ln line)
                              (funcall (mark-mark-compare k) of offset))
                     (funcall (mark-mark-writer k)
                              (make-mark :line ln :offset (+ of (- end start))))))))))
          (t
           (let ((rest (subseq (line-data line) offset)))
             (setf (line-data line)
               (concatenate 'rod
                 (subseq (line-data line) 0 offset)
                 (subseq string start p)))
             (let ((new (make-line)))
               (setf (line-data new) rest
                     (line-prev new) line
                     (line-next new) (line-next line)
                     (line-next line) new)
               (when (line-next new)
                 (setf (line-prev (line-next new)) new))
               (setf (line-n new)
                 (/ (+ (line-n (line-prev new))
                       (if (line-next new)
                           (line-n (line-next new))
                         (+ 1024 (line-n (line-prev new)))))
                    2))
               (dolist (k (buffer-marks buf))
                 (let ((m (funcall (mark-mark-reader k))))
                   (when m
                     (let ((ln (mark-line m))
                           (of (mark-offset m)))
                       (when (and (eq ln line)
                                  (funcall (mark-mark-compare k) of offset))
                         (funcall (mark-mark-writer k)
                                  (make-mark :line new :offset (- of offset))))))))
               (buffer-insert/aux buf new 0 string (+ 1 p) end)))) )))

(defun buffer-delete (buffer line-0 offset-0 line-1 offset-1)
  buffer
  ;; update marks
  (dolist (k (buffer-marks buffer))
    (let ((m (funcall (mark-mark-reader k))))
      (when m
        (cond ((and (eq (mark-line m) line-1)
                    (funcall (mark-mark-compare k) (mark-offset m) offset-1))
               (funcall (mark-mark-writer k)
                        (make-mark :line line-0
                                   :offset (+ offset-0 (- (mark-offset m) offset-1)))) )
              ((and (mark-<= (make-mark :line line-0 :offset offset-0) m)
                    (mark-<= m (make-mark :line line-1 :offset offset-1)))
               (funcall (mark-mark-writer k)
                        (make-mark :line line-0 :offset offset-0)) )))))
  (setf (line-next line-0) (line-next line-1))
  (when (line-next line-1)
    (setf (line-prev (line-next line-1)) line-0))
  (setf (line-data line-0)
    (concatenate 'rod
      (subseq (line-data line-0) 0 offset-0)
      (subseq (line-data line-1) offset-1))))

(defun buffer-substring (buffer line-0 offset-0 line-1 offset-1)
  buffer
  (cond ((eq line-0 line-1)
         (subseq (line-data line-0) offset-0 offset-1))
        ((concatenate 'rod
           (subseq (line-data line-0) offset-0)
           (rod #\newline)
           (buffer-substring buffer
                             (line-next line-0) 0
                             line-1 offset-1)))))

;;;; Editing context

;; (defstruct is above).

;; editing context variables aka buffer variables.

(defvar *ec-vars* nil
  "Plist of of all defined buffer variables; Looks like (sym-0 (init-0 doc-0) ... sym-n (init-n doc-n).")

(defun ec-var (sym &optional (ec *ec*))
  "Return the value of the buffer variable `sym'."
  (multiple-value-bind (value success?) (gethash sym (ec-vars ec))
    (if success?
        value
      (error "EC variable ~S is unbound." sym))))

(defun (setf ec-var) (new-value sym &optional (ec *ec*))
  "Set the value of the buffer variable `sym'."
  (setf (gethash sym (ec-vars ec)) new-value))

(defmacro def-ec-var (symbol initial-value &optional documentation)
  "Defines a new buffer local variable."
  `(PROGN
     (SETF (GETF *EC-VARS* ',symbol)
           (CONS ,initial-value ,documentation))
     ',symbol))

;;;; some utilities

(defun line-length (line)
  (length (line-data line)))

(defun mark-<= (m0 m1)
  (cond ((< (line-n (mark-line m0)) (line-n (mark-line m1)))
         t)
        ((> (line-n (mark-line m0)) (line-n (mark-line m1)))
         nil)
        ((<= (mark-offset m0) (mark-offset m1)))))

(defun mark-backward (mark)
  (cond ((= (mark-offset mark) 0)
         (cond ((line-prev (mark-line mark))
                (let ((ln (line-prev (mark-line mark))))
                  (values
                   (make-mark :line ln
                              :offset (line-length ln))
                   t)))
               (t
                (values
                 mark
                 nil))))
        (t
         (values
          (make-mark :offset (1- (mark-offset mark))
                     :line (mark-line mark))
          t) )))

(defun mark-forward (mark)
  (cond ((= (mark-offset mark) (line-length (mark-line mark)))
         (let ((ln (line-next (mark-line mark))))
           (if ln
               (values
                (make-mark :line ln :offset 0)
                t)
             (values
              mark
              nil))))
        (t
         (values
          (make-mark :line (mark-line mark)
                     :offset (1+ (mark-offset mark)))
          t)) ))

(defun sort-marks (m0 m1)
  "Returns the marks `m0' and `m1' in sorted order as multiple values."
  (cond ((< (line-n (mark-line m0)) (line-n (mark-line m1)))
         (values m0 m1))
        ((> (line-n (mark-line m0)) (line-n (mark-line m1)))
         (values m1 m0))
        ((< (mark-offset m0) (mark-offset m1))
         (values m0 m1))
        (t
         (values m1 m0))))

(defun buffer-last-line (buffer &optional (k (buffer-top buffer)))
  (cond ((null (line-next k)) k)
        ((buffer-last-line buffer (line-next k)))))

;;;; Editing function

(def-ec-var last-command-char nil)
(def-ec-var last-command nil)
(def-ec-var goal-column nil)
(def-ec-var zmacs-region-stays-p nil)
(def-ec-var zmacs-region-active-p nil)
(def-ec-var highlighted-paren nil)
(def-ec-var kill-buffer nil)

(defun zmacs-activate-region ()
  (setf (ec-var 'zmacs-region-stays-p) t)
  (setf (ec-var 'zmacs-region-active-p) t))

(defun current-buffer ()
  (ec-buffer *ec*))

(defun point (&optional buffer)
  (buffer-point (or buffer (current-buffer))))

(defun (setf point) (new-value &optional buffer)
  (setf (buffer-point (or buffer (current-buffer)))
    new-value))

(defun eobp (&optional buffer)
  ;; Return T if point is at the end of the buffer.
  ;; If `buffer' is nil, the current buffer is assumed.
 (and (= (mark-offset (point buffer))
         (line-length (mark-line (point buffer))))
      (null (line-next (mark-line (point buffer))))))

(defun char-at (buffer &optional (position (buffer-point buffer)))
  (let ((line (mark-line position))
        (offset (mark-offset position)))
    (cond ((= offset (line-length line))
           +c/newline+)
          (t
           (rune (line-data line) offset)))))

(defun beginning-of-buffer-p (buffer position)
  buffer
  (and (= (mark-offset position) 0)
       (null (line-prev (mark-line position)))))

(defun end-of-buffer-p (buffer position)
  buffer
  (and (= (mark-offset position)
          (line-length (mark-line position)))
       (null (line-next (mark-line position)))))

;;;

(defun buffer-kill-append (buffer p0 p1)
  (setf (ec-var 'kill-buffer)
    (concatenate 'rod
      (if (member (ec-var 'last-command) *kill-commands*)
          (ec-var 'kill-buffer)
        (rod '#()))
      (buffer-substring buffer
                        (mark-line p0) (mark-offset p0)
                        (mark-line p1) (mark-offset p1))))
  (buffer-delete  buffer
                  (mark-line p0) (mark-offset p0)
                  (mark-line p1) (mark-offset p1)))

(defun find-matching-parenthesis-forward (buffer pos &optional (n 5000))
  (let ((level 0))
    (do ((p pos (mark-forward p))
         (i 0 (+ i 1)))
        ((or (end-of-buffer-p buffer p)
             (> i n)))
      (cond ((= (char-code #\() (char-at buffer p))
             (incf level))
            ((= (char-code #\)) (char-at buffer p))
             (decf level)))
      (cond ((= 0 level)
             (return p))))))

(defun find-matching-parenthesis-backward (buffer pos &optional (n 5000))
  (let ((level 0))
    (do ((p (mark-backward pos) (mark-backward p))
         (i 0 (+ i 1)))
        ((or (end-of-buffer-p buffer p)
             (> i n)))
      (cond ((= (char-code #\() (char-at buffer p))
             (incf level))
            ((= (char-code #\)) (char-at buffer p))
             (decf level)))
      (cond ((= 0 level)
             (return p))))))

(defun care-for-parenthesis (buffer)
  (cond ((= (char-code #\() (char-at buffer))
         (setf (ec-var 'highlighted-paren)
           (find-matching-parenthesis-forward buffer (buffer-point buffer))))
        ((= (char-code #\)) (char-at buffer (mark-backward (buffer-point buffer))))
         (setf (ec-var 'highlighted-paren)
           (find-matching-parenthesis-backward buffer (buffer-point buffer))))
        (t
         (setf (ec-var 'highlighted-paren) nil))
        ))

;;; User commands

(def-ec-var overlap
    ;; overlap for next-page/previous-page
    2)

(defparameter *kill-commands*
    '(cmd/kill-line cmd/region-kill cmd/kill-word))

(defun cmd/forward-char (&optional (buffer (current-buffer)))
  (setf (ec-var 'zmacs-region-stays-p) t)
  (multiple-value-bind (m s?) (mark-forward (buffer-point buffer))
    (unless s?
      (error "End of buffer."))
    (setf (buffer-point buffer) m)))

(defun cmd/backward-char (&optional (buffer (current-buffer)))
  (setf (ec-var 'zmacs-region-stays-p) t)
  (multiple-value-bind (m s?) (mark-backward (buffer-point buffer))
    (unless s?
      (error "Beginning of buffer."))
    (setf (buffer-point buffer) m)))

(defun cmd/delete-char (&optional (buffer (current-buffer)))
  (let ((m0 (buffer-point buffer)))
    (multiple-value-bind (m1 s?) (mark-forward m0)
      (when s?
        (buffer-delete buffer
                       (mark-line m0) (mark-offset m0)
                       (mark-line m1) (mark-offset m1))))))

(defun cmd/backward-delete-char (&optional (buffer (current-buffer)))
  (cmd/backward-char buffer)
  (cmd/delete-char buffer))

(defun cmd/beginning-of-line (&optional (buffer (current-buffer)))
  (setf (ec-var 'zmacs-region-stays-p) t)
  (setf (buffer-point buffer)
    (make-mark :line (mark-line (buffer-point buffer))
               :offset 0)))

(defun cmd/end-of-line (&optional (buffer (current-buffer)))
  (setf (ec-var 'zmacs-region-stays-p) t)
  (setf (buffer-point buffer)
    (make-mark :line (mark-line (buffer-point buffer))
               :offset (line-length (mark-line (buffer-point buffer))))))

(defun cmd/beginning-of-buffer (&optional (buffer (current-buffer)))
  (setf (buffer-point buffer)
    (make-mark :line (buffer-top buffer)
               :offset 0)))

(defun cmd/end-of-buffer (&optional (buffer (current-buffer)))
  (let ((ln (buffer-last-line buffer)))
    (setf (buffer-point buffer)
      (make-mark :line ln :offset (line-length ln)))))

(defun cmd/self-insert (buffer)
  (buffer-insert buffer
                 (mark-line (buffer-point buffer))
                 (mark-offset (buffer-point buffer))
                 (ec-var 'last-command-char)))

(defun cmd/newline (buffer)
  (buffer-insert buffer
                 (mark-line (buffer-point buffer))
                 (mark-offset (buffer-point buffer))
                 #\newline))

(defun current-column (&optional (buffer (current-buffer)))
  (mark-offset (buffer-point buffer)))

(defun line-move (buffer n)
  (setf (ec-var 'zmacs-region-stays-p) t)
  (unless (member (ec-var 'last-command) '(cmd/next-line cmd/previous-line))
    (setf (ec-var 'goal-column) (current-column buffer)))
  (let ((q (mark-line (buffer-point buffer))))
    (cond ((< n 0)
           (dotimes (k (- n))
             (setf q (or (line-prev q) q))))
          ((> n 0)
           (dotimes (k n)
             (setf q (or (line-next q) q)))))
    (setf (buffer-point buffer)
      (make-mark :line q
                 :offset (min (line-length q) (ec-var 'goal-column)))) ))

(defun cmd/next-line (buffer)
  (line-move buffer +1))

(defun cmd/previous-line (buffer)
  (line-move buffer -1))

(defun cmd/set-mark (buffer)
  (zmacs-activate-region)
  (setf (buffer-mark buffer) (buffer-point buffer)))

(defun cmd/exchange-point-and-mark (buffer)
  (zmacs-activate-region)
  (when (buffer-mark buffer)
    (rotatef (buffer-mark buffer) (buffer-point buffer))))

(defun cmd/region-delete (buffer)
  (when (buffer-mark buffer)
    (multiple-value-bind (m0 m1) (sort-marks (buffer-mark buffer)
                                             (buffer-point buffer))
      (buffer-delete buffer
                     (mark-line m0) (mark-offset m0)
                     (mark-line m1) (mark-offset m1)) )))

(defun cmd/region-kill (buffer)
  (cmd/region-copy buffer)
  (cmd/region-delete buffer))

(defun cmd/region-copy (buffer)
  (when (buffer-mark buffer)
    (multiple-value-bind (m0 m1) (sort-marks (buffer-mark buffer)
                                             (buffer-point buffer))
      (setf (ec-var 'kill-buffer)
        (buffer-substring buffer
                          (mark-line m0) (mark-offset m0)
                          (mark-line m1) (mark-offset m1))))))

(defun cmd/yank (buffer)
  (cmd/set-mark buffer)
  (buffer-insert buffer
                 (mark-line (buffer-point buffer))
                 (mark-offset (buffer-point buffer))
                 (ec-var 'kill-buffer)))

(defun cmd/kill-line (buffer)
  (multiple-value-bind (p0 p1)
      (cond ((= (mark-offset (buffer-point buffer))
                (line-length (mark-line (buffer-point buffer))))
             (values (buffer-point buffer)
                     (mark-forward (buffer-point buffer))))
            (t
             (values (buffer-point buffer)
                     (make-mark :line (mark-line (buffer-point buffer))
                                :offset (line-length (mark-line (buffer-point buffer)))))))
    (buffer-kill-append buffer p0 p1)))

(defun cmd/previous-page (buffer)
  (let ((n (max 0 (- (window-nrows (current-window)) (ec-var 'overlap)))))
    (dotimes (i n)
      (cmd/previous-line buffer))))

(defun cmd/next-page (buffer)
  (let ((n (max 0 (- (window-nrows (current-window)) (ec-var 'overlap)))))
    (dotimes (i n)
      (cmd/next-line buffer))))

(defun cmd/forward-word (buffer)
  (do ()
      ((word-rune-p (char-at buffer)))
    (cmd/forward-char buffer))
  (do ()
      ((not (word-rune-p (char-at buffer))))
    (cmd/forward-char buffer)))

(defun cmd/backward-word (buffer)
  (cmd/backward-char buffer)
  (do ()
      ((word-rune-p (char-at buffer)))
    (cmd/backward-char buffer))
  (do ()
      ((not (word-rune-p (char-at buffer))))
    (cmd/backward-char buffer))
  (cmd/forward-char buffer))

(defun cmd/kill-word (buffer)
  (let ((p0 (buffer-point buffer))
        (p1 (progn
              (cmd/forward-word buffer)
              (buffer-point buffer))))
    (buffer-kill-append buffer p0 p1)))

(defun cmd/open-line (buffer)
  (buffer-insert buffer
                 (mark-line (buffer-point buffer))
                 (mark-offset (buffer-point buffer))
                 #\newline)
  (cmd/backward-char buffer))

(defun cmd/back-to-indentation (buffer)
  ;; Move point to the first non-whitespace character on this line.
  (cmd/beginning-of-line buffer)
  (do ()
      ((not (member (char-at buffer) '(8 32))))
    (cmd/forward-char buffer)))

(defun cmd/delete-horizontal-space (buffer)
  ;; Delete all spaces and tabs around point.
  (let ((p1 (buffer-point buffer))
        (p2 (buffer-point buffer)))
    (do ()
        ((or (beginning-of-buffer-p buffer (mark-backward p1))
             (not (member (char-at buffer (mark-backward p1)) '(8 32)))))
      (setf p1 (mark-backward p1)))
    (do ()
        ((or (beginning-of-buffer-p buffer p2)
             (not (member (char-at buffer p2) '(8 32)))))
      (setf p2 (mark-forward p2)))
    (buffer-delete buffer
                   (mark-line p1) (mark-offset p1)
                   (mark-line p2) (mark-offset p2))))

(defun cmd/just-one-space (buffer)
  ;;Delete all spaces and tabs around point, leaving one space.
  (cmd/delete-horizontal-space buffer)
  (setf (ec-var 'last-command-char) 32)
  (cmd/self-insert buffer))

;;; Keymap

(defparameter *keymap*
    '(((#\space :control) . cmd/set-mark)
      ((#\a :control)     . cmd/beginning-of-line)
      ((#\b :control)     . cmd/backward-char)
      ((#\d :control)     . cmd/delete-char)
      ((#\e :control)     . cmd/end-of-line)
      ((#\f :control)     . cmd/forward-char)
      ((#\h :control)     . cmd/backward-delete-char)
      ((#\k :control)     . cmd/kill-line)
      ((#\m :control)     . cmd/newline)
      ((#\n :control)     . cmd/next-line)
      ((#\o :control)     . cmd/open-line)
      ((#\p :control)     . cmd/previous-line)
      ((#\w :control)     . cmd/region-kill)
      ((#\y :control)     . cmd/yank)
      ((#\v :control)     . cmd/next-page)

      ((#\w :meta)        . cmd/region-copy)
      ((#\f :meta)        . cmd/forward-word)
      ((#\b :meta)        . cmd/backward-word)
      ((#\d :meta)        . cmd/kill-word)
      ((#\v :meta)        . cmd/previous-page)
      ((#\m :meta)        . cmd/back-to-indentation)
      ((#\\ :meta)        . cmd/delete-horizontal-space)
      ((#\space :meta)    . cmd/just-one-space)

      ((#\< :meta)        . cmd/beginning-of-buffer)
      ((#\> :meta)        . cmd/end-of-buffer)

      ((#\z :control)     . cmd/magic)

      ((:left)            . cmd/backward-char)
      ((:right)           . cmd/forward-char)
      ((:up)              . cmd/previous-line)
      ((:down)            . cmd/next-line)
      ((:begin)           . cmd/beginning-of-line)
      ((:end)             . cmd/end-of-line)
      ((:page-up)         . cmd/previous-page)
      ((:page-down)       . cmd/next-page)

      ((:return)          . cmd/newline)
      ((:backspace)       . cmd/backward-delete-char)
      ((:delete)          . cmd/delete-char)
      ))

;;; ---------------------------------------------------------------------------
;;;  CLUE contact class
;;;

(clue:defcontact glue:text-area (glue:3d)
  ((font)
   (ifont)
   (ascent)
   (descent)
   (curx :initform nil)
   (cury :initform nil)
   (fg-nil :initform nil)
   (fg-block :initform nil)
   (fg-cursor :initform nil)
   (fg-foo :initform nil)
   (fg-par :initform nil)
   (bg-nil :initform nil)
   (bg-block :initform nil)
   (bg-cursor :initform nil)
   (bg-foo :initform nil)
   (bg-par :initform nil)
   (ncols)
   (nrows)
   (ec)
   (display-line-buffer :initform nil)
   )
  (:Resources
   (ncols
    :type integer
    :initform 60)
   (nrows
    :type integer
    :initform 20)
   (font
    :type glue:afont
    :initform
    "fixed"
    ;; "-microsoft-courier new-medium-r-*-*-*-110-*-*-*-*-unicode-2"
    ;; "-microsoft-times new roman-medium-r-*-*-*-120-*-*-*-*-unicode-2"
    )
   (ifont
    :type glue:afont
    :initform
    "fixed"
    ;;"-microsoft-courier new-medium-r-*-*-*-120-*-*-*-*-unicode-2"
    ;; "-microsoft-times new roman-medium-r-*-*-*-120-*-*-*-*-unicode-2"
    ))
  )

(clue:defevent glue:text-area :key-press key-press)

(defmethod initialize-instance :after ((self glue:text-area) &rest ignore)
  (with-slots (ncols nrows ec) self
    (setf ec (init ncols nrows)) ))

(defmethod clue:realize :after ((self glue:text-area))
  (with-slots (font ascent descent buffer
               fg-nil fg-block fg-cursor fg-foo fg-par
               bg-nil bg-block bg-cursor bg-foo bg-par
               ncols nrows ec)
      self
    (setf ascent (glue:font-ascent self font)
          descent (glue:font-descent self font)
          fg-nil    (clue:convert self :black 'xlib:pixel)
          bg-nil    (clue:convert self :white 'xlib:pixel)
          fg-foo    (clue:convert self :black 'xlib:pixel)
          bg-foo    (clue:convert self :white 'xlib:pixel)
          fg-block  (clue:convert self :black 'xlib:pixel)
          bg-block  (clue:convert self :gray76 'xlib:pixel)
          fg-cursor (clue:convert self :black 'xlib:pixel)
          bg-cursor (clue:convert self :red 'xlib:pixel)
          fg-par    (clue:convert self :red 'xlib:pixel)
          bg-par    (clue:convert self :white 'xlib:pixel)) ))

(defmethod clue:preferred-size ((self glue:text-area) &key width height border-width)
  width height border-width
  (with-slots (ncols nrows font) self
    (let ((font (clue:convert (clue:contact-root self) font 'glue:afont)))
      (values (+ (glue:left-leading self)
                 (glue:right-leading self)
                 (* ncols 
                    (glue:char-width self font (char-code #\m))))
              (+ (glue:top-leading self)
                 (glue:bottom-leading self)
                 (* nrows (+ (glue:font-ascent self font)
                             (glue:font-descent self font))))
            0))))

(defun nuke-display-cache (screen)
  (dotimes (i (length (screen-display-cache screen)))
    (setf (aref (screen-display-cache screen) i)
      (make-line))))

(defmethod clue:display ((self glue:text-area) &optional x y width height &key)
  x y width height
  (let ((*ec* (slot-value self 'ec)))
    (multiple-value-bind (x0 y0 x1 y1) (glue:interior-rectangle* self)
      x0 y0 x1 y1
      (nuke-display-cache (current-screen))
      (buffer-redisplay self (current-buffer)))))

(defmethod key-press ((self glue:text-area))
  (let ((*ec* (slot-value self 'ec)))
    (clue:with-event (clue::state clue::code)
      (let ((kp (glue::translate-key-press-event (clue:contact-display self)
                                                 clue::code clue::state)))
        (let ((cmd (or (cdr (assoc kp *keymap*
                                   :test #'glue::key-press-match-p))
                       (when (and (integerp (car kp)) (null (cdr kp)))
                         'cmd/self-insert))))
          (when cmd
            (setf (ec-var 'last-command-char) 
              (and (integerp (car kp)) (car kp)))
            (setf (ec-var 'zmacs-region-stays-p) nil)
            (multiple-value-bind (x y) (ignore-errors (funcall cmd (current-buffer)))
              x
              (and y
                   (print (princ-to-string y))))
            (unless (ec-var 'zmacs-region-stays-p)
              (setf (ec-var 'zmacs-region-active-p) nil))
            (setf (ec-var 'last-command) cmd)
            (progn 'time (buffer-redisplay self (current-buffer)))))))))

(defmethod glue:text-area-string ((self glue:text-area))
  (with-slots (ec) self
    (let* ((buf (ec-buffer ec))
           (last-line (buffer-last-line buf)))
      (buffer-substring buf
                        (buffer-top buf)
                        0
                        last-line
                        (line-length last-line)))))

(defmethod (setf glue:text-area-string) (new-value (self glue:text-area))
  (with-slots (ec) self
    (let* ((buf (ec-buffer ec))
           (last-line (buffer-last-line buf)))
      ;; first nuke everything
      (buffer-delete buf
                     (buffer-top buf)
                     0
                     last-line
                     (line-length last-line))
      ;; now insert new-value
      (buffer-insert buf 
                     (buffer-top buf)
                     0
                     new-value)
      new-value)))

(defun display-line (contact
                     y
                     data-n deco-n
                     data-o deco-o)
  (declare (type rod data-n data-o))
  (with-slots (display-line-buffer) contact
    (let* ((buf display-line-buffer)
           (buflen (length buf)))
      (let ((font    (slot-value contact 'font))
            (ifont    (slot-value contact 'ifont))
            (ascent  (slot-value contact 'ascent))
            (descent (slot-value contact 'descent))
            (win     (current-window))) ;xxx
        (multiple-value-bind (ix0 iy0 ix1 iy1) (glue:interior-rectangle* contact)
          ix1 iy1
          (with-slots (fg-nil fg-block fg-cursor fg-foo fg-par
                       bg-nil bg-block bg-cursor bg-foo bg-par)
              contact
            (let ((xo 0)
                  (xn 0)
                  (cx 0)
                  (cx1 0)
                  (j 0)
                  (cfg :void)
                  (cbg :void)
                  (cfn :void)
                  (cd :void))
              (labels ((flush (fg bg fn)
                         (when (> j 0)
                           (glue:draw-glyphs 
                            contact fn
                            (+ ix0 cx) (+ iy0 (+ ascent (* y (+ ascent descent))))
                            buf 
                            :end j
                            :image-p t
                            :foreground fg
                            :background bg)
                           (setf j 0)))
                       (fg+bg+fn (d)
                         (ecase d
                           ((nil)     (values fg-nil    bg-nil    font))
                           ((:region) (values fg-block  bg-block  font))
                           ((:foo)    (values fg-foo    bg-foo    ifont))
                           ((:par)    (values fg-par    bg-par    font))
                           ((:cursor) (values fg-cursor bg-cursor font))))
                       (draw (x c d)
                         (let ()
                             (when (or (not (eql cd d))
                                       (/= cx1 x))
                               (flush cfg cbg cfn)
                               (multiple-value-bind (fg bg fn) (fg+bg+fn d)
                                 (setf cd d j 0  cx x  cx1 x  cfg fg  cbg bg cfn fn)))
                           (when (>= j buflen)
                             ;; evil hack here: buf might as well be uninitialised aka NIL 
                             ;; => (= buflen 0) 
                             (incf buflen 30)
                             (setf display-line-buffer
                               (setf buf (if buf
                                             (adjust-array buf buflen :element-type 'rune)
                                           (make-array buflen :element-type 'rune)))))
                           (setf (aref buf j) c;zzz
                                 cx1 (+ cx1 (width c d cfn))
                                 j (+ j 1))))
                       (width (c d fn)
                         d c
                         (glue:char-width contact fn c)
                         ))
                (declare (inline width))
                (let* ((len-o (length data-o))
                       (len-n (length data-n))
                       (n (1+ len-n)))
                  (declare (type fixnum len-o len-n n xn xo))
                  (loop for i from (window-hoff win) to (1- n) do
                        (locally
                            (declare (type fixnum i))
                          (let ((co (if (< i len-o) (rune data-o i) 32))
                                (cn (if (< i len-n) (rune data-n i) 32))
                                (do (deco-at deco-o i))
                                (dn (deco-at deco-n i)))
                            (when (or (/= co cn) (not (eq do dn)) (/= xo xn))
                              (draw xn cn dn))
                            (incf xo (width co do (nth-value 2 (fg+bg+fn do))))
                            (incf xn (width cn dn (nth-value 2 (fg+bg+fn dn)))))))
                  (unless (eq cd :void)
                    (multiple-value-bind (fg bg fn) (fg+bg+fn cd) 
                      (flush fg bg fn)))
                  (unless (and (= xn xo)
                               (= len-n len-o))
                    (xlib:clear-area contact
                                     :x (+ ix0 xn)
                                     :y (+ iy0 (* y (+ ascent descent)))
                                     :height (+ ascent descent))))))))))))

;;;; ------------------------------------------------------------------------------------------
;;;; unsorted
;;;;

;;;; Redisplay

(defun buffer-recenter (buf)
  (let* ((win (current-window)) ;xxx
         (n (window-nrows win)))
    (do ((i 0 (+ i 1))
         (q (mark-line (buffer-display-start buf)) (and q (line-next q))))
        ((= i n)
         ;; fall thru'
         (let ((q (mark-line (buffer-point buf))))
           (dotimes (i (floor n 2))
             (setf q (or (line-prev q) q)))
           (setf (buffer-display-start buf)
             (make-mark :line q :offset 0))
           (buffer-recenter buf)) )
      (when (eq (mark-line (buffer-point buf)) q)
        (return i)))))

(defun line-similiar-p (ln1 ln2)
  (and (eq (line-data ln1) (line-data ln2))
       (equal (line-deco ln1) (line-deco ln2)) ;xxx
       ))

(defun line-dyn-deco (buf line)
  (append
   (and t ;;nil
        (eq line (mark-line (buffer-point buf)))
        (list (list (mark-offset (buffer-point buf))
                    (+ 1 (mark-offset (buffer-point buf)))
                    :cursor)))
   (and (ec-var 'highlighted-paren)
        (eq line (mark-line (ec-var 'highlighted-paren)))
        (list (list (mark-offset (ec-var 'highlighted-paren))
                    (+ 1 (mark-offset (ec-var 'highlighted-paren)))
                    :par)))
   (and (ec-var 'zmacs-region-active-p)
        (let ((m0 (buffer-mark buf))
              (m1 (buffer-point buf)))
          (cond ((null m0)
                 nil)
                (t
                 (multiple-value-setq (m0 m1) (sort-marks m0 m1))
                 (cond ((and (eq (mark-line m0) line)
                             (eq (mark-line m1) line))
                        (list (list (mark-offset m0) (mark-offset m1) :region)))
                       ((eq (mark-line m0) line)
                        (list (list (mark-offset m0) (line-length (mark-line m0)) :region)))
                       ((eq (mark-line m1) line)
                        (list (list 0 (mark-offset m1) :region)))
                       ((<= (line-n (mark-line m0))
                            (line-n line)
                            (line-n (mark-line m1)))
                        (list (list 0 (line-length line) :region)))
                       (t
                        nil))))))
   #+(OR)
   (let ((p (position (char-code #\;) (line-data line))))
      (and p
           (list (list p (line-length line) :foo))))
   (and (> (line-length line) 0)
        (= (char-code #\>) (rune (line-data line) 0))
        (list (list 1 (line-length line) :foo)))

   (line-deco line)))

(defun line-deco-at (ln i)
  (dolist (k (line-deco ln))
    (cond ((and (<= (first k) i) (< i (second k)))
           (return (third k))))))

(defun slurp-file (buffer filename)
  (with-open-file (input filename :direction :input)
    (do ((x (read-line input nil nil)
            (read-line input nil nil)))
        ((null x))
      (let ((l (buffer-last-line buffer)))
        (buffer-insert buffer l (line-length l) #\newline)
        (buffer-insert buffer (line-next l) 0 x)))))

(defun create-screen (ncols nrows)
  (let ((dc (make-array nrows :initial-element nil)))
    (dotimes (i (length dc))
      (setf (aref dc i) (make-line)))
    (make-screen :ncols ncols
                 :nrows nrows
                 :display-cache dc)))

(defun init (ncols nrows)
  (let ((ec (make-editing-context
             :vars (make-hash-table :test #'eq)
             :screen (create-screen ncols nrows)
             :window (make-window :nrows nrows)
             :buffer (create-buffer))))
    (do ((q *ec-vars* (cddr q)))
        ((null q))
      (setf (ec-var (car q) ec) (car (cadr q))))
    '(let ((*ec* ec))
      (slurp-file (current-buffer) "b.lisp"))
    ec))

;;;; Editing commands

(defun forward-char (&optional buffer)
  (when (eobp buffer)
    (error "End of buffer."))
  (setf (point buffer)
    (mark-forward (point buffer))))

;;; ---------------------------------------------------------------------------

(defun tyo-flush ()
  )


#+ALLEGRO
(defmacro prof (&rest body)
  `(prog1
       (prof:with-profiling () ,@body)
     (prof:show-flat-profile)))

(defun buffer-redisplay (contact buf)
  (care-for-parenthesis buf)
  (let ((cy (buffer-recenter buf))
        (cx (mark-offset (buffer-point buf)));xxx
        (dc (screen-display-cache (current-screen)))
        (nrows (screen-nrows (current-screen)))
        (win (current-window))          ;xxx
        )
    cy
    (unless (<= 0 (- cx (window-hoff win)) (1- (screen-ncols (current-screen))))
      (setf (window-hoff win)
        (max 0 (- cx
                  (floor (screen-ncols (current-screen)) 2))))
      (nuke-display-cache (current-screen))
      (xlib:clear-area contact) )
    (do ((i 0 (+ i 1))
         (q (mark-line (buffer-display-start buf))
            (and q (line-next q))))
        ((or (= i nrows))
         )
      (let ((s (if q (line-data q) +tilde-rod+))
            (d (and q (line-dyn-deco buf q))))
        (unless (and (eq (line-data (aref dc i)) s)
                     (equal d (line-deco (aref dc i))))
          (progn
           (progn ;;dotimes (j 1000)
             (display-line contact i
                           s d
                           (line-data (aref dc i))
                           (line-deco (aref dc i)))))
          (setf (line-data (aref dc i)) s
                (line-deco (aref dc i)) d))))))

(defun deco-at (deco i)
  (dolist (k deco)
    (cond ((and (<= (first k) i) (< i (second k)))
           (return (third k))))))

(defun current-screen ()
  (ec-screen *ec*))

(defun current-window ()
  (ec-window *ec*))




(defun cmd/magic (&optional (buffer (current-buffer)))
  (with-open-file (sink "/tmp/TEXTAREA-DUMP"
                   :direction :output
                   :if-exists :new-version)
    (write-string
     (map 'string (lambda (x) (code-char x))
          (buffer-substring buffer
                            (buffer-top buffer)
                            0
                            (buffer-last-line buffer)
                            (line-length (buffer-last-line buffer))))
     sink)))

;;; an implementation of the shape extension


;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLIM-USER; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: CLIM GUI
;;;   Created: 2002-07-22
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: GPL (See file COPYING for details).
;;;       $Id$
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2002 by Gilbert Baumann

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

;; $Log$
;; Revision 1.6  2003/03/13 20:17:23  gilbert
;; CLX bug: xlib:put-image grind to halt when the image is widther than 2048 pixels.
;;
;; Revision 1.5  2003/03/13 19:29:17  gilbert
;; lots of hacking
;;
;; Revision 1.4  2002/08/16 17:20:50  gilbert
;; url-entry fix
;;
;; Revision 1.3  2002/07/29 12:39:08  gilbert
;; - we pass more tests now
;;
;; Revision 1.2  2002/07/24 04:11:51  gilbert
;; Tex Mode On and Tex Mode Off commands
;;
;; Revision 1.1.1.1  2002/07/22 02:27:22  gilbert
;; imported sources
;;

(in-package :CLIM-USER)
(use-package :clim)

;;;;;;;

(defvar *medium*)

(defvar *initial-url* nil)

(defvar closure:*home-page* "http://www.stud.uni-karlsruhe.de/~unk6/closure/user.html")
(defvar closure:*user-wants-images-p* t)

(defvar *closure-process* nil)

(defclass closure-pane (application-pane)
  ())

;;; Curde History

(defvar *back-history* nil)
(defvar *forw-history* nil)

(define-application-frame closure ()
  ()
  (:menu-bar menubar-command-table)
  (:panes
   (canvas (make-pane 'closure-pane
            :height 2000
            :width 800
            :display-time nil))
   (aux :application
    :height 300
    :width 300
    :min-width 100
    :min-height 100
    :max-width 300
    :max-height 20000
    :display-function 'aux-display
    :display-time :command-loop)
   (status :pointer-documentation
    :text-style (make-text-style :sans-serif :roman :normal)
    :scroll-bar nil
    :height 20
    :min-height 20
    :max-height 20
    :width 200
    :background +black+
    :foreground +white+)
   (interactor
    :interactor
    :foreground +black+
    :background (make-rgb-color 1 1 7/8)
    :text-style (make-text-style :sans-serif nil :normal)
    :height 50 :min-height 50 :max-height 50
    :scroll-bars nil :border nil)
   (wholine
    :pointer-documentation :width 5 :max-width +fill+
    :height 25
    :text-style (make-text-style :sans-serif :roman 10)
    :foreground +white+
    :background +black+)
   ;;(menu-bar (climi::make-menu-bar 'menubar-command-table :height 25))
   )
  (:layouts
   (default
       (vertically ()
         (spacing (:thickness 5)
                  (scrolling (:width 830 :height 600 :min-height 600 :max-height 20000
                              :scroll-bar :vertical)
                    canvas))
         (spacing (:thickness 5)
                  interactor)
         (horizontally (:height 80 :min-height 80 :max-height 80)
           wholine
           2
           (200 status))))
   (hidden-listener
       (vertically ()
         (spacing (:thickness 5)
                  (scrolling (:width 830 :height 600 :min-height 600 :max-height 20000
                              :scroll-bar :vertical)
                    canvas))
         (horizontally (:height 80 :min-height 80 :max-height 80)
           wholine
           2
           (200 status))))
   #+NIL
   (hidden-listener
       (vertically ()
         menu-bar
         (horizontally ()
           (vertically ()
             (climi::scrolling (:width 830 :height 600 :min-height 400 :max-height 20000)
                               canvas)) )
         (horizontally ()
           wholine
           2
           (200 status)))))
  (:top-level (closure-frame-top-level . nil))
  )


(make-command-table 'menubar-command-table
		    :errorp nil
		    :menu '(("File" :menu file-command-table)
                            ("Go"   :menu go-command-table)
                            ;; ("Bookmarks"   :menu bookmarks-command-table)
                            ;; ("View"     :menu view-command-table)
                            ("Appearance" :menu appearance-command-table)
                            ))

(make-command-table 'appearance-command-table :errorp nil
                    :menu '(("Show Listener" :command com-show-listener)
                            ("Hide Listener" :command com-hide-listener)))

(make-command-table 'file-command-table
                    :errorp nil
                    :menu '(("Quit" :command com-quit)))

(make-command-table 'go-command-table
		    :errorp nil
		    :menu '(("Back" :command com-back)
                            ("Forward" :command com-forward)
                            ("Home" :command com-home)))

(make-command-table 'view-command-table
		    :errorp nil
		    :menu '(("Zoom"    :menu    zoom-command-table)))

(make-command-table 'zoom-command-table
		    :errorp nil
		    :menu '(("Zoom In" :command com-zoom-in)
                            ("Zoom Out" :command com-zoom-out)
                            ("Zoom 100%" :command com-zoom-100%)))

(make-command-table 'bookmarks-command-table
		    :errorp nil
		    :menu '(("Add" :command com-add-bookmark)
                            ;;("Forward" :command com-forward)
                            ))

(defmethod closure-frame-top-level
    ((frame application-frame)
     &key (command-parser 'command-line-command-parser)
     (command-unparser 'command-line-command-unparser)
     (partial-command-parser
      'command-line-read-remaining-arguments-for-partial-command)
     (prompt "Closure => "))
  (catch 'closure-quit
    (loop
        (with-simple-restart (forget "Just forget this command, restart the command loop.")
          (let ((*standard-input* (frame-standard-input frame))
                (*standard-output* (frame-standard-output frame))
                (*query-io* (frame-query-io frame))
                (*pointer-documentation-output* (frame-pointer-documentation-output
                                                 frame))
                ;; during development, don't alter *error-output*
                ;; (*error-output* (frame-error-output frame))
                (*command-parser* command-parser)
                (*command-unparser* command-unparser)
                (*partial-command-parser* partial-command-parser)
                (prompt-style (make-text-style :sans-serif :bold :normal)))
            (map-over-sheets #'(lambda (pane)
                                 (if (and (typep pane 'clim-stream-pane)
                                          (eq (climi::pane-display-time pane) :command-loop)
                                          (climi::pane-display-function pane))
                                     (let ((func (climi::pane-display-function pane)))
                                       (window-clear pane)
                                       (funcall func frame pane) ; XXX other arguments
                                        ; XXX incremental redisplay
                                       )))
                             (frame-top-level-sheet frame))
            (let ((*application-frame* frame))
              (when *initial-url*
                (com-visit-url *initial-url*))
              (setf *initial-url* nil)
              (setf *closure-inited-p* t)
              (when *standard-input*
                (setf (cursor-visibility (stream-text-cursor *standard-input*)) t)
                (when prompt
                  (with-text-style (*standard-input* prompt-style)
                    (if (stringp prompt)
                        (write-string prompt *standard-input*)
                        (funcall prompt *standard-input* frame))
                    (finish-output *standard-input*)))
                (let ((command (read-frame-command frame)))
                  (fresh-line *standard-input*)
                  ;;(window-clear *standard-output*)
                  (clim:window-clear *query-io*)
                  (when command
                    (execute-frame-command frame command))
                  (fresh-line *standard-input*)))))))))

(define-presentation-type url ())
(define-presentation-type r2::pt ())
(define-presentation-type r2::hyper-link ())

;;;; ----------------------------------------------------------------------------------------------------
;;;; Commands
;;;;

(define-closure-command com-show-listener ()
  (setf (sheet-enabled-p (sheet-parent (find-pane-named *application-frame* 'interactor))) t))

(define-closure-command com-hide-listener ()
  (setf (sheet-enabled-p (sheet-parent (find-pane-named *application-frame* 'interactor))) nil))

(define-closure-command com-visit-url ((url 'url)) ;;; :gesture :select))
  (let ((*standard-output* *query-io*)) ;;(find-pane-named *frame* 'interactor)))
    (with-text-style (*standard-output* (make-text-style :sans-serif :roman :normal))
      (format t "You are visiting "))
    (present url 'url)
    (with-text-style (*standard-output* (make-text-style :sans-serif :roman :normal))
      (format t ".~%")))
  (setf *forw-history* nil
        *back-history* (cons url *back-history*))
  (let ((*standard-output* *trace-output*))
    (foo url)))

(define-closure-command com-reflow ()
  (reflow))

(define-closure-command com-back ()
  (let ((*standard-output* *query-io*)) 
    (cond ((null (cdr *back-history*))
           (format t "There is nowhere you can go back to.~%"))
          (t
           (push (pop *back-history*) *forw-history*)
           (format t "Going back to ~S.~%" (first *back-history*))
           (foo (first *back-history*))))))

(define-closure-command com-forward ()
  (let ((*standard-output* *query-io*)) 
    (cond ((null *forw-history*)
           (format t "There is nowhere you can go forward to.~%"))
          (t
           (push (pop *forw-history*) *back-history*)
           (format t "Going forward to ~S.~%" (first *back-history*))
           (foo (first *back-history*))))))

(define-closure-command com-reload ()
  (let ((*standard-output* *query-io*)) 
    (cond ((null *back-history*)
           (format t "There is nothing to reload.~%"))
          (t
           (format t "Reloading ~S.~%" (first *back-history*))
           (foo (first *back-history*))))))

(define-closure-command com-images-off ()
  (setf closure:*user-wants-images-p* nil)
  (format *query-io* "Images are now off.~%"))

(define-closure-command com-images-on ()
  (setf closure:*user-wants-images-p* t)
  (format *query-io* "Images are now on. You may want to reload.~%"))

(define-closure-command com-quit ()
  (throw 'closure-quit nil))

(defun make-google-search-url (string)
  (url:merge-url
   (url:make-url :query (list
                         (cons "hl" "en")
                         (cons "ie" "ISO-8859-1")
                         (cons "q" string)))
   (url:parse-url "http://www.google.com/search")))

(define-closure-command com-reverse-search-google ((url 'url))
  (let ((*standard-output* *trace-output*))
    (com-visit-url
     (make-google-search-url (format nil "link:~A" url)))))

(define-closure-command com-search-google ((what 'string))
  (com-visit-url (make-google-search-url what)))

(define-closure-command com-home ()
  (com-visit-url closure:*home-page*))

(define-presentation-translator fofo
    (url command closure
     :gesture :select
     :documentation ((object presentation stream)
                     (princ "Goto " stream)
                     (with-text-style (stream (make-text-style :fix nil nil))
                       (princ (url:unparse-url object) stream))
                     (princ "." stream)))
  (object)
  object)

(define-presentation-to-command-translator fofo
    (url com-visit-url closure
         :gesture :select
         :pointer-documentation ((object presentation stream)
                                 (princ "GOTO " stream)
                                 (with-text-style (stream (make-text-style :fix nil nil))
                                   (princ (if (url:url-p object)
                                              (url:unparse-url object)
                                              object)
                                          stream))
                                 (princ "." stream)))
  (object)
  (list object))

;;;; ----------------------------------------------------------------------------------------------------
;;;; Lisp Interface
;;;;

(defvar *closure-lock* (clim-sys:make-recursive-lock "Closure"))
(defvar *closure-inited-p* nil)

(defmacro with-closure (ignore &body body)
  (declare (ignore ignore))
  `(clim-sys:with-lock-held (*closure-lock*)
    ,@body))

(defun parse-url* (url)
  (etypecase url
    (string (url:parse-url url))
    (url:url url)))

(defun send-closure-command (command &rest args)
  (ensure-closure)
  (with-closure ()
    (mp:process-interrupt *closure-process*
                          #'(lambda () (apply command args)))))

(defun closure:visit (&optional (url closure:*home-page*))
  (and url (setf url (parse-url* url)))
  (cond ((and (null *closure-process*) (null url))
         (setf *initial-url* url)
         (ensure-closure))
        (t
         (ensure-closure)
         (when url
           (send-closure-command 'com-visit-url url)))))

(defun closure:start ()
  (closure:visit))

(defun closure:stop ()
  (with-closure ()
    (when *closure-process*
      (send-closure-command 'com-quit))))

(defun ensure-closure ()
  (with-closure ()
    (unless *closure-process*
      (setf *closure-inited-p* nil)
      (run-closure)
      (clim-sys:process-wait "Waiting for closure init"
                             (lambda ()
                               *closure-inited-p*)))))


(defvar *frame*)
(defvar *pane*)

(defun run-closure ()
  ;; Care for proxy
  (let* ((proxy (glisp:getenv "http_proxy"))
         (url   (and proxy (url:parse-url proxy))))
    (cond ((and url
                (equal (url:url-protocol url) "http"))
           (format t "~&;; Using HTTP proxy ~S port ~S~%"
                   (setf netlib::*http-proxy-host* (url:url-host url))
                   (setf netlib::*http-proxy-port* (url:url-port url))
                   (setf netlib::*use-http-proxy-p* t)))
          (t
           ;; we go without one:
           (setf netlib::*use-http-proxy-p* nil))))
  ;;
  (setf CLUE-GUI2::*PIXMAP-CACHE* nil)
  (setf CLUE-GUI2::*PIXMAP-CACHE* nil)
  (setf CLUE-GUI2::*DCACHE* nil)
  (setf climi::*3d-dark-color*   (make-gray-color .45))
  (setf climi::*3d-normal-color* (make-gray-color .75))
  (setf climi::*3d-light-color*  (make-gray-color .92))
  (setf climi::*3d-inner-color*  (make-gray-color .65))
  (setf clim-clx::*clx-text-sizes*
        '(:normal 12
          :tiny 8
          :very-small 10
          :small 10
          :large 14
          :very-large 18
          :huge 24))
  (gui::init-closure)
  #+NIL
  (loop for port in climi::*all-ports*
        do (destroy-port port))
  (setq climi::*all-ports* nil)
  ;;
  (setf *frame* (make-application-frame 'closure))
  (setf *pane*  (find-pane-named *frame* 'canvas))
  (setf *closure-process*
        (clim-sys:make-process
         (lambda ()
           (unwind-protect
                (run-frame-top-level *frame*)
             (ignore-errors (ws/netlib::commit-cache))
             (setf *closure-process* nil)))
         :name "Closure")))

(defun write-status (string)
  (window-clear (find-pane-named *frame* 'status))
  (write-string string (find-pane-named *frame* 'status))
  (xlib:display-finish-output (clim-clx::clx-port-display (find-port))))

(defun foo (url)
  (let ((*standard-output* *trace-output*))
    (clim-sys:make-process
     (lambda ()
       (with-simple-restart (forget "Just forget rendering this page.")
         (let ((*package* (find-package :r2)))
           (window-clear (find-pane-named *frame* 'canvas))
           (progn;;with-sheet-medium (medium *pane*)
             (let ((*medium* (find-pane-named *frame* 'canvas)))
               (let ((device (make-instance 'closure/clim-device::clim-device :medium *medium*)))
                 (setq url (r2::parse-url* url))
                 (let ((request (clue-gui2::make-request :url url :method :get)))
                   (multiple-value-bind (io header) (clue-gui2::open-document-4 request)
                     (write-status "Fetching Document ...")
                     (let* ((doc (make-instance 'r2::document
                                                :processes-hooks nil
                                                :location
                                                (r2::parse-url* url)
                                                :http-header header
                                                :pt (clue-gui2::make-pt-from-input 
                                                     io 
                                                     (netlib::get-header-field header :content-type) url) )))
                       (write-status "Rendering ...")
                       (setf *current-document* doc)
                       (let ((closure-protocol:*document-language*
                              (if (sgml::pt-p (r2::document-pt doc))
                                  (make-instance 'r2::html-4.0-document-language)
                                  (make-instance 'r2::xml-style-document-language)
                                  ))
                             (closure-protocol:*user-agent*
                              nil)
                             (r2::*canvas-width*
                              (bounding-rectangle-width (sheet-parent *medium*))))
                         (closure-protocol:render
                          closure-protocol:*document-language* 
                          doc
                          device
                          (setf *current-pt* (r2::document-pt doc))
                          600           ;xxx width
                          t             ;?
                          0)
                         (let ((x2 (bounding-rectangle-max-x (stream-output-history (find-pane-named *frame* 'canvas))))
                               (y2 (bounding-rectangle-max-y (stream-output-history (find-pane-named *frame* 'canvas)))))
                           (setf y2 (max y2 r2::*document-height*))
                           (clim:change-space-requirements *medium* :width x2 :height y2)
                           ;; While we are at it, force a repaint
                           (handle-repaint *medium* (sheet-region (pane-viewport *medium*)))
                           )))))
                 (write-status "Done."))))))
       (xlib:display-finish-output (clim-clx::clx-port-display (find-port)))))))

(defun reflow ()
  (let ((*standard-output* *trace-output*))
    (funcall ;;clim-sys:make-process
     (lambda ()
       (with-simple-restart (forget "Just forget rendering this page.")
         (let ((*package* (find-package :r2)))
           (window-clear (find-pane-named *frame* 'canvas))
           (let* ((*medium* (find-pane-named *frame* 'canvas)) )
             (write-status "Rendering ...")
             (let ((closure-protocol:*document-language*
                    (if (sgml::pt-p (r2::document-pt *current-document*))
                        (make-instance 'r2::html-4.0-document-language)
                        (make-instance 'r2::xml-style-document-language) ))
                   (closure-protocol:*user-agent*
                    nil)
                   (r2::*canvas-width*
                    (bounding-rectangle-width (sheet-parent *medium*))))
               (r2::reflow)
               (let ((x2 (bounding-rectangle-max-x (stream-output-history (find-pane-named *frame* 'canvas))))
                     (y2 (bounding-rectangle-max-y (stream-output-history (find-pane-named *frame* 'canvas)))))
                 (setf y2 (max y2 r2::*document-height*))
                 (clim:change-space-requirements *medium* :width x2 :height y2)
                 ;; While we are at it, force a repaint
                 (handle-repaint *medium* (sheet-region (pane-viewport *medium*)))))
             (write-status "Done."))))))))

(defvar *current-document*)
(defvar *current-pt*)

(defun parse-x11-color (string &aux sym r gb)
  ;; ### pff this really needs to be more robust.
  (cond ((and (= (length string) 4) (char= (char string 0) #\#))
         (make-rgb-color
          (/ (parse-integer string :start 1 :end 2 :radix 16) #xF)
          (/ (parse-integer string :start 2 :end 3 :radix 16) #xF)
          (/ (parse-integer string :start 3 :end 4 :radix 16) #xF)))
        ((and (= (length string) 7) (char= (char string 0) #\#))
         (make-rgb-color
          (/ (parse-integer string :start 1 :end 3 :radix 16) #xFF)
          (/ (parse-integer string :start 3 :end 5 :radix 16) #xFF)
          (/ (parse-integer string :start 5 :end 7 :radix 16) #xFF)))
        ((and (= (length string) 6) (every #'(lambda (x) (digit-char-p x 16)) string))
         (let ((r (parse-integer (subseq string 0 2) :radix 16))
               (g (parse-integer (subseq string 2 4) :radix 16))
               (b (parse-integer (subseq string 4 6) :radix 16)))
           (warn "Color malformed: ~S" string)
           (and r g b 
                (make-rgb-color (/ r 255) (/ g 255) (/ b 255)))))
        ((and (= (length string) 13) (char= (char string 0) #\#))
         (make-rgb-color
          (/ (parse-integer string :start 1 :end 5 :radix 16) #xFFFF)
          (/ (parse-integer string :start 5 :end 9 :radix 16) #xFFFF)
          (/ (parse-integer string :start 9 :end 13 :radix 16) #xFFFF)))
        ((and (setf sym (find-symbol (concatenate 'string "+" (string-upcase string) "+")
                                     (find-package :clim)))
              (boundp sym)
              (clim:colorp (symbol-value sym)))
         (symbol-value sym))
        (t
         (warn "~S: foo color: ~S." 'parse-x11-color string)
         +red+)))

;;;; ----------------------------------------------------------------------------------------------------

#+NIL
(define-closure-command com-reflow ()
  (window-clear (find-pane-named *frame* 'canvas))
  (let ((*medium* (find-pane-named *frame* 'canvas)))
    (let ((device (make-instance 'closure/clim-device::clim-device :medium *medium*)))
      (let ((closure-protocol:*document-language*
             (make-instance 'r2::html-4.0-document-language))
            (closure-protocol:*user-agent*
             nil))
        (r2::reflow)))))

(define-presentation-translator url-from-string
    (string url closure)
  (x)
  (url:parse-url x))

(define-presentation-method accept ((type url)
                                    stream
                                    (view (eql +textual-view+))
                                    &key default default-type)
  (url:parse-url (accept 'string :stream stream :prompt nil)))




(define-closure-command (com-clear-interactor :name t) ()
    (clim:window-clear (clim:frame-query-io clim:*application-frame*)))

;;;; ----------------------------------------------------------------------------------------------------


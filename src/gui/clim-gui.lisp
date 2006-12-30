;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLIM-USER; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: CLIM GUI
;;;   Created: 2002-07-22
;;;    Author: Gilbert Baumann <gilbert@base-engineering.com>
;;;   License: MIT style (see below)
;;;       $Id$
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2002 by Gilbert Baumann

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

;; $Log$
;; Revision 1.24  2006/12/30 15:07:31  emarsden
;; Minor improvements to user interface:
;;   - enable double buffering
;;   - wait until page has been downloaded before erasing previous page
;;   - enable busy cursor while downloading and rendering
;;
;; Revision 1.23  2006/12/29 17:37:07  dlichteblau
;; Make closure start on Gtkairo:
;;
;; 	* src/gui/clim-gui.lisp (WRITE-STATUS, FOO, COM-REDRAW): Replace
;;         calls to xlib:display-finish-output with
;;         clim-backend:port-force-output.
;;
;; Revision 1.22  2005/08/25 15:14:14  crhodes
;; OpenMCL support (from Dave Murray aka JQS)
;;
;; Revision 1.21  2005/08/25 15:05:48  crhodes
;; Work around problems related to *closure-inited-p* (see #lisp logs for
;; 2005-08-25 for more discussion).  Not clear where the fault lies: sbcl,
;; clx, mcclim[-freetype] or closure itself.
;;
;; Revision 1.20  2005/07/11 15:58:03  crhodes
;; Complete the renaming *MEDIUM* -> *PANE*.
;;
;; Panes are CLIM extended-streams, and remember output to them in output
;; records.  Mediums are much simpler, and don't have this kind of
;; memory.  So, though the same drawing functions (DRAW-TEXT, DRAW-LINE)
;; can have the same initial effect applied to a pane and a medium, the
;; output-record state is very different.
;;
;; Revision 1.19  2005/07/10 11:18:34  emarsden
;; Distinguish between pane and medium in the CLIM GUI. This should
;; fix image display.
;;
;; Revision 1.18  2005/07/10 10:57:20  emarsden
;; Move a number of global variables from the CL-USER to the GUI package.
;;
;; Revision 1.17  2005/04/12 10:28:55  tdalyjr
;; Since closure-frame-top-level is no longer used, comment it out.
;;
;; Use a :before method on run-frame-top-level to set
;; *closure-inited-p*. (this used to be done by closure-frame-top-level)
;;
;; Reimplement the quit command using frame-exit, since the 'closure-quit
;; catch tag no longer exists.
;;
;; Use process-interrupt from the clim-sys package, instead of from the
;; mp package, since clim-sys should be more portable.
;;
;; Turn on scrollbars for the interactor pane, since otherwise it trashes
;; the status line on the bottom of the window and stops updating.
;;
;; Revision 1.16  2005/03/13 21:17:28  emarsden
;;  - Implement PageUp and PageDown support in the CLIM GUI.
;;  - Add a Redraw command (with Ctrl-R accelerator)
;;
;; Revision 1.15  2005/03/13 21:15:06  emarsden
;; Add zoom support to the renderer, accessible via the commands com-zoom-in,
;; com-zoom-out and com-zoom-100%.
;;
;; Revision 1.14  2005/03/13 20:58:31  emarsden
;;  - Update to new McCLIM requirements on DEFINE-xx-COMMAND, adding :name t
;;    so that commands are available from listener pane
;;
;; Revision 1.13  2005/03/13 19:24:14  gbaumann
;; make it at least compile and show a window with CMUCL 19a and cvs mcclim.
;;
;; Revision 1.12  2005/03/13 18:01:37  gbaumann
;; Gross license change
;;
;; Revision 1.11  2003/06/15 17:24:24  gilbert
;; fixes to the recent patches.
;;
;; Revision 1.10  2003/06/15 16:47:44  gilbert
;; OpenMCL patches by Patrik Nordebo
;;
;; Revision 1.9  2003/03/16 17:46:19  gilbert
;; we call xlib:display-finish-output when a page is finished.
;;
;; Revision 1.8  2003/03/14 17:06:16  dan
;; replace defconstants for non-constant variables with defvar, to placate SBCL, which suffers from offensively ANSI behaviour with same
;;
;; Revision 1.7  2003/03/14 14:14:36  gilbert
;; adjusted frame-top-level loop
;;
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
(defvar *frame*)
(defvar *pane*)

(defvar *initial-url* nil)

(defvar *closure-process* nil)

(defclass closure-pane (application-pane)
  ())

;;; Crude History

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
    :incremental-redisplay t
    :double-buffering t
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
    :scroll-bars t :border nil)
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
                  (scrolling (:width 830 :height 600 :min-height 400 :max-height 20000
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
  ;; (:top-level (closure-frame-top-level . nil))
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

;;; This top level has been abandoned in favor of CLIM's built-in one,
;;; but let's keep it for a little while to pillage.  -- tpd 2005.4.9
;; (defmethod closure-frame-top-level
;;     ((frame application-frame)
;;      &key (command-parser 'command-line-command-parser)
;;      (command-unparser 'command-line-command-unparser)
;;      (partial-command-parser
;;       'command-line-read-remaining-arguments-for-partial-command)
;;      (prompt "Closure => "))
;;   (catch 'closure-quit
;;     (loop
;;         (with-simple-restart (forget "Just forget this command, restart the command loop.")
;;           (let ((*standard-input* (frame-standard-input frame))
;;                 (*standard-output* (frame-standard-output frame))
;;                 (*query-io* (frame-query-io frame))
;;                 (*pointer-documentation-output* (frame-pointer-documentation-output
;;                                                  frame))
;;                 ;; during development, don't alter *error-output*
;;                 ;; (*error-output* (frame-error-output frame))
;;                 (*command-parser* command-parser)
;;                 (*command-unparser* command-unparser)
;;                 (*partial-command-parser* partial-command-parser)
;;                 (prompt-style (make-text-style :sans-serif :bold :normal)))
;;             (let ((*application-frame* frame))
;;               (when *initial-url*
;;                 (com-visit-url *initial-url*))
;;               (setf *initial-url* nil)
;;               (setf *closure-inited-p* t)
;;               (when *standard-input*
;;                 (setf (cursor-visibility (stream-text-cursor *standard-input*)) t)
;;                 (when prompt
;;                   (with-text-style (*standard-input* prompt-style)
;;                     (if (stringp prompt)
;;                         (write-string prompt *standard-input*)
;;                         (funcall prompt *standard-input* frame))
;;                     (finish-output *standard-input*)))
;;                 (let ((command (read-frame-command frame)))
;;                   (fresh-line *standard-input*)
;;                   ;;(window-clear *standard-output*)
;;                   (clim:window-clear *query-io*)
;;                   (when command
;;                     (execute-frame-command frame command))
;;                   (fresh-line *standard-input*)))))))))

(define-presentation-type url ())
(define-presentation-type r2::pt ())
(define-presentation-type r2::hyper-link ())

;;;; ----------------------------------------------------------------------------------------------------
;;;; Commands
;;;;

(define-closure-command (com-show-listener :name t) ()
  (setf (sheet-enabled-p (sheet-parent (find-pane-named *application-frame* 'interactor))) t))

(define-closure-command (com-hide-listener :name t) ()
  (setf (sheet-enabled-p (sheet-parent (find-pane-named *application-frame* 'interactor))) nil))

(define-closure-command (com-visit-url :name t) ((url 'url)) ;;; :gesture :select))
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

(define-closure-command (com-reflow :name t) ()
  (reflow))

(define-closure-command (com-back :name t) ()
  (let ((*standard-output* *query-io*)) 
    (cond ((null (cdr *back-history*))
           (format t "There is nowhere you can go back to.~%"))
          (t
           (push (pop *back-history*) *forw-history*)
           (format t "Going back to ~S.~%" (first *back-history*))
           (foo (first *back-history*))))))

(define-closure-command (com-forward :name t) ()
  (let ((*standard-output* *query-io*)) 
    (cond ((null *forw-history*)
           (format t "There is nowhere you can go forward to.~%"))
          (t
           (push (pop *forw-history*) *back-history*)
           (format t "Going forward to ~S.~%" (first *back-history*))
           (foo (first *back-history*))))))

(define-closure-command (com-reload :name t) ()
  (let ((*standard-output* *query-io*)) 
    (cond ((null *back-history*)
           (format t "There is nothing to reload.~%"))
          (t
           (format t "Reloading ~S.~%" (first *back-history*))
           (foo (first *back-history*))))))

(define-closure-command (com-images-off :name t) ()
  (setf gui:*user-wants-images-p* nil)
  (format *query-io* "Images are now off.~%"))

(define-closure-command (com-images-on :name t) ()
  (setf gui:*user-wants-images-p* t)
  (format *query-io* "Images are now on. You may want to reload.~%"))

(define-closure-command (com-quit :name t) ()
  (frame-exit *application-frame*))

(defun make-google-search-url (string)
  (url:merge-url
   (url:make-url :query (list
                         (cons "hl" "en")
                         (cons "ie" "ISO-8859-1")
                         (cons "q" string)))
   (url:parse-url "http://www.google.com/search")))

(define-closure-command (com-reverse-search-google :name t) ((url 'url))
  (let ((*standard-output* *trace-output*))
    (com-visit-url
     (make-google-search-url (format nil "link:~A" url)))))

(define-closure-command (com-search-google :name t) ((what 'string))
  (com-visit-url (make-google-search-url what)))

(define-closure-command (com-home :name t) ()
  (com-visit-url gui:*home-page*))

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
    (clim-sys:process-interrupt *closure-process*
                          #'(lambda () (apply command args)))))


(defun closure:visit (&optional (url gui:*home-page*))
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

(defvar *closure-inited-p* nil)
(defmethod clim:read-frame-command :before ((frame closure)
                                               &key &allow-other-keys)
  (unless *closure-inited-p*
    (setf *closure-inited-p* t)))

(defun ensure-closure ()
  (with-closure ()
    (unless *closure-process*
      (setf *closure-inited-p* nil)
      (run-closure)
      (clim-sys:process-wait "Waiting for closure init" 
                             (lambda () *closure-inited-p*)))))

(defun run-closure ()
  ;; Care for proxy
  (let* ((proxy (glisp:getenv "http_proxy"))
         (url   (and proxy (url:parse-url proxy))))
    (cond ((and url
                (equal (url:url-protocol url) "http"))
           (format t "~:[~&;; Using HTTP proxy ~S port ~S~%~;~]"
                   (setf netlib::*use-http-proxy-p* t)
                   (setf netlib::*http-proxy-host* (url:url-host url))
                   (setf netlib::*http-proxy-port* (url:url-port url))))
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
  ;;
  (setf *closure-process*
        (clim-sys:make-process
         (lambda ()
           (unwind-protect
                (progn
                  (setf *frame* (make-application-frame 'closure))
                  (setf *pane*  (find-pane-named *frame* 'canvas))
                  (run-frame-top-level *frame*))
             (ignore-errors (ws/netlib::commit-cache))
             (setf *closure-process* nil)))
         :name "Closure")))

(defun write-status (string)
  (window-clear (find-pane-named *frame* 'status))
  (write-string string (find-pane-named *frame* 'status))
  (clim-backend:port-force-output (find-port)))

(defun foo (url)
  (let ((*standard-output* *trace-output*))
    (clim-sys:make-process
     (lambda ()
       (with-simple-restart (forget "Just forget rendering this page.")
         (let* ((*package* (find-package :r2))
                (*pane* (find-pane-named *frame* 'canvas))
                (*medium* (sheet-medium *pane*)))
           (progn ;; with-sheet-medium (*medium* *pane*)
             (let ((device (make-instance 'closure/clim-device::clim-device :medium *pane*)))
               (setf (sheet-pointer-cursor *pane*) :busy)
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
                                (make-instance 'r2::xml-style-document-language)))
                           (closure-protocol:*user-agent* nil)
                           (r2::*canvas-width* (bounding-rectangle-width (sheet-parent *pane*))))
                       (window-clear *pane*)
                       (closure-protocol:render
                        closure-protocol:*document-language* 
                        doc
                        device
                        (setf *current-pt* (r2::document-pt doc))
                        600           ;xxx width
                        t             ;?
                        0)
                       (let ((x2 (bounding-rectangle-max-x (stream-output-history *pane*)))
                             (y2 (bounding-rectangle-max-y (stream-output-history *pane*))))
                         (setf y2 (max y2 r2::*document-height*))
                         (clim:change-space-requirements *pane* :width x2 :height y2)
                         ;; While we are at it, force a repaint
                         (handle-repaint *pane* (sheet-region (pane-viewport *pane*)))
                         (clim-backend:port-force-output (find-port)))))))
               (setf (sheet-pointer-cursor *pane*) :default)
               (write-status "Done.")))))
       #+nil (clim-backend:port-force-output (find-port))))))

(defun reflow ()
  (let ((*standard-output* *trace-output*))
    (funcall ;;clim-sys:make-process
     (lambda ()
       (with-simple-restart (forget "Just forget rendering this page.")
         (let ((*package* (find-package :r2))
               (*pane* (find-pane-named *frame* 'canvas)))
           (window-clear *pane*)
           (with-sheet-medium (*medium* *pane*)
             (write-status "Rendering ...")
             (let ((closure-protocol:*document-language*
                    (if (sgml::pt-p (r2::document-pt *current-document*))
                        (make-instance 'r2::html-4.0-document-language)
                        (make-instance 'r2::xml-style-document-language) ))
                   (closure-protocol:*user-agent* nil)
                   (r2::*canvas-width*
                    (bounding-rectangle-width (sheet-parent *pane*))))
               (r2::reflow)
               (let ((x2 (bounding-rectangle-max-x (stream-output-history *pane*)))
                     (y2 (bounding-rectangle-max-y (stream-output-history *pane*))))
                 (setf y2 (max y2 r2::*document-height*))
                 (clim:change-space-requirements *pane* :width x2 :height y2)
                 ;; While we are at it, force a repaint
                 (handle-repaint *pane* (sheet-region (pane-viewport *pane*)))))
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


(define-closure-command (com-zoom-100% :name t) ()
  (setq gui:*zoom-factor* 1.0)
  (send-closure-command 'com-reflow))

;; FIXME the :shift here is a McCLIM bug
(define-closure-command (com-zoom-in :name t :keystroke (#\+ :control :shift)) ()
  (write-status "Zooming in...")
  (setq gui:*zoom-factor* (* gui:*zoom-factor* 1.2))
  (send-closure-command 'com-reflow))

(define-closure-command (com-zoom-out :name t :keystroke (#\- :control :shift)) ()
  (write-status "Zooming out...")
  (setq gui:*zoom-factor* (* gui:*zoom-factor* 0.8))
  (send-closure-command 'com-reflow))

(define-closure-command (com-page-up :name t
                                     :keystroke :prior) ()
  (let* ((pane (find-pane-named *frame* 'canvas))
         (scrollbar (slot-value (pane-scroller pane) 'climi::vscrollbar))
         (current-y (gadget-value scrollbar))
         (window-height (bounding-rectangle-height (pane-viewport-region pane))))
    (scroll-extent pane 0 (max (gadget-min-value scrollbar) (- current-y (* 0.9 window-height))))))

(define-closure-command (com-page-down :name t
                                       :keystroke :next) ()
  (let* ((pane (find-pane-named *frame* 'canvas))
         (scrollbar (slot-value (pane-scroller pane) 'climi::vscrollbar))
         (current-y (gadget-value scrollbar))
         (window-height (bounding-rectangle-height (pane-viewport-region pane))))
    (scroll-extent pane 0
                   (min (gadget-max-value scrollbar) (+ current-y (* 0.9 window-height))))))

(define-closure-command (com-redraw :name t :keystroke (#\r :control)) ()
  (let* ((*pane* (find-pane-named *frame* 'canvas)) )
    (handle-repaint *pane* (sheet-region (pane-viewport *pane*))))
  (clim-backend:port-force-output (find-port)))


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
;; Revision 1.1  2002/07/22 02:27:22  gilbert
;; Initial revision
;;

(in-package :CLIM-USER)
(use-package :clim)

;;;;;;;

(defvar *medium*)

(defclass closure-pane (sheet-multiple-child-mixin
                        application-pane)
  ())

(define-application-frame closure ()
  ()
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
    :width 200
    :background +black+
    :foreground +white+)
   (interactor :interactor :height 200 :min-height 50 :max-height 200)
   (back (make-pane 'push-button :label "Back"))
   (forward (make-pane 'push-button :label "Forward"))
   (stop (make-pane 'push-button :label "Stop"))
   (url-entry (make-pane 'text-field
               :value "http://www.w3.org/"
               :max-width +fill+
               :background +white+))
   (wholine
    :pointer-documentation :width 5 :max-width +fill+ :height 20
    :foreground +white+
    :background +black+)
   (menu-bar (climi::make-menu-bar 'menubar-command-table :height 25)))
  (:layouts
   (default
       (vertically ()
         menu-bar
         (horizontally ()
           (vertically ()
             (climi::scrolling (:width 830 :height 600 :min-height 400 :max-height 20000)
                               canvas)
             (vertically () 
               interactor))
           #+NIL
           (labelling (:label "Auxillary Pane")
             aux))
         (horizontally ()
           wholine
           2
           (200 status))))
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
  ;;(:top-level (closure-frame-top-level . nil))
  )

(make-command-table 'menubar-command-table
		    :errorp nil
		    :menu '(("File" :menu file-command-table)
                            ("Go"   :menu go-command-table)
                            ("Bookmarks"   :menu bookmarks-command-table)
                            ("View"     :menu view-command-table)
                            ("Appearance" :menu appearance-command-table)
                            ))

(make-command-table 'appearance-command-table :errorp nil
                    :menu '(("Show Listener" :command com-show-listener)
                            ("Hide Listener" :command com-hide-listener)))

(define-closure-command com-show-listener ()
  (setf (sheet-enabled-p (sheet-parent (find-pane-named *application-frame* 'interactor))) t))

(define-closure-command com-hide-listener ()
  (setf (sheet-enabled-p (sheet-parent (find-pane-named *application-frame* 'interactor))) nil))

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


(defparameter *shopping-list* nil)



(define-closure-command com-hide-aux-pane ()
  (setf (sheet-enabled-p (sheet-parent(find-pane-named *application-frame* 'aux))) nil))
(define-closure-command com-show-aux-pane ()
  (setf (sheet-enabled-p (sheet-parent(find-pane-named *application-frame* 'aux))) t))

(define-closure-command com-add-to-shopping-list ((url 'url))
  (setf *shopping-list* (append *shopping-list* (list url)))
  #+NIL
  (setf (pane-needs-redisplay (sheet-parent (find-pane-named *application-frame*
                                                             'aux)))
        t))

(defun aux-display (frame pane)
  (dolist (k *shopping-list*)
    (present k 'url)
    (terpri)))

(define-closure-command nop ()
  )

(defmethod closure-frame-top-level ((frame application-frame)
				       &key (command-parser 'command-line-command-parser)
				       (command-unparser 'command-line-command-unparser)
				       (partial-command-parser
					'command-line-read-remaining-arguments-for-partial-command)
				       (prompt "Command: "))
  (declare (ignore command-parser command-unparser partial-command-parser prompt))
  (clim-extensions:simple-event-loop))

(define-closure-command com-foo ()
  (let ((*standard-output* *trace-output*))
    (let #+CMU
      ((*standard-output* sys:*tty*)
       (*standard-input* sys:*tty*)
       (*debug-io* sys:*tty*)
       (*error-output* sys:*tty*)
       (*trace-output* sys:*tty*))
      #-CMU
      ()
      (foo)
      )))

(define-presentation-type url ())
(define-presentation-type r2::pt ())
(define-presentation-type r2::hyper-link ())

(define-closure-command com-visit-url ((url 'url :gesture :select))
  (let ((*standard-output* *query-io*)) ;;(find-pane-named *frame* 'interactor)))
    (with-text-style (*standard-output* (make-text-style :sans-serif :roman :normal))
      (format t "~%You are visiting "))
    (present url 'url)
    (with-text-style (*standard-output* (make-text-style :sans-serif :roman :normal))
      (format t ".~%")))
  (let ((*standard-output* *trace-output*))
    (foo url)))

(defun make-google-search-url (string)
  (url:unparse-url
   (url:merge-url
    (url:make-url :query (list
                          (cons "hl" "en")
                          (cons "ie" "ISO-8859-1")
                          (cons "q" string)))
    (url:parse-url "http://www.google.com/search"))))

(define-closure-command com-reverse-lookup ((url 'url))
  (let ((*standard-output* *trace-output*))
    (com-visit-url
     (make-google-search-url (format nil "link:~A" url)))))

(define-closure-command com-search-google ((what 'string))
  (com-visit-url (make-google-search-url what)))
  

(define-closure-command com-visit-string ((url 'string :gesture :select))
  (let ((*standard-output* *trace-output*))
    (foo url)))

(define-closure-command com-visit-hyper-link ((url 'r2::hyper-link :gesture :select))
  (let ((*standard-output* *trace-output*))
    (foo (r2::hyper-link-url url))))

(define-closure-command com-describe-pt ((pt 'r2::pt :gesture :describe))
  (print pt))

(define-closure-command com-quit ()
  (unix:unix-exit 0))

(defvar *frame*)
(defvar *pane*)

(defun foo-init ()
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
  ;;;+XXX
  (loop for port in climi::*all-ports*
      do (destroy-port port))
  (setq climi::*all-ports* nil)
  ;;;-XXX
  (setf *frame* (make-application-frame 'closure))
  (setf *pane*  (find-pane-named *frame* 'canvas))
  (funcall;;progn ;;clim-sys:make-process
   (lambda ()
     (run-frame-top-level *frame*))))

(defun write-status (string)
  (window-clear (find-pane-named *frame* 'status))
  (write-string string (find-pane-named *frame* 'status)))


(defun foo (&optional (url "file:/home/gilbert/work/closure/simple.html")) ;;http://127.1/~gilbert/tests/"))
  ;;
  (clim-sys:make-process
   (lambda ()
     (window-clear *pane*)
     (setf (gadget-value (find-pane-named *frame* 'url-entry)) url)
     (progn;;with-sheet-medium (medium *pane*)
       (let ((*medium* *pane*))
         (let ((device (make-instance 'closure/clim-device::clim-device :medium *medium*)))
           (baz device url))))
     (xlib:display-finish-output (clim-clx::clx-port-display (find-port))))))

(defvar *current-document*)

(defun baz (device url)
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
        (time
         (multiple-value-bind (x1 y1 x2 y2)
             (r2::render-pt
              device
              doc
              (r2::document-pt doc)
              700                       ;xxx width
              t                         ;?
              0)
           (clim:change-space-requirements *pane* :width x2 :height y2)))
        (write-status "Done.")
        '(describe doc)))))

(defun parse-x11-color (string &aux sym)
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

;;;;;

(defun invoke-for-all-links (cont &optional (doc *current-document*)
                                  &aux url)
  (sgml:map-pt (lambda (pt)
                 (cond ((and (eq (sgml:gi pt) :A)
                             (setf url (r2::pt-effective-url-attr doc pt :href)))
                        (funcall cont url))))
   (r2::document-pt doc)))

(defmacro over-all-links ((var) &body body)
  `(invoke-for-all-links (lambda (,var) .,body)))

(define-closure-command com-fetch-files-matching ((pattern 'string))
  (over-all-links (url)
                  (when (equal (url:url-extension url) pattern)
                    (print url))))

(define-closure-command com-search-clx-manual ((query 'string))
  (com-visit-url
   (url:unparse-url
    (url:merge-url
     (url:make-url :query (list
                           (cons "q" query)))
     (url:parse-url "http://127.1/clxman/doc-index.cgi")))))

(define-closure-command com-search-java-doc ((query 'string))
  (com-visit-url
   (url:unparse-url
    (url:merge-url
     (url:make-url :query (list
                           (cons "q" query)))
     (url:parse-url "http://127.1/~delly/javadoc/doc-index.pl")))))

(define-closure-command com-bar ()
  (clim-sys:make-process
   (lambda ()
     (run-frame-top-level (make-application-frame 'shopping-list)))))

;;;;

(define-application-frame shopping-list ()
  ()
  (:panes
   (shoping-list :application
    :width 400
    :height 600))
  (:layouts
   (:default
    shoping-list))
  (:top-level
   (shopping-list-top-level)))

(defmethod shopping-list-top-level ((frame application-frame) &key)
  (let ((*standard-input* (frame-standard-input frame))
	(*standard-output* (frame-standard-output frame))
	(*query-io* (frame-query-io frame)))
    (catch 'exit
      (clim-extensions:simple-event-loop))
    (frame-exit frame)))

;;;;;;;;

(defmethod clim:sheet-native-transformation ((sheet null)) clim:+identity-transformation+)
(defmethod clim:medium-sheet ((sheet sheet)) sheet)


;;;;;;;;

;; Now finally it would be good, if we had something like
;; SPACE-REQUIREMENT-BASELINE, so that we can align these gadgets at
;; the baseline. This however is not really sufficient, since the
;; baseline of a gadget depends on its assigned size.

(defmethod draw-gadget (sheet gadget x y)
  (let ((sq (compose-space gadget)))
    (allocate-space gadget
                    (space-requirement-width sq)
                    (space-requirement-height sq))
    (move-sheet gadget x y)
    (sheet-adopt-child sheet gadget)))

(define-closure-command bar ()
;;  (window-clear *pane*)
;;  (with-output-as-gadget *pane*
  (let ((gadget
         (make-pane-1 (frame-manager *application-frame*)
                      *application-frame*
                      'push-button :label "Click me")))
    (draw-gadget *pane* gadget 100 200)))


(define-presentation-translator url-from-string
    (string url closure)
  (x)
  (url:parse-url x))

(define-presentation-method accept ((type url)
                                    stream
                                    (view (eql +textual-view+))
                                    &key default default-type)
  (url:parse-url (accept 'string :stream stream :prompt nil)))


(define-closure-command com-search-google ((what 'string))
  (com-visit-url
   (url:merge-url
    (url:make-url :query `(("hl" . "en")
                           ("ie" . "ISO-8859-1")
                           ("q"  . ,string)))
    (url:parse-url "http://www.google.com/search"))))


(define-closure-command com-init-test ()
  (setf *test-urls*
        (mapcar (lambda (x)
                  (url:merge-url
                   (url:parse-url x)
                   (url:parse-url "http://www.w3.org/Style/CSS/Test/CSS1/current/")))
                '("test11.htm"
                  "test12.htm"
                  "test13.htm"
                  "test14.htm"
                  "test15.htm"
                  "test16.htm"
                  "test17.htm"
                  "test21.htm"
                  "test23.htm"
                  "test24.htm"
                  "test25.htm"
                  "test26.htm"
                  "test31.htm"
                  "test32.htm"
                  "test411.htm"
                  "test412.htm"
                  "test414.htm"
                  "test42.htm"
                  "test43.htm"
                  "test44.htm"
                  "test45.htm"
                  "test522.htm"
                  "test523.htm"
                  "test524.htm"
                  "test525.htm"
                  "test526.htm"
                  "test527.htm"
                  "test531.htm"
                  "test532.htm"
                  "test533.htm"
                  "test534.htm"
                  "test535.htm"
                  "test536.htm"
                  "test537.htm"
                  "test541.htm"
                  "test542.htm"
                  "test543.htm"
                  "test544.htm"
                  "test545.htm"
                  "test546.htm"
                  "test547.htm"
                  "test548.htm"
                  "test5501.htm"
                  "test5501b.htm"
                  "test5502.htm"
                  "test5502b.htm"
                  "test5503.htm"
                  "test5503b.htm"
                  "test5504.htm"
                  "test5504b.htm"
                  "test5505.htm"
                  "test5505b.htm"
                  "test5506.htm"
                  "test5506b.htm"
                  "test5507.htm"
                  "test5507b.htm"
                  "test5508.htm"
                  "test5508b.htm"
                  "test5509.htm"
                  "test5509b.htm"
                  "test5510.htm"
                  "test5510b.htm"
                  "test5511.htm"
                  "test5511b.htm"
                  "test5512.htm"
                  "test5512b.htm"
                  "test5513.htm"
                  "test5513b.htm"
                  "test5514.htm"
                  "test5514b.htm"
                  "test5515.htm"
                  "test5515b.htm"
                  "test5516.htm"
                  "test5516b.htm"
                  "test5517.htm"
                  "test5517b.htm"
                  "test5518.htm"
                  "test5518b.htm"
                  "test5519.htm"
                  "test5519b.htm"
                  "test5520.htm"
                  "test5520b.htm"
                  "test5521.htm"
                  "test5521b.htm"
                  "test5522.htm"
                  "test5522b.htm"
                  "test5523.htm"
                  "test5524.htm"
                  "test5525.htm"
                  "test5525b.htm"
                  "test5525c.htm"
                  "test5525d.htm"
                  "test5526.htm"
                  "test5526b.htm"
                  "test5526c.htm"
                  "test561.htm"
                  "test562.htm"
                  "test563.htm"
                  "test564.htm"
                  "test565.htm"
                  "test566.htm"
                  "test61.htm"
                  "test62.htm"
                  "test63.htm"
                  "test64.htm"
                  "test71.htm"))))

(define-closure-command com-next-test ()
  (com-visit-url (pop *test-urls*)))



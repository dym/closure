
;;;;
;;;; Last minute patches
;;;;


;;;; ----------------------------------------------------------------------------------------------------

(in-package :clim-clx)

(defclass clx-medium (basic-medium)
  ())

;; "So einfach und doch so schnell ..."

;; Only problem: clipping ;( we want to cache regions to gcontexts and
;; setup clipping by copying.

;; Further: split into line gcontexts and text-gcontext or cope with
;; don't care. A rete network might be now bad idea

(defmethod (setf medium-text-style) :before (text-style (medium clx-medium)))
(defmethod (setf medium-line-style) :before (line-style (medium clx-medium)))
(defmethod (setf medium-clipping-region) :after (region (medium clx-medium)))

(eval-when (compile eval load)
  (fmakunbound 'medium-gcontext))

(defvar *gc-hash* (make-hash-table :test #'equal))

(defun medium-gcontext (medium ink)
  (with-slots (climi::foreground climi::background line-style text-style climi::clipping-region) medium
    (let ((foreground climi::foreground)
          (background climi::background)
          (clipping-region climi::clipping-region))
      (let* ((key (list foreground background line-style text-style ink :clipping-region))
             (gc
              (or (gethash key *gc-hash*)
                  (setf (gethash key *gc-hash*)
                        (funcall 'make-medium-gcontext*
                                 medium foreground background
                                 line-style text-style ink
                                 clipping-region)))))
        (cond ((region-equal clipping-region +nowhere+)
               )
              ((region-equal clipping-region +everywhere+)
               (setf (xlib:gcontext-clip-mask gc :unsorted) :none))
              (t
               (let ((rect-seq (clipping-region->rect-seq clipping-region)))
                 (when rect-seq
                   #+nil
                   ;; ok, what McCLIM is generating is not :yx-banded... (currently at least)
                   (setf (xlib:gcontext-clip-mask gc :yx-banded) rect-seq)
                   #-nil
                   ;; the region code doesn't support yx-banding...
                   ;; or does it? what does y-banding mean in this implementation?
                   ;; well, apparantly it doesn't mean what y-sorted means
                   ;; to clx :] we stick with :unsorted until that can be sorted out
                   (setf (xlib:gcontext-clip-mask gc :unsorted) rect-seq)))))
        gc))))

(defmethod make-medium-gcontext* (medium foreground background line-style text-style (ink color) clipping-region)
  (let* ((drawable (sheet-mirror (medium-sheet medium)))
         (port (port medium)))
    (let ((gc (xlib:create-gcontext :drawable drawable)))
      (setf (xlib:gcontext-font gc) (text-style-to-X-font port text-style)
            (xlib:gcontext-foreground gc) (X-pixel port ink)
            )
      gc)))

(defmethod make-medium-gcontext* (medium foreground background line-style text-style (ink (eql +flipping-ink+)) clipping-region)
  (let* ((gc (make-medium-gcontext* medium foreground background line-style text-style +black+ clipping-region))
         (port (port medium))
	 (flipper (logxor (X-pixel port (medium-foreground medium))
			  (X-pixel port (medium-background medium)))))
    ;; Now, (logxor flipper foreground) => background
    ;; (logxor flipper background) => foreground
    (setf (xlib:gcontext-function gc) boole-xor)
    (setf (xlib:gcontext-foreground gc) flipper)
    (setf (xlib:gcontext-background gc) flipper)
    gc))

(defmethod make-medium-gcontext* (medium foreground background line-style text-style (ink (eql +foreground-ink+)) clipping-region)
  (make-medium-gcontext* medium foreground background line-style text-style foreground clipping-region))

(defmethod make-medium-gcontext* (medium foreground background line-style text-style (ink (eql +background-ink+)) clipping-region)
  (make-medium-gcontext* medium foreground background line-style text-style background clipping-region))

;;;;;

(defmethod initialize-clx ((port clx-port))
  (let ((options (cdr (port-server-path port))))
    (setf (clx-port-display port)
	  #+cmucl
	  (xlib:open-display (getf options :host "") :display (getf options :display-id 0))
	  #+sbcl
	  (xlib:open-default-display))
    (progn
      #+NIL
      (setf (xlib:display-error-handler (clx-port-display port))
        #'clx-error-handler)
      )
    
    (setf (clx-port-screen port) (nth (getf options :screen-id 0)
				      (xlib:display-roots (clx-port-display port))))
    (setf (clx-port-window port) (xlib:screen-root (clx-port-screen port)))

    (make-graft port)
    (when clim-sys:*multiprocessing-p*
      (setf (port-event-process port)
        (clim-sys:make-process
         (lambda ()
           (loop
             (with-simple-restart
                 (restart-event-loop
                  "Restart CLIM's event loop.")
               (loop
                 (process-next-event port)))))
         :name (format nil "~S's event process." port)))) ))


;;;; ----------------------------------------------------------------------------------------------------

(in-package :climi)

(defmethod clim:sheet-native-transformation ((sheet null)) clim:+identity-transformation+)
(defmethod clim:medium-sheet ((sheet sheet)) sheet)


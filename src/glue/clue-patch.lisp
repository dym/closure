;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLUEI; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: CLUE patches
;;;   Created: 1999-05-15 08:08
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1999 by Gilbert Baumann
;;;
;;; Since these are only small diffs to the original CLUE functions,
;;; the original copyright applies as well:
;;;
;;; Copyright (C)1987,1988,1989,1990 Texas Instruments Incorporated.
;;;
;;; Permission is granted to any individual or institution to use, copy, modify,
;;; and distribute this software, provided that this complete copyright and
;;; permission notice is maintained, intact, in all copies and supporting
;;; documentation.
;;;
;;; Texas Instruments Incorporated provides this software "as is" without
;;; express or implied warranty.
;;;
(in-package :CLUEI)

;; from src/clue/shells.lisp
(defmethod shell-mapped :before ((shell shell))
  ;; Place up front when mapped, since the  window manager cannot
  ;; intervene to inform stacking priority.
  ;; This sucks considerable -- GB
  '(setf (window-priority shell) :above))

;; only do it on override-shell shells
;; all other shells the window manager is reponsible for.
(defmethod shell-mapped :before ((shell override-shell))
  ;; Place up front when mapped, since the  window manager cannot
  ;; intervene to inform stacking priority.
  ;; DONT DO IT AT ALL!
  '(setf (window-priority shell) :above))

;; from src/clue/intrinsics.lisp
(defmethod initialize-instance :after ((self basic-contact)
				       &rest initargs
				       &key resource-table defaults
				       &allow-other-keys)  
  (with-slots (name display parent event-translations event-mask initialization callbacks) self

    ;; Initialize and save initial values for slot resources
    (setf initialization
	  (initialize-resource-slots self resource-table defaults))
    ;; Copy initial callback list, because this is destructively modified by add/delete-callback.
    (setf callbacks (copy-tree callbacks))

    ;; Save initial values for non-slot resources
    (let ((options (copy-list initargs)))
      ;; Allow resource-table to be GC'd
      (remf options :resource-table)     
      (setf initialization
	    (nconc initialization options)))

    ;; Initialize and save initial values for constraint resources
    (when parent
      (setf initialization
	    (nconc initialization
		   (setf (contact-constraints self)
			 (initialize-constraints parent initargs resource-table)))))
      
    ;; Initialize name to class name by default
    (when (eq name :unnamed)
      (setf name (class-name-of self)))
    
    ;; Parse event-translations
    (setf event-translations
	  (mapcar #'(lambda (et) (parse-event-translation (first et) (rest et)))
		  event-translations)
	  event-mask
	  (xlib::encode-event-mask event-mask))

    ;; Add to composition hierarchy
    (when parent ; root contacts don't have a parent
      (setf display (contact-display parent)))
    (initialize-contact self)
    (when parent
      (add-to-parent self))))

;; new

(defmethod initialize-contact ((contact basic-contact))
  nil)

;; from src/clue/events.lisp
(defmethod dispatch-event ((event event) event-key send-event-p sequence (contact contact))
  ;; Called from PROCESS-NEXT-EVENT to filter events and call event handlers.
  (declare (type event   event)
	   (type keyword event-key)
	   (type boolean send-event-p)
	   (type card16  sequence)
	   (type contact contact))
  (declare (inline sensitive-p))
  
  (with-slots ((event_key key)
	       (event-sequence sequence)
	       (event-send-event-p send-event-p)
	       (event-contact contact)) (the event event)
    (setf event_key event-key
	  event-send-event-p send-event-p
	  event-sequence sequence
	  event-contact contact))

  (let ((class (class-name-of contact)))
    ;;
    ;; Check for non-contact event drawables.
    ;;
    (if (or (eq class 'window) (eq class 'pixmap))
	
	(handle-event (display-root (drawable-display contact)) event)
	
	(if (destroyed-p contact)
	    
	    ;; Destroyed-contact!
	    (when (eq event-key :destroy-notify)
	      (destroy-finish contact))
	    
	    ;; Bind event for reference within with-event forms
	    (let ((display (slot-value contact 'display))
		  ($event$ event))
	      (declare (special $event$))
	      
	      ;;
	      ;; Do key translation
	      ;;
	      (when (or (eq event-key :key-press)
			(eq event-key :key-release))
		(with-slots (keysym character code state) (the event event)
		  (let ((keysym-index (default-keysym-index display code state)))
		    (setf keysym (keycode->keysym display code keysym-index)
			  character (keycode->character display code state :keysym-index keysym-index)))))
	      ;;
	      ;; Call the before event handlers
	      ;;
	      (dolist (before-action (before-actions display))
		(when (subtypep class (first before-action))
		  (call-action-internal contact (rest before-action))))
	      ;;
	      ;; Handle insensitive contacts
	      ;;
	      (when (and (member event-key *sensitive-events* :test #'EQ)
			 (not (sensitive-p contact)))
		(return-from dispatch-event nil))
	      
	      ;;
	      ;; Handle modes 
	      ;;
	      (let ((modes (display-mode-stack display)))
		(when (and modes (not (contact-mode contact)))
		  (when
		    (or (member event-key *restrict-events* :test #'eq)
			(and (member event-key *remap-events* :test #'eq)
			     (dolist (mode modes t) ;; Search for first :spring-loaded mode
			       (when (eq (second mode) :spring-loaded)
				 (format t "~%Remapping ~s from ~s to ~s" event-key contact (first mode)) ;; *** DEBUG ***
				 (setq contact (first mode)) ;; Remap contact
				 (return nil)))))
		    ;; Call mode action on for first :exclusive or :spring-loaded mode
		    (dolist (mode modes)
		      (unless (eq (second mode) :non-exclusive)
			(call-action-internal (first mode) (cddr mode))
			;; quit
			(return-from dispatch-event nil))))))
	      
	      ;; 
	      ;; Handle event compression
	      ;;
	      (with-slots ((contact-compress-motion compress-motion)
			   (contact-compress-exposures compress-exposures))
			  (the contact contact)
		(case event-key
		  (:exposure
                   ;; we always compress exposure events now, we even combine multiple exposure requests
                   
                   (let ((region (with-slots (x y width height) event
                                   (gu:make-rectangle* x y (+ x width) (+ y height)))))
                     
                     (event-case (display :force-output-p nil :discard-p nil :peek-p t :timeout 0)
                       (:exposure (window x y width height)
                          (when (eq window contact)
                            (setf region (gu:region-union (gu:make-rectangle* x y (+ x width) (+ y height)) region))
                            (discard-current-event display)
                            nil))
                       (otherwise
                        nil))
                     ;; for now we deliver that directly
                     (display-region contact region) 
                     (return-from dispatch-event nil);skip handle-event below
                     ))
		  
		  (:motion-notify		; Check for motion compression
		   (when (eq contact-compress-motion :on)
		     (let ((count 0))
		       
		       ;; Count consecutive :motion-notify's currently in queue
		       (event-case (display :force-output-p nil :peek-p t :timeout 0)
			 (:motion-notify (window)
					 (not (and (eq window contact) (incf count))))
			 (otherwise ()   t))
		       
		       (when (plusp count) 
			 ;; Remove all but last and quit immediately
			 (do () ((zerop (decf count)))
			   (event-case (display :timeout 0)
			     (otherwise ()   t)))
			 (return-from dispatch-event nil)))))))
	      ;;
	      ;; Handle event translations
	      ;;
	      (handle-event contact event))))))

(import 'display-region :clue)
(export 'display-region :clue)

(defmethod display-region ((contact contact) region)
  ;; default display-region method
  (gu:map-region-rectangles
   (lambda (x1 y1 x2 y2)
     (display contact x1 y1 (- x2 x1) (- y2 y1)))
   region))


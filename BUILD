#! /bin/sh
lisp << EOF
(in-package :cl-user)

;;(gc-off)

(setf *print-case* :upcase)

(setf *features* (delete :no-clx *features* :test #'eq))

(or
 (ignore-errors (require :clx))
 (ignore-errors (require :cmucl-clx)))

(unless (member :clx *features*)
  (error "CLX was not loaded successfully."))

(ignore-errors (require :gray-streams))

(unless (find-class 'ext::fundamental-stream)
  (error "The graystreams was not loaded successfully."))

;; Switch off bullshit
(setf *TOP-LEVEL-AUTO-DECLARE* nil)
(setf *GC-VERBOSE* nil)
(setf *LOAD-VERBOSE* nil)
(setf *LOAD-PRINT* nil)

;;;; --------------------------------------------------------------------------------

;;;; fixes

;;
;; SIGINTs are now delivered to the "main thread"
;;

(in-package "MULTIPROCESSING")

(defvar *top-level-loop* nil)

(defun startup-idle-and-top-level-loops ()
  "Enter the idle loop, starting a new process to run the top level loop.
  The awaking of sleeping processes is timed better with the idle loop process
  running, and starting a new process for the top level loop supports a
  simultaneous interactive session. Such an initialisation will likely be the
  default when there is better MP debug support etc."
  (assert (eq *current-process* *initial-process*) ()
	  "Only the *initial-process* is intended to run the idle loop")
  (init-multi-processing)               ; Initialise in case MP had been shutdown.
  ;; Start a new Top Level loop.
  (setq *top-level-loop*
        (make-process #'top-level :name "Top Level Loop") )
  ;; start the listener
  (ignore-errors (eval (read-from-string "(telnet::start :port 60666)")))

  ;; Enter the idle loop.
  (idle-process-loop))

(in-package "UNIX")

(defun my-sigint-handler (signal code scp)
  (declare (ignore signal code)
           (type system-area-pointer scp)
           (optimize (inhibit-warnings 3)))
  (if mp::*top-level-loop*
      (let ((adr (with-alien ((scp (* sigcontext) scp))
                   (sap-int (vm:sigcontext-program-counter scp)))))
        (mp:process-interrupt mp::*top-level-loop*
                              (lambda ()
                                (break "Interrupted at #x~x."
                                       adr))))
      (break "Interrupted at #x~x." adr)))

(setf ext:*after-save-initializations*
      (append ext:*after-save-initializations*
              (list (lambda ()
                      (enable-interrupt :sigint #'my-sigint-handler))
                    ;; mp::start-sigalrm-yield -- not reliable enough :-(
                    )))

(in-package "CL-USER")


;;;; --------------------------------------------------------------------------------

(load "closure.system") 

(oos :clim :compile)
(oos :clim-clx :compile)
(oos :closure :compile)

(setf ext:*herald-items*
      (list* :clim (list (format nil "    Closure (loaded ~D-~2,'0D-~2,'0D)"
                                 (nth-value 5 (decode-universal-time (get-universal-time)))
                                 (nth-value 4 (decode-universal-time (get-universal-time)))
                                 (nth-value 3 (decode-universal-time (get-universal-time)))))
             ext:*herald-items*))

;;;; Dump

;; Enable the garbage collector.  But first fake it into thinking that
;; we don't need to garbage collect.  The save-lisp is going to call
;; purify so any garbage will be collected then.
(setf lisp::*need-to-collect-garbage* nil)
;; (ext:gc-on)
;;
;; Save the lisp.
(setf lisp::*internal-real-time-base-seconds* nil)
(ext:save-lisp "lisp.core"
               :init-function 'mp::startup-idle-and-top-level-loops)
(ext:quit)
EOF

;;; -*- Mode:Lisp; Package:USER; Syntax:COMMON-LISP; Base:10; Lowercase:T -*-

;;;
;;;			 TEXAS INSTRUMENTS INCORPORATED
;;;				  P.O. BOX 149149
;;;			       AUSTIN, TEXAS 78714-9149
;;;
;;; Copyright (C)1988,1989,1990 Texas Instruments Incorporated.
;;;
;;; Permission is granted to any individual or institution to use, copy, modify,
;;; and distribute this software, provided that this complete copyright and
;;; permission notice is maintained, intact, in all copies and supporting
;;; documentation.
;;;
;;; Texas Instruments Incorporated provides this software "as is" without
;;; express or implied warranty.
;;;

(in-package "USER")

;; Ensure VALUES is a legal declaration
(proclaim '(declaration values))

;; Ensure *features* knows about CLOS and PCL
(when (find-package 'pcl)
  (pushnew :pcl  *features*)
  (pushnew :clos *features*))

(when (find-package 'clos)
  (pushnew :clos *features*))

;; Ensure *features* knows about the Common Lisp Error Handler
(when (find-package 'conditions)
  (pushnew :cleh *features*))

;;
;; Check required packages
;;
(unless (find-package "XLIB")
  #-explorer
  (error "CLX must be loaded before making CLUE.")
  #+explorer
  (make-system 'clx :noconfirm))

(eval-when (compile load eval)
  (unless (find-package "COMMON-LISP")
    (rename-package "LISP" "LISP" '("COMMON-LISP"))
) )

(eval-when (compile load eval)
(assert
  (find-package "COMMON-LISP") ()
  "COMMON-LISP package does not exist. 

 Please create a package named COMMON-LISP which exports CLOS and CLCS.
 (You may need to make this a nickname of the LISP package.)
")
)
 
;;
;; Make packages
;;
;;
;; Internal interfaces...
#+CLISP
(lisp:in-package "CLUEI" :use '(common-lisp xlib))
#-CLISP
(in-package "CLUEI" :use '(common-lisp xlib))

;; External interfaces...
#+CLISP
(lisp:in-package "CLUE" :use '(common-lisp xlib cluei))
#-CLISP
(in-package "CLUE" :use '(common-lisp xlib cluei))

;;
;; Ensure CLUE and CLUEI use a CLOS
;;
(in-package "LISP")
(eval-when (compile load eval)
(cond ((find-symbol "DEFCLASS" 'cluei)) ;; clos-kludge must be around      

      ((find-package "CLOS")
       (funcall 'use-package "CLOS" "CLUEI")
       (funcall 'use-package "CLOS" "CLUE"))

      ((find-package "PCL")
       (funcall 'use-package "PCL" "CLUEI")
       (funcall 'use-package "PCL" "CLUE"))

      (t (error "CLOS must be loaded before making CLUE.")))
)

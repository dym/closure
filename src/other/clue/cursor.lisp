;;; -*- Mode:Lisp; Package:CLUEI; Syntax:COMMON-LISP; Base:10; Lowercase:T -*-

;;;
;;;			 TEXAS INSTRUMENTS INCORPORATED
;;;				  P.O. BOX 149149
;;;			       AUSTIN, TEXAS 78714-9149
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

(in-package "CLUEI")

(export '(
	  x-cursor
	  arrow-cursor
	  based-arrow-down-cursor
	  based-arrow-up-cursor
	  boat-cursor
	  bogosity-cursor
	  bottom-left-corner-cursor
	  bottom-right-corner-cursor
	  bottom-side-cursor
	  bottom-tee-cursor
	  box-spiral-cursor
	  center-ptr-cursor
	  circle-cursor
	  clock-cursor
	  coffee-mug-cursor
	  cross-cursor
	  cross-reverse-cursor
	  crosshair-cursor
	  diamond-cross-cursor
	  dot-cursor
	  dotbox-cursor
	  double-arrow-cursor
	  draft-large-cursor
	  draft-small-cursor
	  draped-box-cursor
	  exchange-cursor
	  fleur-cursor
	  gobbler-cursor
	  gumby-cursor
	  hand1-cursor
	  hand2-cursor
	  heart-cursor
	  icon-cursor
	  iron-cross-cursor
	  left-ptr-cursor
	  left-side-cursor
	  left-tee-cursor
	  leftbutton-cursor
	  ll-angle-cursor
	  lr-angle-cursor
	  man-cursor
	  middlebutton-cursor
	  mouse-cursor
	  pencil-cursor
	  pirate-cursor
	  plus-cursor
	  question-arrow-cursor
	  right-ptr-cursor
	  right-side-cursor
	  right-tee-cursor
	  rightbutton-cursor
	  rtl-logo-cursor
	  sailboat-cursor
	  sb-down-arrow-cursor
	  sb-h-double-arrow-cursor
	  sb-left-arrow-cursor
	  sb-right-arrow-cursor
	  sb-up-arrow-cursor
	  sb-v-double-arrow-cursor
	  shuttle-cursor
	  sizing-cursor
	  spider-cursor
	  spraycan-cursor
	  star-cursor
	  target-cursor
	  tcross-cursor
	  top-left-arrow-cursor
	  top-left-corner-cursor
	  top-right-corner-cursor
	  top-side-cursor
	  top-tee-cursor
	  trek-cursor
	  ul-angle-cursor
	  umbrella-cursor
	  ur-angle-cursor
	  watch-cursor
	  i-bar-cursor))

(defparameter x-cursor                    0)
(defparameter arrow-cursor                2)
(defparameter based-arrow-down-cursor	 4)
(defparameter based-arrow-up-cursor	 6)
(defparameter boat-cursor		 8)
(defparameter bogosity-cursor		 10)
(defparameter bottom-left-corner-cursor	 12)
(defparameter bottom-right-corner-cursor	 14)
(defparameter bottom-side-cursor		 16)
(defparameter bottom-tee-cursor		 18)
(defparameter box-spiral-cursor		 20)
(defparameter center-ptr-cursor		 22)
(defparameter circle-cursor		 24)
(defparameter clock-cursor		 26)
(defparameter coffee-mug-cursor		 28)
(defparameter cross-cursor		 30)
(defparameter cross-reverse-cursor        32)
(defparameter crosshair-cursor            34)
(defparameter diamond-cross-cursor        36)
(defparameter dot-cursor                  38)
(defparameter dotbox-cursor               40)
(defparameter double-arrow-cursor         42)
(defparameter draft-large-cursor          44)
(defparameter draft-small-cursor          46)
(defparameter draped-box-cursor           48)
(defparameter exchange-cursor             50)
(defparameter fleur-cursor                52)
(defparameter gobbler-cursor              54)
(defparameter gumby-cursor                56)
(defparameter hand1-cursor                58)
(defparameter hand2-cursor                60)
(defparameter heart-cursor                62)
(defparameter icon-cursor                 64)
(defparameter iron-cross-cursor           66)
(defparameter left-ptr-cursor             68)
(defparameter left-side-cursor            70)
(defparameter left-tee-cursor             72)
(defparameter leftbutton-cursor           74)
(defparameter ll-angle-cursor             76)
(defparameter lr-angle-cursor             78)
(defparameter man-cursor                  80)
(defparameter middlebutton-cursor         82)
(defparameter mouse-cursor                84)
(defparameter pencil-cursor               86)
(defparameter pirate-cursor               88)
(defparameter plus-cursor                 90)
(defparameter question-arrow-cursor       92)
(defparameter right-ptr-cursor            94)
(defparameter right-side-cursor           96)
(defparameter right-tee-cursor            98)
(defparameter rightbutton-cursor          100)
(defparameter rtl-logo-cursor             102)
(defparameter sailboat-cursor             104)
(defparameter sb-down-arrow-cursor        106)
(defparameter sb-h-double-arrow-cursor    108)
(defparameter sb-left-arrow-cursor        110)
(defparameter sb-right-arrow-cursor       112)
(defparameter sb-up-arrow-cursor          114)
(defparameter sb-v-double-arrow-cursor    116)
(defparameter shuttle-cursor              118)
(defparameter sizing-cursor               120)
(defparameter spider-cursor               122)
(defparameter spraycan-cursor             124)
(defparameter star-cursor                 126)
(defparameter target-cursor               128)
(defparameter tcross-cursor               130)
(defparameter top-left-arrow-cursor       132)
(defparameter top-left-corner-cursor      134)
(defparameter top-right-corner-cursor     136)
(defparameter top-side-cursor             138)
(defparameter top-tee-cursor              140)
(defparameter trek-cursor                 142)
(defparameter ul-angle-cursor             144)
(defparameter umbrella-cursor             146)
(defparameter ur-angle-cursor             148)
(defparameter watch-cursor                150)
(defparameter i-bar-cursor                152)

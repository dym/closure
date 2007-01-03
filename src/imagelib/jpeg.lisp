;;; jpeg.lisp -- render JPEG files in Closure
;;;
;;; Author: Eric Marsden <eric.marsden@free.fr>
;;
;;
;; This will soon be replaced by an implementation based on cl-jpeg. 

(in-package :imagelib)


(defun jpeg-stream->aimage (input)
  (any->aimage-by-filter "djpeg" input))


;; EOF

(defpackage :imagelib
  (:use :glisp)
  (:export
   #:aimage
   #:aimage-width
   #:aimage-height
   #:aimage-data
   #:aimage-alpha-p
   #:aimage-plist
   #:make-aimage
   #:scale-aimage
   #:pnm-stream->aimage))

(defpackage :imagelib.gif
  (:use :glisp :imagelib))

(defpackage :png
  (:use :glisp :imagelib)
  (:export #:png-stream->aimage))


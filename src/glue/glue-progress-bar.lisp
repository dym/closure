;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: GLUE; -*-

(in-package :GLUE)

;;; ---------------------------------------------------------------------------
;;;  Progress Bar Contact
;;;

(clue:defcontact progress-bar (3d)
  ((value :initarg :value :initform 0 :reader progress-bar-value)
   (foreground :initarg :foreground))
  (:Resources
   (foreground
    :type xlib:pixel
    :initform "#00C")) )

(defmethod (setf progress-bar-value) (new-value (self progress-bar))
  (with-slots (value) self
    (check-type new-value real)
    (unless (= value new-value)
      (setf value new-value)
      (when (clue:realized-p self)
        (clue:display-region self +everywhere+)))
    new-value))

(defmethod clue:preferred-size ((self progress-bar) &key width height border-width)
  (declare (ignore border-width))
  (let ((w2 (clue:contact-width self))
        (h2 (clue:contact-height self)))
    (values
     (or width
         (and (not (zerop w2)) w2)
         (+ (left-leading self) (right-leading self) 100))
     (or height
         (and (not (zerop h2)) h2)
         (+ (top-leading self) (bottom-leading self) 20))
     0)))

(defmethod clue:display-region ((self progress-bar) region)
  (declare (ignore region))
  (with-slots (value foreground) self
    (multiple-value-bind (x0 y0 x1 y1) (interior-rectangle* self)
      (let ((p (floor (* (min 1 (max 0 value)) (- x1 x0)))))
        (xlib:clear-area self
                         :x (max 0 (+ x0 p))
                         :y (max 0 y0)
                         :width (max 1 (- x1 (+ x0 p) -1))
                         :height (max 1 (- y1 y0 -1))
                         :exposures-p nil)
        (old-draw-rectangle* self x0 y0 (+ x0 p) y1
                         :filled-p t 
                         :foreground foreground)
        (draw-glyphs self (convert self *default-font* 'xlib:font)
                     (floor (- x1 x0) 2)
                     (- y1 3)
                     (format nil "~D%" (floor (* value 100)))) ))))




;;;;
;;;; Last minute patches
;;;;

;; from http://paste.lisp.org/display/6063, to allow SBCL to display
;; non-ASCII characters via CLX
(in-package :clim-clx)

(defun translate (src src-start src-end afont dst dst-start)
  (let ((min-char-index (xlib:font-min-char afont))
        (max-char-index (xlib:font-max-char afont)))
    (if (stringp src)
        (loop for i from src-start below src-end
              for j from dst-start
              for index = (char-code (aref src i))
              while (<= min-char-index index max-char-index)
              do (setf (aref dst j) index)
              finally (return i))
        (loop for i from src-start below src-end
              for j from dst-start
              for index = (if (characterp (aref src i)) (char-code (aref src i)) (aref src i))
              while (<= min-char-index index max-char-index)
              do (setf (aref dst j) index)
              finally (return i)))))

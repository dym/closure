;;; gif.lisp --- render GIF files in Closure
;;;
;;; Author: Eric Marsden <eric.marsden@free.fr>
;;
;;
;; Read GIF files using the Skippy library
;; (http://www.xach.com/lisp/skippy/) and convert them to Closure's
;; internal AIMAGE format. 


(in-package :imagelib)

(defgeneric flexi-stream-from (thing))

(defmethod flexi-stream-from ((thing cl:pathname))
  (let ((data (make-array 1024 :element-type '(unsigned-byte 8)
                          :fill-pointer 0
                          :adjustable t)))
    (with-open-file (in thing :direction :input
                        :element-type '(unsigned-byte 8))
      (loop :for b = (read-byte in nil nil)
            :while b
            :do (vector-push-extend b data)))
    (flexi-streams:make-in-memory-input-stream data)))

(defmethod flexi-stream-from ((gstream glisp:gstream))
  (let ((data (make-array 1024 :element-type '(unsigned-byte 8)
                          :fill-pointer 0
                          :adjustable t)))
    (loop :for b = (g/read-byte gstream nil nil)
          :while b
          :do (vector-push-extend b data))
    (g/close gstream)
    (flexi-streams:make-in-memory-input-stream data)))

(defmethod flexi-stream-from ((stream flexi-streams:flexi-stream))
  stream)

(defmethod flexi-stream-from ((stream flexi-streams:in-memory-stream))
  stream)


(defun gif-stream->aimage (stream)
  (let* ((data-stream (skippy:read-data-stream (flexi-stream-from stream)))
         (image (skippy:last-image data-stream))
         (transparent-index (skippy:transparency-index image))
         (gif-color-table (skippy:color-table data-stream))
         (aimage (make-aimage (skippy:width image)
                              (skippy:height image) :alpha-p transparent-index))
         (aimage-data (aimage-data aimage)))
    (dotimes (x (skippy:width image))
      (dotimes (y (skippy:height image))
        (multiple-value-bind (r g b a)
            (let ((color-index (skippy:pixel-ref image x y)))
              (if (eql color-index transparent-index)
                  (values 0 0 0 255)
                  (skippy:color-rgb
                   (skippy:color-table-entry gif-color-table color-index))))
          (setf (aref aimage-data y x)
9D                (dpb r (byte 8 0)
                     (dpb g (byte 8 8)
                          (dpb b (byte 8 16)
                               (dpb (or a 0) (byte 8 24) 0))))))))
    aimage))


;; this is the historical version of GIF-STREAM->AIMAGE, that calls
;; the external program gif2png
(defun gif-stream->aimage/gif2png (input)
  (with-temporary-file (temp-filename)
    (let ((png-filename (merge-pathnames (make-pathname :type "png")
                                         temp-filename)))
      (with-open-file (sink temp-filename
                       :direction :output
                       :if-exists :overwrite
                       :element-type '(unsigned-byte 8))
        (let ((sink (make-instance 'glisp:cl-byte-stream :cl-stream sink)))
          (let ((tmp (make-array 4096 :element-type '(unsigned-byte 8))))
            (do ((n (g/read-byte-sequence tmp input)
		    (g/read-byte-sequence tmp input)))
                ((= n 0))
              (g/write-byte-sequence tmp sink :end n)))))
      (unwind-protect
          (progn
            (run-unix-shell-command
             (format nil "gif2png -r ~A >/dev/null 2>/dev/null"
                     (namestring (truename temp-filename))))
            (with-open-file (input png-filename
                             :direction :input
                             :element-type '(unsigned-byte 8))
              (let ((i (make-instance 'cl-byte-stream :cl-stream input)))
                (png:png-stream->aimage i))))
        (ignore-errors
         (mapc #'(lambda (x) (ignore-errors (delete-file x)))
               (directory (merge-pathnames (make-pathname :type :wild)
                                           temp-filename)))) ))))


;; EOF

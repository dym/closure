;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: XLIB; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Some CLX patches
;;;   Created: 1999-05-10 23:42
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;; ---------------------------------------------------------------------------
;;; (c) copyright 1999 by Gilbert Baumann
;;; Portions Copyright (C) 1987 Texas Instruments Incorporated.
;;;

(in-package :xlib)

;; acl put-image bug in fast-write-pixarray

;;
;; Returns nil if the color specification is illegal or is not found by
;; the server.  See C Xlib manual section 10.4 for documentation.
;;
;; warum wird dies nicht von CMUCL honoriert?!
;;
;; uebrigens: woher?
;;
(defun parse-color (colormap spec)
  (declare (type colormap colormap)
         (type stringable spec))
  (declare (values (or null color)))
  (setq spec (string spec))
  (let ((len (length spec)))
    (declare (type array-index len))
    (if (eql len 0)
      (return-from parse-color))
    (cond ((eq (aref spec 0) #\#)
         (decf len)
         (if (and (not (eql len 3)) (not (eql len 6)) (not (eql len 9))
                  (not (eql len 12)))
             (return-from parse-color))
         (setq len (the array-index (/ len 3)))
         (let ((start 1) red green blue
               (multiplier (expt 16 (- 4 len))))
           (declare (type array-index start)
                    (type (integer 1 4096) multiplier))
           (setq red (parse-integer spec :start start
                                    :end (+ start len)
                                    :radix 16))
           (incf start len)
           (setq green (parse-integer spec :start start
                                      :end (+ start len)
                                      :radix 16))
           (incf start len)
           (setq blue (parse-integer spec :start start
                                     :end (+ start len)
                                     :radix 16))
           (make-color :red (card16->rgb-val (* red multiplier))
                       :green (card16->rgb-val (* green multiplier))
                       :blue (card16->rgb-val (* blue multiplier)))))
        (t
         (handler-case
             (lookup-color colormap spec)
           (name-error ()
               (return-from parse-color)))))))

(export 'parse-color)

(defun char-width (font index)
  (let ((byte1 (ldb (byte 8 8) index))  ;msb
        (byte2 (ldb (byte 8 0) index))) ;lsb
    (when (and (font-name font)
               (index<= (font-min-byte1 font) byte1 (font-max-byte1 font))
               (index<= (font-min-byte2 font) byte2 (font-max-byte2 font)))
      (let ((char-info-vector (font-char-infos font)))
        (declare (type char-info-vec char-info-vector))
        (if (index-zerop (length char-info-vector))
            ;; Fixed width font
            (aref (the char-info-vec (font-max-bounds font)) 2)
          ;; Variable width font
          (aref char-info-vector
                (+ 2
                   (* 6
                      (+ (- byte2 (font-min-byte2 font))
                         (* (- byte1 (font-min-byte1 font))
                            (+ (- (font-max-byte2 font) (font-min-byte2 font)) 1)))))) )))))


#+ALLEGRO
(progn
  (defvar *orig-xlib-put-image* #'xlib:put-image)

  (defun xlib:put-image (drawable gcontext image
                         &rest args
                         &key src-x src-y width height x y bitmap-p)
    (cond ((= (xlib:image-depth image) 1)
           (apply *orig-xlib-put-image* drawable gcontext image args))
          ((= (xlib:image-depth image) 8)
           (apply *orig-xlib-put-image* drawable gcontext image args))
          (t
           (loop for i from 0 to (1- width) do
                 (loop for j from 0 to (1- height) do
                       (progn
                         (setf (xlib:gcontext-foreground gcontext)
                           (aref (xlib:image-z-pixarray image)
                                 (+ src-y j) (+ src-x i)))
                         (xlib:draw-point drawable gcontext
                                          (+ x i) (+ y j)))))))) )

;;; xauthority
;; This is stolen form CMUCL

#-(OR CMU ;; under CMUCL this all isn't needed
      (AND EXCL (NOT (VERSION>= 5))))   ;ACL 4.3 lacks hostname
(progn

  (defun authority-pathname ()
    (or (let ((xauthority (sys:getenv "XAUTHORITY")))
          (and xauthority
               (pathname xauthority)))
        (merge-pathnames (user-homedir-pathname) (make-pathname :name ".Xauthority"))))

  (defparameter *known-authorizations* '("MIT-MAGIC-COOKIE-1"))

  ;; they got that wrong:
  ;; patched from contrib/clx/dependent.cl
  #+(and allegro (version>= 5 0))
  (defun host-address (host &optional (family :internet))
    (ecase family
      (:internet
       (progn ;;values-list
         (cons :internet
               (multiple-value-list
                (socket::ipaddr-to-dotted (socket::lookup-hostname host)
                                          :values t)))))))


  ;; ACL 4.3 does not define that yet. and the obvious choice
  ;;         EXCL::GETHOSTBYNAME doesn't work.
  ;; CMUCL has that correcly
  ;; In CLISP this is undefinable

;;; GET-HOSTNAME

  #+ALLEGRO
  (defun get-hostname ()
    (multiple-value-bind (stream dummy pid)
        (excl:run-shell-command "hostname"
                                :input nil
                                :output :stream
                                :error-output nil
                                :wait nil)
      (declare (ignore dummy))
      (unwind-protect
          (ignore-errors (read-line stream))
        (sys:os-wait nil pid))))

  #+CLISP
  (defun get-hostname ()
    (let ((input nil))
      (unwind-protect 
          (progn
            (setf input (lisp:make-pipe-input-stream "hostname"))
            (ignore-errors (read-line input)))
        (when input
          (close input)))))
  
;;; GET-BEST-AUTHORIZATION

  (defun get-best-authorization (host display protocol)
    (labels ((read-short (stream &optional (eof-errorp t))
               (let ((high-byte (read-byte stream eof-errorp nil)))
                 (and high-byte
                      (dpb high-byte (byte 8 8) (read-byte stream)))))
             (read-short-length-string (stream)
               (let ((length (read-short stream)))
                 (let ((string (make-string length)))
                   (dotimes (k length)
                     (setf (schar string k) (card8->char (read-byte stream))))
                   string)))
             (read-short-length-vector (stream)
               (let ((length (read-short stream)))
                 (let ((vector (make-array length :element-type '(unsigned-byte 8))))
                   (dotimes (k length)
                     (setf (aref vector k) (read-byte stream)))
                   vector))))
      (let ((pathname (authority-pathname)))
        (when pathname
          (with-open-file (stream pathname :element-type '(unsigned-byte 8)
                           :if-does-not-exist nil)
            (when stream
              (let* ((host-family (ecase protocol
                                    ((:tcp :internet nil) 0)
                                    ((:dna :DECnet) 1)
                                    ((:chaos) 2)
                                    ((:unix) 256)))
                     (host-address (if (eq protocol :unix)
                                       (map 'list #'char-int (get-hostname))
				     (rest (host-address host protocol))))
                     (best-name nil)
                     (best-data nil))
                (loop
                  (let ((family (read-short stream nil)))
                    (when (null family)
                      (return))
                    (let* ((address (read-short-length-vector stream))
                           (number (parse-integer (read-short-length-string stream)))
                           (name (read-short-length-string stream))
                           (data (read-short-length-string stream)))
                      #+(OR)
                      (print (list family (if (= family 256)
                                              (map 'string #'code-char address)
                                            address)
                                   number name data))
                      (when (and (= family host-family)
                                 (equal host-address (coerce address 'list))
                                 (= number display)
                                 (let ((pos1 (position name *known-authorizations* :test #'string=)))
                                   (and pos1
                                        (or (null best-name)
                                            (< pos1 (position best-name *known-authorizations*
                                                              :test #'string=))))))
                        (setf best-name name)
                        (setf best-data data)))))
                (when best-name
                  (return-from get-best-authorization
                    (values best-name best-data)))))))))
    (values "" ""))

  ;;; finally patch xlib:open-display
  
  (defvar *orig-open-display* #'xlib:open-display)
  
  (defun xlib:open-display (host
                            &key (display 0) protocol authorization-name authorization-data
                            &allow-other-keys)
    ;; Force the special case of a host name of "" or "unix" to the
    ;; :UNIX protocol.
    ;; This is now compatible to CMUCL's implementation.
    (cond ((and (null protocol) (or (equal host "") (equal host "unix")))
           (setf host "")
           (setf protocol :unix)))
    (setf protocol (or protocol :internet))
    ;; get authorization data unless already specified
    (when (null authorization-name)
      (multiple-value-setq (authorization-name authorization-data)
        (get-best-authorization host display protocol)))
      (funcall *orig-open-display*
               host 
               :display display
               :protocol protocol
               :authorization-name authorization-name
               :authorization-data authorization-data))

  )

;; we could as well revoke this hack:

#||
(defun open-display (host &optional (display 0))
  (with-input-from-shell-command (input (format nil "xauth list ~A:~D" host display))
    (let* ((ln (read-line input))
           (i (search "  " ln))
           (j (search "  " ln :start2 (+ i 2))))
      (xlib:open-display host :display display
                         :authorization-name (subseq ln (+ i 2) j)
                         :authorization-data
                         (let ((str (subseq ln (+ j 2))))
                           (map 'string #'code-char
                                (loop for i from 0 to (1- (length str)) by 2
                                    collect (parse-integer str :start i :end (+ i 2) :radix 16)))) ))))
||#
  
  

#+CLISP
(progn
  (defun process-block (whostate predicate &rest predicate-args)
    (apply #'mp::process-wait whostate predicate predicate-args)))



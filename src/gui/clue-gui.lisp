;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLUE-GUI2; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: A GUI based on the CLUE library
;;;   Created: 1999-02-02 07:42
;;;    Author: Gilbert Baumann <gilbert@base-engineering.com>
;;;   License: MIT style (see below)
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1999 by Gilbert Baumann

;;;  Permission is hereby granted, free of charge, to any person obtaining
;;;  a copy of this software and associated documentation files (the
;;;  "Software"), to deal in the Software without restriction, including
;;;  without limitation the rights to use, copy, modify, merge, publish,
;;;  distribute, sublicense, and/or sell copies of the Software, and to
;;;  permit persons to whom the Software is furnished to do so, subject to
;;;  the following conditions:
;;; 
;;;  The above copyright notice and this permission notice shall be
;;;  included in all copies or substantial portions of the Software.
;;; 
;;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. 
;;;  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;  SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;; Changes
;; =======
;;
;;  When        Who     What
;; ----------------------------------------------------------------------------
;;  1999-09-15  GB      - (GET-DRAWABLE+GCONTEXT HT-VIEW) now returns a per 
;;                        process gcontext
;;                      - DESTROY callback on HT-VIEW adjusted accordingly
;;  1999-09-12  GB      - twix from NEW-[HV]BOX to [HV]BOX
;;  1999-08-19  GB      - SUBMIT-FORM-BY-GET, SUBMIT-FORM-BY-POST: 
;;                        changed signature (caller now passes values)

(in-package :clue-gui2)

(defparameter *dcache* nil)
(defparameter *dcache-lock* (mp/make-lock :name "dcache"))

(defparameter *pixmap-cache*
    nil)

(defparameter *debug-pixmap-cache-p*
    t)

(defparameter *multiple-selection-title* "(Multiple Selection)")

(defparameter *gui-element-font* "-*-lucida-medium-r-*-*-*-120-*-*-*-*-iso8859-1"
  "Default font for GUI elements.")

;; some widgets^H^H^H^H^H^H^Hcontacts (these should be bundled into some application object)

(defvar *application-shell*)
(defvar *wholine-label*)
(defvar *url-label*)

(defvar *style-menu-button-foo*)
(defvar *style-menu-pane*)


;;;
;;; Requests
;;;

(defstruct (request (:constructor make-request/low))
  (method :get :type (member :get :post))
  url
  post-data                             ;for post requests
  cache-file-head
  post-header
  cache-file-data)

(defun make-request (&rest options)
  (let ((res (apply #'make-request/low options)))
    #+EXCL
    (excl:schedule-finalization 
     res
     (lambda (x)
       (format T "~&********* shadow of ~A deleted." (request-url x))
       (glisp:delete-temporary-file (request-cache-file-head x))
       (glisp:delete-temporary-file (request-cache-file-data x)) ))
    res))

;;; ---------------------------------------------------------------------------
;; we refine GUI:PRIM-HT-VIEW

(defun make-pt-from-input (input mime-type url)
  (let* ((charset :iso-8859-1)
         (mime-type (multiple-value-bind (type subtype parameters)
                        (netlib::parse-mime-content-type mime-type)
                      (let ((cs (assoc :charset parameters :test #'string-equal)))
                        (when cs
                          (setf charset (cdr cs))))
                      (netlib::find-mime-type (format nil "~A/~A" type subtype)))))
    (let ((pt (progn 
                (cond ((member mime-type (list (netlib:find-mime-type "image/png")
                                               (netlib:find-mime-type "image/gif")
                                               (netlib:find-mime-type "image/jpeg")))
                       (sgml:lhtml->pt
                        `(:HTML
                          (:BODY
                           (:IMG :SRC ,(url:unparse-url url))))))
                      ((member mime-type (list (netlib:find-mime-type "text/lml")))
                       (sgml:lhtml->pt (read-from-string (with-output-to-string (bag)
                                                           (do ((x (glisp:g/read-byte input nil nil)
                                                                   (glisp:g/read-byte input nil nil)))
                                                               ((null x))
                                                             (write-char (code-char x) bag))))))
                      ((member mime-type (list (netlib:find-mime-type "text/html")))
                       (sgml::parse-html input charset))
                      ((member mime-type (list (netlib:find-mime-type "text/xml")))
                       (cxml:parse-stream input (rune-dom:make-dom-builder) :recode nil))

                      ((or t
                           #+NIL
                           (member mime-type (list (netlib:find-mime-type "text/plain")
                                                   (netlib:find-mime-type "text/css"))))
                       (sgml:lhtml->pt
                        `(:HTML
                          (:BODY
                           (:PRE
                            ,(gstream-as-string input))))))
                      #||
                      (t
                       (download input charset url))
                      ||#
                                 
                      ))))
      pt)))

(defun open-document-4 (request)
  (check-type request request)
  (cond ((not (null (request-cache-file-head request)))
         (values (cl-byte-stream->gstream
                  (open (request-cache-file-data request)
                        :direction :input
                        :element-type '(unsigned-byte 8)))
                 (with-open-file (input 
                                  (request-cache-file-head request)
                                  :direction :input)
                   (with-standard-io-syntax
                     (read input)))))
        (t
         (let* ((fn-head (glisp:find-temporary-file))
                (fn-data (glisp:find-temporary-file)))
              (setf (request-cache-file-head request) fn-head)
              (setf (request-cache-file-data request) fn-data)
           (ecase (request-method request)
             ((:GET)
              ;; ZZZ we will probably only what to update the cahce-file-name 
              ;; field, if the transfer was complete.
              (multiple-value-bind (io header) (netlib::open-document-2 (request-url request))
                (with-open-file (sink fn-head 
                                 :direction :output
                                 :if-exists :overwrite)
                  (with-standard-io-syntax 
                    (print header sink)))
                (let ((dumpee (cl-byte-stream->gstream 
                               (open fn-data 
                                     :direction :output
                                     :if-exists :overwrite
                                     :element-type '(unsigned-byte 8)))))
                  (let ((ntot (maybe-parse-integer (netlib::get-header-field header :content-length))))
                    (setf ntot (or ntot 100000)) ;xxx
                    (let ((res (make-instance 'pb-stream
                                 :dumpee dumpee
                                 :ntotal ntot
                                 :nread 0
                                 :proxee io)))
                      (pb-stream-update res)
                      (values res header))))))
             ((:POST)
              ;; ZZZ cut + paste alert
              ;; ZZZ we will probably only what to update the cahce-file-name 
              ;; field, if the transfer was complete.
              (unless (string-equal (url:url-protocol (request-url request)) "http")
                (error "I could only POST via HTTP: ~S" (request-url request)))
              (multiple-value-bind (io header)
                  (netlib::http-open-document (request-url request) 
                                              :method :post
                                              :post-header (request-post-header request)
                                              :post-data   (request-post-data request) )
                (with-open-file (sink fn-head :direction :output :if-exists :overwrite)
                  (with-standard-io-syntax 
                    (print header sink)))
                (let ((dumpee (cl-byte-stream->gstream 
                               (open fn-data
                                     :if-exists :overwrite
                                     :direction :output
                                     :element-type '(unsigned-byte 8)))))
                  (let ((ntot (maybe-parse-integer (netlib::get-header-field header :content-length))))
                    (setf ntot (or ntot 100000)) ;xxx
                    (let ((res (make-instance 'pb-stream
                                 :dumpee dumpee
                                 :ntotal ntot
                                 :nread 0
                                 :proxee io)))
                      (pb-stream-update res)
                      (values res header)))))) )))))


(defclass pb-stream (use-byte-for-char-stream-flavour gstream)
  ((nread        :initarg :nread        :initform 0 )
   (ntotal       :initarg :ntotal       :initform 0)
   (progress-bar :initarg :progress-bar :initform nil  :accessor pb-stream-progress-bar)
   (old-value                           :initform nil)
   (proxee       :initarg :proxee)
   (serial       :initarg :serial)
   (dumpee       :initarg :dumpee       :initform nil) ) )

(defmethod runes::figure-encoding ((stream glisp:gstream))
  ;; For HTML iso-8859-1 is the default
  (values (cxml::find-encoding :iso-8859-1) nil))

(defmethod g/read-byte ((stream pb-stream)  &optional (eof-error-p t) eof-value)
  (with-slots (nread ntotal proxee dumpee) stream
    (let ((res (g/read-byte proxee eof-error-p eof-value)))
      (when (and (not (eq res eof-value)) dumpee)
        (g/write-byte res dumpee))
      (incf nread)
      (pb-stream-update stream)
      res)))

(defmethod g/read-byte-sequence (sequence (stream pb-stream) &key (start 0) (end (length sequence)))
  (with-slots (nread ntotal proxee dumpee) stream
    (let ((res (g/read-byte-sequence sequence proxee :start start :end end)))
      (incf nread (- res start))
      (when dumpee
        (g/write-byte-sequence sequence dumpee :start start :end res))
      (pb-stream-update stream)
      res)))

(defmethod g/close ((stream pb-stream) &key abort)
  (with-slots (proxee dumpee) stream
    (when dumpee
      (g/close dumpee))
    (g/close proxee :abort abort)))

(defmethod pb-stream-update ((self pb-stream))
  (declare (ignorable self))
  )

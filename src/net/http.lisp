;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: NETLIB; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: HTTP and Stuff
;;;   Created: 1997-09-25
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: GPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1997-2001 by Gilbert Baumann

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

(in-package :NETLIB)

;;; TODO

;; . I would a higher level interface to making POST request which
;;   abstracts from the correct method needed to encode the data and
;;   stuff.

;; . Also: there is some interface needed with abstracts above the
;;   POST/GET methods. Maybe we would just handle that adding gaining
;;   the posted values from the url and gaining the exact encoding
;;   method used for such a POST from some url parameter.
;;
;;   That is we something like a REQUEST object which denotes a
;;   complete request to be made.
;;
;;   Also take a peek into HTTP/1.1 and see what is needed for e.g. 
;;   PUT and stuff.

;; For extra confusion there is both MAKE-HTTP-REQUEST and
;; HTTP-MAKE-REQUEST.

;;; TODO

;; Configuration of access methods must be more flexible. There must
;; be a mapping of an URL to some access method.

;;; Options

(defparameter *use-http-proxy-p* nil
  "Whether to use the HTTP proxy as defined by *HTTP-PROXY-HOST* and *HTTP-PROXY-PORT*.")

(defparameter *http-proxy-host* nil
  "Specifies the HTTP proxy host; see also *USE-HTTP-PROXY-P* and *HTTP-PROXY-PORT*.")

(defparameter *http-proxy-port* nil
  "Specifies the HTTP proxy port; see also *USE-HTTP-PROXY-P* and *HTTP-PROXY-HOST*.")

#+NIL
(defparameter *http-cache-dir* "test-cache/*"
  "A directory, where the HTTP document cache resides.")

(defparameter *always-use-cache-p* nil)

(defparameter *send-host-field-never-the-less-p* t
  "Insert the 'Host:' request header field when talking directly with a HTTP server?

   Albeit HTTP/1.0 states that the \"Host:\" request header field should
   only been sent, when talking with a proxy and *not* when talking
   with a server directly, some servers seem to need it never the
   less. Example: 'http://validator.w3.org/images/vh40.gif'. Sigh!
   So finally make it a user tweakable option.")

(defparameter *trace-http-p* nil
  "Trace all HTTP traffic; see also *HTTP-TRACE-OUTPUT*")

(defparameter *http-trace-output* t
  "Output stream to send http traces to; see also *TRACE-HTTP-P*")

(defparameter *trust-expires-p* t
  "Whether to trust servers 'Expires' header field [and your clock].")

(defvar *referer* nil)

(defvar *http-cache* nil)

;;;

(defparameter *http-proxy-host* "www-cache.rz.uni-karlsruhe.de")
(defparameter *http-proxy-port* 3128)

(defparameter *user-agent* "Lynx/2.7.1ac-0.98 libwww-FM/2.14")
(defparameter *user-agent* "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1)")
(defparameter *user-agent* "CLOSURE/0.1")


#||
(defparameter *options/connection-timeout* 30)
(defvar *trace-http-p* nil)
||#

(defun http-cache ()
  (or *http-cache*
      (let ((dir (merge-pathnames
                  (make-pathname
                   :directory '(:relative ".closure" "test-cache")
                   :name nil
                   :type nil
                   :defaults (user-homedir-pathname))
                  (user-homedir-pathname))))
        (ensure-directories-exist dir)
        (setf *http-cache* (uncommit-cache dir)))))

;;; ---- HTTP dates ---------------------------------------------------------------------------

;;  HTTP-date    = rfc1123-date | rfc850-date | asctime-date
;;  rfc1123-date = wkday "," SP date1 SP time SP "GMT"
;;  rfc850-date  = weekday "," SP date2 SP time SP "GMT"
;;  asctime-date = wkday SP date3 SP time SP 4DIGIT
;;
;;  date1        = 2DIGIT SP month SP 4DIGIT
;;  date2        = 2DIGIT "-" month "-" 2DIGIT
;;  date3        = month SP ( 2DIGIT | ( SP 1DIGIT ))
;;  time         = 2DIGIT ":" 2DIGIT ":" 2DIGIT
;;  wkday        = "Mon" | "Tue" | "Wed" | "Thu" | "Fri" | "Sat" | "Sun"
;;  weekday      = "Monday" | "Tuesday" | "Wednesday" | "Thursday" | "Friday" | "Saturday" | "Sunday"
;;  month        = "Jan" | "Feb" | "Mar" | "Apr" | "May" | "Jun" | "Jul" | "Aug" | "Sep" | "Oct" | "Nov" | "Dec"

(defun maybe-parse-http-date (string)
  (and string
       (let ((toks (split-by-member '(#\space #\tab #\newline #\return #\- #\,) string
                                    :nuke-empty-p t))
             day month year time)
         (cond ((and (setq day (maybe-parse-day (elt toks 1)))
                     (setq month (maybe-parse-month (elt toks 2)))
                     (setq year (maybe-parse-year (elt toks 3)))
                     (setq time (maybe-parse-time (elt toks 4))))
                (ignore-errors (encode-universal-time (third time) (second time) (first time)
                                                      day month year 0)) )
               ((and (>= (length toks) 5)
                     (setq month (maybe-parse-month (elt toks 1)))
                     (setq day (maybe-parse-day (elt toks 2)))
                     (setq time (maybe-parse-time (elt toks 3)))
                     (setq year (maybe-parse-year (car (last toks)))))
                (ignore-errors (encode-universal-time (third time) (second time) (first time)
                                                      day month year 0)) )
               (t
                nil)))))

(defun maybe-parse-day (string)
  (and (<= 1 (length string) 2)
       (let ((r (maybe-parse-integer string)))
         (and (<= 1 r 31) r))))

(defun maybe-parse-year (string)
  (cond ((= (length string) 4)
         (maybe-parse-integer string) )
        ((= (length string) 2)
         (maybe-parse-integer string))))

(defun maybe-parse-month (string)
  (let ((r (position string '#("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                               "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
                     :test #'string-equal)))
    (and r (+ r 1))))

(defun maybe-parse-time (string)
  (let ((s (split-by #\: string))
        hour minute second)
    (cond ((and (= 3 (length s))
                (setq hour (maybe-parse-integer (elt s 0)))
                (setq minute (maybe-parse-integer (elt s 1)))
                (setq second (maybe-parse-integer (elt s 2)))
                (<= 0 hour 23)
                (<= 0 minute 59)
                (<= 0 second 59))
           (list hour minute second)) )))

(defun unparse-http-date (ut)
  ;; nach rfc1123
  (multiple-value-bind (second minute hour date month year day) (decode-universal-time ut 0)
    (format nil
            "~[Mon~;Tue~;Wed~;Thu~;Fri~;Sat~;Sun~], ~
             ~2,'0D ~[Jan~;Feb~;Mar~;Apr~;May~;Jun~;Jul~;Aug~;Sep~;Oct~;Nov~;Dec~;~] ~4,'0D ~
             ~2,'0D:~2,'0D:~2,'0D GMT"
            day date (1- month) year hour minute second)))


;;; ---- basic http protocol ------------------------------------------------------------------

(defun http-send-request-line (sink method uri http-version)
  (g/write-string method sink)
  (g/write-char #\space sink)
  (g/write-string uri sink)
  (g/write-char #\space sink)
  (g/write-string http-version sink)
  (g/write-byte 13 sink)
  (g/write-byte 10 sink))

(defun http-send-request-header (sink header)
  (dolist (k header)
    (g/write-string (car k) sink)
    (g/write-char #\: sink)
    (g/write-char #\space sink)
    (g/write-string (cdr k) sink)
    (g/write-byte 13 sink)
    (g/write-byte 10 sink)))

(defun http-send-request (sink method uri http-version header)
  (http-send-request-line sink method uri http-version)
  (http-send-request-header sink (cons (cons "User-Agent" *user-agent*) header))
  (g/write-byte 13 sink)
  (g/write-byte 10 sink))

(defun read-response-line (input)
  (finish-output (slot-value input 'glisp:cl-stream))
  (let ((res (g/read-line* input)))
    (when *trace-http-p*
      (ignore-errors
       (format *http-trace-output* "~%;; <-- ~A." res)
       (finish-output *http-trace-output*)))
    (parse-response-line res)))

(defun parse-response-line (string)
  (let* ((p0 (position #\space string))
         (p1 (position #\space string :start (+ p0 1))))
    (unless (and p0 p1 (= (- p1 p0) 4))
      (error "HTTP repsonse line '~A' has illegal syntax." string))
    (values (string-upcase (subseq string 0 p0))
            (parse-integer string :junk-allowed nil :start (+ 1 p0) :end p1)
            (subseq string (+ 1 p1)))))

(defun make-http-request (io method uri http-version header &optional data)
  ;; -> version code explanation header
  (http-send-request io method uri http-version header)
  (when (and *trace-http-p*)
    (ignore-errors
     (format *http-trace-output* "~&;; --> ")
     (loop for ch across
           (with-output-to-string (sink)
             (let ((q (make-instance 'glisp:cl-char-stream :cl-stream sink)))
               (http-send-request q method uri http-version header)))
         do
           (cond ((char= ch #\newline)
                  (terpri *http-trace-output*)
                  (princ ";; --> " *http-trace-output*))
                 (t
                  (princ ch *http-trace-output*))))))
  (when (equal method "POST")           ;zzz
    (cond ((stringp data)
           (dotimes (i (length data))
            (g/write-byte (char-code (char data i)) io)))
          (t
           (dotimes (i (length data))
            (g/write-byte (aref data i) io)))))
  (multiple-value-bind (version code explanation) (read-response-line io)
    (values version code explanation
            (read-response-header io))))

(defun read-response-header (input)
  (let ((res nil))
    (do ((line (g/read-line* input) (g/read-line* input)))
        ((= (length line) 0)
         (nreverse res))
      (when *trace-http-p*
        (ignore-errors
         (format *http-trace-output* "~&;; <-- ~A" line)
         (finish-output *http-trace-output*)))
      (let (p)
        (cond ((or (char= (char line 0) #\space)
                   (char= (char line 0) #\tab))
               (if (null res)
                   (error "Badly formed response header.")
                 (setf (cdar res)
                   (concatenate 'string (cdar res) " " (string-trim '(#\space #\tab) line)))))
              ((setq p (search ": " line))
               (push (cons (subseq line 0 p) (subseq line (+ p 2))) res))
              (t
               (warn "Badly formed response header line: '~A' -- ignored." line)) )))))

(defun unparse-url-for-http (url)
  (let ((url (url::copy-url url)))
    (setf (url:url-protocol url) nil
	  (url:url-host url) nil
	  (url:url-port url) nil
	  (url:url-user url) nil
	  (url:url-password url) nil
	  (url:url-parameters url) nil
	  (url:url-anchor url) nil
          )
    (url:unparse-url url)))

(defun unparse-url-for-http/proxy (url)
  (let ((url (url::copy-url url)))
    (setf (url:url-user url) nil
	  (url:url-password url) nil
	  (url:url-anchor url) nil)
    (url:unparse-url url)))

(defun open-socket-for-http (url)
  "This is the basic switch to decide whether to use a proxy and which proxy to use."
  ;; -> io proxyp
  (let* ((host (or (url:url-host url) "localhost"))
         (https-p (string= (url:url-protocol url) "https"))
         (port (or (url:url-port url)
                   (if https-p
                       443
                     80)))
         (opener (if https-p
                     #'glisp::g/open-inet-socket-ssl
                   #'g/open-inet-socket))
         (proxyp (and *use-http-proxy-p*
                      (= port 80)
                      (not (url:url-port url))
                      (not (string-equal host "localhost")))))
    (values
     (cond (proxyp
            (funcall opener *http-proxy-host* *http-proxy-port*))
           (t
            (funcall opener host port)))
     proxyp)))

(defun http-make-request (method url header post-data)
  "Makes a single HTTP request for the URL url;
   Returns: io protocol-version response-code response-message response-header."
  ;; eval hack
  #+NIL
  (cond ((string-equal (url:url-host url) "images.cjb.net")
         (error "No data from images.cjb.net!")))
  (when *trace-http-p*
    (ignore-errors
     (format *http-trace-output* "~&;; Making ~S request for ~S ..." method url)
     (finish-output *http-trace-output*)))
  (let ((host (or (url:url-host url) "localhost")))
    (multiple-value-bind (io proxyp) (open-socket-for-http url)
      (let ((method-string (ecase method (:GET "GET") (:POST "POST")))
            (url-for-server (if proxyp
                                (unparse-url-for-http/proxy url)
                              (unparse-url-for-http url)))
            (header (append (if (and (or *send-host-field-never-the-less-p*
                                         proxyp)
                                     (not (member :host header :test #'string-equal :key #'car)))
                                (list (cons "Host" host))
                              nil)
                            (if *referer*
                                (list (cons "Referer" (if (url:url-p *referer*)
                                                          (url:unparse-url *referer*)
                                                          *referer*)))
                                nil)
                            (if (eq method :post)
                                (list (cons "Content-Length" (format nil "~D" (length post-data))))
                              nil)
                            header)))
        (multiple-value-bind (protocol-version response-code response-message response-header)
            (make-http-request io method-string url-for-server "HTTP/1.0" header post-data)
          (values io protocol-version response-code response-message response-header))))))

(defun get-header-field (header field)
  (check-type field (member :LOCATION :LAST-MODIFIED
                            :CONTENT-TYPE :EXPIRES
                            :PRAGMA :DATE
                            :CONTENT-LENGTH))
  (cdr (assoc field header :test #'string-equal)) )

;;; ---- HTTP Cache ---------------------------------------------------------------------------

;;;
;; 
;; get-hce cache url
;; put-hce cache hce
;; invent-cache-filename cache
;; uncommit-cache directory -> cache
;; commit-cache cache

;; o Timeout handhaben
;; o hook fuer 'nk von mk gelesen (r bps)'
;; o expire routine fuer den cache schreiben.
;; o robustere fehler behandung
;; o dem 'benuzter' eine moeglichkeit geben zusaetzliche header felder zu uebergeben
;; o schon mal ueber cookies nachdenken
;; o so aus Spass schon mal eine CLM-Routine schreiben, um sich den Cache an zu schauen.

;; funktioniert bis jetzt aber gut ...


(defstruct http-cache
  lock
  entries
  serial
  directory)

(defstruct (http-cache-entry (:conc-name "HCE-"))
  url
  last-modified                         ;NIL or UT server time
  expires                               ;NIL or UT client time 
                                        ; we don't make use of this field yet
  last-visit                            ;NIL or UT client time
  filename                              ;file name in cache
  filetype                              ;file type in cache
  original-header)                      ;original document header

(defun uncommit-cache (directory)
  (let ((fn (merge-pathnames "index" directory)))
    (cond ((probe-file fn)
           (with-open-file (stream fn :direction :input)
             (let ((*package* (symbol-package 'http-cache-entry)))
               (let ((res (make-http-cache :lock (mp/make-lock :name "HTTP cache lock")
                                           :directory directory
                                           :entries (make-hash-table :test #'equal))))
                 (setf (http-cache-serial res) (read stream))
                 (do ((x (read stream nil nil) (read stream nil nil)))
                     ((null x))
                   (assert (http-cache-entry-p x))
                   (put-hce res x))
                 res))))
          (t
           (make-http-cache :lock (mp/make-lock :name "HTTP cache lock")
                            :directory directory
                            :entries (make-hash-table :test #'equal)
                            :serial 0)) )))

(defun commit-cache (&optional (cache (http-cache)))
  (mp/with-lock ((http-cache-lock cache))
    (with-open-file (sink (merge-pathnames "index" (http-cache-directory cache))
                     :direction :output :if-exists :new-version)
      (let ((*print-pretty* nil)
            (*print-readably* t)
            (*package* (symbol-package 'http-cache-entry)))
        (print (http-cache-serial cache) sink)
        (maphash (lambda (key value)
                   (declare (ignore key))
                   (print value sink))
                 (http-cache-entries cache)) ))) )

(defun invent-cache-filename (cache)
  (mp/with-lock ((http-cache-lock cache))
    (format nil "~5,'0D" (incf (http-cache-serial cache)))))

(defun get-hce (cache url)
  (mp/with-lock ((http-cache-lock cache))
    (gethash url (http-cache-entries cache))))

(defun put-hce (cache hce)
  (mp/with-lock ((http-cache-lock cache))
    ;; if there was already an entry for that URL with under a different filename,
    ;; delete the old file
    (let ((old-ce (gethash (hce-url hce) (http-cache-entries cache))))
      (cond ((and old-ce (not (and (string-equal (hce-filename old-ce) (hce-filename hce))
                                   (string-equal (hce-filetype old-ce) (hce-filetype hce)))))
             (ignore-errors (delete-file (hce-pathname cache old-ce))))))
    (setf (gethash (hce-url hce) (http-cache-entries cache)) hce)))

(defun hce-pathname (cache hce)
  (merge-pathnames (make-pathname :name (hce-filename hce)
                                  :type (hce-filetype hce))
                   (http-cache-directory cache)))

;;; ---------------------------------------------------------------------------

(defun http-open-document (url 
                           &rest options
                           &key (yet-urls nil)
                                (method :get)
                                (post-data )
                                (post-header ))
  ;; -> io ; header
  (cond 
   ;; url lacks a path?
   ((null (url:url-path url))
    (let ((new-url (url:copy-url url)))
      (setf (url:url-path new-url) (list :ABSOLUTE ""))
      (multiple-value-bind (input header)
          (apply #'http-open-document new-url options)
        (values input
                (append header
                        (list (cons "Location" (url:unparse-url new-url))))))))
   
   ;; circular 302 chain?
   ((member url yet-urls :test #'url:url-equal-p)
    (error "Circular 301/302 chain: ~S." yet-urls))

   (t
    (let ((ce (and (eq method :get)
                   (get-hce (http-cache) (url:unparse-url url))))
          (hd nil)
          (cache-p (and (eq method :get)
                        (null (url:url-query url))
                        (null (url:url-password url)))))
      (when (and *trust-expires-p*
                 ce 
                 (integerp (hce-expires ce))
                 (> (hce-expires ce) (get-universal-time)))
        (format *http-trace-output* "~&;; Serving ~S from cache." url)
        (return-from http-open-document
          (values (cl-byte-stream->gstream (open (hce-pathname (http-cache) ce) 
                                                 :direction :input 
                                                 :element-type '(unsigned-byte 8)))
                  (hce-original-header ce))))
      (when (and ce (hce-last-modified ce))
        (push (cons "If-Modified-Since" 
                    (unparse-http-date (1+ (hce-last-modified ce)))) hd))
      (when (and ce *always-use-cache-p*)
        (format *http-trace-output* "~&;; Unkosherly serving ~S from cache." url)
        (return-from http-open-document
          (values (cl-byte-stream->gstream (open (hce-pathname (http-cache) ce) 
                                                 :direction :input 
                                                 :element-type '(unsigned-byte 8)))
                  (hce-original-header ce))))
      (multiple-value-bind (io protocol-version 
                            response-code response-message
                            response-header)
          (http-make-request method url (append hd post-header) post-data)
        (case response-code
          ((200 204)
           ;; everything fine
           (let* ((her-date    (maybe-parse-http-date (get-header-field response-header :date)))
                  (her-expires (maybe-parse-http-date (get-header-field response-header :expires)))
                  (really-cache-p (and cache-p
                                      (or
                                       (not (null (get-header-field response-header :last-modified)))
                                       ;; xxx when exactly to cache?
                                       (and her-date her-expires
                                            (> her-expires her-date))))))
             (when cache-p
               (unless really-cache-p
                 (warn "~A will not be cached; cache-p = ~S; header: ~S"
                       url cache-p response-header)))
             (values
              (make-instance 'http-stream
                :header response-header
                :url (url:unparse-url url)
                :input io
                :buffer (make-array 1024 
                                    :element-type '(unsigned-byte 8) 
                                    :adjustable t 
                                    :fill-pointer 0)
                :cache-p really-cache-p
                :my-expires (and her-date her-expires
                                 (+ (get-universal-time)
                                    (- her-expires her-date))) )
              response-header)))
        
          ((301 302 303) 
           ;; moved permanently; moved temponary; see other
           (multiple-value-bind (input header)
               (http-open-document
                (url:parse-url
                 (or (get-header-field response-header :location)
                     (error "301/302 Response from ~A lacks a 'Location' field."
                            (url:url-host url))))
                :yet-urls (cons url yet-urls))
             (values input
                     (append header
                             (list 
                              (cons "Location" 
                                    (get-header-field response-header :location)))))))
        
          (304
           ;; not modified
           (values (cl-byte-stream->gstream (open (hce-pathname (http-cache) ce) 
                                                  :direction :input 
                                                  :element-type '(unsigned-byte 8)))
                   (hce-original-header ce)))
        
          (otherwise
           (warn "Response ~D (~A) from ~A not understood."
                 response-code response-message (url:url-host url))
           (values io response-header)) )) )) ))

(defclass http-stream (use-byte-for-char-stream-flavour gstream)
  ((header      :initarg :header)
   (url         :initarg :url)
   (input       :initarg :input)
   (buffer      :initarg :buffer)
   (eof-seen-p  :initform nil)
   (cache-p     :initarg :cache-p)
   (my-expires  :initarg :my-expires :initform nil)          ;servers expires in my date
   ))

(defmethod g/read-byte ((stream http-stream) &optional (eof-error-p t) eof-value)
  (with-slots (input eof-seen-p buffer cache-p) stream
    (let ((ch (g/read-byte input nil :eof)))
      (cond ((eq ch :eof)
             (setf eof-seen-p t)
             (when eof-error-p
               (error "EOF seen on ~S." stream))
             eof-value)
            (t
             (when cache-p
               (vector-push-extend ch buffer 1024))
             ch) ))))

(defmethod g/unread-byte (byte (stream http-stream))
  (with-slots (input buffer cache-p) stream
    (when cache-p
      (decf (fill-pointer buffer)))
    (g/unread-byte byte input)))

(defmethod g/read-byte-sequence (sequence (stream http-stream) 
                                 &key (start 0) (end (length sequence)))
  (with-slots (input buffer eof-seen-p cache-p) stream
    (let ((n (g/read-byte-sequence sequence input :start start :end end)))
      (when (= start n)
        (setf eof-seen-p t))
      (when cache-p
        (do ((i start (+ i 1)))
            ((>= i n))
          (vector-push-extend (elt sequence i) buffer 1024)))
      n)))

(defmethod g/write-byte (byte (stream http-stream))
  (error "No ~S on ~S." 'g/write-byte stream))

(defmethod g/finish-output ((stream http-stream))
  nil)

(defmethod g/close ((stream http-stream) &key abort)
  (with-slots (cache-p eof-seen-p header url buffer input my-expires) stream
    (g/close input :abort abort)
    (when (and (not abort) cache-p eof-seen-p)
      (let ((filename (invent-cache-filename (http-cache)))
            (filetype nil))

        (when (get-header-field header :content-type)
          (multiple-value-bind (type subtype parameters)
              (parse-mime-content-type (get-header-field header :content-type))
            (declare (ignore parameters))
            (let ((mt (find-mime-type (format nil "~A/~A" type subtype))))
              (when mt
                (setf filetype (car (mime-type-extensions mt)))))))
          
        (let ((ce (make-http-cache-entry
                   :url url
                   :last-modified (and (get-header-field header :last-modified)
                                       (maybe-parse-http-date
                                        (get-header-field header :last-modified)))
                   :expires my-expires
                   :last-visit (get-universal-time)
                   :filename filename
                   :filetype filetype
                   :original-header header)))
          (cond ((probe-file (hce-pathname (http-cache) ce))
                 (warn "File already exists: '~A'." 
                       (namestring (hce-pathname (http-cache) ce)))
                 (ignore-errors (delete-file (hce-pathname (http-cache) ce)))))
          (with-open-file (sink (hce-pathname (http-cache) ce) 
                           :direction :output 
                           :element-type '(unsigned-byte 8))
            (g/write-byte-sequence buffer (cl-byte-stream->gstream sink) 
                                   :start 0 
                                   :end (fill-pointer buffer)))
          (put-hce (http-cache) ce))))))

;;; -------------------------------------------------------------------------------------------
;;;  File protocol
;;;

;; Ich will jetzt folgenes:
;;  a. file:/foo/bar/baz und file:/foo/bar/baz/ sollen zunaechst gleich behandelt werden.
;;  b. wenn der name ein directory bezeichnet:
;;     b1. es ex. eine datei index.html -> diese liefern
;;     b2. sonst ein standard UNIX Listing decorieren
;;  c. wenn der name eine datei ist -> kein problem wie immer
;;  d. sonst fehler

;; Alles unter UNIX:

;; CLISP:
;;     [COMMON-LISP-USER]> (setq i (open "." :direction :input))
;;     
;;     *** - no file name given: #P"/usr/gilbert/closure/src/."
;;     1. Break[COMMON-LISP-USER]> 
;;     [COMMON-LISP-USER]> (setq i (open "/usr/gilbert" :direction :input))
;;     
;;     *** - NAMESTRING: "/usr/gilbert" names a directory, not a file
;;     1. Break[COMMON-LISP-USER]> 

;; ACL 4.3
;;     USER(1): (setq i (open "." :direction :input))
;;     Error: File #p"./" is a DIRECTORY type file and cannot reasonably be opened as
;;            a Lisp stream.
;;       [condition type: FILE-ERROR]

;; CMUCL 18a+
;;     * (setq i (open "." :direction :input))
;;     #<Stream for file ".">
;;     * (read-char i)
;;     Error in function COMMON-LISP::FD-STREAM-READ-N-BYTES:
;;        Error reading #<Stream for file ".">: Is a directory

;; GCL 
;;     >(setq i (open "." :direction :input))
;;     #<input stream ".">
;;     >(read-char i)
;;     #\\377           ;; Benutzen die stdio oder was?!

(defun really-probe-file (filename)
  ;; Probe that a file named 'filename' is really readable. The only
  ;; reliable way to achieve that is to actual attempt to open the
  ;; file; But beware: Under some UNIX implementations a directory
  ;; is actually readable like any other file.
  ;; Under UNIX it would be probably better to spawn a 'ls -l'.
  (and (ignore-errors (probe-file filename))
       (let (stream)
         (unwind-protect 
             (progn
               (setf stream (ignore-errors (open filename :direction :input
                                                 :if-does-not-exist nil)))
               (and stream
                    (ignore-errors (read-char stream nil :eof))))
           (when stream
             (close stream)) )) ))

#+CMU
(defun really-probe-file (filename)
  ;; Probe that a file named 'filename' is really readable. The only
  ;; reliable way to achieve that is to actual attempt to open the
  ;; file; But beware: Under some UNIX implementations a directory
  ;; is actually readable like any other file.
  ;; Under UNIX it would be probably better to spawn a 'ls -l'.
  (and (ignore-errors (probe-file filename))
       (not (is-directory-p filename))))

#+CLISP
(defun is-directory-p (pathname)
  (ignore-errors (lisp:probe-directory pathname)))

#+CMU
(defun is-directory-p (pathname)
  (multiple-value-bind (success? dev ino mode) (unix:unix-stat (namestring pathname))
    (declare (ignore dev ino))
    (and success? (logbitp 14 mode))))

#+CMU
;; #define S_IFMT  00170000
;; #define S_IFSOCK 0140000
;; #define S_IFLNK  0120000
;; #define S_IFREG  0100000
;; #define S_IFBLK  0060000
;; #define S_IFDIR  0040000
;; #define S_IFCHR  0020000
;; #define S_IFIFO  0010000
(defun is-regualar-file-p (pathname)
  (multiple-value-bind (success? dev ino mode) (unix:unix-stat (namestring pathname))
    (declare (ignore dev ino))
    (and success? (= (logand mode #o170000) #o100000))))

#+ALLEGRO
(defun is-directory-p (pathname)
  (let ((pn (mungle-pathname-to-directory pathname)))
    (probe-file (merge-pathnames pn (make-pathname :name ".")))))

#-(OR CLISP CMU ALLEGRO)
(progn
  #.(warn "Define IS-DIRECTORY-P for your flavour of LISP.")
  (defun is-directory-p (pathname)
    (eql 0 (glisp:run-unix-shell-command (format nil "test -d ~A" (namestring pathname))))))

;; Wuenschenwert waere natuerlich auch, wenn wir es vermeiden wuerden
;; von Geraeten und Sockets und der gleichen mehr zu lesen.

(defun mungle-pathname-to-directory (pathname)
  (let ((dir (or (pathname-directory pathname)
                 (list :relative)))
        (name (pathname-name pathname))
        (type (pathname-type pathname)))
    (cond ((and name type)
           (make-pathname :name nil
                          :type nil
                          :directory (append dir (list (concatenate 'string name "." type)))
                          :host (pathname-host pathname)
                          :device (pathname-device pathname)
                          :version (pathname-version pathname)))
          (name
           (make-pathname :name nil
                          :type nil
                          :directory (append dir (list (concatenate 'string name)))
                          :host (pathname-host pathname)
                          :device (pathname-device pathname)
                          :version (pathname-version pathname)))
          (type
           (make-pathname :name nil
                          :type nil
                          :directory (append dir (list (concatenate 'string "." type)))
                          :host (pathname-host pathname)
                          :device (pathname-device pathname)
                          :version (pathname-version pathname)))
          (t
           pathname))))

(defun url-with-index.html-appended (url)
  (setf url (url-with-slash-appened url))
  (let ((new (url:copy-url url)))
    (setf (url:url-path new) (append (butlast (url:url-path url))
                                     (list "index.html")))
    new))

(defun url-with-slash-appened (url)
  (cond ((string-equal "" (car (last (url:url-path url))))
         url)
        (t
         (let ((new (url:copy-url url)))
           (setf (url:url-path new) (append (url:url-path url) 
                                            (list "")))
           new))))

(defun open-file-document (url)
  ;; this optimize declaration is to prevent an ACL compiler bug:
  #+EXCL (declare (optimize (speed 0) (safety 3))) 
  (labels ((try (url)
             (let ((pathname (url:url-pathname url)))
               (cond ((string-equal "" (car (last (url:url-path url))))
                      (open-directory-as-file-document pathname))
                     ((really-probe-file pathname)
                      (multiple-value-bind (input header) 
                          (open-file-document-2 pathname)
                        (values input
                                (append header
                                        (list (cons "Location"
                                                    (url:unparse-url url :readably-p t)))))))
                     (t
                      nil)))))
    (multiple-value-or 
     (try url)
     (multiple-value-or 
      (try (url-with-index.html-appended url))
      (multiple-value-or 
       (try (url-with-slash-appened url))
       (make-file-not-found-stream (url:url-pathname url)))))))

(defun pathname->url (pathname)
  (let ((dir (pathname-directory pathname))
        (name (pathname-name pathname))
        (type (pathname-type pathname)))
    (concatenate 'string
      (ecase (car dir)
        (:absolute
         (format nil "file:/~{~A/~}" (cdr dir)))
        (:relative
         (format nil "file:~{~A/~}" (cdr dir))))
      (cond ((and name type) (format nil "~A.~A" name type))
            (name (format nil "~A" name))
            (type (format nil ".~A" type))
            (t "")))))

(defun open-directory-as-file-document (pathname)
  (values
   (cl-char-stream->gstream 
    (make-string-input-stream
     (with-output-to-string (sink)
       (format sink "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\">~%")
       (format sink "<LINK REL=STYLESHEET HREF='file://closure/resources/css/directory.css' TYPE='text/css'>")
       (format sink "<TABLE width='100%'>~%")
       (format sink "<COLGROUP><COL width='0*'><COL width='0*'><COL width='1*'></COLGROUP>")
       (format sink "<TR><TH><B>Date</B><TH><B>Size</B><TH><B>Filename</B></TH>~%")
       (format sink "<TR><TD colspan=3><HR>")
       (let ((file-names (directory (merge-pathnames (make-pathname :name :wild :type :wild)
                                                     pathname))))
         (setf file-names (sort file-names #'string< :key #'namestring))
         (dolist (k file-names)
           (let ((url (pathname->url k))
                 (nam (namestring k))
                 (size (and #+CMU (is-regualar-file-p k)
                            (ignore-errors (with-open-file (in k :direction :input) 
                                             (file-length in)))))
                 (date (ignore-errors (unparse-http-date (file-write-date k)))))
             (format sink "<TR><TD class=date>~A<TD right class=size>~A<TD class=name><A href='~A'>~A</a>~%" 
                     (substitute (code-char 160) #\space date)
                     size url nam))))
       (format sink "</TABLE>~%"))))
   (list (cons "Content-Type" "text/html")) ))

(defun make-file-not-found-stream (pathname)
  (setq pn pathname)
  (error "File not found ~S" pathname))

(defun open-file-document-2 (pathname)
  ;; -> io ; header
  (let ((input (open pathname :direction :input :element-type '(unsigned-byte 8))))
    (let ((len (ignore-errors (file-length input)))
          (tm (ignore-errors (file-write-date input)))
          (mt (and (stringp (pathname-type pathname))
                   (find-mime-type-from-extension (pathname-type pathname)))))
      (values (cl-byte-stream->gstream input)
              (append (if len (list (cons "Content-Length" (format nil "~D" len))) nil)
                      (list (cons "Content-Type" (if mt 
                                                     (mime-type-name mt)
                                                   "text/plain")))
                      (if tm (list (cons "Last-Modified" (unparse-http-date tm))) nil))) )))

;;; -------------------------------------------------------------------------------------------
;;;  zip protocol
;;;

;; Reading on-line documentation thru' an HTML browser has various drawbacks.
;; Since most files are smaller than a reasonable file system block size, all
;; these little files consume more disk space than necessary. The solution is
;; to put all the files into a .zip file and browse them from therein.

;; zip files are "seekable", extracting a random archive element is
;; in O(1) wrt to the number of archive components. With .tar.gz
;; files it would involve decompression of the whole archive first,
;; which is O(n).

;; When specifying a document within a zip file, the `zip:' protocol is
;; used. A zip url consists of two parts: the specification of the zip
;; archive and the specification of the document within that archive. Syntax
;; is as follows:

;;
;;  zip:/<filename of zip archive>/<filename of document within archive>
;;
;;  e.g. #u"zip:documents/glspec.zip/index.html"
;;

;;  If no filename compoment is given the default is 'index.html' as
;;  always.

;; <FUTURE>

;; 	However this syntax is a kludge, since it involves an empty path component
;; 	as delimiter between the archive name and the document name. It would be
;; 	much more orthogonal, if the specification of the archive file itself could
;; 	be any url. Of course this would nested recursive urls. My suggestion for this is
;; 	using the host component and dropping in an url instead of a real host
;; 	name. You probably need parenthesis to be able to nest unambiguously.
 
;; 	So #u"zip:documents/glspec.zip//index.html" 
;; 	could become #u"zip://[file:documents/glspec.zip]/index.html"
 
;; 	These urls could get recursive then, so it would be possible to say
;; 	something like: 
;; 	  #u"zip://[zip://[file:/cdrom/dist/html-docu.zip]/opengl/glspec.zip]/index.html"
 
;; 	I'll eventually implement the above behaviour.

;; </FUTURE>

;; Back to what is actually implemented. To read a document from within a zip
;; archive, we simply pass the request to the `unzip' command. So you must
;; have installed this for a working zip protocol.

;; TODO
;;  - detect non-existing archives and non-existing archive documents.
;;  - when no archive file name is given, attempt to format the zip file
;;    directory as HTML, to be able to inspect the zip file.
;;  - detect the non-existence of the `unzip' command and give a reasonable 
;;    error message.

(defun open-zip-document (url)
  (multiple-value-bind (zip-archive-pathname archive-component-file-name) (split-zip-url url)
    (cond ((null zip-archive-pathname)
           (error "Bad zip url: ~S" url))
          (t
           (with-temporary-file (temp-filename)
             (let ((res (run-unix-shell-command (format nil "unzip -p ~A ~A >~A"
                                                        (namestring zip-archive-pathname)
                                                        archive-component-file-name
                                                        temp-filename))))
               (cond ((zerop res)
                      (values
                       (cl-byte-stream->gstream (open temp-filename
                                                      :direction :input
                                                      :element-type '(unsigned-byte 8)))
                       (list (cons "Content-Type" 
                                   (let ((mt (find-mime-type-from-extension
                                              (url-extension url))))
                                     (if mt
                                         (mime-type-name mt)
                                       "text/plain"))))))
                     (t
                      (error "unzip failed on ~S" url)) )))))))

(defun split-zip-url (url)
  ;; -> zip-archive-pathname ; archive-component-file-name
  ;;    or NIL
  (let* ((path (url:url-path url))
         (len (length path))
         n)
    (dotimes (i (length path))
      (setq n (url::copy-url url))
      (setf (url:url-path n) (subseq path 0 (- len i)))
      (when (ignore-errors (probe-file (url:url-pathname n)))
        (let ((component-filename (format nil "~{~A~#[~:;/~]~}" (subseq path (- len i)))))
          (when (or (string-equal component-filename "")
                    (char= (char component-filename (1- (length component-filename))) #\/))
            (setf component-filename (concatenate 'string component-filename "index.html")))
          (return (values (url:url-pathname n) component-filename)) )))))

;;; ===========================================================================================
;;;  Main Entry
;;;

(defun open-document-2 (url)
  ;; returns I/O and header
  (cond ((string-equal (url:url-protocol url) "file")
         (open-file-document url))
        ((string-equal (url:url-protocol url) "http")
         (http-open-document url :method :get))
        ((string-equal (url:url-protocol url) "https")
         (http-open-document url :method :get))
        ((string-equal (url:url-protocol url) "zip")
         (open-zip-document url))
        ((string-equal (url:url-protocol url) "ftp")
         (open-ftp-document url))
        (t
         (error "Unknown URL scheme in ~S" url))))

;;; ===========================================================================================
;;;  fake of ancient API

(defun open-document (url &optional reload-p binary-p any-mine-type?)
  "Opens an http document and returns two values: stream and mime-type"
  (multiple-value-bind (io header) (open-document-2 url)
    (values io 
            (multiple-value-bind (type subtype parameters)
                (parse-mime-content-type (get-header-field header :content-type))
              (find-mime-type (format nil "~A/~A" type subtype))))))

(defmacro with-open-document (((input mime-type) url 
                               &optional (reload-p nil) (binary-p nil) (cache-p t) (any-p nil))
			      &rest body)
  `(with-open-document-fn ,url ,reload-p ,binary-p ,cache-p ,any-p
                          #'(lambda (,input ,mime-type)
                              ,@body)))

(defun with-open-document-fn (url reload-p binary-p cache-p any-p cont)
  (multiple-value-bind (input mime) (open-document url reload-p binary-p any-p)
    (unwind-protect
        (funcall cont input mime)
      (g/close input))))

;;;

(defun dump-cache-toc (&optional (cache (http-cache)))
  (maphash (lambda (key value)
             key
             (print (list (hce-url value)
                          (hce-filename value)
                          (hce-filetype value))))
           (http-cache-entries cache)))

#||

(defclass proxy-http-connection-mixin () ())
(defclass direct-http-connection-mixin () ())
(defclass ssl-http-connection-mixin () ())
(defclass tcp-http-connection-mixin () ())

(defmethod format-url ((connection proxy-http-connection-mixin) url)
  (unparse-url-for-http/proxy url))

(defmethod format-url ((connection direct-http-connection-mixin) url)
  (unparse-url-for-http url))

(defmethod make-client-socket ((connection ssl-http-connection-mixin) host port)
  (g/open-inet-socket-ssl host port))

(defmethod make-client-socket ((connection http-connection) host port)
  (g/open-inet-socket host port))

||#

#||
(defclass uri () ())
(defclass pathname-uri-mixin () ())
(defclass an-http-url (uri pathname-uri-mixin) ())
(defclass http-url (an-http-url) ())
(defclass https-url (an-http-url) ())
(defclass file-url (uri pathname-uri-mixin) ())
(defclass zip-url (uri pathname-uri-mixin) ())
(defclass ftp-url (uri pathname-uri-mixin) ())

(defmethod open-document ((uri an-http-url)
                          &key direction)
  )

(defclass file-document () ())
(defclass directory-document () ())

||#

#||
;;;; ===========================================================================================

(defclass ftp-access-method () ())

(defmethod am/open-document ((self ftp-access-method) uri &rest options)
  (open-ftp-document uri))

(defconstant +ftp-access-methd+ (make-instance 'ftp-access-method))

;;;

(defun map-uri-to-access-method (uri)
  (cond ((string-equal (url:url-protocol url) "file")
         +file-access-method+)
        ((string-equal (url:url-protocol url) "ftp")
         +ftp-access-method+)
        ((string-equal (url:url-protocol url) "http")
         +http-access-method+)
        ((string-equal (url:url-protocol url) "https")
         +https-access-method+)
        ((string-equal (url:url-protocol url) "zip")
         +zip-access-method+) ))

(define-access-method #'(lambda (url)
                          (string-equal (url:url-protocol url) "file"))
    +file-access-method+)
        


;;;

(defclass http-access-method () ())

(defclass http-proxy-access-method (http-access-method)
  ((host :reader host-of :initarg :host)
   (port :reader port-of :initarg :port)))

(defclass http-direct-access-method (http-access-method)
  ())

||#

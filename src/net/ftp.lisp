;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: NETLIB; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: The FTP Protocol (rfc-959)
;;;   Created: 1997-08-27
;;;    Author: Gilbert Baumann <gilbert@base-engineering.com>
;;;   License: MIT style (see below)
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1997,1999 by Gilbert Baumann

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

(in-package :NETLIB)

;;; TODO
;; - actually implement the code to glue this to URLs.

;; - improve error handling, it easily happens that errors result in
;;   dangling connections.

;; - using SIZE etc. to retrieve the directory listings is time consuming,
;;   so try get a normal directory listing and then decide, if we could
;;   parse that. Also when using interactive, I would prefer to see the
;;   original (host-dependent formated) directory listing decorated with
;;   hyperlinks.

;; - implement the dispatch on the URL's type parameter

;; - clean up;

;; - implement the dir herald fetching and the site's herald fetching.
;;   (when I look to some directory I ideally want to see all the heralds on
;;   the way up to the particular directory).


;;; About URLs, FTP and Directories.

;; RFC-1738 states exactly how to retrieve a particular URL. The actions to be
;; taken are the following:

;; 1. Initiate the connection to the ftp server given by the host and port part
;;    of the URL.

;; 2. Login using the user and password field of the url. [Anonymous FTP is the
;;    default].

;; 3. For each directory component of the URL issue _one_ CWD command. For
;;    instance the URL ftp://ftp.rz.uni-karlsruhe.de/pub/linux/mirror.sunsite/ should 
;;    translate into:
;;      CWD pub                 [not /pub!]
;;      CWD linux
;;      CWD mirror.sunsited

;;    [Some servers require, that you CWD to the directory in one operation; to
;;    achieve this with URLs requires you to quote some #\/'s. 
;;    E.g. "ftp://foohost/%2Fpub%2Fdocs%2Fmumble" => CWD /pub/docs/mumble
;;    An URL for the TOPS-20 host in RFC-959 would be 
;;    "ftp://foohost/<some.where.overrainbow>/file" ].

;; 4. Dispatch on the `type` parameter

;;      'd' - retrieve directory listing via NLST
;;      'a' - use TYPE A and RETR
;;      'i' - use TYPE I and RETR

;; If somebody knows some non-UNIX FTP host, with unusual FS semantics, let me
;; know.

;;; The pool

;; Since initiating a new ftp connection is quite time consuming, ftp
;; connections are maintain in a pool, which is not unlike a resource. Somebody
;; could aquire a ftp connection by MAKE-FTP-CONNECTION. If there is an idle
;; ftp connection to that particular host (user name and password must match
;; also) that connection is returned, otherwise a new one is created. There is
;; a daemon, which takes care of expired ftp connections and removes them from
;; the pool arcordingly.

;;; Reusing a Connection

;; When recycling an established connection, we have to CWD to different
;; directories eventually. Unfortunately there is no reliable way to undo a
;; given CWD operation. (Even not with UNIX hosts -- Somebody would think a
;; simple "CDUP" or "CWD .." would be enough; Not quite, due to symbolic
;; links, which are an afterthought, there is confusion about what CL calls
;; :UP and :BACK).

;; So I implemented the following scheme:

;; Upon login to the server issue PWD. The server then tells me its default
;; working directory. Later, when forced to change the directory, I'll try a
;; CWD with exactly the string PWD gave me and verify with another PWD. If that
;; fails I must resort to attempt to REIN the connection. If REIN happens fail,
;; I will have to give up the connection and reinitiate a new one.

;;; Security (or lack thereof)

;; The ftp connection object has a slot where the password is stored. There is
;; absolutely no provision to hide or otherwise encryt this password in core.
;; However I tried to take care that ftp passwords are not printed to the users
;; terminal unless explicitly requested so (e.g. via describe), so that your
;; password should be pretty safe from bystanders.

;;; Another Word of Advice

;; Since years, I have a named pipe in my home directory and so funny things
;; happen from time to time. Guess what happens, when you try to issue SIZE
;; on it? The ftp server (wu-2.4.2) hangs. (Are they issuing 'wc' on it or
;; what?) So much to the reliabilty of Unix, it's all a hack. So be careful;
;; I am not willing to fix other peoples bugs.

(defconstant *default-ftp-port* 21
  "The standard port for ftp control connections.")

(defparameter *command-not-understood-reply-codes*
    '(500  ;; Syntax error, command unrecognized.
      501  ;; Syntax error in parameters or arguments.
      202  ;; Command not implemented, superfluous at this site.
      502  ;; Command not implemented.
      503  ;; Bad sequence of commands.
      504) ;; Command not implemented for that parameter.
  "A list of reply codes which are good indicators, that a command is not understood.")

(defparameter *trace-ftp-p* nil)

(defclass ftp-connection ()
  ((host       
    :documentation "The name of the ftp server's host"
    :initarg :host)

   (port
    :documentation "The number of the control connection's TCP/IP port."
    :initarg :port)

   (username
    :documentation "The username used to login."
    :initarg :username)

   (password
    :documentation "The password used to login."
    :initarg :password)
   
   (io          
    :documentation "A gstream; The bidirectional control connection."
    :initform nil)

   (initial-pwd 
    :documentation "The servers initial PWD. NIL == not yet queried. :UNKNOWN == couldn't been queried."
    :initform nil)

   (current-dir 
    :documentation "The current directory. A list of CWD arguments."
    :initform nil)

   (herald
    :documentation "The ftp servers herald as often given during login.")

   (current-dir-herald
    :documentation "The herald of the current directory. (often given when CWD'ing).") 

   (has-size-p 
    :documentation "The server understands SIZE. One of T, NIL or :UNKNOWN"
    :initform :unknown)
   
   (data-transfer-running-p :initform nil) )

  (:documentation 
   "A connection to an FTP host."))

;;;
   
(defclass ftp-data-connection (use-byte-for-char-stream-flavour gstream)
  ((ftp-connection :initarg :ftp-connection)
   (socket :initarg :socket)
   (auto-put-back-p
    :initarg :auto-put-back-p
    :initform nil
    )))

(defmethod g/read-byte ((self ftp-data-connection) &optional (eof-error-p t) eof-value)
  (with-slots (socket) self
    (g/read-byte socket eof-error-p eof-value)))

(defmethod g/unread-byte (byte (self ftp-data-connection))
  (with-slots (socket) self
    (g/unread-byte byte socket)))

(defmethod g/close ((self ftp-data-connection) &key abort)
  (with-slots (socket ftp-connection auto-put-back-p) self
    (g/close socket)
    (with-slots (data-transfer-running-p) ftp-connection
      (multiple-value-prog1 (ftp/read-respose ftp-connection)
        (setf data-transfer-running-p nil)))
    (when auto-put-back-p
      (put-ftp-connection-into-pool ftp-connection)) ))

;;;

(defmethod print-object ((self ftp-connection) sink)
  (with-slots (host port data-transfer-running-p io) self
    (format sink "#<~S to `~A'~A~A~A~A>"
            (type-of self) host 
            (if (= port *default-ftp-port*) "" (format nil " on port ~D" port))
            (if data-transfer-running-p " [data connection exists]" "")
            (if (member self *connection-pool*) " [lurking in pool]" "")
            (if (not io) " [unconnected]" ""))))

(defmethod ftp/trace-connection-p ((self ftp-connection))
  t)

(defmethod ftp/read-line ((self ftp-connection) &optional (error-p nil) eof-value)
  "Read one line from the control connection."
  (with-slots (io) self
    (let ((r (g/read-line* io error-p eof-value)))
      (when (ftp/trace-connection-p self)
        (format t "~&;; <-- ~A" r))
      r)))

(defmethod ftp/read-respose ((self ftp-connection))
  "Read a repsonse from the ftp server."
  (with-slots (host) self
    (let ((first-line (ftp/read-line self nil :eof)))
      (unless (and (>= (length first-line) 4)
                   (digit-char-p (char first-line 0))
                   (digit-char-p (char first-line 1))
                   (digit-char-p (char first-line 2))
                   (member (char first-line 3) '(#\space #\-)))
        (error "Bad response line from ftp server ~A: ~S" host first-line))
      ;;
      (let ((response-code (parse-integer first-line :end 3))
            (message (subseq first-line 4)))
        (when (char= (char first-line 3) #\-)
          (do ((line (ftp/read-line self t) (ftp/read-line self t)))
              ((and (>= (length line) 4)
                    (char= (char line 3) #\space)
                    (string= first-line line :start1 0 :end1 3 :start2 0 :end2 3))
               (setf message (concatenate 'string message (string #\newline) (subseq line 4))))
            (setf message (concatenate 'string message (string #\newline) line))))
        (values response-code message)))))
     
(defmethod ftp/initiate-connection ((self ftp-connection))
  (with-slots (io host port) self
    (setf io (g/open-inet-socket host port))
    (ftp/read-respose self)))

(defmethod ftp/issue-command ((self ftp-connection) command-string)
  (with-slots (io data-transfer-running-p) self
    (cond (data-transfer-running-p
           (error "Attempt to issue a command to an ftp connection, while a data transfer is active."))
          (t
           ;; we really should listen before issueing an command.
           (when (ftp/trace-connection-p self)
             (format t "~&;; --> ~A" command-string))
           (g/write-string command-string io)
           (g/write-byte 13 io)
           (g/write-byte 10 io)
           (g/finish-output io)
           (ftp/read-respose self)))))

(defmethod ftp/login ((self ftp-connection) username password)
  "Attempt to login into the ftp server using the given username and password."
  (with-slots (host) self
    (multiple-value-bind (code message)
        (ftp/issue-command self (format nil "USER ~A" username))
      (cond ((<= 200 code 299)
             ;; everything fine, we are in
             nil)
            ((<= 300 code 399)
             ;; password needed
             (multiple-value-bind (code message) 
                 (ftp/issue-command self (format nil "PASS ~A" password))
               (cond ((<= 200 code 299)
                      ;; fine, all went well
                      (values code message))
                     ((<= 300 code 399)
                      (error "ftp server '~A' want an ACCT response, we have no such concept; you lost."
                             host))
                     (t
                      (error "While attempting to login to ftp server '~A', it said:~%~D ~A"
                             host code message)))))
            (t
             (error "While attempting to login to ftp server '~A', it said:~%~D ~A"
                    host code message))))))

(defmethod ftp/simple-error ((self ftp-connection) code message desc)
  (with-slots (host) self
    (error "~A: ftp server '~A' said:~%~D ~A"
           desc host code message)))
  
(defmethod ftp/enter-passive-mode ((self ftp-connection))
  (multiple-value-bind (code message) (ftp/issue-command self "PASV")
    (cond ((<= 200 code 299)
           (ftp/parse-pasv-response message))
          (t
           (ftp/simple-error self code message "While attempting to switch to passive mode")))))

(defun ftp/parse-pasv-response (str)
  (let* ((i (position #\( str :test #'char=))
         (j (position #\) str :test #'char=))
         (nums (and i j (< i j)
                    (mapcar (lambda (x)
                              (ignore-errors (parse-integer x :junk-allowed nil)))
                            (split-by #\, (subseq str (+ i 1) j))))))
    (cond ((and (= (length nums) 6)
                (every (lambda (x)
                         (and (integerp x) (<= 0 x 255)))
                       nums))
           (values (format nil "~D.~D.~D.~D" (first nums) (second nums) (third nums) (fourth nums))
                   (dpb (fifth nums) (byte 8 8) (sixth nums))))
          (t
           (error "This response to a PASV request is not syntactically right: ~S" str)) )))

(defmethod ftp/ensure-connected ((self ftp-connection))
  nil)

(defmethod ftp/cwd ((self ftp-connection) directory &key (errorp t))
  "Issue an CWD command. If the operation fails and errorp is non-NIL raise an error.
   Returns two values: successp and a possible herald."
  (with-slots (io) self
    (ftp/ensure-connected self)
    (multiple-value-bind (code message) (ftp/issue-command self (format nil "CWD ~A" directory))
      (cond ((<= 200 code 299)
             ;; FIXME -- strip message down to herald.
             (values t message))
            (t
             (if errorp
                 (ftp/simple-error self code message "While attempt to change directory")
               (values nil nil)))))))

(defmethod ftp/pwd ((self ftp-connection))
  "Issue an PWD command and returns the string or NIL if operation failed."
  (with-slots (io) self
    (ftp/ensure-connected self)
    (multiple-value-bind (code message) (ftp/issue-command self "PWD")
      (cond ((= code 257)
             (ftp/maybe-parse-PWD-response message))
            (t
             nil)))))

(defun ftp/maybe-parse-PWD-response (string)
  "Attempt to parse a PWD respone. Returns either the actual directory string,
   or NIL if parsing failed."
  ;; according to RFC959 syntax is: 257 <space> <string> <space> <comment>, where
  ;; <string> is delimited by #\" and uses a double #\" to escape an embedded #\".
  (and (> (length string) 0) (char= (char string 0) #\")
       (labels ((looop (i)
                  (let ((j (position #\" string :start i)))
                    (if j
                        (if (and (< (1+ j) (length string)) (char= (char string (1+ j)) #\"))
                            (let ((more (looop (+ j 2))))
                              (and more
                                   (concatenate 'string (subseq string i (1+ j)) more)))
                          (subseq string i j))
                      nil))))
         (looop 1))))

(defmethod ftp/fetch-initial-pwd ((self ftp-connection))
  (with-slots (initial-pwd) self
    (setf initial-pwd (or (ftp/pwd self) :unknown))))

(defmethod ftp/move-to-initial-pwd ((self ftp-connection))
  (with-slots (current-dir initial-pwd username password) self
    (labels ((by-rein ()
               (if (ftp/re-initialize self)
                   (ftp/login self username password)
                 (ftp/re-open self))))
      (unless (null current-dir)
        (setf current-dir nil)
        (cond ((stringp initial-pwd)
               (if (and (ftp/cwd self initial-pwd :errorp nil)
                        (equal initial-pwd (ftp/pwd self)))
                   t
                 (by-rein)))
              (t
               (by-rein)))))))

(defmethod ftp/re-open ((self ftp-connection))
  (with-slots (host username password current-dir data-transfer-running-p io) self
    (when io
      (when (ftp/trace-connection-p self)
        (format t "~&;; *** reconnecting to ~A." host))
      (ignore-errors (ftp/close self)))
    (setf data-transfer-running-p nil)
    (ftp/initiate-connection self)
    (ftp/login self username password)
    (ftp/fetch-initial-pwd self)
    (setf current-dir nil) ))

(defmethod ftp/cd-to ((self ftp-connection) directory)
  (with-slots (current-dir) self
    (cond ((and (>= (length directory) (length current-dir))
                (equal current-dir (subseq directory 0 (length current-dir))))
           (ftp/cd-to-relative self (subseq directory (length current-dir))))
          (t
           (ftp/move-to-initial-pwd self)
           (ftp/cd-to-relative self directory)))))

(defmethod ftp/cd-to-relative ((self ftp-connection) directory)
  (with-slots (current-dir) self
    (dolist (component directory)
      (ftp/cwd self component :errorp t)
      (setf current-dir (nconc current-dir (list component))))))
  
(defmethod ftp/re-initialize ((self ftp-connection))
  (multiple-value-bind (code message) (ftp/issue-command self "REIN")
    (while (<= 100 code 199)
      (multiple-value-setq (code message) (ftp/read-respose self)))
    (cond ((<= 200 code 299)
           t)
          (t
           nil))))

(defmethod ftp/looks-like-directory-p ((self ftp-connection) path)
  (with-slots (current-dir) self
    (ftp/cd-to self (butlast path))
    (if (ftp/cwd self (car (last path)) :errorp nil)
        (progn
          (setf current-dir (nconc current-dir (list (car (last path)))))
          t)
      nil)))

(defstruct directory-entry
  name
  size)

(defmethod ftp/get-directory-listing ((self ftp-connection) path &key (fetch-size-p nil))
  (ftp/cd-to self path)
  (multiple-value-bind (code message io) (ftp/issue-command-with-transfer self "NLST")
    (declare (ignore code message))
    (when io
      (let ((res nil))
        (unwind-protect
            (do ((x (g/read-line* io nil nil) (g/read-line* io nil nil)))
                ((null x))
              (push (make-directory-entry :name x :size nil) res))
          (g/close io))
        (when fetch-size-p
          (dolist (k res)
            (setf (directory-entry-size k) (ftp/query-file-size self (directory-entry-name k)))))
        res))))

(defmethod ftp/get-verbose-directory-listing ((self ftp-connection) path)
  (ftp/cd-to self path)
  (multiple-value-bind (code message io) (ftp/issue-command-with-transfer self "LIST")
    (declare (ignore code message))
    (when io
      (let ((res ""))
        (unwind-protect
            (do ((x (g/read-line* io nil nil) (g/read-line* io nil nil)))
                ((null x))
              (setq res (concatenate 'string res x (string #\newline))))
          (g/close io))
        res))))

(defmethod ftp/query-file-size ((self ftp-connection) filename)
  (with-slots (has-size-p) self
    (unless (eq nil has-size-p)
      (multiple-value-bind (code message) (ftp/issue-command self (format nil "SIZE ~A" filename))
          (cond ((member code *command-not-understood-reply-codes*)
                 (setf has-size-p nil)
                 nil)
                (t
                 (setf has-size-p t)
                 (maybe-parse-integer message)))))))

(defmethod ftp/issue-command-with-transfer ((self ftp-connection) command-string)
  (with-slots (data-transfer-running-p) self
    (let (data-io)
      (multiple-value-bind (data-host data-port) (ftp/enter-passive-mode self)
        (setf data-io (make-instance 'ftp-data-connection
                        :socket (g/open-inet-socket data-host data-port)
                        :ftp-connection self))
        (multiple-value-bind (code message) (ftp/issue-command self command-string)
          (cond ((<= 100 code 199)
                 (setf data-transfer-running-p t)
                 (values code message data-io))
                (t
                 (g/close (slot-value data-io 'socket))
                 (values code message nil))))))))


(defmethod ftp/get ((self ftp-connection) url &key type auto-put-back-p)
  ;; -> directory-listing; :directory
  ;; or stream ; :file
  (let ((path (url-path url)))
    (setf path (or path (list :absolute)))
    (assert (eq (car path) :absolute))
    ;; ? index.html
    ;; ? handle empty file components
    (let ((dir  (butlast (cdr path)))
          (file (car (last (cdr path)))))
      (cond ((or (null file) (string= file ""))
             (values
              (prog1 (ftp/get-directory-listing self dir :fetch-size-p t) 
                (when auto-put-back-p
                  (put-ftp-connection-into-pool self)))
              :directory))
            ((ftp/looks-like-directory-p self (append dir (list file)))
             (values
              (prog1
                  (ftp/get-directory-listing self (append dir (list file)) :fetch-size-p t)
                (when auto-put-back-p
                  (put-ftp-connection-into-pool self)))
              :directory))
            (t
             (ftp/cd-to self dir)
             (multiple-value-bind (code message io)
                 (ftp/issue-command-with-transfer self (format nil "RETR ~A" file))
               (declare (ignore code message))
               (cond (io
                      (setf (slot-value io 'auto-put-back-p) auto-put-back-p)
                      (values io :file))
                     (t
                      nil)))) ))))

(defun ftp-get-url (url)
  (let ((conn (make-ftp-connection-for-url url)))
    (ftp/get conn url :auto-put-back-p t)))

(defmethod ftp/retrieve ((self ftp-connection) url &key (local-name nil))
  (ftp/issue-command self "TYPE I")
  (multiple-value-bind (thing kind) (ftp/get self url)
    (ecase kind 
      (:directory 
       (format T "~&Directory listing of `~A\'" (url:unparse-url url))
       (dolist (k thing)
         (format T "~&~8D~10T~A" (directory-entry-size k) (directory-entry-name k)))
       )
      (:file
       (unwind-protect
           (progn
             (unless local-name
               (setf local-name
                     (merge-pathnames (car (last (url:url-path url)))
                                      *incoming-pathname-defaults*)))
             (with-open-file (sink local-name :direction :output :element-type '(unsigned-byte 8))
               (setf sink (cl-byte-stream->gstream sink))
               (format T "~&;; retrieving `~A' as '~A' " (url:unparse-url url) (namestring local-name))
               (copy-gstream thing sink)
               (format T " done") (finish-output)))
             (g/close thing))))))


;;;;;;;;;;;;;;;;;;;;;


(defmethod ftp/cd ((self ftp-connection) dir)
  (with-slots (current-dir) self
    (multiple-value-bind (code message) (ftp/issue-command self (format nil "CWD ~A" dir))
      (unless (<= 200 code 299)
        (ftp/simple-error self code message "While attempt to change working directory"))
      (push dir current-dir))))

(defmethod ftp/cdup ((self ftp-connection))
  (with-slots (current-dir) self
    (multiple-value-bind (code message) (ftp/issue-command self (format nil "CDUP"))
      (unless (<= 200 code 299)
        (ftp/simple-error self code message "While attempt to climb up the directory hierarchy"))
      (pop current-dir))))

(defmethod ftp/cdto ((self ftp-connection) dir)
  (with-slots (current-dir) self
    (dolist (k (compute-cd-commands (reverse current-dir) dir))
      (ecase (car k)
        (:up (ftp/cdup self))
        (:cd (ftp/cd self (cadr k)))))))

(defmethod ftp/is-directory-p ((self ftp-connection) path)
  (with-slots (current-dir) self
    (ftp/cdto self (butlast path))
    (multiple-value-bind (code message) (ftp/issue-command self (format nil "CWD ~A" (car (last path))))
      (declare (ignore message))
      (if (<= 200 code 299)
          (progn
            (push (car (last path)) current-dir)
            t)
        nil))))

(defmethod ftp/fetch-directory-listing ((self ftp-connection) path &optional (cmd "LIST"))
  (ftp/cdto self path)
  (multiple-value-bind (code message io) (ftp/issue-command-with-transfer self cmd)
    (cond ((<= 100 code 199)
           (prog1
               (gstream-as-string io)
             (g/close io)))
          (t
           (ftp/simple-error self code message "While trying to retrieve directory listing")))))

(defmethod ftp/close ((self ftp-connection))
  ;; be nice
  (with-slots (io) self
    (ftp/issue-command self "QUIT")
    (g/close io)
    (setf io nil)))

#||
(defun dump (io &optional (n most-positive-fixnum))
  (do ((x (g/read-line* io nil nil) (g/read-line* io nil nil)))
      ((or (<= n 0) (null x)))
    (decf n)
    (fresh-line)
    (write-string x)))

||#

(defparameter *anonymous-ftp-password* "joe@random.host.org")

(defun make-ftp-connection-for-url (url)
  (let ((host (or (url:url-host url) "localhost"))
        (port (or (url:url-port url) *default-ftp-port*)))
    (multiple-value-bind (user pass)
        (if (url:url-user url)
            (values (url:url-user url) (url:url-password url))
          (values "anonymous" *anonymous-ftp-password*))
      (make-ftp-connection host port user pass))))


#|

      One important group of informational replies is the connection
      greetings.  Under normal circumstances, a server will send a 220
      reply, "awaiting input", when the connection is completed.  The
      user should wait for this greeting message before sending any
      commands.  If the server is unable to accept input right away, a
      120 "expected delay" reply should be sent immediately and a 220
      reply when ready.  The user will then know not to hang up if there
      is a delay.



      |#

(defmethod ftp/retrieve-files ((self ftp-connection) file-list)
  (dolist (k file-list)
    (ftp/retrieve self k)))


;;; TODO
;; upon initial connection also account for 1xx responses
;; via cerror, give the user the opportunity to correct wrong passwds
;; timeouts
;; better error handling
;; we really want 421 handling (reopening code is allready there).
;; MDTM

#||
(defparameter /dev/console nil)
(defvar *ftp-maintainance-daemon-process* nil)
(defparameter *ftp-maintainance-interval* 60)

(defun ftp/maintainace-daemon-tick ()
  '(format /dev/console ";; ~A - ftp maintainance tick ~%" (unparse-http-date (get-universal-time)))
  (finish-output /dev/console))

(defun ftp/start-maintainace-daemon ()
  (when *ftp-maintainance-daemon-process*
    (cerror "Kill daemon and restart." "Daemon already running.")
    (mp:process-kill *ftp-maintainance-daemon-process*))
  (setf /dev/console
    (open "/dev/console" :direction :output :if-exists :overwrite))
  (setf *ftp-maintainance-daemon-process*
    (mp:process-run-function "FTP maintainance daemon"
                             #'(lambda ()
                                 (loop
                                   (ftp/maintainace-daemon-tick)
                                   (sleep *ftp-maintainance-interval* "sleeping (snore, snore ...)"))))))
||#

(defvar *connection-pool* nil)
(defvar *connection-pool-lock* (mp/make-lock :name "FTP connections pool lock"))

(defmacro with-ftp-connection-pool (dummy &body body)
  dummy
  `(mp/with-lock (*connection-pool-lock*)
     ,@body))

(defun put-ftp-connection-into-pool (connection)
  (with-ftp-connection-pool ()
    (push connection *connection-pool*)))

(defun remove-ftp-connection-from-pool (connection)
  (with-ftp-connection-pool ()
    (setf *connection-pool* (delete connection *connection-pool*))))

(defmethod ftp-connection-tick ((self ftp-connection))
  (with-slots (host io) self
    (with-ftp-connection-pool ()
      (let ((died-p (when (listen (slot-value io 'glisp::cl-stream))
                      (multiple-value-bind (code message) (ftp/read-respose self)
                        (and (= code 421) message)))))
        (when died-p
          (format /dev/console
                  ";; ~A -- ftp connection to ~A died.~%"
                  (unparse-http-date (get-universal-time))
                  host)
          (format /dev/console ";;                               -- ~A~%" died-p)
          (finish-output /dev/console)
          (setf io nil)
          (remove-ftp-connection-from-pool self))))))

(defun ftp/maintainace-daemon-tick ()
  (with-ftp-connection-pool ()
    (mapc #'ftp-connection-tick *connection-pool*))
  (finish-output /dev/console))


(defun make-ftp-connection (host port username password)
  (or (with-ftp-connection-pool ()
        (dolist (conn *connection-pool*)
          (when (and (equal (slot-value conn 'host) host)
                     (equal (slot-value conn 'port) port)
                     (equal (slot-value conn 'username) username)
                     (equal (slot-value conn 'password) password))
            (ftp-connection-tick conn)
            (cond ((member conn *connection-pool*)
                   (remove-ftp-connection-from-pool conn)
                   (return conn))
                  (t
                   (return (make-ftp-connection host port username password)))))))
      (let ((res (make-instance 'ftp-connection 
                   :host host
                   :port port
                   :username username
                   :password password)))
        (ftp/re-open res)
        res)))


;;;; ----------------------------------------------------------------------------------------------------


(defparameter *incoming-pathname-defaults* 
    (merge-pathnames (make-pathname :directory (list :relative "incoming")) (user-homedir-pathname)))

(defmethod ftp/retrieve ((self ftp-connection) filename &key (local-name nil))
  (multiple-value-bind (code message io) (ftp/issue-command-with-transfer self (format nil "RETR ~A" filename))
    (cond ((<= 100 code 199)
           (unwind-protect
               (progn
                 (unless local-name
                   (setf local-name (merge-pathnames filename *incoming-pathname-defaults*)))
                 (with-open-file (sink local-name :direction :output :element-type '(unsigned-byte 8))
                   (format T "~&;; retrieving `~A' as '~A' " filename (namestring local-name))
                   (copy-gstream io sink)
                   (format T " done") (finish-output)))
             (g/close io)))
          (t
           (ftp/simple-error self code message "While trying to retrieving.")))))

(defun ftp/retrieve-if-not-yet-existing (conn filename &key (local-name nil))
  (unless local-name
    (setf local-name (merge-pathnames filename *incoming-pathname-defaults*)))
  (if (probe-file local-name)
      (format nil "~&;; skiping '~A'" local-name)
    (ftp/retrieve conn filename :local-name local-name)))

(defmethod g/write-byte (byte (sink stream))
  (write-byte byte sink))

(defun copy-gstream (input output &key (buffer-size 4096) (element-type '(unsigned-byte 8)))
  (multiple-value-bind (read-fn write-fn)
      (cond ((subtypep element-type 'integer) 
             (values #'g/read-byte-sequence #'g/write-byte-sequence))
            ((subtypep element-type 'character) 
             (values #'g/read-char-sequence #'g/write-char-sequence))
            (t 
             (error "Element type ~S is ambiguous." element-type)))
    (let ((buffer (make-array buffer-size :element-type element-type)))
      (do ((n (funcall read-fn buffer input)
              (funcall read-fn buffer input)))
          ((= n 0))
        ;;(princ "@") (finish-output)
        (funcall write-fn buffer output :end n))
      (g/finish-output output))))
    

;;;;;;;





(defmethod ftp/get ((self ftp-connection) url &key type auto-put-back-p)
  ;; -> directory-listing; :directory
  ;; or stream ; :file
  (let ((path (url-path url)))
    (setf path (or path (list :absolute)))
    (assert (eq (car path) :absolute))
    ;; ? index.html
    ;; ? handle empty file components
    (let ((dir  (butlast (cdr path)))
          (file (car (last (cdr path)))))
      (cond ((or (null file) (string= file ""))
             (values
              (prog1 (ftp/get-directory-listing self dir :fetch-size-p t) 
                (when auto-put-back-p
                  (put-ftp-connection-into-pool self)))
              :directory))
            ((ftp/looks-like-directory-p self (append dir (list file)))
             (values
              (prog1
                  (ftp/get-directory-listing self (append dir (list file)) :fetch-size-p t)
                (when auto-put-back-p
                  (put-ftp-connection-into-pool self)))
              :directory))
            (t
             (ftp/cd-to self dir)
             (multiple-value-bind (code message io)
                 (ftp/issue-command-with-transfer self (format nil "RETR ~A" file))
               (declare (ignore code message))
               (cond (io
                      (setf (slot-value io 'auto-put-back-p) auto-put-back-p)
                      (values io :file))
                     (t
                      nil)))) ))))

(defun ftp-get-url (url)
  (let ((conn (make-ftp-connection-for-url url)))
    (ftp/get conn url :auto-put-back-p t)))


(defun html-escape-string (string)
  "Given a string, escape the characters #\< and #\& using entities."
  (let ((lt-count (count #\< string))
	(amp-count (count #\& string)))
    (cond ((and (zerop lt-count) (zerop amp-count))
	   ;;nothing to do we are done
	   string)
	  (t
	   (let* ((new-length (+ (length string) (* 3 lt-count) (* 4 amp-count)))
		  (res (make-array new-length :element-type 'base-char))
		  (d-ptr 0))
	     (labels ((put (c)
			(setf (char res d-ptr) c)
			(incf d-ptr)))
	       (do ((s-ptr 0 (+ s-ptr 1)))
		   ((= s-ptr (length string)))
		 (let ((ch (char string s-ptr)))
		   (cond ((char= ch #\&) (map nil #'put "&amp;"))
			 ((char= ch #\<) (map nil #'put "&lt;"))
			 (t (put ch))))))
	     (assert (= d-ptr new-length))
	     res)) )))

(defun decorate-ftp-directory-listing (string url)
  (let ((title "ftp Directory"))
    (with-output-to-string (sink)
      (format sink "<head><title>~A</title>~%" title)
      (format sink "<body><h1>~A</h1>~%" title)
      (format sink "<hr>~%")
      (format sink "Directory <CODE>~A</CODE> on ~A.~%" 
              (url-filename url) (or (url-host url) "local host"))
      #||
      (when (>= (count #\newline who) 1)
	 (format sink "<pre>~%")
	 (write-string (hyperlinkify-urls-in-string (html-escape-string who)) sink)
	 (format sink "</pre>~%")
	 (format sink "<hr>~%"))
      ||#
      (format sink "<pre>~%")
      (with-input-from-string (input string)
        (do ((line (read-line input nil nil) (read-line input nil nil)))
            ((null line))
          (setq line (delete #\return line))
          (setq line (html-escape-string line))
          (let ((k (search " -> " line)))
            (when k
              ;; xxx bad hack
              (setq line (subseq line 0 k))))
          (let ((j (position #\space line :from-end t)))
            (cond ((and j (find #\space line :end j))
                   (cond ((char= (char line 0) #\d)
                          (format sink "    ~A <b><a href='~A'>~A</a></b>~%"
                                  (subseq line 0 j) (subseq line (+ j 1)) (subseq line (+ j 1))) )
                         (t
                          (format sink "    ~A <a href='~A'>~A</a>~%"
                                  (subseq line 0 j) (subseq line (+ j 1)) (subseq line (+ j 1))))))
                  (t
                   (format sink "    ~A~%" line)))))
        (format sink "</pre>~%")))))

(defun open-ftp-directory (conn url)
  (let ((listing
         (ftp/get-verbose-directory-listing conn (cdr (url:url-path url)))))
    (put-ftp-connection-into-pool conn)
    (values
     (cl-char-stream->gstream (make-string-input-stream (decorate-ftp-directory-listing listing url)))
     (list (cons "Location" (url:unparse-url 
                             (let ((new (url:copy-url url)))
                                   (setf (url:url-path new) (append (url:url-path url) (list "")))
                                   new)
                             :readably-p t))
           (cons "Content-Type" "text/html")) )))

(defun open-ftp-file (conn url)
  (let ((path (url-path url))
        (header nil))
    (let ((dir  (butlast (cdr path)))
          (file (car (last (cdr path)))))
      (ftp/cd-to conn dir)
      (ftp/issue-command conn "TYPE I") ;xxx should depend on mime type
      (let ((size
             (ftp/query-file-size conn file)))
        (when size
          (push (cons "Content-Length" (format nil "~D" size))
                header)))
      (multiple-value-bind (code message io)
          (ftp/issue-command-with-transfer conn (format nil "RETR ~A" file))
        (declare (ignore code message))
        (cond (io
               (setf (slot-value io 'auto-put-back-p) t)
               (values io
                       (list*
                        (cons "Location" (url:unparse-url url))
                        (cons "Content-Type" 
                              (let ((mt (find-mime-type-from-extension (url-extension url))))
                                (if mt
                                    (mime-type-name mt)
                                  "text/plain")))
                        header
                        )))
              (t
               (error "Cannot retrieve ~A" url)))))))


(defun open-ftp-document (url)
  ;; -> input ; header
  (let ((conn (make-ftp-connection-for-url url)))
    (let ((path (or (url:url-path url) (list :absolute ""))))
      (assert (eq (car path) :absolute))
      (cond ((string= (car (last path)) "")
             (open-ftp-directory conn
                                 (let ((new (url:copy-url url)))
                                   (setf (url:url-path new) (butlast path))
                                   new)))
            ((ftp/looks-like-directory-p conn (cdr path))
             (open-ftp-directory conn
                                 (let ((new (url:copy-url url)))
                                   (setf (url:url-path new) path)
                                   new)))
            (t
             (open-ftp-file conn
                            (let ((new (url:copy-url url)))
                              (setf (url:url-path new) path)
                              new))) ))))
              

;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: URL; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Parsing and Unparsing URLs, second approach
;;;   Created: 1997-05-10
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: GPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1997-2002 by Gilbert Baumann

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

;;; Changes

;;  When        Who     What
;; ----------------------------------------------------------------------------
;;  2002-07-23  GB      - new functions:
;;                        UNMERGE-URLS
;;                        PATHNAME-URL
;;
;;  2001-05-25  GB      - DEFPACKAGE
;;                      - URL-EXTENSION, new function
;;  1999-08-26  GB      - PARSE-PATH, new function

;(require :glisp)

(defpackage :url
  (:use :glisp)
  (:export
   #:parse-url
   #:unparse-url
   #:make-url
   #:url-protocol
   #:url-host
   #:url-port
   #:url-user
   #:url-password
   #:url-path
   #:url-parameters
   #:url-query
   #:url-anchor
   #:merge-url
   #:url-pathname
   #:pathname-url
   #:url-filename
   #:url-equal-p
   #:url-p
   #:url
   #:copy-url
   #:url-logical-host-translator
   #:translate-logical-url
   #:parse-path
   #:url-extension
   #:unmerge-urls))

(in-package :URL)

;;; Part of the Closure browser.
;; implements rfc1738, rfc1808 and parts of rfc1866.
;; Assumes your Lisp uses the ASCII character set or a super set thereof.

;;; TODO / future

;; We should soon extend this to parse URIs. The URL data type itself must
;; then become a CLOS class, with individual subclasses for individual
;; protocols. Also the parser, unparser and the merger should be modular.
;; PARSE-URL should pick the actual parser to use based on the scheme.
;; This way we could then easily extend the URL syntax to new schemes.

;; Then there also will be a higher level resource identifier to also
;; specify the action to take. For instance we need a way to distinguish
;; POST from GET requests. (also POST request are not considered to be
;; idempotent).

;; --

;; Also, I'd like that the url accessors also accept strings and rods
;; like the Common Lisp password accessors do it.

;; Likewise it would be a good idea, if the URL functions also accept
;; Common Lisp pathnames. Only design question: Where to put the
;; device? Make it a parameter?

;; --GB 2002-07-23

;;; Hmm?!
;; Where is the substitution of #\Space for #\+ handled? There is nothing
;; said about that in RFC 1808 itself. Several places are possible:
;;  - instrict to url encoding
;;  - property of the query part  [as implemented currently]
;;  - method to encode spaces in the GET method

(defstruct (url (:print-function url-printer))
  protocol              ;NIL or a string denoting the protocol
  host                  ;NIL or a string
  port                  ;NIL or an integer
  user                  ;NIL or a string
  password              ;NIL or a string
  path                  ;NIL or a list ([:relative | :absolute] . components)
  parameters            ;NIL or a string
  query                 ;NIL or a string or a list of pairs (<field> . <value>)
  anchor)               ;NIL or a string

;;; 
;;; Interface
;;; 

;; PARSE-URL string &key (plain-query-p t)
;;   parses a the string into the an URL structure.

;; UNPARSE-URL url &key (readably nil)
;;   unparses an URL. If `readably' is NIL, the password part of the url is
;;   omitted, which is useful, when displaying an URL in some interactive
;;   setting.

;; MERGE-URL url base
;;   merges the url `url' with the base url `base' according to rfc1808.

;; URL-PATHNAME url
;;   makes up a Lisp pathname out of an URL.

;; (setf (URL-LOGICAL-HOST-TRANSLATOR host) translator)
;; (URL-LOGICAL-HOST-TRANSLATOR host)
;; TRANSLATE-LOGICAL-URL url

;;   URLs of scheme 'file' are possible logical URLs. To be translated. The
;;   host component of those url are used to pick a translator function.
;;   Translator functions are defined by (setf URL-LOGICAL-HOST-TRANSLATOR).
;;   These functions are then simply applied to the whole url at hand. This
;;   may also be a good substitute for (CL) logical pathnames, which prove to
;;   be anything else than hassle free.
;;   (This is not well thought out either, I could imagine logical schemes
;;   also).

;;; URL-PATH

;; Like in CL pathnames, the `path' component of an url is either
;;
;;  NIL - unspecified
;;  (:relative component-1 ... component-n)  - a relative path
;;  (:absolute component-1 ... component-n)  - an absolute path

;; I do not distinguishing between a directory name and a filename. Note
;; however that there is a difference between:

;;      "/foo/" = (:absolute "foo" "")
;; and  "/foo"  = (:absolute "foo")

;;; URL-QUERY

;; There is a design failure with regard to CGI scripts:

;; Normally, we store URL unencoded internally. But this fails on the query
;; part of URLs, since the application of the query part uses the same
;; escape mechanism as URLs itself. For instance: If you want to append
;; the query (("x" . "foo&bar") ("y" . "p&q")) to an URL, you have to pass
;;
;;   "...?x=foo%26bar&y=p%26q"
;;
;; to the server. When we simply to un-url-encode the query part of this
;; URL, we come up with:
;;
;;   "x=foo&bar&y=p&q",
;;
;; which is not the same as the initial query. So some information is lost
;; here. It would have been much more logical to escape the query part of an
;; url twice. (Also the query mechanism lacks iso-10646 support).

;; It would have a much clearer design, if one would first construct the
;; query string using say #\\ escapes:
;;
;; (("x" . "foo&bar") ("y" . "p&q")) ->  "x=foo\&bar&y=p\&q"
;;
;; and appending this string as query while escaping characters, which are
;; part of the general URL syntax.

;; So I have to resort to storing the query part of an url encoded. There is
;; an alternative representation using an alist for this particular
;; application, but it is discouraged to use this. I am in fear that similar
;; design failures may happen in use of other url parts also.

(defun ascii-alpha-char-p (ch)
  (or (char<= #\A ch #\Z)
      (char<= #\a ch #\z)))

(defun ascii-digit-char-p (ch &optional (radix 10))
  (position (char-upcase ch) (subseq "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" 0 radix)))

(declaim (inline ascii-alpha-char-p))
(declaim (inline ascii-digit-char-p))

(defun parse-url (input &key (plain-query-p t))
  (cond ((sloopy-rod-p input)
         ;; zzz use UTF-8
         (parse-url (map 'string (lambda (x) (or (code-char x) #\?)) input) 
                    :plain-query-p plain-query-p))
        ((stringp input)
         (let ((anchor nil)
               (protocol nil)
               (net-loc nil)
               (query nil)
               (parameters nil)
               i)
           (labels ((cut-from (char)
                      (let ((i (position char input)))
                        (when i
                          (prog1 
                              (subseq input (+ i 1))
                            (setq input (subseq input 0 i)))))))
             ;; cut away the appended #... anchor
             (setq anchor (cut-from #\#))
             ;; separate the protocol specification
             (when (and (setq i
                          (position-if #'(lambda (ch) 
                                           (not (or (ascii-alpha-char-p ch)
                                                    (ascii-digit-char-p ch)
                                                    (find ch "+.-"))))
                                       input))
                        (char= (aref input i) #\:))
               (setf protocol (subseq input 0 i)
                     input (subseq input (+ i 1))))
             ;; Parsing the Network Location/Login
             (when (eql (search "//" input) 0)
               (cond ((setq i (position #\/ input :start 2))
                      (setq net-loc (subseq input 2 i)
                            input   (subseq input i)))
                     (t
                      (setq net-loc (subseq input 2)
                            input ""))))
             ;; Parsing the Query Information
             (setq query (cut-from #\?))
             ;; Parsing the Parameters
             (setq parameters (cut-from #\;))
             ;; Parsing the Path
             ;; all what is left is the path
             (multiple-value-bind (host port user password) (parse-net-loc net-loc)
               (make-url :protocol   (if protocol (string-downcase protocol) protocol)
                         :host       (unescape-string host)
                         :port       port
                         :user       (unescape-string user)
                         :password   (unescape-string password)
                         :parameters (unescape-string parameters)
                         :path       (mapcar #'unescape-string (url-parse-path input))
                         :query      (if plain-query-p query (parse-query query))
                         :anchor     (unescape-string anchor)) ))))
        ((eq input NIL)
         (warn "Saw NIL as input to URL:PARSE-URL; fix your program.")
         (parse-url "" :plain-query-p plain-query-p))
        (t
         (error "~S is a bad argument to URL:PARSE-URL." input)) ))

(defun url-parse-path (input)
  (cond ((equal input "")
         nil)
        (t
         (labels ((split (str)
                    (let ((p (position #\/ str)))
                      (if p
                          (cons (subseq str 0 p) (split (subseq str (+ p 1))))
                        (list str))) ))
           (let ((dir (split input)))
             (cond ((equal (car dir) "")
                    (cons :absolute (cdr dir)))
                   (t
                    (cons :relative dir)))) ))))

(defun has-integer-syntax-p (string &key (start 0) (end (length string)) (radix 10))
  "Has the substring [start;end] of 'string' integer syntax using radix 'radix'?"
  (multiple-value-bind (value last-parsed) 
      (parse-integer string :junk-allowed t :start start :end end :radix radix)
    (and value (= last-parsed end))))

(defun parse-net-loc (net-loc)
  (when net-loc
    (let ((host net-loc)
          (port nil)
          (user nil)
          (password nil)
          i)
      (when (setq i (position #\@ host))
        (setq user (subseq host 0 i)
              host (subseq host (+ i 1)))
        (when (setq i (position #\: user))
          (setq password (subseq user (+ i 1))
                user (subseq user 0 i))))
      (when (and (setq i (position #\: host)) 
                 (has-integer-syntax-p host :start (+ i 1)))
        (setq port (parse-integer host :start (+ i 1))
              host (subseq host 0 i)))
      (values host port user password) )))

(defun unescape-string (string)
  (if (and (stringp string) (find #\% string))
      (with-output-to-string (sink)
        (do ((i 0 (+ i 1)))
            ((= i (length string)))
          (cond ((and (< (+ i 2) (length string))
                      (char= (char string i) #\%)
                      (digit-char-p (char string (+ i 1)) 16)
                      (digit-char-p (char string (+ i 2)) 16))
                 (write-char (code-char (parse-integer string :start (+ i 1) :end (+ i 3) :radix 16)) sink)
                 (incf i 2))
                ((write-char (char string i) sink)))))
    string))

(defun escape-string (string additional-safe)
  "Returns `string' with all characters not (ascii) alphanumeric or
  element of 'additional-safe' substituted by %HH; where HH is the hex
  value of the characters code. When the argument is no string it is
  returned without modification. [This is useful for e.g. NIL]"
  (if (not (stringp string))
      string
    (let ((n 0))
      (dotimes (j (length string))
        (let ((ch (aref string j)))
          (unless (or (ascii-alpha-char-p ch) (ascii-digit-char-p ch) 
                      (find ch additional-safe))
            (incf n))))
      (if (= n 0)
          string
        (let ((res (make-array (+ (length string) (* n 2)) :element-type (array-element-type string)))
              (i 0))
          (dotimes (j (length string))
            (let ((ch (aref string j)))
              (if (or (ascii-alpha-char-p ch) (ascii-digit-char-p ch) (find ch additional-safe))
                  (setf (aref res i) ch 
                        i (+ i 1))
                (setf (aref res i) #\%
                      (aref res (+ i 1)) (aref "0123456789ABCDEF" (floor (char-code ch) 16))
                      (aref res (+ i 2)) (aref "0123456789ABCDEF" (logand (char-code ch) 15))
                      i (+ i 3)))))
          res)))))

(defvar *the-crlf-sequence* (map 'string #'code-char '#(#x0d #x0a)))

(defun newline->crlf (string)
  "Substitue all occurrences of #\\Newline in `string' by #\\Return #\\Newline."
  (let ((i (position #\Newline string)))
    (cond (i
           (concatenate 'string 
             (subseq string 0 i)
             *the-crlf-sequence*
             (newline->crlf (subseq string (+ i 1)))))
          (t string))))

(defun crlf->newline (string)
  (let ((i (search *the-crlf-sequence* string)))
    (cond (i
           (concatenate 'string 
             (subseq string 0 i)
             (string #\Newline)
             (crlf->newline (subseq string (+ i 2)))))
          (t
           string))))

(defun parse-query (string)
  (let ((pairs nil))
    ;; first split the string at the #\&'s
    (labels ((split (str)
               (let ((p (position #\& str)))
                 (if p
                     (cons (subseq str 0 p)
                           (split (subseq str (+ p 1))))
                   (list str))) ))
      (dolist (pair (split string))
        (let ((i (position #\= pair)))
          (if i
              (push (cons (crlf->newline (unescape-string (substitute #\Space #\+ (subseq pair 0 i))))
                          (crlf->newline (unescape-string (substitute #\Space #\+ (subseq pair (+ i 1))))))
                    pairs)
            (progn
              ;; How should parts of the query without a value at all be handled?
              ))))
      (reverse pairs))))

(defun unparse-query (query)
  (cond ((null query) nil)
        ((stringp query)
         ;; a plain query
         ;; note that we generally leave the escape characters in the query
         ;; string, because unescaping would loose information -- design
         ;; failure --
         query)
        (t
         (format
          nil "~{~{~A=~A~}~#[~:;&~]~}"
          (mapcar #'(lambda (pair)
                      (list             ;hack here for format
                       ;; NOTE: I have striped #\; from the set of safe chars here, because
                       ;; in RFC1866 is stated, that servers should handle #\; as synonym for #\&.
                       (substitute #\+ #\Space (escape-string (newline->crlf (car pair)) " $-_.!*'(),/?:@"))
                       (substitute #\+ #\Space (escape-string (newline->crlf (cdr pair)) " $-_.!*'(),/?:@"))))
                  (remove-if #'(lambda (x) (null (cdr x))) query))))))

(let ((quux (formatter "~:[~;~:*~A:~]~:[~*~;~:*//~A~:[~;~:*:~D~]~]~
                        ~:[~;/~]~{~A~#[~:;/~]~}~
                        ~:[~;~:*;~A~]~:[~;~:*?~A~]~:[~;~:*#~A~]")))
  ;; I love monster format strings
  (defun unparse-url (url &key readably-p)
    (with-output-to-string (sink)
      ;; Why is #\+ in the set of safe characters? I'm puzzled
      (funcall quux sink
               (url-protocol url)
               (and (or (url-host url) (and readably-p (url-user url)))
                    (if readably-p
                        (format nil "~:[~*~;~:*~A~:[~;~:*:~D~]@~]~:[~;~:*~A~]"
                                (escape-string (url-user url) "$-_.+!*'(),&=;?")
                                (escape-string (url-password url) "$-_.+!*'(),&=;?")
                                (escape-string (url-host url) "$-_.+!*'(),&=;?"))
                      (escape-string (url-host url) "$-_.+!*'(),&=;?")))
               (url-port url)   
               (eq (car (url-path url)) :absolute)
               (mapcar #'(lambda (x) 
                           (escape-string x "$-_.+!*'(),/:@&=~"));xxx "~" added
                       (cdr (url-path url)))
               (escape-string (url-parameters url) "$-_.+!*'(),&=")
               (and (url-query url) (unparse-query (url-query url)))
               (escape-string (url-anchor url) "$-_.+!*'(),;/?:@&=")))))

(defun clean-path (path)
  (let ((flag t))
    ;; This is essentially a markov algorithmn
    (labels ((walk (x)
               (cond ((null x) nil)
                     ((and (string= (car x) ".")        ; x "." y -> xy
                           (not (null (cdr x))))
                      (setq flag t)
                      (cdr x))
                     ((and (string= (car x) ".")        ; x "." -> x ""
                           (null (cdr x)))
                      (setq flag t)
                      (list ""))
                     ((and (not (string= (car x) "..")) ; x ".." y -> y    | x /= ".."
                           (cdr x)
                           (string= (cadr x) "..") 
                           (not (null (cddr x))))
                      (setf flag t)
                      (cddr x))
                     ((and (not (string= (car x) "..")) ; x ".." -> ""    | x /= ".."
                           (cdr x)
                           (string= (cadr x) "..")
                           (null (cddr x)))
                      (setq flag t)
                      (list ""))
                     ((cons (car x) (walk (cdr x)))))))
      (do () 
          ((not flag))
        (setf flag nil)
        (setq path (walk path)))
      path)))

(defun merge-url (url base)
  (let ((res (copy-url url)))
    (when (null (url-protocol res))
      (setf (url-protocol res) (url-protocol base))
      (when (null (url-host res))
        (setf (url-host res) (url-host base)
              (url-port res) (url-port base)
              (url-user res) (url-user base)
              (url-password res) (url-password base))
        (unless (eq (car (url-path res)) :absolute)
          (cond ((null (url-path res))
                 (setf (url-path res) (url-path base))
                 (when (null (url-parameters res))
                   (setf (url-parameters res) (url-parameters base))
                   (when (null (url-query res))
                     (setf (url-query res) (url-query base))
                     (when (null (url-anchor res))
                       (setf (url-anchor res) (url-anchor base))))))
                (t
                 (let ((base-path (cons (car (url-path base)) (butlast (cdr (url-path base))))))
                   (setf (url-path res) 
                     (cons (car base-path)
                           (clean-path (append (cdr base-path) (cdr (url-path res))))))))))))
    res))

#+(OR)
(defun merge-url (url base)
  (let ((res (copy-url url)))
    (when (or t (null (url-protocol res)));much more sane, but breaks test below
      (setf (url-protocol res) (url-protocol base))
      (when (null (url-host res))
        (setf (url-host res) (url-host base)
              (url-port res) (url-port base)
              (url-user res) (url-user base)
              (url-password res) (url-password base))
        (unless (eq (car (url-path res)) :absolute)
          (cond ((null (url-path res))
                 (setf (url-path res) (url-path base))
                 (when (null (url-parameters res))
                   (setf (url-parameters res) (url-parameters base))
                   (when (null (url-query res))
                     (setf (url-query res) (url-query base))
                     (when (null (url-anchor res))
                       (setf (url-anchor res) (url-anchor base))))))
                (t
                 (let ((base-path (cons (car (url-path base)) (butlast (cdr (url-path base))))))
                   (setf (url-path res) 
                     (cons (car base-path)
                           (clean-path (append (cdr base-path) (cdr (url-path res))))))))))))
    res))

(let ((formatter (formatter "~:[~;/~]~{~A~#[~:;/~]~}")))
  (defun url-filename (url)
    (with-output-to-string (sink)
      (funcall formatter sink (eq (car (url-path url)) :absolute) (cdr (url-path url))))))

;;;; --------------------------------------------------------------------------------
;;;; reader macro for url's
;;;;

(defun url-reader (stream subchar arg)
  (declare (ignore subchar arg))
  (parse-url (read stream t nil t)))

(defun url-printer (self stream depth)
  (declare (ignore depth))
  (format stream "#u~S" (unparse-url self)))

(set-dispatch-macro-character #\# #\u #'url-reader)

;;;; --------------------------------------------------------------------------------
;;;; Convertion to Lisp pathnames
;;;;

(defvar *url-logical-hosts* nil)

(defun url-logical-host-translator (host)
  (cdr (assoc host *url-logical-hosts* :test #'string-equal)))

(defun (setf url-logical-host-translator) (new host)
  (let ((x (assoc host *url-logical-hosts* :test #'string-equal)))
    (if x
        (setf (cdr x) new)
      (push (cons host new) *url-logical-hosts*))
    new))

(defun translate-logical-url (url)
  (cond ((and (equalp (url-protocol url) "file")
              (url-logical-host-translator (url-host url)))
         (funcall (url-logical-host-translator (url-host url)) url))
        (t
         url)))

(defun url-pathname (url)
  (setq url (translate-logical-url url))
  (let (i
        (name nil)
        (type nil))
    (when (and (url-path url) (stringp (car (last (url-path url)))))
      (setq i (position #\. (car (last (url-path url))) :test #'char= :from-end t))
      (if i
          (setq type (subseq (car (last (url-path url))) (+ i 1))
                name (subseq (car (last (url-path url))) 0 i))
        (setq type nil
              name (car (last (url-path url))))))
    (apply #'make-pathname 
           :directory
           (mapcar #'(lambda (x)
                       (cond
                        ((equal x "..") 
                         ;;NOTE: Since URL's use UNIX syntax I also choose
                         ;;      UNIX semantics here. Generally the UNIX 
                         ;;      ambiguousity of ".." is something not thought
                         ;;      about.  As always Lisp got it right. ;-)
                         :up)
                        (t x)))
                   (butlast (url-path url)))
           :name name
           :type type
           (if (string-equal (url-host url) "localhost")
               nil
             (list :host (url-host url))) )))

;;;; --------------------------------------------------------------------------------
;;;; Test suite directly from the RFC1808 document
;;;;

#+(OR)
(defun url-test-suite ()
  ;; returns T if test passes, NIL otherwise
  (let ((base-url "http://a/b/c/d;p?q#f")
        (test-urls
         '(;; This test data is taken directly from RFC1808
           ;; Normal Examples; BTW what is normal?
           ("g:h"           "g:h")
           ("g"             "http://a/b/c/g")
           ("./g"           "http://a/b/c/g")
           ("g/"            "http://a/b/c/g/")
           ("/g"            "http://a/g")
           ("//g"           "http://g")
           ("?y"            "http://a/b/c/d;p?y")
           ("g?y"           "http://a/b/c/g?y")
           ("g?y/./x"       "http://a/b/c/g?y/./x")
           ("#s"            "http://a/b/c/d;p?q#s")
           ("g#s"           "http://a/b/c/g#s")
           ("g#s/./x"       "http://a/b/c/g#s/./x")
           ("g?y#s"         "http://a/b/c/g?y#s")
           (";x"            "http://a/b/c/d;x")
           ("g;x"           "http://a/b/c/g;x")
           ("g;x?y#s"       "http://a/b/c/g;x?y#s")
           ("."             "http://a/b/c/")
           ("./"            "http://a/b/c/")
           (".."            "http://a/b/")
           ("../"           "http://a/b/")
           ("../g"          "http://a/b/g")
           ("../.."         "http://a/")
           ("../../"        "http://a/")
           ("../../g"       "http://a/g")
           ;; An empty reference resolves to the complete base URL:
           (""              "http://a/b/c/d;p?q#f")
           ;; Parsers must be careful in handling the case where there are more
           ;; relative path ".." segments than there are hierarchical levels in the
           ;; base URL's path.  Note that the ".." syntax cannot be used to change
           ;; the <net_loc> of a URL.
           ("../../../g"    "http://a/../g")
           ("../../../../g" "http://a/../../g")
           ;; Similarly, parsers must avoid treating "." and ".." as special when
           ;; they are not complete components of a relative path.
           ("/./g"          "http://a/./g")
           ("/../g"         "http://a/../g")
           ("g."            "http://a/b/c/g.")
           (".g"            "http://a/b/c/.g")
           ("g.."           "http://a/b/c/g..")
           ("..g"           "http://a/b/c/..g")
           ;; Less likely are cases where the relative URL uses unnecessary or
           ;; nonsensical forms of the "." and ".." complete path segments.
           ("./../g"        "http://a/b/g")
           ("./g/."         "http://a/b/c/g/")
           ("g/./h"         "http://a/b/c/g/h")
           ("g/../h"        "http://a/b/c/h")
           ;; Finally, some older parsers allow the scheme name to be present in a
           ;; relative URL if it is the same as the base URL scheme.  This is
           ;; considered to be a loophole in prior specifications of partial URLs
           ;; [1] and should be avoided by future parsers.

           ;; Hmm, but it breaks some web pages -- GB.
           ("http:g"        "http:g")
           ("http:"         "http:"))))
    (let ((failed? nil))
      (labels ((test-0 ()
                 ;; parses an url and unparses it, then compares the two
                 ;; strings. This is not really suffient, since we do not
                 ;; know if each component was parsed in the right slot, but
                 ;; I believe that this would be catched by test-1
                 (labels ((foo (x)
                            (unless (string= x (unparse-url (parse-url x)))
                              (setq failed? t)
                              (format t "~%;;URL-TEST/FAILURE: ~A --> ~S --> ~A"
                                      x (parse-url x) (unparse-url (parse-url x))))))
                   (dolist (k test-urls)
                     (foo (car k))
                     (foo (cadr k)))))
               (test-1 ()
                 ;; tests the merging of urls
                 (let ((base (parse-url base-url)))
                   (dolist (k test-urls)
                     (let* ((raw (first k))
                            (parsed (parse-url raw))
                            (unparsed (unparse-url (merge-url parsed base))))
                       (unless (string= unparsed (second k))
                         (setq failed? t)
                         (format T "~%;;URL-TEST/FAILURE:: ~S ~20T+ ~S ~40T--> ~S~80T[should be ~S]" 
                                 (unparse-url base) (first k) unparsed (second k))))))))
        (test-0)
        (test-1)
        (not failed?)) )))

(defun url-equal-p (url1 url2)
  (and
   (equal (url-protocol url1) (url-protocol url2))
   (equal (url-host url1) (url-host url2))
   (equal (url-port url1) (url-port url2))
   (equal (url-user url1) (url-user url2))
   (equal (url-path url1) (url-path url2))
   (equal (url-query url1) (url-query url2))))

(defun parse-path (string)
  (let ((path (url-parse-path string)))
    (cond ((null path)
           nil)
          (t
           (assert (member (car path) '(:absolute :relative)))
           (cons (car path)
                 (mapcar #'unescape-string (clean-path (cdr path))))))))

(defun url-extension (url)
  (let (fn p)
    (setq fn (url:url-filename url))
    (setq p (1+ (or (position #\. fn :from-end t)
                    (1- (length fn)))))
    (subseq fn p)))

(defun unmerge-urls (url base-url)
  ;;
  (cond ((and (equal (url:url-protocol base-url) (url:url-protocol url))
              (equal (url:url-host base-url)     (url:url-host url))
              (equal (url:url-port base-url)     (url:url-port url))
              (equal (url:url-user base-url)     (url:url-user url))
              (equal (url:url-password base-url) (url:url-password url)))
         ;; unmerge possible
         (let ((base-path (url:url-path base-url))
               (goal-path (url:url-path url)))
           (assert (eq :absolute (car base-path)))
           (assert (eq :absolute (car goal-path)))
           ;; find the longest common prefix
           (let ((i (or (mismatch (butlast base-path) (butlast goal-path)
                                  :test #'string=)
                        (1- (length base-path)))))
             ;; climb up as necessary
             (let ((relative-path
                    (append (list :relative)
                            ;; (subseq base-path 1 i)
                            (make-list (- (length base-path) i 1) :initial-element "..")
                            (subseq goal-path i))))
               (url:make-url :path relative-path
                             :parameters (url:url-parameters url)
                             :query (url:url-query url)
                             :anchor (url:url-anchor url)))))) 
        (t
         ;; no merge possible return 'url' verbatim
         url)))

(defun pathname-url (pathname)
  (make-url
   :protocol "file"
   :path (append
          (pathname-directory pathname)
          (list (format nil "~:[~;~A~:[~;.~A~]~]"
                        (pathname-name pathname)
                        (pathname-name pathname)
                        (pathname-type pathname)
                        (pathname-type pathname))))))

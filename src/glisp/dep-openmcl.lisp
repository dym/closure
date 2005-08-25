BABYL OPTIONS: -*- rmail -*-
Version: 5
Labels:
Note:   This is the header of an rmail file.
Note:   If you are seeing it in rmail,
Note:    it means the file has no messages in it.

1,,
X-From-Line: splittist@yahoo.com Thu Aug 25 15:53:13 2005
Return-path: <splittist@yahoo.com>
Envelope-to: real-csr21@localhost
Delivery-date: Thu, 25 Aug 2005 15:53:13 +0100
Received: from [127.0.0.1] (helo=localhost)
	by mu with esmtp (Exim 4.52)
	id 1E8J6P-0003Ya-6b
	for real-csr21@localhost; Thu, 25 Aug 2005 15:53:13 +0100
Received: from imap.hermes.cam.ac.uk [131.111.8.159]
	by localhost with IMAP (fetchmail-6.2.5.1)
	for real-csr21@localhost (single-drop); Thu, 25 Aug 2005 15:53:13 +0100 (BST)
Received: from ppsw-9-intramail.csi.cam.ac.uk ([192.168.128.139])
	by cyrus-5.csi.private.cam.ac.uk (Cyrus v2.1.16-HERMES)
	with LMTP; Thu, 25 Aug 2005 15:47:51 +0100
X-Sieve: CMU Sieve 2.2
X-Cam-SpamScore: ss
X-Cam-SpamDetails: scanned, SpamAssassin (score=2.174,
	FORGED_YAHOO_RCVD 2.17)
X-Cam-AntiVirus: No virus found
X-Cam-ScannerInfo: http://www.cam.ac.uk/cs/email/scanner/
Received: from cmailg1.svr.pol.co.uk ([195.92.195.171]:3765)
	by ppsw-9.csi.cam.ac.uk (mx.cam.ac.uk [131.111.8.149]:25)
	with esmtp (csa=unknown) id 1E8J16-0005kv-Tr (Exim 4.51) for csr21@cam.ac.uk
	(return-path <splittist@yahoo.com>); Thu, 25 Aug 2005 15:47:44 +0100
Received: from user-2261.l2.c5.dsl.pol.co.uk ([81.76.40.213] helo=[192.168.1.26])
	by cmailg1.svr.pol.co.uk with esmtp (Exim 4.41)
	id 1E8J12-0007ZL-9M; Thu, 25 Aug 2005 15:47:40 +0100
X-Gnus-Mail-Source: file:/var/mail/csr21
Message-ID: <430DDA0B.3010307@yahoo.com>
Date: Thu, 25 Aug 2005 15:47:39 +0100
From: John Q Splittist <splittist@yahoo.com>
User-Agent: Mozilla Thunderbird 1.0.6 (Macintosh/20050716)
X-Accept-Language: en-us, en
MIME-Version: 1.0
To:  closure-devel@common-lisp.net
CC: Christophe Rhodes <csr21@cam.ac.uk>
Subject: Openmcl patches
Content-Type: multipart/mixed;
 boundary="------------030203070203000802030803"
Lines: 285
Xref: mu list.closure-devel:8

*** EOOH ***
;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-USER; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: OpenMCL dependent stuff + fixups
;;;   Created: 2005-08-25 11:50
;;;    Author: 
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

(export 'glisp::read-byte-sequence :glisp)
(export 'glisp::read-char-sequence :glisp)
(export 'glisp::run-unix-shell-command :glisp)

(export 'glisp::getenv :glisp)

(export 'glisp::make-server-socket :glisp)
(export 'glisp::close-server-socket :glisp)

(defun glisp::read-byte-sequence (&rest ap)
  (apply #'read-sequence ap))

(defun glisp::read-char-sequence (&rest ap)
  (apply #'read-sequence ap))

(defmacro glisp::with-timeout ((&rest options) &body body)
  (declare (ignore options))
  `(progn
     ,@body))

(defun glisp::open-inet-socket (hostname port)
  (values
   (ccl::make-socket :address-family :internet
		     :type :stream
		     :remote-host hostname
		     :remote-port port)
   :byte))

(defstruct (server-socket (:constructor make-server-socket-struct))
  fd
  element-type
  port)


#||
(defun glisp::make-server-socket (port &key (element-type '(unsigned-byte 8)))
  (make-server-socket-struct :fd (ext:create-inet-listener port)
                             :element-type element-type
                             :port port))


(defun glisp::accept-connection/low (socket)
  (mp:process-wait-until-fd-usable (server-socket-fd socket) :input)
  (values
   (sys:make-fd-stream (ext:accept-tcp-connection (server-socket-fd socket))
                       :input t :output t
                       :element-type (server-socket-element-type socket))
   (cond ((subtypep (server-socket-element-type socket) 'integer)
          :byte)
         (t
          :char))))

(defun glisp::close-server-socket (socket)
  (unix:unix-close (server-socket-fd socket)))
||#

;;;;;;

(defun glisp::g/make-string (length &rest options)
  (apply #'make-array length :element-type 'base-char options))



(defun glisp::run-unix-shell-command (command)
  (nth-value 1 (ccl:external-process-status
   (ccl:run-program "/bin/sh" (list "-c" command) :wait t :input nil
		       :output nil))))

(defmacro glisp::defsubst (name args &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name ,args .,body)))


;;; MP

(export 'glisp::mp/process-yield :glisp)
(export 'glisp::mp/process-wait :glisp)
(export 'glisp::mp/process-run-function :glisp)
(export 'glisp::mp/make-lock :glisp)
(export 'glisp::mp/current-process :glisp)
(export 'glisp::mp/process-kill :glisp)

(defun glisp::mp/make-lock (&key name)
  (clim-sys::make-lock name))

(defmacro glisp::mp/with-lock ((lock) &body body)
  `(clim-sys:with-lock-held (,lock)
    ,@body))

(defun glisp::mp/process-yield (&optional process-to-run)
  (declare (ignore process-to-run))
  (clim-sys:process-yield))

(defun glisp::mp/process-wait (whostate predicate)
  (clim-sys:process-wait whostate predicate))

(defun glisp::mp/process-run-function (name fun &rest args)
  (clim-sys:make-process
   (lambda ()
     (apply fun args))
   :name name))

(defun glisp::mp/current-process ()
  (clim-sys:current-process))

(defun glisp::mp/process-kill (process)
  (clim-sys:destroy-process process))

(defun glisp::getenv (string)
  (ccl::getenv string))

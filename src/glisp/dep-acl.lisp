;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: GLISP; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: ACL-4.3 dependent stuff + fixups
;;;   Created: 1999-05-25 22:33
;;;    Author: Gilbert Baumann <gilbert@base-engineering.com>
;;;   License: MIT style (see below)
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1998,1999 by Gilbert Baumann

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
(export 'glisp::mp/process-run-function :glisp)
(export 'glisp::mp/process-kill :glisp)
(export 'glisp::mp/seize-lock :glisp)
(export 'glisp::mp/release-lock :glisp)
(export 'glisp::mp/transfer-lock-owner :glisp)
(export 'glisp::mp/current-process :glisp)
(export 'glisp::mp/process-yield :glisp)
(export 'glisp::mp/process-wait :glisp)
(export 'glisp::getenv :glisp)

(defun glisp::read-byte-sequence (&rest ap)
  (apply #'read-sequence ap))

(defun glisp::read-char-sequence (&rest ap)
  (apply #'read-sequence ap))

#+ALLEGRO-V5.0
(defun glisp::open-inet-socket (hostname port)
  (values
   (socket:make-socket :remote-host hostname 
                       :remote-port port 
                       :format :binary)
   :byte))

#-ALLEGRO-V5.0
(defun glisp::open-inet-socket (hostname port)
  (values
   (ipc:open-network-stream :host hostname
                            :port port
                            :element-type '(unsigned-byte 8) 
                            :class 'EXCL::BIDIRECTIONAL-BINARY-SOCKET-STREAM)
   :byte))

#||
(defun glisp::make-server-socket (port &key (element-type '(unsigned-byte 8)))
)
||#

(defun glisp::mp/make-lock (&key name)
  (mp:make-process-lock :name name))

(defmacro glisp::mp/with-lock ((lock) &body body)
  `(mp:with-process-lock (,lock)
     ,@body))

(defmacro glisp::with-timeout ((&rest options) &body body)
  `(mp:with-timeout ,options . ,body))

(defun glisp::g/make-string (length &rest options)
  (apply #'make-array length :element-type 'base-char options))

(defun glisp:run-unix-shell-command (cmd)
  (excl:shell cmd))

(defun glisp:mp/process-run-function (name fn &rest args)
  (apply #'mp:process-run-function name fn args))

(defun glisp:mp/process-kill (proc)
  (mp:process-kill proc))

(defun glisp:mp/current-process ()
  sys:*current-process*)

(defun glisp::mp/seize-lock (lock &key whostate)
  whostate
  (mp:process-lock lock))

(defun glisp::mp/transfer-lock-owner (lock old-process new-process)
  (assert (eql (mp:process-lock-locker lock) old-process))
  (setf (mp:process-lock-locker lock) new-process)
  )

(defun glisp::mp/release-lock (lock)
  (mp:process-unlock lock))

(defun glisp::mp/process-yield (&optional process-to-run)
  (mp:process-allow-schedule process-to-run))

(defun glisp::mp/process-wait (whostate predicate)
  (mp:process-wait whostate predicate))

;; ACL is incapable to define compiler macros on (setf foo)
;; Unfortunately it is also incapable to declaim such functions inline.
;; So we revoke the DEFUN hack from dep-gcl here.

(defmacro glisp::defsubst (fun args &body body)
  (if (and (consp fun) (eq (car fun) 'setf))
      (let ((fnam (intern (concatenate 'string "(SETF " (symbol-name (cadr fun)) ")")
                          (symbol-package (cadr fun)))))
        `(progn
           (defsetf ,(cadr fun) (&rest ap) (new-value) (list* ',fnam new-value ap))
           (glisp::defsubst ,fnam ,args .,body)))
    `(progn
       (defun ,fun ,args .,body)
       (define-compiler-macro ,fun (&rest .args.)
         (cons '(lambda ,args .,body)
               .args.)))))


(defun glisp::getenv (string)
  (sys:getenv string))

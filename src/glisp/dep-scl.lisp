;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-USER; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: [Originally CMUCL dependent stuff + fixups], probably for SCL
;;;   Created: 1999-05-25 22:32
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

(export 'glisp::read-byte-sequence :glisp)
(export 'glisp::read-char-sequence :glisp)
(export 'glisp::run-unix-shell-command :glisp)

(export 'glisp::getenv :glisp)

(defun glisp::read-byte-sequence (&rest ap)
  (apply #'read-sequence ap))

(defun glisp::read-char-sequence (&rest ap)
  (apply #'read-sequence ap))

(defun glisp::read-byte-sequence (sequence input &key (start 0) (end (length sequence)))
  (let (c (i start))
    (loop
      (cond ((= i end) (return i)))
      (setq c (read-byte input nil :eof))
      (cond ((eql c :eof) (return i)))
      (setf (aref sequence i) c)
      (incf i) )))

(defun glisp::read-byte-sequence (sequence input &key (start 0) (end (length sequence)))
  (let ((r (read-sequence sequence input :start start :end end)))
    (cond ((and (= r start) (> end start))
           (let ((byte (read-byte input nil :eof)))
             (cond ((eq byte :eof)
                    r)
                   (t
                    (setf (aref sequence start) byte)
                    (incf start)
                    (if (> end start)
                        (glisp::read-byte-sequence sequence input :start start :end end)
                      start)))))
          (t
           r))))

(defmacro glisp::with-timeout ((&rest ignore) &body body)
  (declare (ignore ignore))
  `(progn
     ,@body))

(defun glisp::open-inet-socket (hostname port)
  (let ((fd (extensions:connect-to-inet-socket hostname port)))
    (values
     (sys:make-fd-stream fd
                         :input t
                         :output t
                         :element-type '(unsigned-byte 8)
                         :name (format nil "Network connection to ~A:~D" hostname port))
     :byte)))

(defun glisp::g/make-string (length &rest options)
  (apply #'make-array length :element-type 'base-char options))

(defun glisp:run-unix-shell-command (command)
  (ext:process-exit-code (ext:run-program "/bin/sh" (list "-c" command) :wait t :input nil :output nil)))

(defun glisp::getenv (string)
  (cdr (assoc string ext:*environment-list* :test #'string-equal)))

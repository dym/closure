;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: XTERM; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Open a stream to an xterm window
;;;   Created: 1999-05-16
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


(defpackage :XTERM
  (:use :glisp)
  (:export
   #:open-terminal
   ))

(in-package :XTERM)

(defun gen-tmp-filename (&optional (prefix "/tmp/clisp_") (length 8))
  (loop
    (let ((filename
            (let ((l '()))
              (dotimes (i length)
                (push (+ (char-code #\a) (random 26)) l))
              (concatenate 'string prefix (map 'string #'code-char l)) )) )
      (unless (probe-file filename) 
        (return filename))) ) )

(defun open-terminal (&key (title "Lisp Window"))
  (check-type title string)
  (let ((tmpfile (gen-tmp-filename)))
    (glisp:run-unix-shell-command
     (with-output-to-string (sink)
       (format sink "xterm -title ~S" title)
       (format sink " -b 6")
       (format sink " -e sh -c \"(tty > ~A; trap '' 2; while true; do sleep 30000; done)\" " tmpfile)
       (format sink " &")))
    ;; Now wait until filename exists and contains an entire line:
    (let ((ttyname
           (dotimes (k 50)
             (when (probe-file tmpfile)
               (with-open-file (stream tmpfile :direction :input)
                 (multiple-value-bind (first-line eof-p)
                     (read-line stream nil nil)
                   (when (and first-line (not eof-p))
                     ;; We can now delete the file.
                     (delete-file tmpfile)
                     (return first-line) )) ) )
             (sleep .1))))
      (and ttyname
           (open ttyname :direction :io :if-exists :overwrite)))))

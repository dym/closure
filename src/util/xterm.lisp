;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: XTERM; -*-
;;; --------------------------------------------------------------------------------------
;;;     Title: Open a stream to an xterm window
;;;   Created: 1999-05-16 09:54
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;; --------------------------------------------------------------------------------------
;;;  (c) copyright 1999 by Gilbert Baumann

;; This is inspired by the original xterm.lsp by
;;  Bruce Krulwich <krulwich@ils.nwu.edu> 30.12.1991
;;  Bruno Haible <haible@ma2s2.mathematik.uni-karlsruhe.de> 4.1.1994
;; but much more minimalistic.

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

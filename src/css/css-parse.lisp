;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CSS; Encoding: utf-8; Readtable: GLISP; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Parsing CSS1 Style Sheets (according to W3C REC-CSS1-961217)
;;;   Created: 1998-02-08
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: GPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1998-2001 by Gilbert Baumann

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
;;  2001-05-19  GB      - selector stuff more to extra file 'css-selector.lisp'
;;  2001-05-14  GB      - ID selectors cannot begin with a digit
;;  1999-08-16  GB      - url merging is now done upon parsing (URL are relative
;;                        to the style sheet the assignment occurs in).
;;                        [PARSE-STYLE-SHEET and P/URL].
;;                      - new special variable *STYLE-SHEET-BASE-URL*.
;;                      - LOOKUP-ALL-STYLE: fixed bug wrt STYLE attribute style.
;;  1999-08-15  GB      - implemented CSS-2 selectors
;;                      - changed signature of LOOKUP-ALL-STYLE to reflect 
;;                        difference between implicit style and style by STYLE
;;                        attribute

(in-package :CSS)

(defvar *style-sheet-base-url*
    NIL
  "Base URL of style sheet currently parsed; used from within the parse and
   should be bound appropriate.")

(defparameter *css-2-enabled-p* t)
;; eigentlich könnten wir hier CSS-1, "CSS-P" und CSS-2 unterscheiden.

;;; Some differences between CSS-1 and CSS-2 parsing:

;;; A comment in CSS-1 should behave like a whitespace in CSS-2 is
;;; behaves like if it where not present. [big pit fall].

(defmacro and-css2 (&rest forms)
  `(and *css-2-enabled-p*
        ,@forms))
;;

(defstruct assignment
  selector slot value
  importantp            ;"!important" present?
  media-type            ;media type this assignment is for
  specifity             ;specifity of selector
  )

(defstruct rule
  selector              ;selector
  specifity             ;specifity of selector (currently unused)
  media-type            ;media type this rule matches
  assignments)          ;list of assignments

(defstruct gi-selection
  hashtable             ;hash table, which maps from GI symbols to
                        ; list of rules.
  default)              ;a list of rules, if GI does not match

(defstruct singleton-selector
  gi            ;a symbol, but should be a ROD eventually
  id            ;a ROD
  class         ;a ROD
  pseudo-class)

(defstruct (style-sheet (:print-function print-style-sheet))
  base-url
  name
  rules ;list of RULEs
  super-sheet
  imported-sheets
  pcode
  (media-type :all)                     ;media type given on creation
  (j 0))                                ;position

(defun read-char* (input skip-comments-p)
  (let ((ch (g/read-char input nil :eof)))
    (cond ((eq ch :eof) :eof)
          ((char= ch #\\)
           (setq ch (g/peek-char nil input nil :eof))
           (cond ((eq ch :eof) 
                  (error "EOF reached after '\\'"))
                 ((digit-char-p ch 16)
                  (let ((v 0))
                    (do* ((i 0 (+ i 1))
                          (c (g/peek-char nil input nil :eof)
                             (g/peek-char nil input nil :eof)))
                        ((or (not (and (characterp c) (digit-char-p c 16)))
                             (= i 4))
                         (logior #x10000 v))
                      (setf v (+ (* v 16) (digit-char-p c 16)))
                      (g/read-char input))))
                 (t
                  (g/read-char input)   ;consume the character
                  (logior #x10000 (char-code ch))) ))
          ((and skip-comments-p
                (char= ch #\/)
                (eql (g/peek-char nil input nil :eof) #\*))
           ;; A comment was seen.
           (g/read-char input)          ;consume the #\*
           (do ()
               ((and (eql (g/read-char input t) #\*)
                     (eql (g/peek-char nil input t) #\/))
                (g/read-char input)     ;consume #\/
                (char-code #\Space))))  ;threat comments as white-spaces
          (t (char-code ch)) )))

(defun slurp (input)
  (let ((bag (make-array 100 :fill-pointer 0 :adjustable t)))
    (labels ((put (ch)
               (vector-push-extend ch bag 100))
             (slurp-literal (delim)
               (do ((ch (read-char* input nil) (read-char* input nil)))
                   ((eql ch delim)
                    (put ch))
                 (when (eql ch :eof)
                   (error "EOF reached within literal"))
                 (put (logior #x10000 ch)))))
      (do ((ch (read-char* input t) (read-char* input t)))
          ((eql ch :eof) 
           bag)
        (cond ((or (= ch #.(char-code #\')) (= ch #.(char-code #\")))
               (put ch)
               (slurp-literal ch))
              ((put ch)) )))))

(defun white-space-p* (ch)
  (member ch
          '#.(mapcar #'char-code '(#\space #\tab #\newline #\return #\page))))

(defun digit-char-p* (ch)
  (<= #.(char-code #\0) ch #.(char-code #\9)))

(defun hex-digit-char-p* (ch)
  (or (<= #.(char-code #\0) ch #.(char-code #\9))
      (<= #.(char-code #\a) ch #.(char-code #\f))
      (<= #.(char-code #\A) ch #.(char-code #\F))))

(defun as-string (seq)
  (map 'string #'(lambda (x) (or (code-char (logand x #xFFFF)) #\?)) seq))

(defun multiplex-selectors (selectors assignments)
  (mapcan (lambda (selector)
            (mapcar (lambda (assignment)
                      (make-assignment
                       :selector selector
                       :slot (assignment-slot assignment)
                       :value (assignment-value assignment)
                       :importantp (assignment-importantp assignment)))
                    assignments))
          selectors))

(defun parse-selector-list (string)
  (mapcar #'parse-selector
          (split-by (char-code #\,) string)))

(defun parse-selector (x) 
  (mapcar #'parse-singleton-selector
          (split-by-if #'white-space-p* x :nuke-empty-p t)))

(defun parse-assignment-list (string)
  (mapcan #'parse-assignment
          (split-by (char-code #\;) string :nuke-empty-p t)))

(defun parse-assignment (x) 
  (let* ((p0 (or (position-if-not #'white-space-p* x) (length x)))
         (p1 (or (position-if (lambda (x)
                                (or (white-space-p* x)
                                    (eql x #.(char-code #\:))))
                              x :start p0) 
                 (length x)))
         (p (position (char-code #\:) x :start p1)))
    (when p
      (multiple-value-bind (res importantp)
          (let ((slot (subseq x p0 p1))
                (value (subseq x (+ p 1))))
            (parse-splited-assignment slot value))
        (unless res
          (warn "CSS assigment does not parse: `~A'."
                (as-string (subseq x p0))))
        (when (and res importantp)
          (dolist (r res) (setf (assignment-importantp r) t)))
        res))))

(defun ident-char-p (ch)
  (or (ident-start-char-p ch)
      (<= (char-code #\0) ch (char-code #\9))
      (= ch (char-code #\-)) ))
      
(defun ident-start-char-p (ch)
  (or (<= (char-code #\a) ch (char-code #\z))
      (<= (char-code #\A) ch (char-code #\Z))
      (>= ch #o241)))
;;;

;;;
;;; ident ['#' ident] ['.' ident] [':' ident]
;;;

(defun parse-ident (string start)
  ;; --> end
  (and (< start (length string))
       (ident-start-char-p (aref string start))
       (or (position-if-not #'ident-char-p string :start start)
           (length string))))

(defun parse-singleton-selector (string)
  (let (element-name id class pclass
        (ptr 0)
        (end 0)
        (len (length string)))
    (when (and (< ptr len)
               (setq end (parse-ident string ptr)))
      (setq element-name (subseq string ptr end)
            ptr end))
    
    (when (and (< ptr len) 
               (eql (aref string ptr) (char-code #\#))
               (setq end (parse-ident string (+ ptr 1))))
      (setq id (subseq string (+ ptr 1) end)
            ptr end))
    
    (when (and (< ptr len) 
               (eql (aref string ptr) (char-code #\.))
               (setq end (parse-ident string (+ ptr 1))))
      (setq class (subseq string (+ ptr 1) end)
            ptr end))
    
    (when (and (< ptr len) 
               (eql (aref string ptr) (char-code #\:))
               (setq end (parse-ident string (+ ptr 1))))
      (setq pclass (subseq string (+ ptr 1) end)
            ptr end))
    
    (and (= ptr len)
         (make-singleton-selector
          :gi (if element-name
                  (intern (string-upcase (as-string element-name)) :keyword)
                nil)
          :id (if id (coerce id 'rod))
          :class (if class (coerce class 'rod))
          :pseudo-class (if pclass
                            (intern (string-upcase (as-string pclass))
                                    :keyword)
                          nil))) ))

;;; ---------------------------------------------------------------------------

(defun match-* (sub-matcher string start)
  (loop
    (let ((i (funcall sub-matcher string start)))
      (if i
          (setq start i)
        (return start)))))

(defun match-concat (matcher-0 matcher-1 string start)
  (and (setq start (funcall matcher-0 string start))
       (funcall matcher-1 string start)))

(defun match-satisfies (predicate string start)
  (and (< start (length string))
       (funcall predicate (aref string start)) 
       (+ start 1)))

(defun match-digit (string start)
  (match-satisfies #'digit-char-p* string start))

(defun match-hex-digit (string start)
  (match-satisfies #'hex-digit-char-p* string start))

(defun match-eql (ch string start)
  (and (< start (length string))
       (eql (aref string start) ch) 
       (+ start 1)))

(defun match-+ (sub string start)
  (match-concat sub (curry #'match-* sub) string start))

(defun match-integer (string start)
  ;; digit +
  (match-+ #'match-digit string start))

(defun match-signed-integer (string start)
  ;; (+ | -)? | digit +
  (cond ((or (match-char #\+ string start)
             (match-char #\- string start))
         (match-integer string (+ start 1)))
        ((match-integer string start))))

(defun match-percentage (string start)
  ;; number '%'
  (match-concat #'match-number (curry #'match-char #\%) string start))

(defun match-signed-percentage (string start)
  ;; (+ | -) percentage
  (cond ((or (match-char #\+ string start)
             (match-char #\- string start))
         (match-percentage string (+ start 1)))
        ((match-percentage string start))))

(defun match-number (string start)
  ;; digit* '.' digit+ | digit +
  (or (match-concat (curry #'match-* #'match-digit)
                    (curry #'match-concat 
                           (curry #'match-eql (char-code #\.))
                           (curry #'match-+ #'match-digit))
                    string start)
      (match-+ #'match-digit string start)))

(defun match-ident (string start)
  ;; ident-start-char (ident-char)*
  (match-concat (curry #'match-satisfies #'ident-start-char-p)
                (curry #'match-* (curry #'match-satisfies #'ident-char-p))
                string start))

(defun match-white-space (string start)
  (or (position-if-not #'white-space-p* string :start start)
      (length string)))

(defun match-char (char string start)
  (and (< start (length string))
       (or (eql (aref string start) (char-code (char-upcase char)))
           (eql (aref string start) (char-code (char-downcase char))))
       (+ start 1)))

(defun match-string (matchee string start)
  (do ((i start (+ i 1))
       (j 0 (+ j 1)))
      ((or (= j (length matchee))
           (not (match-char (char matchee j) string i)))
       (and (= j (length matchee)) i))))

(defun match-a-string (string start)
  (let (p)
    (or (and (match-eql (char-code #\') string start)
             (setq p (position (char-code #\') string :start (+ start 1)))
             (+ p 1))
        (and (match-eql (char-code #\") string start)
             (setq p (position (char-code #\") string :start (+ start 1)))
             (+ p 1)))) )

;;; ---------------------------------------------------------------------------

(defun parse-float (string)
  ;;XXX
  (read-from-string string))

(defun tokenize (sequence &optional (start 0))
  (multiple-value-bind (value ptr) (tokenize-one sequence start)
    (and ptr
         (cons value (tokenize sequence ptr)))))

(defun tokenize-one (x start)
  (let ((p nil)
        p1 p2 p3 p4 p5 p6)
    (setq start (match-white-space x start))
    (values
     (cond
       ((= start (length x))
        (setq p nil))
           
       ((eql (aref x start) (char-code #\+)) (setq p (+ start 1)) '+)
       ((eql (aref x start) (char-code #\-)) (setq p (+ start 1)) '-)
       ((eql (aref x start) (char-code #\/)) (setq p (+ start 1)) '/)
       ((eql (aref x start) (char-code #\,)) (setq p (+ start 1)) '\,)
           
       ((setq p (match-concat (curry #'match-char #\!)
                              (curry #'match-concat 
                                     (curry #'match-white-space)
                                     (curry #'match-string "important"))
                              x start))
        (values :important))
           
       ((setq p (match-a-string x start))
        (values (cons 'string (as-string (subseq x (+ start 1) (- p 1))))))
           
       ((and (setq p (match-string "url(" x start))
             (setq p1 (match-white-space x p))
             (setq p2 (match-a-string x p1))
             (setq p (match-white-space x p2))
             (setq p (match-char #\) x p)))
        (cons 'url (as-string (subseq x (+ p1 1) (- p2 1)))))
           
       ((and (setq p (match-string "url(" x start))
             (setq p1 (match-white-space x p))
             (setq p2 (match-+ (curry #'match-satisfies 
                                      (lambda (ch)
                                        (and (not (eql ch (char-code #\))))
                                             (not (white-space-p* ch)))))
                               x p1))
             (setq p (match-white-space x p2))
             (setq p (match-char #\) x p)))
        (cons 'url (as-string (subseq x p1 p2))))

       ;; XXX -- the rgb(..) syntax parsing here does not work any more.
       ;; XXX -- negative values are o.k. here?! 
           
       ;; rgb([ ]+[0-9]+[ ]+,[ ]+[0-9]+[ ]+,[ ]+[0-9]+[ ]+)
       ((and (setq p (match-string "rgb(" x start))
             (setq p1 (match-white-space x p))
             (setq p2 (match-signed-integer x p1))
             (setq p (match-white-space x p2))
             (setq p (match-char #\, x p))
             (setq p3 (match-white-space x p))
             (setq p4 (match-signed-integer x p3))
             (setq p (match-white-space x p4))
             (setq p (match-char #\, x p))
             (setq p5 (match-white-space x p))
             (setq p6 (match-signed-integer x p5))
             (setq p (match-white-space x p6))
             (setq p (match-char #\) x p)))
        (cons 'color 
              (format nil "#~2,'0X~2,'0X~2,'0X"
                      (max 0 (min 255 (parse-integer
                                       (as-string (subseq x p1 p2)))))
                      (max 0 (min 255 (parse-integer
                                       (as-string (subseq x p3 p4)))))
                      (max 0 (min 255 (parse-integer
                                       (as-string (subseq x p5 p6))))))))
           
       ((and (setq p (match-string "rgb(" x start))
             (setq p1 (match-white-space x p))
             (setq p2 (match-signed-percentage x p1))
             (setq p (match-white-space x p2))
             (setq p (match-char #\, x p))
             (setq p3 (match-white-space x p))
             (setq p4 (match-signed-percentage x p3))
             (setq p (match-white-space x p4))
             (setq p (match-char #\, x p))
             (setq p5 (match-white-space x p))
             (setq p6 (match-signed-percentage x p5))
             (setq p (match-white-space x p6))
             (setq p (match-char #\) x p)))
        (cons 'color 
              (format nil "#~2,'0X~2,'0X~2,'0X"
                      (max 0 (min 255 (floor (* 255 (parse-float (as-string (subseq x p1 (1- p2)))))
                                             100)))
                      (max 0 (min 255 (floor (* 255 1/100 (parse-float (as-string (subseq x p3 (1- p4))))))))
                      (max 0 (min 255 (floor (* 255 1/100 (parse-float (as-string (subseq x p5 (1- p6)))))))))))
           
       ((and (setq p (match-char #\# x start))
             (setq p (match-hex-digit x p))
             (setq p (match-hex-digit x p))
             (setq p (match-hex-digit x p))
             (setq p (match-hex-digit x p))
             (setq p (match-hex-digit x p))
             (setq p (match-hex-digit x p)))
        (cons 'color (as-string (subseq x start p))))
           
       ((and (setq p (match-char #\# x start))
             (setq p (match-hex-digit x p))
             (setq p (match-hex-digit x p))
             (setq p (match-hex-digit x p)))
        (cons 'color (concatenate 'string "#"
                                  (string (code-char (aref x (+ start 1))))
                                  (string (code-char (aref x (+ start 1))))
                                  (string (code-char (aref x (+ start 2))))
                                  (string (code-char (aref x (+ start 2))))
                                  (string (code-char (aref x (+ start 3))))
                                  (string (code-char (aref x (+ start 3)))))))
           
       ((setq p (match-number x start))
        (let ((value (read-from-string (as-string (subseq x start p)))))
          (let ((start p))
            (cond ((setq p (match-char #\% x start))
                   (cons ':% value))
                  ((and (or (setq p (match-string "pt" x start))
                            (setq p (match-string "mm" x start))
                            (setq p (match-string "cm" x start))
                            (setq p (match-string "pc" x start))
                            (setq p (match-string "in" x start))
                            (setq p (match-string "px" x start))
                            (setq p (match-string "em" x start))
                            (setq p (match-string "ex" x start)))
                        (not (match-satisfies #'ident-char-p x p)))
                   (cons (intern (string-upcase (map 'string #'code-char
                                                     (subseq x start p)))
                                 :keyword)
                         value))
                  (t
                   (setq p start)
                   (cons '1 value)) ))))
                                                    
       ((setq p (match-ident x start))
        ;; it this directly followed by an #\( ?
        (cond ((match-char #\( x p)
               (prog1 (cons 'function
                     (as-string (subseq x start p)))
               (incf p)))
              (t
               (cons 'ident (as-string (subseq x start p))))))

       ((setq p (match-char #\) x start))
        (cons 'rparen nil))
           
       (t
        nil))
     p)))

;;; ---------------------------------------------------------------------------

(defun p/+ (tokens fn)
  (let ((r (p/concat tokens fn (rcurry #'p/* fn))))
    (and r
         (cons (cons (first (car r)) (second (car r)))
               (cdr r)))))

(defun p/* (tokens fn)
  (let ((r (funcall fn tokens)))
    (cond ((not (null r))
           (let ((s (p/* (cdr r) fn)))
             (cons (cons (car r) (car s))
                   (cdr s))))
          (t
           (cons nil tokens)))))

(defun p/barbar* (tokens &rest fns)
  (p/barbar-aux tokens fns nil #'append))

(defun p/barbar (tokens &rest fns)
  (p/barbar-aux tokens fns nil #'cons))

(defun p/barbar-aux (tokens fns empty-ok-p combiner)
  (dolist (f fns (if empty-ok-p (cons nil tokens) nil))
    (let ((v (funcall f tokens)))
      (when v
        (let ((more (p/barbar-aux (cdr v) (remove f fns) t combiner))) 
          (return
            (cons (funcall combiner (car v) (car more))
                  (cdr more))))))))

(defun p/maybe (tokens fn)
  (or (funcall fn tokens)
      (cons nil tokens)))

(defun p/\) (tokens)
  (and (consp (car tokens))
       (eq (caar tokens) 'rparen)
       (cons nil (cdr tokens))))

(defun p/function (tokens name subfun)
  (and (and tokens
            (consp (car tokens))
            (eq (caar tokens) 'function)
            (string-equal (cdar tokens) name))
       (let ((v (funcall subfun (cdr tokens))))
         (and v
              (and (consp (cadr v))
                   (eq (caadr v) 'rparen))
              (cons (cons name (car v)) (cddr v))))))
       

;;; ---------------------------------------------------------------------------

(eval-when (compile eval load)
  (defun compile-rule (x)
    (cond
      ((and (consp x) (eq (car x) 'or))
       `(or ,@(mapcar #'compile-rule (cdr x))))
      ((and (consp x) (eq (car x) '**))
       `(P/REPEATED TOKENS ,(cadr x) ,(caddr x)
                    (LAMBDA (TOKENS)
                            ,(compile-rule (cadddr x)))))
      ((and (consp x) (eq (car x) 'barbar))
       `(P/BARBAR TOKENS
         ,@(mapcar (lambda (x)
                     `(LAMBDA (TOKENS)
                       ,(compile-rule x)))
                   (cdr x))))
      ((and (consp x) (eq (car x) 'barbar*))
       `(P/BARBAR* TOKENS
         ,@(mapcar (lambda (x)
                     `(LAMBDA (TOKENS)
                       ,(compile-rule x)))
                   (cdr x))))
      ((and (consp x) (eq (car x) '+))
       `(P/+ TOKENS (LAMBDA (TOKENS) ,(compile-rule (cadr x)))))
      ((and (consp x) (eq (car x) '?))
       `(P/maybe TOKENS (LAMBDA (TOKENS) ,(compile-rule (cadr x)))))
      ((and (consp x) (eq (car x) '&))
       `(P/concat TOKENS ,@(mapcar (lambda (x) 
                                     `(LAMBDA (TOKENS) ,(compile-rule x)))
                                   (cdr x))))
      ((and (consp x) (eq (car x) '&*))
       `(P/concat* TOKENS ,@(mapcar (lambda (x) 
                                      `(LAMBDA (TOKENS) ,(compile-rule x)))
                                    (cdr x))))
      ((and (consp x) (eq (car x) 'function))
       ;; (function <name> <arg-1> <arg-2> .. <arg-n>)
       `(P/function tokens ',(cadr x) 
                    (lambda (tokens)
                      ,(compile-rule
                        `(& ,(caddr x)
                            ,@(mapcar (lambda (x)
                                        `(mungle (& <comma> ,x) second))
                                      (cdddr x)))))))
      ;;
      ((and (consp x) (eq (car x) 'do))
       ;; (do rule fun)
       `(,(caddr x) ,(compile-rule (cadr x))))
      ((and (consp x) (eq (car x) 'mungle))
       ;; (mungle rule fun)
       (let ((g (gensym)))
         `(let ((,g ,(compile-rule (cadr x))))
            (and ,g
                 (cons (,(caddr x) (car ,g))
                       (cdr ,g))))))
      ((keywordp x)
       `(p/simple-enum tokens ,x))
      ((and (symbolp x) 
            (char= (char (symbol-name x) 0) #\<)
            (char= (char (symbol-name x) (1- (length (symbol-name x))))
                   #\>))
       (let ((nam (subseq (symbol-name x) 1 (1- (length (symbol-name x))))))
         `(,(intern (concatenate 'string "P/" nam))
           tokens)) )
      (t (error "Bad rule: ~S" x)) )))

;;;;;

(defun p/ident (tokens)
  (cond ((and (>= (length tokens) 1)
              (consp (car tokens))
              (eql (caar tokens) 'ident))
         (cons (cdar tokens) (cdr tokens)))))

(defun p/string (tokens)
  (cond ((and (>= (length tokens) 1)
              (consp (car tokens))
              (eql (caar tokens) 'string))
         (cons (cdar tokens) (cdr tokens)))))

(defun p/stringable (tokens)
  (or (p/ident tokens) (p/string tokens)))

(defun p/enum (tokens alist)
  (let ((r (p/stringable tokens)))
    (and r
         (cdr (assoc (car r) alist :test #'string-equal))
         (cons (cdr (assoc (car r) alist :test #'string-equal))
               (cdr r)))))

(defun p/simple-enum (tokens &rest keywords)
  (p/enum tokens (mapcar #'(lambda (x)
                                (cons (symbol-name x) x))
                            keywords)))

(defun p/a-font-weight (tokens)
  ;;  normal | bold | bolder | lighter |
  ;;  100 | 200 | 300 | 400 | 500 | 600 | 700 | 800 | 900
  (or (p/enum tokens '(("normal" . 400)
                       ("bold"   . 700)
                       ("bolder" . :bolder)
                       ("lighter" . :lighter)))
      (and (>= (length tokens) 1)
           (consp (car tokens))
           (eql (caar tokens) '1)
           (member (cdar tokens)
                   '(100 200 300 400 500 600 700 800 900))
           (cons (cdar tokens)
                 (cdr tokens)))))

(defun p/absolute-size (tokens)
  (p/enum tokens '(("xx-small" . (:pt . 6))
                   ("x-small"  . (:pt . 8))
                   ("small"    . (:pt . 10))
                   ("medium"   . (:pt . 12))
                   ("large"    . (:pt . 14))
                   ("x-large"  . (:pt . 18))
                   ("xx-large" . (:pt . 28)))))

(defun p/relative-size (tokens)
  (p/simple-enum tokens :larger :smaller))

(defun p/optional-sign (tokens)
  (cond ((eql (car tokens) '+) (cons +1 (cdr tokens)))
        ((eql (car tokens) '-) (cons -1 (cdr tokens)))
        (t (cons +1 tokens))))

(defun p/measure (tokens allowed-units &optional zero-ok?)
  (destructuring-bind (sign . tokens) (p/optional-sign tokens)
    (cond ((or (and (>= (length tokens) 1)
                    (consp (car tokens))
                    (member (caar tokens) allowed-units))
               (and zero-ok? 
                    (>= (length tokens) 1)
                    (equal (car tokens) '(1 . 0))))
           (cons (if (eql (caar tokens) '1)
                     (* sign (cdar tokens))
                   (cons (caar tokens) (* sign (cdar tokens))))
                 (cdr tokens))))))

(defun p/length (tokens)
  (p/measure tokens '(:pt :mm :cm :pc :in :px :em :ex) t))

(defun p/percentage (tokens)
  (p/measure tokens '(:%)))

(defun p/number (tokens)
  (p/measure tokens '(1)))

(defun p/integer (tokens)
  (let ((r (p/measure tokens '(1))))
    (print r)
    (and r
         (integerp (car r))
         r)))

(defun p/concat (tokens &rest fns)
  (cond ((null fns) (cons nil tokens))
        ((let ((r (funcall (car fns) tokens)))
           (cond ((not (null r))
                  (let ((s (apply #'p/concat (cdr r) (cdr fns))))
                    (and s
                         (cons (cons (car r) (car s))
                               (cdr s))))))))))

(defun p/concat* (tokens &rest fns)
  (cond ((null fns) (cons nil tokens))
        ((let ((r (funcall (car fns) tokens)))
           (cond ((not (null r))
                  (let ((s (apply #'p/concat* (cdr r) (cdr fns))))
                    (and s
                         (cons (append (car r) (car s))
                               (cdr s))))))))))

(defun p/slash (tokens)
  (and (eql (car tokens) '/) (cons nil (cdr tokens))))

(defun p/comma (tokens)
  (and (eql (car tokens) '\,) (cons nil (cdr tokens))))

(defun p/one-font-family (tokens)
  (let ((r (p/+ tokens #'p/stringable)))
    (and r
         (cons (list (reduce #'(lambda (x y) (concatenate 'string x " " y))
                             (car r)))
               (cdr r)))))

(defun p/a-font-family (tokens)
  ;;  [[<family-name> | <generic-family>],]* [<family-name> | <generic-family>]
  (p/comma-separated-list tokens #'p/one-font-family))

(defun p/font-family (tokens)
  (let ((r (p/comma-separated-list tokens #'p/one-font-family)))
    (and r
         (cons (list (make-assignment :slot '@font-family :value (car r)))
               (cdr r)))))

(defun p/comma-separated-list (tokens fn)
  (p/concat* tokens 
             fn
             (rcurry #'p/maybe (rcurry #'p/concat* 
                                       #'p/comma 
                                       (rcurry #'p/comma-separated-list fn)))))

(defun p/attcons (att r)
  (and r (cons (list (make-assignment :slot att :value (car r))) (cdr r))))
  
(defun p/url (tokens)
  (and (consp (car tokens))
       (eq (caar tokens) 'url)
       ;;
       (progn
         ;; in (cdar tokens) is the url string
         (let ((url (cdar tokens)))
           (cond ((not *style-sheet-base-url*)
                  (warn "~S is not bound -- fix your program." '*style-sheet-base-url*)
                  (setf url (url:parse-url url)))
                 (t
                  (setf url (url:merge-url
                             (url:parse-url url)
                             *style-sheet-base-url*))))
           ;; the next line should vanish
           (setf url (url:unparse-url url :readably-p t))
           (cons url (cdr tokens))))))

;; Hmm.. einige Leute benutzen auch andere Farbnamen, die z.B.
;; /etc/rgb.txt stehen.

(defparameter *color-names*
    '(("black"   . "#000000")
      ("green"   . "#008000")
      ("silver"  . "#C0C0C0")
      ("lime"    . "#00FF00")
      ("gray"    . "#808080")
      ("olive"   . "#808000")
      ("white"   . "#FFFFFF")
      ("yellow"  . "#FFFF00")
      ("maroon"  . "#800000")
      ("navy"    . "#000080")
      ("red"     . "#FF0000")
      ("blue"    . "#0000FF")
      ("purple"  . "#800080")
      ("teal"    . "#008080")
      ("fuchsia" . "#FF00FF")
      ("aqua"    . "#00FFFF")))

(defun p/a-color (tokens)
  ;;  <color>
  (or (and (consp (car tokens))
           (eq (caar tokens) 'color)
           (cons (cdar tokens) (cdr tokens)))
      (let ((r (p/ident tokens)))
        (cond ((not (null r))
               (let ((v (car r)))
                 (setf v (cdr (assoc v *color-names* :test #'string-equal)))
                 (if v
                     (cons v (cdr r))
                   nil)))
              (t
               nil)))))

;;; ---------------------------------------------------------------------------

(defun pf (fn)
  (lambda (&rest args)
    (let ((r (apply fn args))
          (*print-level* 7))
      (print (list fn r))
      r)))

(defun p/a-margin (tokens)
  (or (p/length tokens)
      (p/percentage tokens)
      (p/simple-enum tokens :auto)))

(defun p/repeated (tokens min max fn)
  (cond ((<= min 0)
         (cond ((<= max 0)
                (cons nil tokens))
               ((let ((r (funcall fn tokens)))
                  (if r
                      (let ((s (p/repeated (cdr r) (1- min) (1- max) fn)))
                        (and s (cons (cons (car r) (car s))
                                     (cdr s))))
                    (cons nil tokens))))))
        ((let ((r (funcall fn tokens)))
           (and r (let ((s (p/repeated (cdr r) (1- min) (1- max) fn)))
                    (and s (cons (cons (car r) (car s))
                                 (cdr s)))))))))

;;; --------------------

(defun p/a-padding (tokens)
  (or (p/length tokens) (p/percentage tokens)))

(defun p/non-negative-length (tokens)
  (let ((r (p/length tokens)))
    (and r (not (negative-value-p (car r))) r)))

(defun p/non-negative-percentage (tokens)
  (let ((r (p/percentage tokens)))
    (and r (not (negative-value-p (car r))) r)))

(defun p/non-negative-number (tokens)
  (let ((r (p/number tokens)))
    (and r (not (negative-value-p (car r))) r)))

(defun negative-value-p (value)
  ;; is `value` a negative length or percentage or number?
  (if (consp value)
      (< (cdr value) 0)
    (< value 0)))

;;; --------------------

(defun p/a-border-width (tokens)
  (or (p/non-negative-length tokens)
      (p/enum tokens '(("thin" . (:px . 1))
                       ("medium" . (:px . 2))
                       ("thick" . (:px . 4))))))

;; border-color and border-style are actually specified to be *one* attribute
;; Bitte was? Woher ist das?

(defun p/a-border-style (tokens)
  (p/simple-enum tokens
                 :none :dotted :dashed :solid :double
                 :groove :ridge :inset :outset))

(defun p/a-list-style-type (tokens)
  (p/simple-enum tokens
                 :disc :circle :square
                 :decimal :lower-roman :upper-roman :lower-alpha :upper-alpha
                 :none))

;; REC-CSS1 has this example:
;;
;; |    BLOCKQUOTE {
;; |      border-color: red;
;; |      border-left: double[;]
;; |      color: black;
;; |    }
;;
;; and then says:
;;
;; | In the above example, the color of the left border will be black, while
;; | the other borders are red. This is due to 'border-left' setting the width,
;; | style and color. Since the color value is not specified on the
;; |                  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; | 'border-left' property, it will be taken from the 'color' property. The
;; | ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; | fact that the 'color' property is set after the 'border-left' property is
;; | not relevant.
;;

;; To accomplish that, I prepend assignments of
;; `border-xyz-{width,style,color}' to NIL before the actual parsing
;; result. By mere order of appearance these then anihilate any
;; previous such definitions.

;; The example assignment above the gets:
;;
;;  border-left: double
;; ==
;;  border-left-color: #nil       // take `color'
;;  border-left-style: #nil       // take `none'
;;  border-left-width: #nil       // take `medium' that is the initial value.
;;  border-left-style: double     // the actual parsing result.
;;

;; I feel, that how `border-left' here is supposed to work, is somewhat
;; weird.



;;; ------------------------------------------------------------
;;;

;;(defun p/position (tokens)
;;  ;;  static | relative | absolute | fixed | inherit
;;  (p/attcons 'position
;;    (p/simple-enum tokens :static :relative :absolute :fixed :inherit)))

;; Puh!

(defun parse-splited-assignment (slot value)
  (let ((parser (find-value-parser slot)))
    (and parser
         (let ((q (funcall parser (tokenize value 0))))
           (cond ((null (cdr q))
                  (values (car q) nil))
                 ((equal (cdr q) '(:important))
                  (values (car q) t))
                 (t
                  nil))))))

;;;; --------------------------------------------------------------------------

(defun print-style-sheet (self sink depth)
  (declare (ignore depth))
  (format sink "#<~S [~A from ~S] ~S>" 
          'style-sheet 
          (style-sheet-name self)
          (style-sheet-base-url self)
          (style-sheet-super-sheet self)))

;; "Pseudo-elements and pseudo-classes are counted as normal elements 
;; and classes, respectively."


(defun selector-num-id (s)
  (count-if-not #'null (mapcar #'singleton-selector-id s)))

(defun selector-num-class (s)
  (+ (count-if-not #'null (mapcar #'singleton-selector-class s))
     (count-if-not #'null (mapcar #'singleton-selector-pseudo-class s))))

(defun selector-num-gi (s)
  (count-if-not #'null (mapcar #'singleton-selector-gi s)))

(defun selector-lessp (s1 s2)
  (let ((num-id-1 (selector-num-id s1))
        (num-id-2 (selector-num-id s2)))
    (or (< num-id-1 num-id-2)
        (and (= num-id-1 num-id-2)
             (let ((num-class-1 (selector-num-class s1))
                   (num-class-2 (selector-num-class s2)))
               (or (< num-class-1 num-class-2)
                   (and (= num-class-1 num-class-2)
                        (let ((num-gi-1 (selector-num-gi s1))
                              (num-gi-2 (selector-num-gi s2)))
                          (< num-gi-1 num-gi-2)))))))))

;; REC-CSS1 says:
;;
;; "Also, the case-sensitivity of the CLASS and ID attributes is under
;; the control of HTML [2]."
;; 
;; Which is bad.
;; in HTML-4.0 'class' and 'id' are CS.

(defun singleton-selector-matches-p (s pt)
  (declare #.cl-user:+optimize-very-fast+)
  (declare (type sgml::pt pt)
           (type singleton-selector s))
  (and (or (null (singleton-selector-id s))
           (id-eq (singleton-selector-id s) (r2::pt-attr* pt :id)))
       (or (null (singleton-selector-gi s))
           (eq (singleton-selector-gi s) (sgml:pt-name pt)))
       (or (null (singleton-selector-class s)) 
           (class-eq (singleton-selector-class s) (r2::pt-attr* pt :class)))
       (or (null (singleton-selector-pseudo-class s))
           (pseudo-class-matches-p (singleton-selector-pseudo-class s) pt))))

(defun selector-matches-p (selector pt)
  (declare #.cl-user:+optimize-very-fast+)
  (declare (type sgml::pt pt))
  (cond ((null selector) t)
        ((null pt) nil)
        ((singleton-selector-matches-p (car selector) pt)
         (if (null (cdr selector))
             t
           (do ((q (sgml:pt-parent pt) (sgml:pt-parent q)))
               ((null q))
             (when (selector-matches-p (cdr selector) q)
               (return t))))) ))

(defun parse-style-sheet (input super-sheet
                          &rest create-options
                          &key base-url
                          &allow-other-keys)
  (let ((res (apply #'create-style-sheet super-sheet create-options)))
    (let ((*style-sheet-base-url* base-url))
      (dolist (a (parse-style-sheet* (slurp input)) res)
        (cond ((assignment-p a)
               (style-sheet-relate res 
                                   (assignment-selector a)
                                   (assignment-slot a)
                                   (assignment-value a)
                                   (assignment-importantp a)))
              ((import-rule-p a)
               (when (is-of-media-type-p :screen (import-rule-media-type a))
                 (multiple-value-bind (s cond)
                     (ignore-errors
                      (renderer::maybe-parse-style-sheet-from-url
                       (url:merge-url (url:parse-url (import-rule-url-str a)) 
                                      (style-sheet-base-url res))
                       :name "Imported Sheet"))
                   cond
                   (when s
                     (setf (style-sheet-imported-sheets res)
                       (append (style-sheet-imported-sheets res)
                               (list s)))) ))))))))

(defun parse-style-sheet-from-url (url &key (name "Anonymous Sheet"))
  (netlib:with-open-document ((input mime-type) url)
    (css:parse-style-sheet input nil
                           :name name
                           :base-url url)))

(defun pprint-selector (sel)
  (format T "[~A ~A ~A] "
          (or (singleton-selector-gi sel) "?")
          (or (singleton-selector-class sel) "?")
          (or (singleton-selector-id sel) "?")))

#||
(defun describe-style-sheet (self)
  (dotimes (i (length (style-sheet-assignments self)))
    (let ((key (find i *atts* :key #'symbol-value))
          (value (aref (style-sheet-assignments self) i))) 
      (unless (null value)
        (format T "~&~% ~(~A~):" key)
        (dolist (as value)
          (format T "~%    ")
          (dolist (s (car as))
            (pprint-selector s))
          (format T "~30T-> ~S."  (cdr as)))))))
||#

;;(export '(create-style-sheet style-sheet-relate
;;          style-sheet-lookup parse-style-sheet
;;          describe-style-sheet))

;;; Liste aller definierten attribute: (alphabetisch sortiert).

;;; ---------------------------------------------------------------------------

(defconstant @BACKGROUND-ATTACHMENT     0)
(defconstant @BACKGROUND-COLOR          1)
(defconstant @BACKGROUND-IMAGE          2)
(defconstant @BACKGROUND-REPEAT         3)
(defconstant @BORDER-BOTTOM-COLOR       4)
(defconstant @BORDER-BOTTOM-STYLE       5)
(defconstant @BORDER-BOTTOM-WIDTH       6)
(defconstant @BORDER-LEFT-COLOR         7)
(defconstant @BORDER-LEFT-STYLE         8)
(defconstant @BORDER-LEFT-WIDTH         9)
(defconstant @BORDER-RIGHT-COLOR        10)
(defconstant @BORDER-RIGHT-STYLE        11)
(defconstant @BORDER-RIGHT-WIDTH        12)
(defconstant @BORDER-TOP-COLOR          13)
(defconstant @BORDER-TOP-STYLE          14)
(defconstant @BORDER-TOP-WIDTH          15)
(defconstant @CLEAR                     16)
(defconstant @COLOR                     17)
(defconstant @DISPLAY                   18)
(defconstant @FLOAT                     19)
(defconstant @FONT-FAMILY               20)
(defconstant @FONT-SIZE                 21)
(defconstant @FONT-STYLE                22)
(defconstant @FONT-VARIANT              23)
(defconstant @FONT-WEIGHT               24)
(defconstant @HEIGHT                    25)
(defconstant @LETTER-SPACING            26)
(defconstant @LINE-HEIGHT               27)
(defconstant @LIST-STYLE-IMAGE          28)
(defconstant @LIST-STYLE-POSITION       29)
(defconstant @LIST-STYLE-TYPE           30)
(defconstant @MARGIN-BOTTOM             31)
(defconstant @MARGIN-LEFT               32)
(defconstant @MARGIN-RIGHT              33)
(defconstant @MARGIN-TOP                34)
(defconstant @PADDING-BOTTOM            35)
(defconstant @PADDING-LEFT              36)
(defconstant @PADDING-RIGHT             37)
(defconstant @PADDING-TOP               38)
(defconstant @TEXT-ALIGN                39)
(defconstant @TEXT-DECORATION           40)
(defconstant @TEXT-INDENT               41)
(defconstant @TEXT-TRANSFORM            42)
(defconstant @VERTICAL-ALIGN            43)
(defconstant @WHITE-SPACE               44)
(defconstant @WIDTH                     45)
(defconstant @WORD-SPACING              46)


(defconstant @POSITION                  47)
(defconstant @TOP                       48)
(defconstant @RIGHT                     49)
(defconstant @BOTTOM                    50)
(defconstant @LEFT                      51)

(defconstant @ORIG-WIDTH                52)

(defconstant @BACKGROUND-POSITION       53)

(defconstant @OVERFLOW                  54)
(defconstant @CLIP                      55)

(defconstant @CONTENT                   56)
(defconstant @QUOTES                    57)
(defconstant @COUNTER-RESET             58)
(defconstant @COUNTER-INCREMENT         59)
(defconstant @MARKER-OFFSET             60)
(defconstant @Z-INDEX                   61)


(defconstant *n-attr*                   62)

(defconstant *atts*
    '(@BACKGROUND-ATTACHMENT @BACKGROUND-COLOR @BACKGROUND-IMAGE
      @BACKGROUND-REPEAT @BORDER-BOTTOM-COLOR @BORDER-BOTTOM-STYLE
      @BORDER-BOTTOM-WIDTH @BORDER-LEFT-COLOR @BORDER-LEFT-STYLE
      @BORDER-LEFT-WIDTH @BORDER-RIGHT-COLOR @BORDER-RIGHT-STYLE
      @BORDER-RIGHT-WIDTH @BORDER-TOP-COLOR @BORDER-TOP-STYLE
      @BORDER-TOP-WIDTH @CLEAR @COLOR @DISPLAY @FLOAT @FONT-FAMILY @FONT-SIZE
      @FONT-STYLE @FONT-VARIANT @FONT-WEIGHT @HEIGHT @LETTER-SPACING
      @LINE-HEIGHT @LIST-STYLE-IMAGE @LIST-STYLE-POSITION @LIST-STYLE-TYPE
      @MARGIN-BOTTOM @MARGIN-LEFT @MARGIN-RIGHT @MARGIN-TOP @PADDING-BOTTOM
      @PADDING-LEFT @PADDING-RIGHT @PADDING-TOP @TEXT-ALIGN @TEXT-DECORATION
      @TEXT-INDENT @TEXT-TRANSFORM @VERTICAL-ALIGN @WHITE-SPACE @WIDTH
      @WORD-SPACING
      @POSITION @TOP @RIGHT @BOTTOM @LEFT @ORIG-WIDTH @BACKGROUND-POSITION
      ;;
      @OVERFLOW @CLIP
      ;; CSS2: Chapter 12
      @CONTENT @QUOTES @COUNTER-RESET @COUNTER-INCREMENT @MARKER-OFFSET
      ))

#||

Testing:

(defun tt (str)
  (parse-assignment
   (slurp (cl-char-stream->gstream (make-string-input-stream str)))))

(defun tt (str)
  (parse-selector
   (slurp (cl-char-stream->gstream (make-string-input-stream str)))))

||#

;;;;;;;;;;;;

(eval-when (compile eval load)
  
  (defclass property-description ()
    ((name :initarg :name :reader property-description-name)))

  (defclass concrete-property-description (property-description)
    (
     (default-value :initarg :default-value :reader property-description-default-value)
     (inheritedp :initarg :inheritedp :reader property-description-inheritedp)
     (value :initarg :value :reader property-description-value)
     (percentage-base :initarg :percentage-base :reader property-description-percentage-base)))

  (defclass simple-abbreviation (property-description)
    ((expansion :initarg :expansion :reader abbreviation-expansion)))

  (defclass edges-abbreviation (property-description)
    ((value :initarg :value :reader edges-abbreviation-value)
     (names :initarg :names :reader edges-abbreviation-names)))

  (defvar *css-properties*
    (make-hash-table :test #'eq))
  )

(defmacro define-css-property (name &key default-value value inheritedp percentage-base)
  `(eval-when (compile eval load)
    (progn
      (setf (gethash ',name *css-properties*)
            (make-instance 'concrete-property-description
                           :name ',name
                           :default-value ',default-value
                           :inheritedp ',inheritedp
                           :value ',value
                           :percentage-base ',percentage-base))
      ',name)))

(defmacro define-simple-short-hand-property (name &key value)
  `(eval-when (compile eval load)
    (progn
      (setf (gethash ',name *css-properties*)
            (make-instance 'simple-abbreviation
                           :name ',name
                           :expansion ',value)))))

(defmacro define-edges-short-hand-property (name &key names value)
  `(eval-when (compile eval load)
    (progn
      (setf (gethash ',name *css-properties*)
            (make-instance 'edges-abbreviation
                           :name ',name
                           :names ',names
                           :value ',value)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile eval load)

(defmethod generate-parser ((def concrete-property-description))
  (with-slots (value name) def
    `(defun ,(intern (format nil "P/~S" name)) (tokens)
      (p/attcons ',(intern (format nil "@~S" name))
                 ,(compile-rule value)))))

(defmethod generate-parser ((def simple-abbreviation))
  (with-slots (name expansion) def
    `(defun ,(intern (format nil "P/~S" name)) (tokens)
      ,(compile-rule expansion))))

(defmethod generate-parser ((def edges-abbreviation))
  (with-slots (names value name) def
    `(defun ,(intern (format nil "P/~S" name)) (tokens)
      (let ((r (p/repeated tokens 1 4 (lambda (tokens) ,(compile-rule value)))))
        (and r (destructuring-bind (top &optional (right top) (bottom top) (left right))
                   (car r)
                 (cons (list (make-assignment :slot ',(intern (format nil "@~A" (symbol-name (first names)))) :value top)
                             (make-assignment :slot ',(intern (format nil "@~A" (symbol-name (second names)))) :value right)
                             (make-assignment :slot ',(intern (format nil "@~A" (symbol-name (third names)))) :value bottom)
                             (make-assignment :slot ',(intern (format nil "@~A" (symbol-name (fourth names)))) :value left))
                       (cdr r))))))))

(defmacro generate-parsers ()
  `(progn
    ,@(loop for x being each hash-value of *css-properties*
            collect (generate-parser x))))

(defmacro register-parsers ()
  `(progn
    (defparameter *value-parsers*
      (let ((r (make-hash-table :test #'equalp)))
        ,@(loop for x being each hash-value of *css-properties*
                collect `(setf (gethash ',(rod (string-downcase (symbol-name (property-description-name x))))
                                r)
                          (function ,(intern (format nil "P/~S" (property-description-name x))))))
        r))))
)

(defun find-value-parser (slot)
  (gethash slot *value-parsers*))

#||
(defun css-prop-raw-accessor (p)
  (intern (format nil "STYLE-~A" (property-description-name p))))

(defun css-prop-cooked-accessor (p)
  (intern (format nil "COMPUTED-STYLE-~A" (property-description-name p))))

(defun css-prop-raw-vector-index (p)
  (intern (format nil "@~A" (property-description-name p))))

(defun css-prop-interpreter (p)
  'identity)

(defun ququ ()
  (let ((props (mapcar (lambda (x) (gethash x *css-properties*))
                       '(font-size text-decoration))))
    (print props)
    `(progn
      (defstruct computed-style
        ,@(mapcar (lambda (prop)
                    (with-slots (name) prop
                      name))
                  props))

      ,@(mapcar (lambda (p)
                  `(defun ,(css-prop-raw-accessor p) (raw-style)
                    (or (aref raw-style ,(css-prop-raw-vector-index p))
                     ',(if (property-description-inheritedp p)
                           :inherit
                           (property-description-default-value p)))))
                props)

      (defun compute-style (raw-style parent-computed-style device)
        (let* (,(mapcar (lambda (p)
                          `(,(property-description-name p)
                            (let ((value (,(css-prop-raw-accessor p) raw-style)))
                              (if (eql value :inherit)
                                  (,(css-prop-cooked-accessor p) parent-computed-style)
                                  (,(css-prop-interpreter p) value)))))
                        props))
          ))
          
      )))

  (mapcar (lambda (x)
            (setf x (gethash x *css-properties*))
            
            )
          '(font-size)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; We distinguish between raw style as retrieved from the style sheet
;;; and cooked style.

;;; Cooking sould be done upon the exact subtype/rhs item.

(defmacro define-rhs-item (name &key predicate cooker)
  )

(define-rhs-item <length>
    :predicate (lambda (object)
                 (or
                  (and (consp object)
                       (member (car object)
                               '(:px :em :ex :in :cm :mm :pt :pc :canvas-h-percentage :canvas-v-percentage)))
                  (eql object 0)))
    :cooker    (lambda (device object property)
                 (declare (ignore property))
                 (interpret-length object (device-dpi device) nil
                                   1em 1ex
                                   (device-canvas-width device)
                                   (device-canvas-height device)))
    :type       real)

(define-rhs-combination or (&rest clauses)
  (

(define-rhs-item <percentage>
    :predicate (lambda (object)
                 (and (consp object)
                      (eql (car object) ':%)))
    :cooker    (lambda (device object property)
                 ...))

||#


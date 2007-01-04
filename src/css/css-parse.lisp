;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CSS; Encoding: utf-8; Readtable: GLISP; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Parsing CSS1 Style Sheets (according to W3C REC-CSS1-961217)
;;;   Created: 1998-02-08
;;;    Author: Gilbert Baumann <gilbert@base-engineering.com>
;;;   License: MIT style (see below)
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1998-2003 by Gilbert Baumann

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

(in-package :CSS)

(defvar *style-sheet-base-url*
    NIL
  "Base URL of style sheet currently parsed; used from within the parse and
   should be bound appropriate.")

(defparameter *css-2-enabled-p* t)

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
      ((and (consp x) (eq (car x) 'comma-list))
       `(p/comma-separated-list tokens (lambda (tokens) ,(compile-rule (cadr x)))))
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
  (or (p/non-negative-length tokens) (p/non-negative-percentage tokens)))

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
    (declare (ignore mime-type))
    (css:parse-style-sheet input nil
                           :name name
                           :base-url url)))

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

(defun generate-slot-constants-1 ()
  (let ((defconstants nil))
    ;; we go to some length to keep the indicies stable ...
    (let ((props nil)
          (taken nil))
      (maphash (lambda (prop def)
                 (when (typep def 'concrete-property-description)
                   (push prop props)))
               *css-properties*)
      (dolist (prop props)
        (let ((sym (intern (format nil "@~A" (symbol-name prop)))))
          (when (boundp sym)
            (push (symbol-value sym) taken))))
      (dolist (prop props)
        (let* ((sym (intern (format nil "@~A" (symbol-name prop))))
               (val (or (and (boundp sym) (symbol-value sym))
                        ;; find the smallest non-taken
                        (loop for i from 0 when (not (member i taken)) return i))))
          (pushnew val taken)
          (push `(defconstant ,sym ,val) defconstants)))
      `(progn
        ,@defconstants
        (defconstant *n-attrs* ,(1+ (reduce #'max taken))))))) )

(defmacro generate-slot-constants ()
  (generate-slot-constants-1))

;;; Fixme! Some parts of the CSS parser use code integers rather than runes.
;;; Here some dummy definitions to use in those cases:
(defun white-space-hieroglyph-p (x)
  (white-space-rune-p (code-rune x)))
(defun hieroglyph= (a b)
  (eql a b))
(defun hieroglyph-equal (a b)
  (equal a b))
(defun hieroglyph-char (x)
  (code-char x))
(defun papyrus-string (v)
  (map 'string (lambda (x) (or (hieroglyph-char x) #\?)) v))

(defun find-value-parser (slot)
  (unless (typep slot 'rod)
    (setf slot (map 'rod #'code-rune slot)))
  (gethash (rod-downcase slot) *value-parsers*))

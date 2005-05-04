;;; -*- Mode: Lisp; Syntax: Common-Lisp; Encoding: utf-8; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: CSS selectors
;;;            [Split off from css-parse.lisp]
;;;   Created: 2001-05-19
;;;    Author: Gilbert Baumann <gilbert@base-engineering.com>
;;;   License: MIT style (see below)
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1998-2001 by Gilbert Baumann

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


(in-package :css)

(defun create-style-sheet (super-sheet &key (name "Anonymous style sheet")
                                            base-url
                                            (media-type :all))
  (unless base-url
    (warn "A proper style sheet has a base url; ~
           (While creating style sheet ~S)." name))
  (make-style-sheet :super-sheet super-sheet
                    :name name
                    :base-url base-url
                    :rules nil))

(defun style-sheet-relate (sheet selector slot value &optional important-p)
  (cond ((gi-selection-p (car (style-sheet-rules sheet)))
         (style-sheet-relate-2 sheet selector slot value important-p))
        (t
         (let ((rule (find selector (style-sheet-rules sheet)
                           :test #'equal :key #'rule-selector)))
           (unless rule
             (setf rule (make-rule :selector selector
                                   :specifity (css2-selector-specificity selector)
                                   :media-type '???
                                   :assignments nil))
             (push rule (style-sheet-rules sheet)))
           (let ((a (make-assignment
                     :selector :unused
                     :importantp important-p
                     :media-type '???
                     :specifity (concatenate 'vector 
                                  (rule-specifity rule) 
                                  (vector
                                   (progn 
                                     (style-sheet-j sheet)
                                     (incf (style-sheet-j sheet)))))
                     :slot slot
                     :value value)))
             (push a (rule-assignments rule)) )))))

(defun style-sheet-relate-2 (sheet selector slot value important-p)
  (let ((q (car (style-sheet-rules sheet)))
        (a (make-assignment
            :selector :unused
            :importantp important-p
            :media-type '???
            :specifity (concatenate 'vector 
                         (css2-selector-specificity selector)
                         (vector
                          (progn 
                            (style-sheet-j sheet)
                            (incf (style-sheet-j sheet)))))
            :slot slot
            :value value)))
    (style-sheet-relate-3 selector q a)))

(defun style-sheet-relate-3 (selector q a)
  (let ((gi (find 'gi selector :key #'car)))
    (cond ((null gi)
           (maphash (lambda (key value)
                      ;; xxx okay?
                      (setf (gethash key (gi-selection-hashtable q))
                        (augment-to-rules value selector selector a))
                      )
                    (gi-selection-hashtable q))
           (setf (gi-selection-default q)
             (augment-to-rules (gi-selection-default q) selector selector a)))
          (t
           (let ((x (gethash (cadr gi) (gi-selection-hashtable q))))
             (cond (x
                    (setf (gethash (cadr gi) (gi-selection-hashtable q))
                      (augment-to-rules x (remove gi selector) selector a)))
                   (t
                    ;; hmm now we need to shovel all default rules back.
                    (setf (gethash (cadr gi) (gi-selection-hashtable q))
                      (augment-to-rules nil (remove gi selector) selector a))
                    (dolist (y (prog1 (gi-selection-default q)
                                 (setf (gi-selection-default q) nil)))
                      (dolist (a (rule-assignments y))
                        (style-sheet-relate-3 selector q a))))))))))

(defun augment-to-rules (rules selector selector-orig a)
  (let ((rule (find selector rules :test #'equal :key #'rule-selector)))
    (unless rule
      (setf rule (make-rule :selector selector
                            :specifity (css2-selector-specificity selector-orig)
                            :media-type '???
                            :assignments nil))
      (push rule rules))
    (push a (rule-assignments rule))
    rules))

#||
(defun style-sheet->assignments-list (s)
  (error "Do not call me")
  (let (res)
    (setf res (reverse (style-sheet-assignments s)))
    (let ((res0 res))
      (setq res (stable-sort res #'selector-lessp :key #'assignment-selector))
      '(assert (every (lambda (x) (member x res0)) res))
      res)))
||#

;;; BUGS:
;; - implicit style
;; - important
;;   das habe ich immer noch nicht ganz verstanden.
;;   ich meine bilden die important regel ein allererstes element fuer den
;;   specificity vector?!
;; - style by `style' attribute

;; implicit style;
;;   is at start of author style sheet with specificity set to 0

;; style attribute:
;;   specificity = #(0 0 1 0 0 inf)
;;   origin after everything else

;; This is both compatible with CSS-1

;; Diese ganze explicite Unterscheidung zwischen Author/User/Default
;; ist mir etwas zu speziell. (grade bei !important wird das alles
;; doch etwas merkwürdig!).

(defun find-style (sheet element implicit-style 
                   &optional (origin 0) (p 0) 
                             (res (make-array *n-attrs* :initial-element nil)))
  ;; handle imported sheets
  (dolist (im (style-sheet-imported-sheets sheet))
    (multiple-value-setq (res p) (find-style im element nil origin p res)))
  ;; handle rules
  (dolist (rule (style-sheet-rules sheet))
    (cond ((rule-p rule)
           (when (css2-selector-matches-p (rule-selector rule) element)
             (dolist (a (rule-assignments rule))
               (let ()
                 (setf res
                       (augment-assignment-to-result* (assignment-slot a)
                                                      (assignment-value a)
                                                      (if (assignment-importantp a) 1 0) ;CSS-1 modell
                                                      origin
                                                      (svref (assignment-specifity a) 0)
                                                      (svref (assignment-specifity a) 1)
                                                      (svref (assignment-specifity a) 2)
                                                      (+ p (svref (assignment-specifity a) 3))
                                                      res)) ))))
          ((gi-selection-p rule)
           (let ((q (gethash (element-gi element) 
                             (gi-selection-hashtable rule)
                             (gi-selection-default rule))))
             (dolist (rule q)
               ;; code duplication alert!
               (when (css2-selector-matches-p (rule-selector rule) element)
                 (dolist (a (rule-assignments rule))
                   (let ()
                     (setf res
                           (augment-assignment-to-result* (assignment-slot a)
                                                          (assignment-value a)
                                                          (if (assignment-importantp a) 1 0) ;CSS-1 modell
                                                          origin
                                                          (svref (assignment-specifity a) 0)
                                                          (svref (assignment-specifity a) 1)
                                                          (svref (assignment-specifity a) 2)
                                                          (+ p (svref (assignment-specifity a) 3))
                                                          res)) )))))) ))
  ;; adjust p
  (incf p (style-sheet-j sheet))
  ;; handle implicit style
  (dolist (k implicit-style)
    (let ((prop (car k)) (value (cdr k)))
      (setf res (augment-assignment-to-result* prop value
                                               0 origin 0 0 0 (prog1 p (incf p))
                                               res))))
  ;; recurse into super sheets
  (cond ((style-sheet-super-sheet sheet)
         (setf res 
               (find-style (style-sheet-super-sheet sheet) element nil (- origin 1) 0 res))))
  ;; return what we found
  (values res p))

(defun augment-assignment-to-result (property value v res)
  (let ((x (svref res (symbol-value property))))
    (cond ((null x)
           (setf (svref res (symbol-value property))
             (list value v)))
          ((vector-greater-p v (second x))
           (setf (first x) value
                 (second x) v)))
    res))

(defun augment-assignment-to-result* (property value v1 v2 v3 v4 v5 v6 res)
  (let ((x (svref res (symbol-value property))))
    (cond ((null x)
           (setf (svref res (symbol-value property))
             (list value (vector v1 v2 v3 v4 v5 v6))))
          ((vector-greater-p* v1 v2 v3 v4 v5 v6 (second x))
           (setf (first x) value
                 (svref (second x) 0) v1
                 (svref (second x) 1) v2
                 (svref (second x) 2) v3
                 (svref (second x) 3) v4
                 (svref (second x) 4) v5
                 (svref (second x) 5) v6)))
    res))

(defun css2-selector-matches-p (selector element)
  ;; here is a kludge
  (cond ((and (pseudo-class-matches-p :first-line element)
              (not (find '(pclass #.(rod "first-line")) selector
                         :test #'equalp)))
         (return-from css2-selector-matches-p nil))
        ((and (pseudo-class-matches-p :before element)
              (not (find '(pclass #.(rod "before")) selector
                         :test #'equalp)))
         (return-from css2-selector-matches-p nil))
        ((and (pseudo-class-matches-p :after element)
              (not (find '(pclass #.(rod "after")) selector
                         :test #'equalp)))
         (return-from css2-selector-matches-p nil))
        ((and (pseudo-class-matches-p :first-letter element)
              (not (find '(pclass #.(rod "first-letter")) selector
                         :test #'equalp)))
         (return-from css2-selector-matches-p nil)))
  ;;
  (dolist (pred selector t)
    (unless
        (case (car pred)
          ((gi) (css2-gi-match-p (cadr pred) element))
          ((id) (css2-id-match-p (cadr pred) element))
          ((class) (css2-class-match-p (cadr pred) element))

          ((attrib-exists)
           (not (null (element-attribute element (intern-attribute-name (cadr pred))))))

          ((attrib)
           (attribute-equal-p element (cadr pred) (caddr pred) nil)) ;CS or CI??

          ((attrib-contain)
           (attribute-contains-p element (cadr pred) (caddr pred) nil))
             
          ((attrib-contain-dash)
           (attribute-contain-dash-p element (cadr pred) (caddr pred) nil))
             
          ((pclass)
           (cond ((and (= (length (cdr pred)) 1)
                       (rod-equal #.(map 'vector #'char-code "first-child") (cadr pred)))
                  (null (pt-predecessor element)))
                 ((and (= (length (cdr pred)) 1)
                       (rod-equal #.(map 'vector #'char-code "link") (cadr pred)))
                  (pseudo-class-matches-p :link element))
                 ((and (= (length (cdr pred)) 1)
                       (rod-equal #.(map 'vector #'char-code "first-line") (cadr pred)))
                  (pseudo-class-matches-p :first-line element))
                 ((and (= (length (cdr pred)) 1)
                       (rod-equal #.(map 'vector #'char-code "first-letter") (cadr pred)))
                  (pseudo-class-matches-p :first-letter element))
                 ((and (= (length (cdr pred)) 1)
                       (rod-equal #.(map 'vector #'char-code "before") (cadr pred)))
                  (pseudo-class-matches-p :before element))
                 ((and (= (length (cdr pred)) 1)
                       (rod-equal #.(map 'vector #'char-code "after") (cadr pred)))
                  (pseudo-class-matches-p :after element))
                 ;; lang fehlt.
                 (t
                  ;; (print (rod-string (cadr pred)))
                  nil)))
             
          ((ancestor)
           (css2-ancester-match-p (cdr pred) element))
             
          ((parent)
           (and (element-parent element)
                (css2-selector-matches-p (cdr pred) (element-parent element))))
          ((preceded-by)
           (let ((prec (pt-predecessor element)))
             (and prec
                  (css2-selector-matches-p (cdr pred) prec))))
          (t
           t))
      (return nil) )))

(defun vector-greater-p (v1 v2)
  (dotimes (i (length v1) nil)
    (let ((a (aref v1 i))
          (b (aref v2 i)))
      (cond ((> a b) (return t))
            ((< a b) (return nil))))))

(defun vector-greater-p* (v1 v2 v3 v4 v5 v6 w)
  (block nil
    (let ((a v1)
          (b (aref w 0)))
      (cond ((> a b) (return t))
            ((< a b) (return nil))))
    (let ((a v2)
          (b (aref w 1)))
      (cond ((> a b) (return t))
            ((< a b) (return nil))))
    (let ((a v3)
          (b (aref w 2)))
      (cond ((> a b) (return t))
            ((< a b) (return nil))))
    (let ((a v4)
          (b (aref w 3)))
      (cond ((> a b) (return t))
            ((< a b) (return nil))))
    (let ((a v5)
          (b (aref w 4)))
      (cond ((> a b) (return t))
            ((< a b) (return nil))))
    (let ((a v6)
          (b (aref w 5)))
      (cond ((> a b) (return t))
            ((< a b) (return nil)))) ))

(defun pt-predecessor (pt)
  (let ((par (element-parent pt)))
    (and par
         (let ((r nil))
           (dolist (k (element-children par))
             (when (eq k pt)
               (return r))
             (unless (text-element-p k)
               (setf r k)))))))

(defun css2-ancester-match-p (selector element)
  (and (element-parent element)
       (or (css2-selector-matches-p selector (element-parent element))
           (css2-ancester-match-p selector (element-parent element)))))

;; class, id are case-sensitive in HTML

(defun attribute-contains-p (element attribute string case-sensitive-p)
  (let ((v (element-attribute element (intern-attribute-name attribute))))
    (and v (rod-contains-p v string case-sensitive-p))))

(defun attribute-equal-p (element attribute string case-sensitive-p)
  (let ((v (element-attribute element (intern-attribute-name attribute))))
    (and v
         (if case-sensitive-p
             (rod= v string)
           (rod-equal v string)))))

(defun rod-contains-p (haystack needle case-sensitive-p)
  ;; what should (rod-contains-p .. "" ..) yield?
  (dotimes (i (- (length haystack) (length needle) -1) nil)
    (when (and (or (= i 0)
                   (white-space-rune-p (rune haystack (1- i))))
               (or (= (+ i (length needle)) (length haystack))
                   (white-space-rune-p (rune haystack (+ i (length needle))))))
      (when (dotimes (j (length needle) t)
              (unless (if case-sensitive-p
                          (rune= (rune needle j) (rune haystack (+ i j)))
                        (rune-equal (rune needle j) (rune haystack (+ i j))))
                (return nil)))
        (return t)))))

(defun attribute-contain-dash-p (element attribute string case-sensitive-p)
  (let ((v (element-attribute element (intern-attribute-name attribute))))
    (and v 
         (>= (length v) (length string))
         (if case-sensitive-p 
             (rod= (subseq v 0 (length string)) string)
           (rod-equal (subseq v 0 (length string)) string))
         (or (= (length string) (length v))
             (rune= (code-rune #.(char-code #\-)) (rune v (length string)))))))

(defun skip-group (seq p &optional (level 0))
  (cond ((>= p (length seq))
         nil)
        ((= (aref seq p) #.(char-code #\{))
         (skip-group seq (+ p 1) (+ level 1)))
        ((= (aref seq p) #.(char-code #\}))
         (cond ((= level 1) p)
               ((skip-group seq (+ p 1) (- level 1)))))
        ((skip-group seq (+ p 1) level))))

(defun parse-at-rule-body (seq p0)
  ;; An at-rule consists of everything up to and including the next
  ;; semicolon (;) or the next block (defined shortly), whichever comes
  ;; first.
  (do ((i p0 (+ i 1)))
      ((= i (length seq))
       (warn "EOF before at-rule group.")
       (values (length seq)))
    (cond ((= (aref seq i) #.(char-code #\;))
           (return (values (+ i 1))))
          ((= (aref seq i) #.(char-code #\{))
           (let ((p1 (skip-group seq i)))
             (cond ((null p1)
                    (warn "EOF within at-rule group.")
                    (return (values nil (length seq))))
                   (t
                    (return (values (+ p1 1))))))) )))

(defstruct import-rule
  url-str
  media-type)

(defun parse-media-type-2 (toks)
  (let ((r (p/comma-separated-list toks (lambda (x) 
                                          (let ((r (p/ident x))) 
                                            (and r (cons (list (intern (string-upcase (car r))
                                                                       :keyword))
                                                         (cdr r))))))))
    (and r
         (cons (cons 'or (car r)) (cdr r)))))

(defun parse-import-rule (seq)
  (let ((toks (tokenize seq))
        (media-type :all))
    (let ((r (or (p/string toks)
                 (p/url toks))))
      (when (cdr r)
        (let ((s (parse-media-type-2 (cdr r))))
          (setf (cdr r) (cdr s)
                media-type (car s))))
      (cond ((and r (null (cdr r)))
             (let ((url (car r)))
               (list (make-import-rule :url-str url :media-type media-type))))
            (t
             (warn "CSS @import rule does not parse: ~S." (as-string seq))
             nil)))))

(defun parse-at-rule (seq start import-ok?)
  (let (p1 p2)
    (assert (= (aref seq start) #.(char-code #\@)))
    (unless (setq p1 (parse-ident seq (+ start 1)))
      (warn "Bad syntax: An '@' must be followed by an identifier")
      (setf p1 (+ start 1)))
    (setq p2 (parse-at-rule-body seq p1))
    (cond ((and p1 p2)
           (let ((ident (as-string (subseq seq (+ start 1) p1))))
             (cond ((string-equal ident "import")
                    (if import-ok?
                        (values (parse-import-rule (subseq seq p1 p2))
                                p2)
                      (progn
                        (parse-import-rule (subseq seq p1 p2))
                        (warn "@import not at start of style sheet - ignored.")
                        (values nil p2))))
                   ((string-equal ident "media")
                    (values nil p2)
                    )
                   (t
                    (values nil p2)))))
          (t
           (values nil (length seq))) )))

(defun assignment-list-adjoin (new assignments)
  (cons (cons (reverse (car new)) (cdr new)) assignments))

;;;; new CSS-2 selectors

(defun nmchar-p (ch)
  (or (<= #.(char-code #\a) ch #.(char-code #\z))
      (<= #.(char-code #\A) ch #.(char-code #\Z))
      (<= #.(char-code #\0) ch #.(char-code #\9))
      (= ch #.(char-code #\-))
      (>= ch 128)))

(defun nmstart-p (ch)
  (or (<= #.(char-code #\a) ch #.(char-code #\z))
      (<= #.(char-code #\A) ch #.(char-code #\Z))
      (>= ch 128)))

(defun q-cons-rod (string start end)
  (let ((res (make-array (- end start) :element-type 'rune)))
    (dotimes (i (- end start))
      (setf (%rune res i) (code-rune 
                           (logand #xFFFF (aref string (+ i start))))))
    res))

(defun q-token (string start end) ;;-> type semantic new-start
  (let (p)
    (cond ((>= start end) 
           (values :eof nil start))
          (t
           (let ((c (aref string start)))
             (cond ((= c #.(char-code #\>)) (values :> nil (+ start 1)))
                   ((= c #.(char-code #\+)) (values :+ nil (+ start 1)))
                   ((= c #.(char-code #\*)) (values :* nil (+ start 1)))
                   ((= c #.(char-code #\[)) (values :|[| nil (+ start 1)))
                   ((= c #.(char-code #\])) (values :|]| nil (+ start 1)))
                   ((= c #.(char-code #\.)) (values :|.| nil (+ start 1)))
                   ((= c #.(char-code #\=)) (values :|=| nil (+ start 1)))
                   ((= c #.(char-code #\:)) (values :|:| nil (+ start 1)))
                   ((= c #.(char-code #\()) (values :|(| nil (+ start 1)))
                   ((= c #.(char-code #\))) (values :|)| nil (+ start 1)))
                   ((= c #.(char-code #\,)) (values :|,| nil (+ start 1)))
                   ((and (= c #.(char-code #\~))
                         (< (+ start 1) end)
                         (= (aref string (+ start 1)) #.(char-code #\=)))
                    (values :|~=| nil (+ start 2)))
                   ((and (= c #.(char-code #\|))
                         (< (+ start 1) end)
                         (= (aref string (+ start 1)) #.(char-code #\=)))
                    (values :\|= nil (+ start 2)))
                   ((and (= c #.(char-code #\#))
                         ;; ID selectors cannot start with an digit
                         ;; xxx is that true?
                         (nmstart-p (aref string (+ start 1)))
                         (setq p (or (position-if-not #'nmchar-p string :start (+ start 1) :end end)
                                     end))
                         (> p (+ 1 start)))
                    (values :hash (q-cons-rod string (+ start 1) p) p))
                   ((and (nmstart-p c)
                         (setq p (or (position-if-not #'nmchar-p string :start (+ start 1) :end end)
                                     end)))
                    (values :ident (q-cons-rod string start p) p))
                   ((and (= c #.(char-code #\'))
                         (setq p (position #.(char-code #\') string :start (+ start 1) :end end)))
                    (values :string (q-cons-rod string (+ start 1) p) (+ p 1)))
                   ((and (= c #.(char-code #\"))
                         (setq p (position #.(char-code #\") string :start (+ start 1) :end end)))
                    (values :string (q-cons-rod string (+ start 1) p) (+ p 1)))
                   ((member c '(9 10 12 13 32))
                    (values :s nil (+ start 1)))
                   (t
                    (values :junk c (+ start 1))) ))))))

;;; traditional recursive descent parser for CSS-2 selectors

;; Each selector is a simple list of predicates, which must turn for
;; this selector to match; the predicates produced by the parser are:

;; (GI string)
;;     the GI of element must be 'string'
;; (CLASS string)
;;     the class attribute of the element must contain 'string'
;; (ID string)
;;     the ID attribute of the element must be 'string'
;; (ATTRIB attr string)
;;     the attribute given by 'attr' must be 'string'
;; (ATTRIB-EXISTS attr string)
;;     the attribute given by 'attr' must exist (been set)
;; (ATTRIB-CONTAIN attr string)
;;     the attribute given by 'attr' must contain 'string'
;; (ATTRIB-CONTAIN-DASH attr string)
;;     the attribute given by 'attr' must contain 'string' (dash variant)
;; (PCLASS name [argument])
;;     the element must belong to the given pseudo class
;; (ANCESTOR . predicates)
;;     there must be an anchestor matching the selector 'predicates'
;; (PARENT . predicates)
;;     the element must have a parent matching the selector 'predicates'
;; (PRECEDED-BY . predicates)
;;     the element must have be preceded by an element matching the selector 'predicates'

;; Example
;; =======
;; "A B C[x]" => ((GI "C") 
;;                (ATTRIB-EXISTS "x") 
;;                (ANCESTOR (GI "B") 
;;                          (ANCESTOR (GI "A"))))



(defvar *tok*)
(defvar *sem*)
(defvar *i*)
(defvar *s*)
(defvar *e*)

(defun parse-css2-selector-list (string &optional (start 0) (end (length string)))
  (let ((res
         (let* ((*s* string)
                (*e* end)
                (*i* start))
           (q/consume)
           (q/s*)
           (prog1
               (q/selector-list)
             (q/s*)
             (unless (eq (q/tok) :eof)
               (error "Unexpected token ~S." (q/tok)))))))
    (setq res
      (mapcar (lambda (selector)
                (let ((x (find 'gi selector :key #'car)))
                  (if x
                      (cons x (remove x selector))
                    selector)))
              res))
    res))
    

(defun q/tok () *tok*)
(defun q/sem () *sem*)
(defun q/at? (tok) (eq *tok* tok))

(defun q/consume ()
  (multiple-value-bind (tok sem j) (q-token *s* *i* *e*)
    (setf *tok* tok)
    (setf *sem* sem)
    (setf *i* j)))

(defun q/selector-list ()
  (let ((x (q/selector)))
    (cond ((eq (q/tok) :|,|)
           (q/consume)
           (q/s*)
           (cons x (q/selector-list)))
          (t
           (list x)))))

(defun q/selector ()
  (let ((res nil))
    (tagbody
      loob
      (let ((a (q/simple-selector)))
        (q/s*)
        (push a res)
        (cond ((member (q/tok) '(:ident :hash :|.| :|[| :|:| :|*|))
               (push 'ancestor res)
               (go loob))
              ((eq (q/tok) :>)
               (q/consume)
               (q/s*)
               (push 'parent res)
               (go loob))
              ((eq (q/tok) :+)
               (q/consume)
               (q/s*)
               (push 'preceded-by res)
               (go loob))
              (t
               (go fin))))
     fin)
    (parse-combinator-sequence res)))

(defun parse-combinator-sequence (q)
  (cond ((null (cdr q)) (car q))
        (t
         (append (car q)
                 (list (cons (cadr q)
                             (parse-combinator-sequence (cddr q))))))))



(defun q/simple-selector ()
  (multiple-value-bind (element presentp) (q/maybe-element-name)
    (let ((modifiers nil)
          x)
      (unless presentp
        (setq x (q/maybe-modifier))
        (unless x
          (error "When there is no element name at least one modifiers is required."))
        (setf modifiers (append modifiers (list x))))
      (loop
        (setq x (q/maybe-modifier))
        (cond (x
               (setf modifiers (append modifiers (list x))))
              ((null x)
               (return))))
      (q/s*)
      ;; xxx does this apply to CSS2 as well.
      ;; xxx also this looks like some bogus random implementation limitation.
      (when (member-if-not (lambda (x) (and (consp x) (eq (car x) 'pclass)))
                           (member-if (lambda (x) (and (consp x) (eq (car x) 'pclass)))
                                      modifiers))
        (error "Psuedoclass in non trailing location: ~S." modifiers))
      ;;
      (append element modifiers))))

(defun q/s* ()
  (while (q/at? :s) (q/consume)))

(defun q/maybe-modifier ()
  (case (q/tok)
    (:hash (prog1 (list 'ID (q/sem)) (q/consume)))
    (:|.|
      (q/consume)
      (cond ((eq (q/tok) :ident)
             (prog1 (list 'CLASS (q/sem)) (q/consume)))
            (t
             (error "Parse error: Expected <ident> after \".\""))))
    (:|[|
      (q/attrib))
    (:|:|
      (q/pclass))
    (t
     nil) ))

(defun q/expect (tok)
  (unless (eq (q/tok) tok)
    (error "Expected ~A." tok))
  (prog1 (q/sem)
    (q/consume)))

(defun q/attrib ()
  (let (name x)
    (q/expect :|[|)
    (q/s*)
    (setf name
      (q/expect :ident))
    (q/s*)
    (setf x (q/maybe-attribut-value name))
    (q/expect :|]|)
    x))

(defun q/maybe-attribut-value (name)
  (case (q/tok)
    (:=
     (q/consume)
     (q/s*)
     (prog1 (list 'attrib name (q/maybe-attribut-value-2))
       (q/s*)) )
    (:\~=
     (q/consume)
     (q/s*)
     (prog1 (list 'attrib-contain name  (q/maybe-attribut-value-2))
       (q/s*)) )
    (:\|=
     (q/consume)
     (q/s*)
     (prog1 (list 'attrib-contain-dash name (q/maybe-attribut-value-2))
       (q/s*)) )
    (t
     (list 'attrib-exists name))))

(defun q/pclass ()
  (let (nam arg)
    (q/expect :|:|)
    (setf nam (q/expect :ident))
    (cons 'pclass
          (cond ((eq (q/tok) :|(|)
                 (q/consume)
                 (q/s*)
                 (setf arg (q/expect :ident))
                 (q/s*)
                 (q/expect :|)|)
                 (list nam arg))
                (t
                 (list nam))))))
  

(defun q/maybe-attribut-value-2 ()
  (unless (member (q/tok) '(:ident :string))
    (error "Expected either a string or an ident here."))
  (prog1
      (q/sem)
    (q/consume)))

(defun q/maybe-element-name ()
  (cond ((q/at? :ident)
         (multiple-value-prog1 
             (values (list (list 'GI (intern-gi (q/sem)))) t)
           (q/consume)))
        ((q/at? :*)
         (multiple-value-prog1
             (values nil t)
           (q/consume)))
        (t
         (values nil nil))))

;;;;

(defun css2-selector-specificity (selector)
  (let ((res (vector 0 0 0)))
    (dolist (p selector)
      (case (car p)
        ((gi) (incf (aref res 2)))
        ((id) (incf (aref res 0)))
        ((class attrib attrib-exists attrib-contain attrib-contain-dash pclass)
         (incf (aref res 1)))
        ((ancestor parent preceded-by)
         (setf res (map 'vector #'+ res (css2-selector-specificity (cdr p)))))))
    res))

(defun parse-style-sheet (str)
  (with-input-from-string (s str)
    (parse-style-sheet* (slurp s) 0 nil)))

(defun parse-assignment-list-string (str)
  (with-input-from-string (s str)
    (parse-assignment-list (slurp s))))

(defun parse-style-sheet* (seq &optional (start 0) (import-ok? t))
  (let (p0 p1)
    (when (setq p0 (position-if-not #'white-space-p* seq :start start))
      (cond
        ((= (aref seq p0) #.(char-code #\@))
         (multiple-value-bind (v p1) (parse-at-rule seq p0 import-ok?)
           (nconc v (parse-style-sheet* seq p1 import-ok?))))
        ((setq p1 (position (char-code #\{) seq :start p0))
         (let ((p2 (skip-group seq p1)))
           (cond ((null p2)
                  (warn "EOF while parsing CSS group.")
                  nil)
                 (t
                  (multiple-value-bind (sel-list condition)
                      (parse-css2-selector-list seq p0 p1)
                   (cond (condition
                           (warn "CSS selector list does not parse: `~A'."
                                 (as-string (subseq seq p0 p1)))
                           (setq sel-list nil)))
                    (nconc (multiplex-selectors sel-list
                                                (parse-assignment-list
                                                 (subseq seq (+ p1 1) p2)))
                           (parse-style-sheet* seq (+ p2 1) nil)) )))))
            (t
             (warn "Bad css syntax: " (as-string seq))
             nil)))))


(defun lookup-all-style (style-sheet pt is ss res)
  (multiple-value-bind (x p) (find-style style-sheet pt is)
    (dolist (k (reverse ss))
      (let ((prop (assignment-slot k))
            (value (assignment-value k))
            (importantp (assignment-importantp k)))
        (setf x (augment-assignment-to-result prop value (vector (if importantp 1 0) 0 1 0 0 
                                                                 (incf p)) 
                                              x))))
    (dotimes (i (length x))
      (let ((q (svref x i)))
        (when q
          (setf (svref res i) (first q)))))
    res))


;;;; Media Types

(defparameter *media-types*
    '(:screen :all))

(defmethod is-of-media-type-p ((medium t) (media-type (eql :all))) t)
(defmethod is-of-media-type-p ((medium (eql :screen)) (media-type (eql :screen))) t)

(defmethod is-of-media-type-p ((medium t) (media-type t)) 
  nil)

(defmethod is-of-media-type-p ((medium t) (media-type cons))
  (ecase (car media-type)
    ((OR)
     (some (curry #'is-of-media-type-p medium) media-type))
    ((AND)
     (every (curry #'is-of-media-type-p medium) media-type))))

(defun intern-media-type (rod)
  (intern-attribute-name rod))

;; TODO
;; - parse-media-type is borken/ugly
;; - parse-style-sheet -- fixed :screen media type is borken
;; - p/ident is not unicode safe!
;; - some how we broke "CSS1 Test Suite: LINK and @import" ;-(


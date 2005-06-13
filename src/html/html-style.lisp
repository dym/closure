;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: RENDERER; Readtable: GLISP; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: HTML-Specific Parts of the Renderer
;;;   Created: 1999-05-25
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

;;;; Notes

;; -- 2003-03-04
;;
;;    This only recently occured to me:
;;
;;    Most of the mappings here are rather primitive and a lot of are
;;    already handled in the default style sheet, so the idea would be to
;;    _compile_ the whole set of mappings, maybe even yielding a
;;    different kind of style object, so that we'd spend less time on
;;    style mappings.
;;

;; Changes
;;
;;  When        Who     What
;; ----------------------------------------------------------------------------
;;  2001-05-14  GB      - primitive caching of parsed style sheets
;;
;;  1999-08-21  GB      - MAKE-TEXT-REPLACEMENT, MAKE-PASSWORD-REPLACEMENT,
;;                        MAKE-OPTION-MENU-REPLACEMENT, MAKE-TEXT-AREA-REPLACEMENT,
;;                        REPLACED-ELEMENT/INPUT: pass text-style
;;                      - MAKE-TEXT-AREA-REPLACEMENT, REPLACED-ELEMENT/INPUT:
;;                        pass document
;;                        
;;                        
;;  1999-08-19  GB      - changed layout of :%REPLACEMENT attribute and 
;;                        PT-%REPLACEMENT function.
;;                      - REPLACED-ELEMENT-P: returns two values now.
;;                      - IMPLICIT-STYLE/IMG: IMG.border now maps to `border-width'
;;                      - COMMAND-DOCUMENTATION: new generic function
;;                      - COMMAND-CURSOR: new generic function
;;                      - SERVER-SIDE-IMAGE-MAP: new class
;;                      - FIND-A-ELEMENT-FOR-IMG-ISMAP: new function
;;                      - IMAGE-REPLACED-ELEMENT/AUX: ISMAP support
;;                      - GRAPHICAL-SUBMIT: new class
;;                      - polygon image maps should work now.
;;
;;  1999-08-18  GB      - PT-STYLE-STYLE now returns a list of assignments to
;;                        carry "!important" to LOOKUP-STYLE.
;;
;;  1999-08-15  GB      - PT-STYLE-STYLE new function to return style by STYLE
;;                        attribute

(in-package :RENDERER)

(defun pt-%replacement (pt &optional default)
  (sgml:pt-attr pt :%replacement default))

(defun (setf pt-%replacement) (new-value pt &optional default)
  default
  (setf (sgml:pt-attr pt :%replacement) new-value))

(defun pt-effective-url-attr (doc pt attr)
  "Return the parsed and merged effective URL of an elements attribute."
  (url:merge-url (url:parse-url (pt-attr/latin1 pt attr ""))
                 (document-base-url doc)))

(defun pt-body-element (pt)
  (cond ((null pt) nil)
        ((eq (element-gi pt) :HTML)
         (dolist (q (element-children pt))
           (when (eq (element-gi q) :BODY)
             (return q))))
        ((pt-body-element (element-parent pt))) ))

(defun pt-all-data (x)
  (cond ((member (element-gi x) '(:pcdata :comment))
	 (map 'string (lambda (x) (or (code-char x) #\?))
              (element-text x)))
	((apply 'concatenate 'string
                (mapcar #'pt-all-data (element-children x))))))

;;; ---- Implicit Style -----------------------------------------------------------------------

(defun pt-attr/low (pt prop &optional (default nil))
  (if (text-element-p pt)
      default
      (element-attribute pt prop)))

(defun pt-attr* (pt attr &optional default)
  (pt-attr/low pt attr default))

(defun pt-attr/latin1 (pt attr &optional default)
  (let ((r (pt-attr/low pt attr)))
    (if r
        (map 'string (lambda (x) (if (< x 256) (code-char x) #\?)) r)
      default)))

(defmethod closure-protocol:element-explicit-style (document (pt sgml::pt))
  (let ((x (pt-attr* pt :style)))
    (when x
      (let ((css::*style-sheet-base-url* (document-base-url document)))
        (css::parse-assignment-list x)))))

;;;;
;;;; Parsers
;;;;

(defun pt-attr-warn (pt format &rest args)
  (apply #'warn format args))

(defun pt-attr/list-style-type (pt slot &optional (default nil))
  (let ((v (pt-attr* pt slot nil)))
    (if v
        (parse-list-style-type pt v)
      default)))

(defun parse-list-style-type (pt value)
  ;; Translate the HTML notation of a list-style-type 'value' to the CSS-1 notation.
  (when (typep value 'rod)
    (setf value (map 'string (lambda (x) (or (code-char x) #\?)) value)))
  (cond ((string= value "1") :decimal)
        ((string= value "a") :lower-alpha)
        ((string= value "A") :upper-alpha)
        ((string= value "i") :lower-roman)
        ((string= value "I") :upper-roman)
        ((string-equal value "disc") :disc)
        ((string-equal value "square") :square)
        ((string-equal value "circle") :circle)
        (t
         (pt-attr-warn pt "The value '~S' is unknown to the 'TYPE' slot." value)
         (if (eq (element-gi (element-parent pt)) :UL) :disc :decimal)) ))

(defun parse-html-multi-length (string &optional (error-p t))
  ;; -> list of  (:% . N) | (* . N) | (:px . N) | NIL
  (cond ((parse-html-length string nil))
        ((and (> (length string) 1)
              (char= (char string 0) #\*)
              (every #'digit-char-p (subseq string 1)))
         (cons '* (parse-integer (subseq string 1))))
        ((and (> (length string) 1)
              (char= (char string (1- (length string))) #\*)
              (every #'digit-char-p (subseq string 0 (1- (length string)))))
         (cons '* (parse-integer (subseq string 0 (1- (length string))))))
        ((string-equal string "*")      ; '*' is short cut for '1*'
         (cons '* 1))
        (error-p
         (error "'~S' is not a valid HTML 'MultiLength'." string))
        (t
         nil)))

(defun parse-html-length (string &optional (error-p t))
  ;; -> (:% . N) | (:px . N) | NIL
  (cond ((and (> (length string) 0) (every #'digit-char-p string))
         (cons :px (parse-integer string)))
        ((and (> (length string) 1)
              (char= (char string (1- (length string))) #\%)
              (every #'digit-char-p (subseq string 0 (1- (length string)))))
         (cons :% (parse-integer (subseq string 0 (1- (length string))))))
        (error-p
         (error "'~S' is not a valid HTML 'Length'." string))
        (t
         nil)) )

(defun unparse-html-length (value)
  (cond ((and (consp value) (eq (car value) :%))
         (format nil "~D%" (cdr value)))
        ((and (consp value) (eq (car value) :px))
         (format nil "~D" (cdr value)))
        ((null value)
         "#implied")
        (t
         (format nil "[invalid html-length: ~S]" value))))

(defun rune->char (x)
  (or (code-char x) #\?))

(defun rod->string (x)
  (map 'simple-string (lambda (x) (or (code-char x) #\?)) x))

(defun pt-attr/with-parser (pt slot default parser pretty-type-name)
  (let ((s (pt-attr* pt slot)))
    (if (not s)
        default
      (let ((value (funcall parser s)))
        (if value
            value
          (progn
            (pt-attr-warn pt "The value of the ~A attribute, ~S, is not ~A."
                          slot (rod->string s) pretty-type-name)
            default))))))

(defun pt-attr/integer (pt slot &optional default)
  (pt-attr/with-parser pt slot default #'html/parse-integer "an integer"))

(defun pt-attr/pixels (pt slot &optional default)
  (pt-attr/with-parser pt slot default #'html/parse-integer "an integer"))

(defun pt-attr/length (pt slot &optional default)
  (pt-attr/with-parser pt slot default #'html/parse-length "a length"))

(defun pt-attr/length-list (pt slot &optional default)
  (pt-attr/with-parser pt slot default #'html/parse-length-list
                       "a comma separated list of lengths"))

(defun pt-attr/multi-length (pt slot &optional default)
  (pt-attr/with-parser pt slot default #'html/parse-multi-length "a length"))

(defun pt-attr/enum (pt slot default keys)
  (let ((s (pt-attr* pt slot)))
    (cond ((not s)
           default)
          (t
           (let ((val (some (lambda (key)
                              (and (= (length s) (length (symbol-name key)))
                                   (every (lambda (x y)
                                            (char-equal (rune->char x) y))
                                          s (symbol-name key))
                                   key))
                            keys)))
             (or val
                 (progn
                   (pt-attr-warn pt "The value of the ~A attribute, ~
                                     should be ~{\"~A\"~#[~; or ~:;, ~]~}, but not ~S."
                                 slot keys (rod->string s))
                   default)))))))

(defun pt-attr/table.frame (pt slot &optional default)
  (pt-attr/enum pt slot default
                '(:void :above :below :hsides :lhs :rhs :vsides :box :border)))

(defun pt-attr/table.rules (pt slot &optional default)
  (pt-attr/enum pt slot default
                '(:none :groups :rows :cols :all)))

(defun pt-attr/table.align (pt slot &optional default)
  (pt-attr/enum pt slot default
                '(:left :center :right)))

(defun pt-attr/cell-halign (pt slot &optional default)
  (pt-attr/enum pt slot default
                '(:left :center :right :justify :char)))

(defun pt-attr/cell-valign (pt slot &optional default)
  ;; Note for compatibility, we also accept :center but map that to :middle
  ;; ### oops, :center actually is HTML, while :middle is CSSSpeak.
  (subst :middle :center
         (pt-attr/enum pt slot default
                       '(:top :middle :bottom :baseline :center))))

(defun pt-attr/input-type (pt slot &optional (default :text))
  (pt-attr/enum pt slot default
                '(:text :password :checkbox :radio :submit :reset
                  :file :hidden :image :button)))

(defun pt-attr/img-align (pt slot &optional default)
  (pt-attr/enum pt slot default '(:top :middle :bottom :left :right)))

(defun pt-attr/align (pt &optional (slot :align) (default nil))
  (pt-attr/enum pt slot default '(:left :center :right :justify)))

(defun pt-attr/clear (pt &optional (slot :clear) (default nil))
  (pt-attr/enum pt slot default '(:left :right :all :none)))

(defun pt-attr/color (pt slot &optional (default nil))
  (pt-attr/latin1 pt slot default))

(defun pt-attr/boolean-flag (pt slot)
  (eq (pt-attr/enum pt slot nil (list slot)) slot))

;; multi-length

;; NOTE: The if-match macro is defined in match.lisp

(define-match-macro integer (&optional (radix 10))
  `(& (? (/ #.(char-code #\+) #.(char-code #\-)))
      (+ (p (lambda (ch) (digit-rune-p ch ,radix))))))

(define-match-macro w* ()
  `(* (p #'white-space-rune-p)))

(defun html/parse-integer (s)
  (if-match (s :type rod :test #'rune=) 
            (& (w*) (= $res (integer)) (w*))
            (parse-integer (rod->string (subseq s $res-start $res-end)))))

(defun html/parse-length (s)
  (or
   (if-match (s :type rod :test #'rune=) 
             (& (w*) (= $res (integer)) (w*))
             (cons :px (parse-integer (rod->string (subseq s $res-start $res-end)))))
   (if-match (s :type rod :test #'rune=) 
             (& (w*) (= $res (integer)) #.(char-code #\%) (w*))
             (cons :% (parse-integer (rod->string (subseq s $res-start $res-end)))))))

(defun html/parse-multi-length (s)
  (or
   (html/parse-length s)
   (if-match (s :type rod :test #'rune=) 
             (& (w*) (= $res (integer)) #.(char-code #\*) (w*))
             (cons '* (parse-integer (rod->string (subseq s $res-start $res-end)))))
   ;; This below is illegal syntax '*i' is not allowed
   #+(OR)
   (if-match (s :type rod :test #'rune=) 
             (& (w*) #.(char-code #\*) (= $res (integer)) (w*))
             (cons '* (parse-integer (rod->string (subseq s $res-start $res-end)))))
   ;; "*" is abbrev for "1*"
   (if-match (s :type rod :test #'rune=) 
             (& (w*) #.(char-code #\*) (w*))
             (cons '* 1)) ))

(defun html/parse-length-list (s)
  ;; A comma separated list of lengths
  (let ((q (mapcar #'html/parse-length (split-by (char-code #\,) s))))
    (unless (member nil q)
      q)))

;;;

(defun pt-attr/link-types (pt att)
  (pt-attr/with-parser pt att nil
                       #'(lambda (rod)
                           ;; profile!!
                           (mapcar #'rod-string 
                                   (split-by-if #'white-space-rune-p
                                                rod
                                               :nuke-empty-p t)))
                       "a list of link types"))

(defun pt-attr/comma-separated-list (pt att)
  (pt-attr/with-parser pt att nil
                       #'(lambda (x)
                           (mapcar #'rod-string 
                                   (mapcar #'(lambda (x)
                                               (r2::rod-trim (list 9 10 12 13 32) x))
                                           (split-by #/, x))))
                       "a comma separated list"))

;;;; --------------------------------------------------------------------------------

(defun find-html-head (pt)
  (dolist (k (element-children (sgml:pt-root pt)))
    (cond ((member (element-gi k) '(:HEAD))
           (return k)))))

(defun pt-map-over-link-nodes (fn pt)
  "Map the function `fn' over all <LINK> nodes in the `pt' document's header."
  (let ((head (find-html-head pt)))
    (and head
	 (dolist (k (element-children head))
	   (when (eq (element-gi k) :LINK)
	     (funcall fn k))))))

(defun the-style-node (pt)
  "Given a parse tree, find the <STYLE> node in its header, if any."
  (let ((head (find-html-head pt)))
    (and head
	 (dolist (k (element-children head))
	   (when (eq (element-gi k) :STYLE)
	     (return k))))))

;; Grff...

(defparameter *style-sheet-cache*
    (make-hash-table :test #'equalp))

(defparameter *style-sheet-cache*/lock
    (mp/make-lock :name "*style-sheet-cache*"))

(defun maybe-parse-style-sheet-from-url (url &key (name "anonymous") 
                                                  (supersheet nil)
                                                  (media-type :all))
  (multiple-value-bind (looked presentp)
      (mp/with-lock (*style-sheet-cache*/lock)
          (gethash url *style-sheet-cache*))
    (cond (presentp
           (format *debug-io* "~&;; Serving style sheet ~S [at ~S] from cache.~%"
                   name url)
           looked)
          (t
           (format *debug-io* "~&;; fetching and parsing style sheet ~S [at ~S].~%"
                   name url)
           (let ((res (maybe-parse-style-sheet-from-url-aux
                       url 
                       :name name 
                       :supersheet supersheet
                       :media-type media-type)))
             (mp/with-lock (*style-sheet-cache*/lock)
                (setf (gethash url *style-sheet-cache*) res))
             res)))))

(defun maybe-parse-style-sheet-from-url-aux (url &key (name "anonymous") 
                                                  (supersheet nil)
                                                  (media-type :all))
  (and (css::is-of-media-type-p :screen media-type) ;xxx
       (let ((netlib::*always-use-cache-p* t)) ;hack to improve performance
         (netlib:with-open-document ((input mime-type) url)
           (cond ((and mime-type (netlib::mime-type-equal mime-type :text :css))
                  (multiple-value-bind (res condition)
                      (ignore-errors
                       (css:parse-style-sheet input supersheet
                                              :name name
                                              :base-url url
                                              :media-type media-type))
                    (cond ((null res)
                           (warn "Error while parsing style sheet from ~S:~% ~A"
                                 url condition)
                           nil)
                          (t
                           res))))
                 (t
                  (warn "The resource `~A' has mime type ~A, ~
                    but I expected text/css; style sheet ignored."
                        (url:unparse-url url) 
                        (and mime-type (netlib::mime-type-name mime-type)))
                  nil))))))



;;; big grrf!
;; * html-4.0.zip/struct/links.html#adef-rel:
;; | rel = cdata 
;; |       This attribute describes the link from the current document
;; |       to the anchor specified by the href attribute. This value
;; |       of this attribute is one or more link types separated by
;; |       white space characters.

(defun style-link-does-apply-p (link select-style)
  (unless (equal (slot-value link 'media) "print") ;### kludge
    (cond ((set-equal (link-rel link) '("stylesheet") :test #'string-equal)
           (or
            (null (link-title link))    ;persistent
            (eq select-style :default))) ;selected
          ((set-equal (link-rel link) '("alternate" "stylesheet") :test #'string-equal)
           (or
            (null (link-title link))    ;persistent
            (string-equal (link-title link) select-style))) )))

(defun style-sheet-link-p (link)
  (or (set-equal (link-rel link) (list "alternate" "stylesheet") :test #'string-equal)
      (set-equal (link-rel link) (list "stylesheet") :test #'string-equal)))

(defun alternate-style-sheet-link-p (link)
  (or (set-equal (link-rel link) (list "alternate" "stylesheet") :test #'string-equal)))

(defun default-style-sheet-link-p (link)
  (or (set-equal (link-rel link) (list "stylesheet") :test #'string-equal)))

(defun style-sheet-name-equal-p (x y)
  ;; just in case this changes
  (string= x y))


;;;; --------------------------------------------------------------------------------

(defclass html-document-language ()
  ())

(defclass html-4.0-document-language (html-document-language)
  ())

(defmethod closure-protocol:element-replaced-element-1
    ((language html-4.0-document-language)
     user-agent document device
     elm)
  (declare (ignorable language user-agent))
  (values-list
   (cond ((and (not closure:*user-wants-images-p*)
               (member (element-gi elm) '(:IMG)))
          nil)
         ((member (element-gi elm) '(:IMG))
          ;;xxx (member (element-gi elm) '(:form :button :input :select :textarea :img))
          (cond ((eq (pt-%replacement elm :unset) :unset)
                 (multiple-value-bind (robj action-map) (build-replaced-element document device elm)
                   (setf (pt-%replacement elm) (list robj action-map) )))
                (t
                 (pt-%replacement elm))))
         (t
          nil))))

;;;; --------------------------------------------------------------------------------

(defun replaced-element-p (document device element)
  (closure-protocol:element-replaced-element-1
   closure-protocol:*document-language*
   closure-protocol:*user-agent*
   document device element))

(defun build-replaced-element (document device elm)
  (case (element-gi elm)
    ((:form)
     (replaced-element/form document device elm))
    ((:button)
     (handle-button device elm))
    ((:input)
     (replaced-element/input document device elm))
    ((:select)
     (make-option-menu-replacement device elm))
    ((:textarea)
     (make-text-area-replacement document device elm))
    ((:img)
     (image-replaced-element document device elm))
    (t
     nil)))

(defun replaced-element/input (document device elm)
  document
  (case (pt-attr/input-type elm :type :text)
    (:submit
     (gui:ro/make-submit-button
      device 
      :pt          elm
      :label       (pt-attr* elm :value (rod "Submit Query"))
      :name        (pt-attr* elm :name)
      :size        (pt-attr/integer elm :size nil)
      :disabled-p  (pt-attr/boolean-flag elm :disabled)
      :read-only-p (pt-attr/boolean-flag elm :readonly)
      :text-style  (pt-text-style device elm)
      :document    document))

    (:button
     (gui:ro/make-button
      device
      :pt          elm
      :label       (pt-attr* elm :value (rod "A Button"))
      :name        (pt-attr* elm :name)
      :size        (pt-attr/integer elm :size nil)
      :disabled-p  (pt-attr/boolean-flag elm :disabled)
      :read-only-p (pt-attr/boolean-flag elm :readonly)
      :text-style  (pt-text-style device elm)
      :document    document))

    (:reset
     (gui:ro/make-reset-button
      device 
      :pt          elm
      :label       (pt-attr* elm :value (rod "Reset"))
      :name        (pt-attr* elm :name)
      :size        (pt-attr/integer elm :size nil)
      :disabled-p  (pt-attr/boolean-flag elm :disabled)
      :read-only-p (pt-attr/boolean-flag elm :readonly)
      :text-style  (pt-text-style device elm)
      :document    document))

    (:password
     (make-password-replacement device elm))

    ((:text)
     (make-text-replacement device elm))

    (:checkbox 
     (make-checkbox-replacement device elm))

    (:radio    
     (make-radio-replacement device elm))

    (:hidden
     ;; Hell, why is this ever reached?
     ;; After all HIDDEN input element have display: none;
     (gui::ro/make-hidden
      device 
      :name       (pt-attr* elm :name)
      :value      (pt-attr* elm :value)
      :disabled-p (pt-attr/boolean-flag elm :disabled)))

    ((:image)
     (let ((ro (image-replaced-element/aux document device elm
                                 :uri (pt-effective-url-attr document elm :src)
                                 :alt (pt-attr* elm :alt (rod "Submit Query")))))
       ;; usemap from image-replaced-element/aux?!?
       (let ((map (list (make-instance 'imap-everywhere
                          :link (make-instance 'graphical-submit
                                  :pt elm
                                  :name (pt-attr* elm :name nil))))))
         (values ro map))) )
    
    (t
     (warn "Unrecognized input type: ~S." (pt-attr/input-type elm :type :text))
     nil)))

(defun replaced-element/form (document device elm)
  ;; hack alert
  (setf (pt-%replacement elm) nil)
  (sgml:map-pt (lambda (x) (replaced-element-p document device x)) elm)
  (pt-%replacement elm :unset))

(defun make-text-replacement (device elm)
  (gui:ro/make-text
   device
   :pt            elm
   :name          (pt-attr* elm :name (rod ""))
   :initial-value (pt-attr* elm :value (rod ""))
   :size          (pt-attr/integer elm :size nil)
   :max-length    (pt-attr/integer elm :maxlength nil)
   :disabled-p    (pt-attr/boolean-flag elm :disabled)
   :read-only-p   (pt-attr/boolean-flag elm :readonly)
   :text-style    (pt-text-style device elm)))

(defun make-password-replacement (device elm)
  (gui:ro/make-password
   device
   :pt            elm
   :name          (pt-attr* elm :name (rod ""));hmm warum "" hier?
   :initial-value (pt-attr* elm :value (rod ""))
   :size          (pt-attr/integer elm :size nil)
   :max-length    (pt-attr/integer elm :maxlength nil)
   :disabled-p    (pt-attr/boolean-flag elm :disabled)
   :read-only-p   (pt-attr/boolean-flag elm :readonly)
   :text-style    (pt-text-style device elm)))

(defun make-text-area-replacement (document device elm)
  (gui::ro/make-text-area
   device
   :pt            elm
   :name          (pt-attr* elm :name nil)
   :initial-value (string-trim '(#\space #\return #\newline #\tab) (pt-data elm))
   :cols          (pt-attr/integer elm :cols nil)
   :rows          (pt-attr/integer elm :rows nil)
   :disabled-p    (pt-attr/boolean-flag elm :disabled)
   :read-only-p   (pt-attr/boolean-flag elm :readonly)
   :text-style    (pt-text-style device elm)
   :document      document))

(defun make-radio-replacement (device elm)
  (gui::ro/make-radio-box
   device
   :pt elm
   :name          (pt-attr* elm :name)
   :initial-value (pt-attr* elm :value)
   :checked-p     (pt-attr/boolean-flag elm :checked)
   :disabled-p    (pt-attr/boolean-flag elm :disabled)
   :read-only-p   (pt-attr/boolean-flag elm :readonly)
   :size          (pt-attr/integer elm :size nil)))

(defun make-checkbox-replacement (device elm)
  (gui:ro/make-check-box
   device
   :name          (pt-attr* elm :name)
   :initial-value (pt-attr* elm :value)
   :checked-p     (pt-attr/boolean-flag elm :checked)
   :disabled-p    (pt-attr/boolean-flag elm :disabled)
   :read-only-p   (pt-attr/boolean-flag elm :readonly)
   :size          (pt-attr/integer elm :size nil)))

(defun rod-trim (bag rod)
  (let ((p1 (position-if-not (lambda (x) (member x bag)) rod))
        (p2 (position-if-not (lambda (x) (member x bag)) rod :from-end t)))
    (if (and p1 p2)
        (subseq rod p1 (+ p2 1))
      (rod ""))))

(defun parse-options (elm)
  (case (element-gi elm)
    ((:OPTION)
     (let ((content (pt-data-iso10646 elm) ))
       (setf content (and content (rod-trim '(9 10 12 13 32) content)))
       (gui:make-option-menu-option
        :label      (pt-attr* elm :label content)
        :value      (or (pt-attr* elm :value content))
        :disabled-p (pt-attr/boolean-flag elm :disabled)
        :selected-p (pt-attr/boolean-flag elm :selected)
        :content    content)))
    ((:OPTGROUP)
     (gui:make-option-menu-option-group
      :disabled-p (pt-attr/boolean-flag elm :disabled)
      :label      (pt-attr* elm :label (rod "--submenu--"))
      :children   (remove nil (mapcar #'parse-options (element-children elm)))))
    (otherwise 
     (warn "The ~A element is not allowed with <SELECT>." (element-gi elm))
     nil)))

(defun make-option-menu-replacement (device elm)
  (gui::make-option-menu
   device 
   :pt         elm
   :options    (remove nil (mapcar #'parse-options (element-children elm)))
   :name       (pt-attr* elm :name)
   :multiple-p (pt-attr/boolean-flag elm :multiple)
   :disabled-p (pt-attr/boolean-flag elm :disabled)
   :size       (pt-attr/integer elm :size nil)
   :text-style (pt-text-style device elm)))

;;;;;

;;; ARG!
;; 
;;   <FORM action="..." method="post">
;;   <P>
;;   <INPUT type="password" style="display:none"  
;;             name="invisible-password"
;;             value="mypassword">
;; 
;; 

;;;

(defparameter *use-images-p* t)

(defun image-replaced-element (document device elm)
  (image-replaced-element/aux document device elm
                              :uri (pt-effective-url-attr document elm :src)
                              :usemap (pt-attr* elm :usemap nil)
                              :width (pt-attr/pixels elm :width nil)
                              :height (pt-attr/pixels elm :height nil)
                              :alt (pt-attr* elm :alt (rod "[image]"))))

(defclass graphical-submit ()
  ((name :initarg :name :reader gs-name)
   (pt   :initarg :pt   :reader gs-pt)
   ))

(defmethod command-documentation ((self graphical-submit))
  "Submit Form")

(defclass server-side-image-map ()
  ((url    :initarg :url    :reader ssim-url)
   (target :initarg :target :reader ssim-target)
   (title  :initarg :title  :reader ssim-title)))

(defun find-A-element-for-img-ismap (img-element)
  (do ((q img-element (element-parent q)))
      ((null q))
    (when (and (eq (element-gi q) :a)
               (not (null (pt-attr* q :href))))
      (return q))))

(defmethod command-documentation ((self server-side-image-map))
  (with-slots (title url) self
    (format nil "Server side image map: ~A (~A)."
            (or title "")
            (url:unparse-url url))))

(defun image-replaced-element/aux (document device elm
                                   &key uri (usemap nil) (width nil) (height nil)
                                        alt)
  (cond (*use-images-p*
         (let* ((w width)
                (h height)
                (usemap (and usemap (url:parse-url 
                                     ;;xxx use something better than rod-string
                                     (rod-string usemap))))
                (url uri))
           (let ((obj (gui::make-image-replacement device document :url url :width w :height h))
                 (map nil)
                 (usemapmap nil))

             (when usemap
               (setf usemapmap
                 (find-and-parse-image-map document usemap)))
             
             (when (pt-attr* elm :ismap nil)
               (cond (usemapmap
                      (warn "ISMAP ignored, since client side image map is present."))
                     (t
                      (let ((aelm (find-A-element-for-img-ismap elm)))
                        (cond ((not aelm)
                               (warn "Found <IMG ISMAP ..> without surounding A element."))
                              (t
                               ;; find the title
                               (let ((title 
                                      (or (pt-attr* elm :alt nil)
                                          (pt-attr* aelm :title nil)
                                          (pt-attr* aelm :alt nil))))
                                 (setf map
                                   (list (make-instance 'imap-everywhere 
                                           :link (make-instance 'server-side-image-map
                                                   :url (pt-effective-url-attr document aelm :href)
                                                   :target "_top" ;; xxx 
                                                   :title title)))))))))))

             (setf map (append map usemapmap))
             (setf (pt-%replacement elm) (list obj nil)) ;warum wird das gebraucht!?
             (values obj map))))
        (t
         (let ((r (sgml::copy-pt elm)))
           (setf (pt-%replacement r) nil)
           (setf (element-children r)
             (list
              (sgml::make-pt/low :name :pcdata
                                 :attrs alt
                                 :parent r
                                 :children nil)))
           r))))

;; wir haben das mit dem impliciten stil noch falsch gemacht:

;; | 3.2 CASCADING ORDER
;; |
;; |  A declaration in the 'STYLE' attribute of an element (see section 1.1 
;; |  for an example) has the same weight as a declaration with an ID-based 
;; |  selector that is specified at the end of the style sheet: 
;; | 
;; |      <STYLE TYPE="text/css"> 
;; |        #x97z { color: blue } 
;; |      </STYLE> 
;; |       
;; |      <P ID=x97z STYLE="color: red"> 
;; |   
;; |  In the above example, the color of the 'P' element would be red. 
;; |  Although the specificity is the same for both declarations, the 
;; |  declaration in the 'STYLE' attribute will override the one in the 
;; |  'STYLE' element because of cascading rule number 5. 
;; |   
;; |  The UA may choose to honor other stylistic HTML attributes, for 
;; |  example 'ALIGN'. If so, these attributes are translated to the 
;; |  corresponding CSS rules with specificity equal to 1. [*] The rules are 
;; |  assumed to be at the start of the author style sheet and may be 
;; |  overridden by subsequent style sheet rules. In a transition phase, 
;; |  this policy will make it easier for stylistic attributes to coexist 
;; |  with style sheets. 

;; [*] that is they should be treated like a selector featuring
;;     exactly the gi.
;; Note: This rule about the HTML attrs is bad. If say e.g. 
;;
;; P { text-align: left; }, i have no option to say in the HTML
;; document e.g. <P align=right> Arg!
;;

;; Command protocol
;;
;; command-documentation command -> rod
;; command-cursor command -> cursor keyword
;; 

(defstruct hyper-link 
  ;; structure for a hyper-link
  url           ;the URL this points to
  alt           ;alternate text or NIL
  target        ;target frame
  ;; wir brauchen noch:
  ;; - is-map-p (für server-side image maps).

  ;; und Möglichkeiten noch zu specifizieren, daß das hier eine FORM
  ;; submitten soll.
  ;; d.h. wir benötigen eher so etwas wie ein assoziertes Commando.
  )

(defmethod command-documentation ((self hyper-link))
  (let ((url-string (if (not (null (hyper-link-url self)))
                        (url:unparse-url (hyper-link-url self))
                      "--no url--")))
    (if (hyper-link-alt self)
        (format nil "~A (~A)"
                (rod-string (hyper-link-alt self)) url-string)
      (format nil "~A" url-string))))

(defmethod command-documentation ((self hyper-link))
  (let ((url-string (if (not (null (hyper-link-url self)))
                        (url:unparse-url (hyper-link-url self))
                      "--no url--")))
    (if (hyper-link-alt self)
        (concatenate 'rod (hyper-link-alt self)
                     (string-rod " (")
                     (string-rod url-string)
                     (string-rod ")"))
      (format nil "~A" url-string))))

(defmethod command-documentation ((self t))
  ;; fall back method
  (format nil "~S" self))

;;;; Image Maps

(defclass imap-area ()
  ((link :initarg :link :initform nil :reader imap-area-link)));hyper link

(defclass imap-everywhere (imap-area)
  () )

(defclass imap-rectangle (imap-area)
  ((x0 :initarg :x0)
   (y0 :initarg :y0)
   (x1 :initarg :x1)
   (y1 :initarg :y1)) )

(defclass imap-circle (imap-area)
  ((x :initarg :x)
   (y :initarg :y)
   (radius :initarg :radius)) )

(defclass imap-polygon (imap-area)
  ((point-seq :initarg :point-seq)))    ;sequence (x0 y0 x1 y1 .. xn yn) as in CLX

(defun area-node-to-imap-area (elm)
  ;; Parses an AREA (or A) element into an imap-area object.
  ;; Returns NIL, if something goes wrong.
  (assert (member (element-gi elm) '(:AREA :A)))
  (let ((href (and (pt-attr* elm :href nil)
                   (url:parse-url (pt-attr/latin1 elm :href))))
        (nohref (pt-attr/boolean-flag elm :nohref))
        (alt    (pt-attr* elm :alt nil))
        (shape  (pt-attr/enum elm :shape :rect '(:default :rect :circle :poly)))
        (coords (pt-attr/length-list elm :coords nil))
        (target (pt-effective-target-attr elm :target)))
    (when (and nohref href)
      (warn "AREA element has HREF attribute despite of present NOHREF attribute.~
             NOHREF takes precedence.")
      (setq href nil))
    (unless alt
      (warn "Each AREA element should have an 'alt' attribute."))
    (let ((link (make-hyper-link
                 :url href
                 :alt alt
                 :target target)))
      (ecase shape
        ((:default) (area-node-to-imap-area/default elm link coords))
        ((:rect)    (area-node-to-imap-area/rect elm link coords))
        ((:circle)  (area-node-to-imap-area/circle elm link coords))
        ((:poly)    (area-node-to-imap-area/poly elm link coords))) )) )

(defun area-node-to-imap-area/default (elm link coords)
  (declare (ignore elm))
  (cond ((= 0 (length coords))
         (make-instance 'imap-everywhere
           :link link))
        (t
         (warn "A `default' AREA should not specify any `coords'.")
         nil)))

(defun area-node-to-imap-area/rect (elm link coords)
  (declare (ignore elm))
  (cond ((= 4 (length coords))
         (make-instance 'imap-rectangle
           :link link
           :x0 (first coords)
           :y0 (second coords)
           :x1 (third coords)
           :y1 (fourth coords) ))
        (t
         (warn "A `rect' AREA should specify exactly four `coords'.")
         nil)))

(defun area-node-to-imap-area/circle (elm link coords)
  (declare (ignore elm))
  (cond ((= 3 (length coords))
         (make-instance 'imap-circle
           :link link
           :x (first coords)
           :y (second coords)
           :radius (third coords) ))
        (t
         (warn "A `circle' AREA should specify exactly three `coords'.")
         nil)))

(defun area-node-to-imap-area/poly (elm link coords)
  (declare (ignore elm))
  (cond ((evenp (length coords))
         (make-instance 'imap-polygon
           :link link
           :point-seq coords))
        (t
         (warn "A `poly' AREA should specify an even number of `coords'.")
         nil)))
    
(defun parse-image-map (document node)
  "Parses a <MAP> node into a list of IMAP-AREA objects"
  ;; TODO: warning message, if old and new style <MAP>'s are mixed.
  ;; for now we simply collect all AREA and A elements.
  (declare (ignore document))
  (assert (eq (element-gi node) :MAP))
  (let ((res nil))
    (sgml:map-pt (lambda (x)
                   (when (member (element-gi x) '(:A :AREA))
                     (let ((q (area-node-to-imap-area x)))
                       (when q
                         (push q res)))))
                 node)
    ;; order is important
    (nreverse res)))

(defun find-image-map (document url)
  (cond ((not (null (url:url-anchor url)))
         (cond ((and (null (url:url-protocol url))
                     (null (url:url-host url))
                     (null (url:url-port url))
                     (null (url:url-path url))
                     (null (url:url-host url)))
                ;; something in this document
                (find-image-map-in-pt (document-pt document) (url:url-anchor url)) )
               (t
                (warn "Image maps in other documents than the current are not supported.")
                nil)))
        (t
         (warn "Found `usemap' URI without an anchor component.")
         nil)))

(defun find-image-map-in-pt (pt anchor)
  (sgml:map-pt (lambda (node)
                 (when (and (eq (element-gi node) :map)
                            (pt-attr* node :name)
                            (string-equal (pt-attr/latin1 node :name) anchor))
                   (return-from find-image-map-in-pt node)))
               pt))

;; Questions with regard to image maps:
;; 1. What is the exact meaning of percentage values?
;; 2. Why does the A element have no "nohref" attribute. Accident?
;; 3. Why has AREA none of the rel, rev, charset etc. Attributes?

;;;;

(defmethod area-contains-point-p ((area imap-everywhere) x y w h)
  (declare (ignore x y w h))
  t)

(defmethod area-contains-point-p ((area imap-rectangle) x y w h)
  (with-slots (x0 y0 x1 y1) area
    (let ((x0 (area-resolve-coordinate x0 w))
          (y0 (area-resolve-coordinate y0 h))
          (x1 (area-resolve-coordinate x1 w))
          (y1 (area-resolve-coordinate y1 h)))
      (and (<= (min x0 x1) x (max x0 x1))
           (<= (min y0 y1) y (max y0 y1))))))

(defmethod area-contains-point-p ((area imap-circle) px py w h)
  (with-slots (x y radius) area
    (let ((x (area-resolve-coordinate x w))
          (y (area-resolve-coordinate y h))
          (radius (min (area-resolve-coordinate radius w)
                       (area-resolve-coordinate radius h))))
      (<= (+ (* (- px x) (- px x))
             (* (- py y) (- py y)))
          (* radius radius)))))

(defun polygon-contains-point-p (coord-seq x y)
  (let ((n (floor (length coord-seq) 2))
        (k 0))
    (macrolet ((px (i) `(elt coord-seq (* 2 (mod ,i n))))
               (py (i) `(elt coord-seq (+ 1 (* 2 (mod ,i n))))))
      (dotimes (i n)
        (let ((x0 (px i)) (y0 (py i))
              (x1 (px (+ i 1))) (y1 (py (+ i 1))))
          (cond ((or (< y0 y y1) 
                     (> y0 y y1))
                 (let ((xi
                        (cond ((= (- x1 x0) 0) x0)
                              (t 
                               (* (+ (* (/ (- y1 y0) (- x1 x0)) x0)
                                     (- y y0))
                                  (/ (- x1 x0) (- y1 y0)))))))
                   (cond ((< xi x) (incf k))
                         ((= xi x) (return-from polygon-contains-point-p t)))))
                ((and (/= y0 y) (= y1 y))
                 (let ((j (do ((j (+ i 1) (+ j 1)))
                              ((= j (+ i n)) j)
                            (when (/= (py j) y)
                              (return j)))))
                   (when (/= (signum (- y y0)) (signum (- y (py j))))
                     (cond ((< x1 x) (incf k))
                           ((= x1 x) (return-from polygon-contains-point-p t))))))
                ((and (= y0 y) (= y 1 y))
                 (when (or (<= x0 x x1) (>= x0 x x1))
                   (return-from polygon-contains-point-p t))))))
      (oddp k))))

(defmethod area-contains-point-p ((area imap-polygon) px py w h)
  (let ((new-point-seq nil))
    ;; resolve all coordinates and put then into points as list of conses
    (with-slots (point-seq) area
      (do ((q point-seq (cddr q)))
          ((or (endp q) (endp (cdr q))))
        (push (area-resolve-coordinate (cadr q) h) new-point-seq)
        (push (area-resolve-coordinate (car q) w) new-point-seq)))
    ;; the hard work is done in GU
    (polygon-contains-point-p new-point-seq px py)))

(defun area-resolve-coordinate (coordinate whole)
  (ecase (car coordinate)
    ((:PX) (cdr coordinate))
    ((:%)  (/ (* (cdr coordinate) whole) 100))))

;;;;

(defun find-and-parse-image-map (document url)
  "Attemps to convert a URL to an image, return NIL, if anything fails."
  (let ((p (find-image-map document url)))
    (when p 
      (setf p (parse-image-map document p)))
    (when p
      (setf p (mapcar (lambda (x)
                        ;;zzz
                        (when (hyper-link-url (slot-value x 'link))
                          (setf (hyper-link-url (slot-value x 'link))
                            (url:merge-url (hyper-link-url (slot-value x 'link))
                                           (document-base-url document))))
                        x)
                      p)))
    p))

(defclass ro/image ()
  ((url :initarg :url)
   (iwidth  :initarg :iwidth)
   (iheight :initarg :iheight)
   (awidth  :initarg :awidth  :initform nil)
   (aheight :initarg :aheight :initform nil)
   (alt :initarg :alt)
   (aimage :initarg :aimage :initform nil)
   (pixmap :initform nil)
   (mask :initform nil)
   (fixed-size-p :initform nil :initarg :fixed-size-p)
   ))

(defmethod ro/intrinsic-size ((self ro/image))
  (with-slots (url iwidth iheight) self
    (cond ((and iwidth iheight)
           (values iwidth iheight 0))
          (t
           (values 20 20 0)))))

(defmethod ro/size ((self ro/image))
  (with-slots (url awidth aheight) self
    (cond ((and awidth aheight)
           (values awidth aheight 0))
          (t
           (values 20 20 0)))))

(defmethod update-lazy-object (document (self ro/image))
  (with-slots (url fixed-size-p) self
    (let ((aim (document-fetch-image document self url)))
      (with-slots (iwidth iheight (self.aimage aimage) awidth aheight) self
        (setf iwidth (aimage-width aim)
              iheight (aimage-height aim)
              self.aimage aim)
        (unless awidth (setf awidth (aimage-width aim)))
        (unless aheight (setf aheight (aimage-height aim)))
        ))
    (cond (fixed-size-p
           ;; **hack**
           (with-slots (aimage awidth aheight) self
             (let ((drawable (xlib:screen-root (xlib:display-default-screen clue-gui2::*dpy*))))
               (with-slots (pixmap mask) self
                 (unless pixmap
                   (let ((q (clue-gui2::make-pixmap-from-aimage drawable aimage awidth aheight)))
                     (setf pixmap (car q)
                           mask   (cadr q)))))))     
           ;; return
           nil)
          (t
           ;; return
           t))))

(defmethod x11-draw-robj (drawable gcontext (self ro/image) box x y)
  drawable gcontext self box x y
  (setf x (floor x))
  (setf y (floor y))
  (with-slots (alt awidth aheight aimage url) self
    (cond (aimage
           (unless awidth (setf awidth (aimage-width aimage)))
           (unless aheight (setf aheight (aimage-height aimage)))
           (with-slots (pixmap mask) self
             (unless pixmap
               (warn "Rendering pixmap while redisplay (~S)" 
                     url)
               (let ((q (clue-gui2::make-pixmap-from-aimage drawable aimage awidth aheight)))
                 (setf pixmap (car q)
                       mask   (cadr q))))
               (cond ((not (null mask))
                      (xlib:with-gcontext (gcontext :clip-mask mask
                                                    :clip-x x
                                                    :clip-y (- y aheight))
                        (xlib:copy-area pixmap gcontext 0 0 awidth aheight
                                        drawable x (- y aheight))) )
                     (t
                      (xlib:copy-area pixmap gcontext 0 0 awidth aheight
                                      drawable x (- y aheight) )))))

          (t
           (multiple-value-bind (w h) (ro/size self)
             (setf w (floor w))
             (setf h (floor h))
             (xlib:with-gcontext (gcontext 
                                  :foreground (ws/x11::x11-find-color drawable :black)
                                  )         
               (xlib:draw-glyphs drawable gcontext x y (rod-string alt))
               (xlib:draw-rectangle drawable gcontext x (- y h) w h)))) )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Now this is misnomer, since being HTML is no property of the tree,
;; but of the "document language", we'll refine that later.
;; 2002-08-07 GB

(defmethod closure-protocol:element-css-class ((element sgml::pt))
  (element-attribute element :CLASS))

(defmethod closure-protocol:element-css-id ((element sgml::pt))
  (element-attribute element :ID))

(defmethod closure-protocol:pseudo-class-matches-p (pseudo-class (pt sgml::pt))
  (case pseudo-class
    ((:link :visited)
     (and (not (null (r2::pt-attr/latin1 pt :href))))) ;this is of course a bit too lazy!
    ((:hover)
     (and
      (sgml:pt-attr pt :%hover-p nil)))
    ((:first-letter)
     (eq (sgml::pt-attr pt :%pseudo-class) pseudo-class))
    (otherwise
     nil)))

(defmethod closure-protocol:element-style-cache ((element sgml::pt))
  (sgml::pt-cache element))

(defmethod (setf closure-protocol:element-style-cache) (new-value (element sgml::pt))
  (setf (sgml::pt-cache element) new-value))

;;;

(defmethod closure-protocol:element-base-url ((language html-document-language) element)
  (let ((base-element (pt-headers-base-element element)))
    (cond ((and (not (null base-element))
                (not (null (pt-attr* base-element :HREF)))) 
           (url:parse-url (pt-attr/latin1 base-element :HREF)))
          (t
           nil) )))

(defun pt-headers-base-element (pt)
  "Return the <BASE> element of the document `pt' is a sub-node of."
  (cond ((null pt) nil)
        ((eq (element-gi pt) :HTML)
         (dolist (q (element-children pt))
           (when (eq (element-gi q) :HEAD)
             (dolist (k (element-children q))
               (when (eq (element-gi k) :BASE)
                 (return-from pt-headers-base-element k))))))
        ((pt-headers-base-element (element-parent pt))) ))

(defun pt-base-target (pt)
  ;; only used from CLUE-GUI and PT-EFFECTIVE-TARGET-ATTR, soon to be nuked.
  (let ((base-element (pt-headers-base-element pt)))
    (and base-element
         (pt-attr/latin1 base-element :TARGET NIL))))

(defun pt-effective-target-attr (pt attr)
  "Return the effective target attribute of `pt' named `attr';
   Does look at the header, if `pt' has no attribute called `attr'."
  (or (pt-attr/latin1 pt attr nil)
      (pt-base-target pt)))

(defmethod closure-protocol:element-imap ((document-language html-document-language)
                                          document
                                          element)
  (cond ((and (eq (element-gi element) :A)
              (pt-attr* element :href nil))
         (make-instance 'imap-everywhere
                        :link (make-hyper-link 
                               :url (pt-effective-url-attr document element :href)
                               :alt (pt-attr* element :title nil)
                               :target (pt-effective-target-attr element :target))))
        (t
         nil))
  )


(defmethod closure-protocol:render
    ((document-language html-document-language)
     document device pt
     w
     &optional (flag t) (h 0)
     &key (selected-style :default))
  (declare (ignorable document-language))

  ;; temporary medicine
  (setf (document-title document) nil
        (document-links document) nil)
  
  ;; Post possible HTTP Link: header fields.
  (dolist (k (document-http-header document))
    (cond ((string-equal :link (car k))
           (let ((links (netlib:parse-http-link-field (cdr k))))
             (cond ((null links)
                    (warn "HTTP \"Link\" header field has illegal syntax: ~S." (cdr k)))
                   (t
                    (dolist (link links)
                      (process-head-child document (sgml:lhtml->pt link)))))))))
  
  ;; First of all process the head
  (dolist (k (element-children pt))
    (when (eq (element-gi k) :head)
      (process-head document k)))

  ;;
  (unless (document-title document)
    (setf (document-title document) "no title")
    (warn "Each document should have a TITLE element."))

  (render2 device document pt selected-style)
  '(render-pt device document pt w flag h :selected-style selected-style)
  )

(defun process-head (document pt)
  (dolist (k (element-children pt))
    (process-head-child document k)))

(defun process-head-child (doc pt)
  (cond ((eq (element-gi pt) :title)
         (setf (document-title doc) (pt-data pt)))
        ((eq :link (element-gi pt))
         (setf (document-links doc)
           (append (document-links doc)
                   (list (make-instance 'link
                           :title  (pt-attr/latin1 pt :title)
                           :rel    (pt-attr/link-types pt :rel)
                           :rev    (pt-attr/link-types pt :rev)
                           :media  (pt-attr/latin1 pt :media) ;; xxx (comma-separated-list k :media)
                           :target (pt-attr/latin1 pt :target)
                           :href   (pt-effective-url-attr doc pt :href))))))
        (t
         )))

(defmethod closure-protocol:root-element-embedded-style ((language html-document-language) pt)
  (let ((style-node (the-style-node pt)))
    (and style-node
         (pt-all-data style-node))))

(defun br-element-p (element)
  (and (not (text-element-p element))
       (eql :BR (element-gi element))))

;;;;

(defgeneric element-attribute-parser (document-language gi attribute))

(defun parsed-attribute (document-language element attribute)
  (funcall (element-attribute-parser document-language (element-gi element) attribute)
           (element-attribute element attribute)))

;;;

(defgeneric eis (document-language element)
  (:method-combination append))

(defgeneric eis-by-gi (document-language element gi)
  (:method-combination append))

(defgeneric eis-by-attr (document-language element gi attr value)
  (:method-combination append))

(defmethod eis append (document-language element)
           ;; fallback returning NIL
  nil)

(defmethod eis :around (document-language element)
  (append
   (eis-by-gi document-language element (element-gi element))
   (call-next-method)))

(defmethod eis-by-gi :around (document-language element gi)
  (append
   (loop for (attr value . rest) = (sgml::pt-attrs element) then rest while attr
         append 
         (eis-by-attr document-language element
                      (element-gi element)
                      attr
                      value))
   (call-next-method)))

(defmethod eis-by-attr append (document-language element gi attr value)
           nil)

(defmethod eis-by-gi append (document-language element gi)
           nil)

(defmacro define-style-mapping (document-language (element &optional gi attr value) &body body)
  (cond ((and (consp gi)
              (consp (cadr gi))
              (eq (car (cadr gi)) 'member))
         `(progn
           ,@(mapcar (lambda (elt)
                       `(define-style-mapping ,document-language
                         (,element (,(car gi) (eql ,elt)) ,attr ,value)
                         .,body))
                     (cdr (cadr gi)))))
        (t
         `(defmethod ,(cond (value 'eis-by-attr)
                            (gi    'eis-by-gi)
                            (t     'eis))
           append
           ((document-language ,document-language)
            ,element
            ,@(and gi `(,gi))
            ,@(and gi attr value `(,attr ,value)))
           (declare (ignorable document-language ,element
                     ,@(and gi (list (car gi)))
                     ,@(and attr (list (car attr)))
                     ,@(and value (list value))))
           (let ((res nil))
             (labels ((add-style (attr value)
                        (push (cons attr value) res)))
               ,@body
               res)))) ))

;;;;

(defmethod closure-protocol:element-implicit-style (document (element sgml::pt))
  (element-implicit-style-2 *document-language* document element))

(defun element-implicit-style-2 (document-language document element)
  (declare (ignore document))
  (eis document-language element))

(defun implicit-style/IMG (pt)
  (let ((res nil) x)
    (case (pt-attr/img-align pt :align nil)
      (:top    (push (cons 'css:@vertical-align :top) res))
      (:bottom (push (cons 'css:@vertical-align :baseline) res)) ;wrong?
      (:middle (push (cons 'css:@vertical-align :img-middle) res))
      (:left   (push (cons 'css:@float :left) res))
      (:right  (push (cons 'css:@float :right) res)))
    (when (setf x (pt-attr/pixels pt :vspace nil))
      (push (cons 'css:@margin-top (cons :px x)) res)
      (push (cons 'css:@margin-bottom (cons :px x)) res))
    (when (setf x (pt-attr/pixels pt :hspace nil))
      (push (cons 'css:@margin-left (cons :px x)) res)
      (push (cons 'css:@margin-right (cons :px x)) res))
    ;;
    (when (setf x (pt-attr/pixels pt :border nil))
      (push (cons 'css:@border-left-width       (cons :px x)) res)
      (push (cons 'css:@border-right-width      (cons :px x)) res)
      (push (cons 'css:@border-top-width        (cons :px x)) res)
      (push (cons 'css:@border-bottom-width     (cons :px x)) res))
    ;;
    (when (setf x (pt-attr/length pt :width nil))
      (push (cons 'css:@width x) res))
    (when (setf x (pt-attr/length pt :height nil))
      (cond ((css:percentage-p x)
             ;; REC-html40-19980424, 13.7.1 says:
             ;; | [...] that lengths expressed as percentages are based on the
             ;; | [...] vertical space currently available
             ;; without defining that "vertical space currently available"
             (warn "No percentage values on IMG.height please."))
            (t
             (push (cons 'css:@height x) res))))
    res))

(defmacro define-simple-style (document-language (gi attr value) (css-attr css-value))
  `(define-style-mapping ,document-language (element (gi (eql ,gi)) (attr (eql ,attr)) (value (eql ,value)))
    (declare (ignore value))
    (add-style ',css-attr ',css-value)))

;;;; IMG

(define-style-mapping html-4.0-document-language (element (gi (eql :IMG)) (attr (eql :ALIGN)) value)
  (case (pt-attr/img-align element :align nil)
    (:top    (add-style 'css:@vertical-align :top))
    (:bottom (add-style 'css:@vertical-align :baseline)) ;wrong?
    (:middle (add-style 'css:@vertical-align :img-middle))
    (:left   (add-style 'css:@float :left))
    (:right  (add-style 'css:@float :right))))

(define-style-mapping html-4.0-document-language (element (gi (eql :IMG)) (attr (eql :VSPACE)) value)
  (let (x)
    (when (setf x (pt-attr/pixels element :vspace nil))
      (add-style 'css:@margin-top    (cons :px x))
      (add-style 'css:@margin-bottom (cons :px x)))))

(define-style-mapping html-4.0-document-language (element (gi (eql :IMG)) (attr (eql :HSPACE)) value)
  (let (x)
    (when (setf x (pt-attr/pixels element :hspace nil))
      (add-style 'css:@margin-left    (cons :px x))
      (add-style 'css:@margin-right   (cons :px x)))))

(define-style-mapping html-4.0-document-language (element (gi (eql :IMG)) (attr (eql :BORDER)) value)
  (let (x)
    (when (setf x (pt-attr/pixels element :border nil))
      (add-style 'css:@border-left-width       (cons :px x))
      (add-style 'css:@border-right-width      (cons :px x))
      (add-style 'css:@border-top-width        (cons :px x))
      (add-style 'css:@border-bottom-width     (cons :px x)))))

(define-style-mapping html-4.0-document-language (element (gi (eql :IMG)) (attr (eql :WIDTH)) value)
  (let (x)
    (when (setf x (pt-attr/length element :width nil))
      (add-style 'css:@width x))))

(define-style-mapping html-4.0-document-language (element (gi (eql :IMG)) (attr (eql :HEIGHT)) value)
  (let (x)
    (when (setf x (pt-attr/length element :height nil))
      (cond ((css:percentage-p x)
             ;; REC-html40-19980424, 13.7.1 says:
             ;; | [...] that lengths expressed as percentages are based on the
             ;; | [...] vertical space currently available
             ;; without defining that "vertical space currently available"
             (warn "No percentage values on IMG.height please."))
            (t
             (add-style 'css:@height x))))))

;;;; UL/OL/LI

(define-style-mapping html-4.0-document-language (element (gi (member :UL :OL :LI)) (attr (eql :TYPE)) value)
  (let ((x (pt-attr/list-style-type element :type)))
    (and x (add-style 'css:@list-style-type x))))

(define-style-mapping html-4.0-document-language (element (gi (member :UL :OL)) (attr (eql :COMPACT)) value)
  (when value (add-style 'css:@list-style-position :inset)))


;;;;

(define-style-mapping html-4.0-document-language (element
                                                  (gi (member :P :DIV :H1 :H2 :H3 :H4 :H5 :H6))
                                                  (attr (eql :align))
                                                  value)
  (let ((x (pt-attr/align element)))
    (and x (add-style 'css:@text-align x))))

;;;; BR

(define-style-mapping html-4.0-document-language (element (gi (eql :BR)) (attr (eql :clear)) value)
  (case (pt-attr/clear element)
    ((:left)  (add-style 'css:@clear :left))
    ((:right) (add-style 'css:@clear :right))
    ((:all)   (add-style 'css:@clear :both))))

;;;; FONT

(define-style-mapping html-4.0-document-language (pt (gi (eql :FONT)))
  (let (sz)
    (when (pt-attr/color pt :color)
      (add-style 'css:@color (pt-attr/color pt :color)))
    (when (pt-attr/latin1 pt :face)
      (let ((f (split-by-if (lambda (x)
                              (member x (list #\, #\space #\tab #\newline #\return) 
                                      :test #'char=))
                            (pt-attr/latin1 pt :face) :nuke-empty-p t)))
        (add-style 'css:@font-family f)))
    (when (setq sz (maybe-parse-integer (pt-attr/latin1 pt :size)))
      (cond ((or (char= (char (pt-attr/latin1 pt :size) 0) #\+)
                 (char= (char (pt-attr/latin1 pt :size) 0) #\-))
             ;;(add-style 'css:@font-size (cons :% (* 100 (expt 7/6 sz))))
             (add-style 'css:@font-size (cons :px (* 14 (expt 7/6 sz)))))
            (t
             (add-style 'css:@font-size
                        (cond ((<= sz 0) (cons :pt 6))
                              ((= sz 1) (cons :pt 8))
                              ((= sz 2) (cons :pt 10))
                              ((= sz 3) (cons :pt 12))
                              ((= sz 4) (cons :pt 14))
                              ((= sz 5) (cons :pt 16))
                              ((= sz 6) (cons :pt 18))
                              ((>= sz 7) (cons :pt 20))))) )) ))

;;;; TR

(define-style-mapping html-4.0-document-language (pt (gi (eql :TR)))
  (let (x)
    (when (member (setq x (pt-attr/cell-halign pt :align)) '(:left :center :right :justify))
      (add-style 'css:@text-align x))))

;;;; HR

(define-style-mapping html-4.0-document-language (pt (gi (eql :HR)))
  (let ((noshade (pt-attr/boolean-flag pt :noshade))
        (size  (pt-attr/pixels pt :size))
        (width (pt-attr/length pt :width))
        (align (pt-attr/align pt :align)))
    (when noshade
      (add-style 'css:@border-top-style :solid)
      (add-style 'css:@border-right-style :solid)
      (add-style 'css:@border-bottom-style :solid)
      (add-style 'css:@border-left-style :solid))
    (when width
      (add-style 'css:@width width))
    (when size
      (add-style 'css:@border-top-width (cons :px (floor size 2)))        
      (add-style 'css:@border-bottom-width (cons :px (ceiling size 2))))
    (case align
      (:left 
       (add-style 'css:@margin-left '(:px . 0))
       (add-style 'css:@margin-right :auto))
      (:right 
       (add-style 'css:@margin-left :auto)
       (add-style 'css:@margin-right '(:px . 0)))
      (:center 
       (add-style 'css:@margin-left :auto)
       (add-style 'css:@margin-right :auto))) ))

;;;; TD/TH

(define-style-mapping html-4.0-document-language (pt (gi (member :TD :TH)))
  (let (x)
    (when (pt-attr/color (element-parent pt) :bgcolor)
      (add-style 'css:@background-color (pt-attr/color (element-parent pt) :bgcolor)))
    (when (pt-attr/color pt :bgcolor)
      (add-style 'css:@background-color (pt-attr/color pt :bgcolor)))
    (when (pt-attr/boolean-flag pt :nowrap)
      (progn;;unless (pt-attr* pt :width)      ;emulate netscape behaviour.
        (add-style 'css:@white-space :nowrap)))
    (when (pt-attr/latin1 pt :background)
      (add-style 'css:@background-image (pt-effective-url-attr *document* pt :background)))
    ;; The valign attribute is inherited
    ;; ### we miss inheriting valign from the column and column-group elements
    ;;     Which is not that easy to gather from here, since for that
    ;;     we actually are forced to parse the table structure, which
    ;;     we do in the CSS renderer already.
    (when (setq x (or (pt-attr/cell-valign pt :valign)
                      (pt-attr/cell-valign (element-parent pt) :valign) ;row
                      (pt-attr/cell-valign (element-parent (element-parent pt)) :valign) ;row-group
                      (pt-attr/cell-valign (element-parent (element-parent (element-parent pt))) :valign) ;table
                      :middle))         ;note that the default is not actually specified
      (add-style 'css:@vertical-align x))
    ;; ### we miss inheriting align from the column and column-group elements
    ;;     (see note above)
    (when (member (setf x (or (pt-attr/cell-halign pt :align)
                              (pt-attr/cell-halign (element-parent pt) :align) ;row
                              (pt-attr/cell-halign (element-parent (element-parent pt)) :align) ;row-group
                              (pt-attr/cell-halign (element-parent (element-parent (element-parent pt))) :align) ;table
                              :left))
                  '(:left :center :right :justify))
      (add-style 'css:@text-align x))
    
    (when (setq x (pt-attr/length pt :width nil))
      (when (css:percentage-p x)
        (setf x (cons :% (cdr x))))
      (add-style 'css:@width x))

    ;; ### we miss height.
    
    (let ((table (element-parent (element-parent (element-parent pt)))))
      (when (and (eq (element-gi table) :table))
        (when (setq x (pt-attr/length table :cellpadding))
          (add-style 'css:@padding-top x)
          (add-style 'css:@padding-bottom x)
          (add-style 'css:@padding-left x)
          (add-style 'css:@padding-right x))
        ;; hmmm
        (add-style 'css:@border-top-width 0)
        (add-style 'css:@border-bottom-width 0)
        (add-style 'css:@border-left-width 0)
        (add-style 'css:@border-right-width 0)
        (add-style 'css:@border-top-style :none)
        (add-style 'css:@border-bottom-style :none)
        (add-style 'css:@border-left-style :none)
        (add-style 'css:@border-right-style :none)
        (when (equal '(:px . 0) (pt-attr/length table :border '(:px . 0)))
          (add-style 'css:@border-top-width 0)
          (add-style 'css:@border-bottom-width 0)
          (add-style 'css:@border-left-width 0)
          (add-style 'css:@border-right-width 0)) ))))

;;;; TABLE

(define-style-mapping html-4.0-document-language (pt (gi (eql :TABLE)))
  (let (x)
    (when (eq (pt-attr/table.align pt :align nil) :left) 
      (add-style 'css:@float :left))
    (when (eq (pt-attr/table.align pt :align nil) :right) 
      (add-style 'css:@float :right))
    #||
    (when (eq (css:style- attr (element-parent pt) 'css:@text-align)
              :right)
      (add-style 'css:@margin-left :auto)
      (add-style 'css:@margin-right 0))
    ||#
    #||
    (when (or (eq (pt-attr/table.align pt :align nil) :center) 
              (eq (css:style- attr (element-parent pt) 'css:@text-align)
                  :center) )
      (add-style 'css:@margin-left :auto)
      (add-style 'css:@margin-right :auto))
    ||#
    (when (setq x (pt-attr/length pt :cellspacing nil))
      (add-style 'css::@border-spacing (list x x)))
    ;;
    (when (setq x (pt-attr/length pt :width nil))
      '(when (css:percentage-p x)
        (setf x (cons :canvas-h-percentage (cdr x))))
      (when (css:percentage-p x)
        (setf x (cons :% (cdr x))))
      (add-style 'css:@width x))
    (when (setq x (pt-attr/color pt :bgcolor))
      (add-style 'css:@background-color x))))

;;;; BODY

(define-style-mapping html-4.0-document-language (pt (gi (eql :BODY)))
  (let (x)
    (when (pt-attr/latin1 pt :background)
      (add-style 'css:@background-image (pt-effective-url-attr *document* pt :background)))
    (when (setq x (pt-attr/color pt :bgcolor))
      (add-style 'css:@background-color x))
    (when (setq x (pt-attr/color pt :text))
      (add-style 'css:@color x))))

;;;; THEAD/TBODY/TFOOT

(define-style-mapping html-4.0-document-language (pt (gi (member :THEAD :TBODY :TFOOT)))
  (let (x)
    ;; Note: align=justify is in the HTML spec but utter nonsense
    (when (member (setq x (pt-attr/cell-halign pt :align))
                  '(:left :center :right :justify))
      (add-style 'css:@text-align x))))

;;;; A

(define-style-mapping html-4.0-document-language (pt (gi (eql :A)))
  (let (x)
    ;; ### Now, this is crude!
    (when (and (pt-attr/latin1 pt :href)
               (setq x (pt-body-element pt))
               (setq x (pt-attr/color x :link)))
      (add-style 'css:@color x))))

;;;; INPUT

(define-style-mapping html-4.0-document-language (pt (gi (eql :INPUT)))
  (when (eq :hidden (pt-attr/input-type pt :type))
    (add-style 'css:@display :none))
  (when (eq :image (pt-attr/input-type pt :type))
    ;; ### - netscape 4 doesn't do this.
    (when (pt-attr/pixels pt :size)
      (add-style 'css:@width (cons :px (pt-attr/pixels pt :size))))))

;;;; CAPTION


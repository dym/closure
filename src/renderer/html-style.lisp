;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: RENDERER; Readtable: GLISP; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: HTML-Specific Parts of the Renderer
;;;   Created: 1999-05-25
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: GPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1998,1999 by Gilbert Baumann

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

;;; ---- Implicit Style -----------------------------------------------------------------------

(defun pt-attr/low (pt prop &optional (default nil))
  (cond ((eq (sgml:gi pt) :PCDATA) default)
	((getf (sgml:pt-attrs pt) prop default))))

(defun pt-attr* (pt attr &optional default)
  (pt-attr/low pt attr default))

(defun pt-attr/latin1 (pt attr &optional default)
  (let ((r (pt-attr/low pt attr)))
    (if r
        (map 'string (lambda (x) (or (code-char x) #\?)) r)
      default)))

(defun pt-style-style (document pt)
  (let ((x (pt-attr* pt :style)))
    (when x
      (let ((css::*style-sheet-base-url* (document-base-url document)))
        (css::parse-assignment-list x)))))

(defun pt-implicit-style (document pt) 
  (let ((res 
         (case (sgml:gi pt)
           ((:TD :TH)                 (implicit-style-TD/TH document pt))
           ((:TR)                     (implicit-style/TR document pt))
           ((:HR)                     (implicit-style/HR pt))
           ((:IMG)                    (implicit-style/IMG pt))
           ((:UL)                     (implicit-style/UL pt))
           ((:OL)                     (implicit-style/OL pt))
           ((:LI)                     (implicit-style/LI pt))
           ((:TABLE)                  (implicit-style/TABLE pt))
           ((:BR)                     (implicit-style/BR pt))
           ((:FONT)                   (implicit-style/FONT pt))
           ((:BODY)                   (implicit-style/BODY document pt))
           ((:THEAD :TBODY :TFOOT)    (implicit-style/TBODY pt))
           ((:H1 :H2 :H3 :H4 :H5 :H6) (implicit-style/Hn pt))
           ((:A)                      (implicit-style/A pt))
           ((:INPUT)                  (implicit-style/INPUT pt))
           ((:P :DIV)                 (implicit-style/para pt))) ))
    (nreverse res)))

(defun implicit-style/A (pt)
  (let ((res nil) x)  
    (when (and (pt-attr/latin1 pt :href)
               (setq x (pt-body-element pt))
               (setq x (pt-attr/color x :link)))
      (push (cons 'css:@color x) res))
    res))

(defun implicit-style/para (pt)
  (let ((res nil) x)
    (when (setf x (pt-attr/align pt))
      (push (cons 'css:@text-align x) res))
    res))

(defun implicit-style/INPUT (pt)
  (let ((res nil))
    (when (eq :hidden (pt-attr/input-type pt :type))
      (push (cons 'css:@display :none) res))
    (when (eq :image (pt-attr/input-type pt :type))
      ;; NNN - netscape 4 doesn't do this.
      (when (pt-attr/pixels pt :size)
        (push (cons 'css:@width (cons :px (pt-attr/pixels pt :size))) res)))
    res))
  
(defun implicit-style/TBODY (pt)
  (let ((res nil) x)
    ;; Note: align=justify is in the HTML spec but utter nonsense
    (when (member (setq x (pt-attr/cell-halign pt :align))
                  '(:left :center :right :justify))
      (push (cons 'css:@text-align x) res))
    res))

(defun implicit-style/Hn (pt)
  (let ((res nil) x)
    (when (setf x (pt-attr/align pt))
      (push (cons 'css:@text-align x) res))
    res))

(defun implicit-style/BODY (document pt)
  (let ((res nil) x)
    (when (pt-attr/latin1 pt :background)
      (push (cons 'css:@background-image (pt-effective-url-attr document pt :background)) res))
    (when (setq x (pt-attr/color pt :bgcolor))
      (push (cons 'css:@background-color x) res))
    (when (setq x (pt-attr/color pt :text))
      (push (cons 'css:@color x) res))
    res))

(defun implicit-style/BR (pt)
  (let ((res nil) x)
    (case (setq x (pt-attr/clear pt))
      ((:left :right)
       (push (cons 'css:@clear x) res))
      ((:all)
       (push (cons 'css:@clear :both) res)))
    res))

(defun implicit-style/TABLE (pt)
  (let ((res nil) x)
    (when (eq (pt-attr/table.align pt :align nil) :left) 
      (push (cons 'css:@float :left) res))
    (when (eq (pt-attr/table.align pt :align nil) :right) 
      (push (cons 'css:@float :right) res))
    '(print `((css:style-attr (sgml:pt-parent pt) 'css:@text-align) 
             = ,(css:style-attr (sgml:pt-parent pt) 'css:@text-align)))
    (when (eq (css:style-attr (sgml:pt-parent pt) 'css:@text-align)
              :right)
      (push (cons 'css:@margin-left :auto) res)
      (push (cons 'css:@margin-right 0) res))
    (when (or (eq (pt-attr/table.align pt :align nil) :center) 
              (eq (css:style-attr (sgml:pt-parent pt) 'css:@text-align)
                  :center)
              )
      (push (cons 'css:@margin-left :auto) res)
      (push (cons 'css:@margin-right :auto) res))
    (when (setq x (pt-attr/length pt :width nil))
      '(when (css:percentage-p x)
        (setf x (cons :canvas-h-percentage (cdr x))))
      (when (css:percentage-p x)
        (setf x (cons :% (cdr x))))
      (push (cons 'css:@width x) res))
    (when (setq x (pt-attr/color pt :bgcolor))
      (push (cons 'css:@background-color x) res))
    res))

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

(defun implicit-style-TD/TH (document pt)
  (let ((res nil) x)
    (when (member (setq x (or (pt-attr/cell-halign pt :align nil)
                              (pt-attr/cell-halign (sgml:pt-parent pt) :align)))
                        '(:left :center :right :justify))
      (push (cons 'css:@text-align x) res))
    (when (pt-attr/color (sgml:pt-parent pt) :bgcolor)
      (push (cons 'css:@background-color (pt-attr/color (sgml:pt-parent pt) :bgcolor)) res))
    (when (pt-attr/color pt :bgcolor)
      (push (cons 'css:@background-color (pt-attr/color pt :bgcolor)) res))
    (when (pt-attr/boolean-flag pt :nowrap)
      (progn ;;unless (pt-attr* pt :width)      ;emulate netscape behaviour.
        (push (cons 'css:@white-space :nowrap) res)))
    (when (pt-attr/latin1 pt :background)
      (push (cons 'css:@background-image (pt-effective-url-attr document pt :background)) res))
    (let ((table (sgml:pt-parent (sgml:pt-parent (sgml:pt-parent pt)))))
      (when (and (eq (sgml:gi table) :table))
        (when (setq x (pt-attr/length table :cellpadding))
          (push (cons 'css:@padding-top x) res)
          (push (cons 'css:@padding-bottom x) res)
          (push (cons 'css:@padding-left x) res)
          (push (cons 'css:@padding-right x) res))))
    res))

(defun implicit-style/TR (document pt)
  ;; seems to be never called!
  (let ((res nil) x)
    (when (member (setq x (pt-attr/cell-halign pt :align)) '(:left :center :right :justify))
      (push (cons 'css:@text-align x) res))
    res))

(defun implicit-style/HR (pt)
  (let ((res nil))
    (let ((noshade (pt-attr/boolean-flag pt :noshade))
          (size  (pt-attr/pixels pt :size))
          (width (pt-attr/length pt :width))
          (align (pt-attr/align pt :align)))
      (when noshade
        (push (cons 'css:@border-top-style :solid) res)
        (push (cons 'css:@border-right-style :solid) res)
        (push (cons 'css:@border-bottom-style :solid) res)
        (push (cons 'css:@border-left-style :solid) res))
      (when width
        (push (cons 'css:@width width) res))
      (when size
        (push (cons 'css:@border-top-width (cons :px (floor size 2))) res)        
        (push (cons 'css:@border-bottom-width (cons :px (ceiling size 2))) res))
      (case align
        (:left 
         (push (cons 'css:@margin-left '(:px . 0)) res)
         (push (cons 'css:@margin-right :auto) res))
        (:right 
         (push (cons 'css:@margin-left :auto) res)
         (push (cons 'css:@margin-right '(:px . 0)) res))
        (:center 
         (push (cons 'css:@margin-left :auto) res)
         (push (cons 'css:@margin-right :auto) res)))
      res)))

(defun implicit-style/FONT (pt)
  (let ((res nil) sz)
    (when (pt-attr/color pt :color)
      (push (cons 'css:@color (pt-attr/color pt :color)) res))
    (when (pt-attr/latin1 pt :face)
      (let ((f (split-by-if (lambda (x)
                              (member x (list #\, #\space #\tab #\newline #\return) 
                                      :test #'char=))
                            (pt-attr/latin1 pt :face) :nuke-empty-p t)))
        (push (cons 'css:@font-family f) res)))
    (when (setq sz (maybe-parse-integer (pt-attr/latin1 pt :size)))
      (cond ((or (char= (char (pt-attr/latin1 pt :size) 0) #\+)
                 (char= (char (pt-attr/latin1 pt :size) 0) #\-))
             ;;(push (cons 'css:@font-size (cons :% (* 100 (expt 7/6 sz)))) res)
             (push (cons 'css:@font-size (cons :px (* 14 (expt 7/6 sz)))) res))
            (t
             (push (cons 'css:@font-size
                         (cond ((<= sz 0) (cons :pt 6))
                               ((= sz 1) (cons :pt 8))
                               ((= sz 2) (cons :pt 10))
                               ((= sz 3) (cons :pt 12))
                               ((= sz 4) (cons :pt 14))
                               ((= sz 5) (cons :pt 16))
                               ((= sz 6) (cons :pt 18))
                               ((>= sz 7) (cons :pt 20))))
                   res)) ))
    res))

(defun implicit-style/UL (pt)
  (let ((res nil) x)
    (when (setq x (pt-attr/list-style-type pt :type))
      (push (cons 'css:@list-style-type x)
            res))
    (when (pt-attr/boolean-flag pt :COMPACT)
      (push (cons 'css:@list-style-position :inset) res))
    res))

(defun implicit-style/OL (pt)
  (let ((res nil) x)
    (when (setq x (pt-attr/list-style-type pt :type))
      (push (cons 'css:@list-style-type x) 
            res))
    (when (pt-attr/boolean-flag pt :COMPACT)
      (push (cons 'css:@list-style-position :inside) res))
    res))

(defun implicit-style/LI (pt)
  (let ((res nil) x)
    (when (setq x (pt-attr/list-style-type pt :type))
      (push (cons 'css:@list-style-type x) 
            res))
    res))

;;;

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
         (if (eq (sgml:gi (sgml:pt-parent pt)) :UL) :disc :decimal)) ))

;;;; ------------------------------------------------------------------------------------------

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

;;;; ------------------------------------------------------------------------------------------

#||

;; Ja was hier wirklich passiert ist, dass wir on the fly
;; modificationen am parse tree vornehmen. Sollte aber kein Problem
;; sein, da auf pt-parent erstaunlich wenig zugeriffen wird. Da wir
;; immer noch nicht nicht-incrementell rendern koennen wir genauso gut
;; auch einen einfachen pre-processor vorschalten. (insb. auch fuer
;; OBJECT gut).

;; Liste aller HTML elemente
;; A          anchor
;; ABBR       abbreviated form (e.g., WWW, HTTP, etc.)
;; ACRONYM    
;; ADDRESS    information on author
;; APPLET     Java applet
;; AREA       client-side image map area
;; B          bold text style
;; BASE       document base URI
;; BASEFONT   base font size
;; BDO        I18N BiDi over-ride
;; BIG        large text style
;; BLOCKQUOTE long quotation
;; BODY       document body
;; BR         forced line break
;; BUTTON     push button
;; CAPTION    table caption
;; CENTER     shorthand for DIV align=center
;; CITE       citation
;; CODE       computer code fragment
;; COL        table column
;; COLGROUP   table column group
;; DD         definition description
;; DEL        deleted text
;; DFN        instance definition
;; DIR        directory list
;; DIV        generic language/style container
;; DL         definition list
;; DT         definition term
;; EM         emphasis
;; FIELDSET   form control group
;; FONT       local change to font
;; FORM       interactive form
;; FRAME      subwindow
;; FRAMESET   window subdivision
;; H1         heading
;; H2         heading
;; H3         heading
;; H4         heading
;; H5         heading
;; H6         heading
;; HEAD       document head
;; HR         horizontal rule
;; HTML       document root element
;; I          italic text style
;; IFRAME     inline subwindow
;; IMG        Embedded image
;; INPUT      form control
;; INS        inserted text
;; ISINDEX    single line prompt
;; KBD        text to be entered by the user
;; LABEL      form field label text
;; LEGEND     fieldset legend
;; LI         list item
;; LINK       a media-independent link
;; MAP        client-side image map
;; MENU       menu list
;; META       generic metainformation
;; NOFRAMES   alternate content container for non frame-based rendering
;; NOSCRIPT   alternate content container for non script-based rendering
;; OBJECT     generic embedded object
;; OL         ordered list
;; OPTGROUP   option group
;; OPTION     selectable choice
;; P          paragraph
;; PARAM      named property value
;; PRE        preformatted text
;; Q          short inline quotation
;; S          strike-through text style
;; SAMP       sample program output, scripts, etc.
;; SCRIPT     script statements
;; SELECT     option selector
;; SMALL      small text style
;; SPAN       generic language/style container
;; STRIKE     strike-through text
;; STRONG     strong emphasis
;; STYLE      style info
;; SUB        subscript
;; SUP        superscript
;; TABLE      table
;; TBODY      table body
;; TD         table data cell
;; TEXTAREA   multi-line text field
;; TFOOT      table footer
;; TH         table header cell
;; THEAD      table header
;; TITLE      document title
;; TR         table row
;; TT         teletype or monospaced text style
;; U          underlined text style
;; UL         unordered list
;; VAR        instance of a variable or program argument


;; Liste aller HTML elemente, die nicht vollstaendig durch CSS abgedeckt werden

;;   A          anchor
;;   BUTTON     push button

;;   FRAME      subwindow
;;   FRAMESET   window subdivision
;;   NOFRAMES   alternate content container for non frame-based rendering

;; Meta Information

;; Container fuer zusaetzliche information [aber ansonsten normal renderbar]
;;   BASE       document base URI
;;   BASEFONT   base font size
;;   LINK       a media-independent link
;;   STYLE      style info
;;   TITLE      document title
;;   HEAD       document head
;;   META       generic metainformation
;;   MAP        client-side image map
;;   FORM       interactive form
;;   AREA       client-side image map area
;;   SCRIPT     script statements
;;   NOSCRIPT   alternate content container for non script-based rendering
;;   PARAM      named property value

;; als einfache replaced elemente handhabbar: [ggf. mit einfachen textuellen fallback].
;;   IMG        Embedded image
;;   INPUT      form control
;;   OBJECT     generic embedded object
;;   TEXTAREA   multi-line text field
;;   SELECT     option selector
;;   OPTION     selectable choice
;;   OPTGROUP   option group
;;   IFRAME     inline subwindow
;;   APPLET     Java applet

;; Tabellen:
;;   TABLE      table
;;   CAPTION    table caption
;;   COL        table column
;;   COLGROUP   table column group
;;   TBODY      table body
;;   TFOOT      table footer
;;   THEAD      table header
;;   TD         table data cell
;;   TH         table header cell
;;   TR         table row

;; zunaechst ignorierbar
;;   FIELDSET   form control group
;;   ISINDEX    single line prompt
;;   LABEL      form field label text
;;   LEGEND     fieldset legend
;;   BDO        I18N BiDi over-ride

;; Zu sammelne Informationen:
;;  o LINKs
;;  o BASE
;;  o STYLE
;;  o META
;;  o TITLE
;;

(defclass link ()
  ((type :initarg :type :reader link-type)      ;link type as rod
   (href :initarg :href :reader link-href)))    ;URI

(defclass rev-link (link)
  ())

(defclass rel-link (link)
  ())

(defclass document ()
  ((url)                                ;URL of document
   (base-url)                           ;base URL for this document
   (parse-tree)                         ;parse tree
   (style-sheet)                        ;style sheet
   (title)                              ;if any
   ;; dann irgendwie ein cache fuer replaced elemente
   ;; der aber durch aus zwei-stufig ist, denn Bilder werden
   ;; z.B. applicationsweit gecached. Bei anderen replaced elementen ist
   ;; documentenweises cachen wichtig. (Eingabefelder oder IFRAMEs).

   ;; evlt. koenne wir auch Framesets ganz "normal" rendern.
   
   ))

||#

;;;;
(defun find-html-head (pt)
  (dolist (k (sgml:pt-children (sgml:pt-root pt)))
    (cond ((member (sgml:pt-name k) '(:HEAD))
           (return k)))))

(defun pt-map-over-link-nodes (fn pt)
  "Map the function `fn' over all <LINK> nodes in the `pt' document's header."
  (let ((head (find-html-head pt)))
    (and head
	 (dolist (k (sgml:pt-children head))
	   (when (eq (sgml:pt-name k) :LINK)
	     (funcall fn k))))))

(defun the-style-node (pt)
  "Given a parse tree, find the <STYLE> node in its header, if any."
  (let ((head (find-html-head pt)))
    (and head
	 (dolist (k (sgml:pt-children head))
	   (when (eq (sgml:pt-name k) :STYLE)
	     (return k))))))

;; Grff...

(defparameter *style-sheet-cache*
    (make-hash-table :test #'equalp))

(defparameter *style-sheet-cache*/lock
    (mp/make-lock :name "*style-sheet-cache*"))

(defun maybe-parse-style-sheet-from-url (url &key (name "anonymous") 
                                                  (supersheet nil)
                                                  (media-type :all))
  (mp/with-lock (*style-sheet-cache*/lock)
    (multiple-value-bind (looked presentp) (gethash url *style-sheet-cache*)
      (cond (presentp
             (format T "~&;; Serving style sheet ~S [at ~S] from cache.~%"
                     name url)
             looked)
            (t
             (format T "~&;; fetching and parsing style sheet ~S [at ~S].~%"
                     name url)
             (let ((res (maybe-parse-style-sheet-from-url-aux
                         url 
                         :name name 
                         :supersheet supersheet
                         :media-type media-type)))
               (setf (gethash url *style-sheet-cache*) res)
               res))))))

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

(defun document-style-sheet (doc &key (selected-style :default))
  "Compute the documents style. This could be either a <STYLE> element
  in the header or an external style sheet defined via <LINK>."
  ;; It isn't exactly specified, what one should do, when multiple
  ;; STYLE nodes are present.
  ;; We take the route to parse all styles present by either LINK or
  ;; STYLE and combine, as if they occured via @import.
  (let ((sheets nil)
        (pt (document-pt doc)))

    (setq user::pt pt)
    
    ;;
    (dolist (link (document-links doc))
      (when (and (style-sheet-link-p link)
                 (style-link-does-apply-p link selected-style)
                 (link-href link))
        (describe link)
        (let* ((media-type 
                (or (ignore-errors
                     (css::parse-media-type (link-media link)))
                    :all))
               (href (link-href link))
               (sheet (maybe-parse-style-sheet-from-url 
                       href
                       :name "Document style via LINK"
                       :media-type media-type)))
          (when sheet
            (push sheet sheets)))))
    ;;
    (let ((style (the-style-node pt)))
      (when style
        (multiple-value-bind (res condition)
            (ignore-errors
             (css:parse-style-sheet (cl-char-stream->gstream
                                     (make-string-input-stream (pt-all-data style)))
                                    nil
                                    :name "Document Style via STYLE"
                                    :base-url (document-base-url doc)))
          (cond ((null res)
                 (warn "Error while parsing embedded style sheet in ~S:~% ~A"
                       (r2::document-location doc)
                       condition))
                (t
                 (push res sheets))))))
    (setf sheets (nreverse sheets))
    (let ((s (css:create-style-sheet *default-style-sheet*
                                     :name "Document style"
                                     :base-url (document-base-url doc))))
      (setf (css::style-sheet-imported-sheets s)
        sheets)
      s)))

;;; big grrf!
;; * html-4.0.zip/struct/links.html#adef-rel:
;; | rel = cdata 
;; |       This attribute describes the link from the current document
;; |       to the anchor specified by the href attribute. This value
;; |       of this attribute is one or more link types separated by
;; |       white space characters.

(defun style-link-does-apply-p (link select-style)
  (cond ((set-equal (link-rel link) '("stylesheet") :test #'string-equal)
         (or
          (null (link-title link))      ;persistent
          (eq select-style :default)))  ;selected
        ((set-equal (link-rel link) '("alternate" "stylesheet") :test #'string-equal)
         (or
          (null (link-title link))      ;persistent
          (string-equal (link-title link) select-style))) ))

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


;;;;

;;; REPLACED-ELEMENT-P is used by:
;;;   render-normal-block
;;;   render-inline
;;;   pt-minimum-width/block
;;;   pt-minimum-width/line
;;;   pt-maximum-width/block
;;;   pt-maximum-width/line
;;;   resolve-width/1

(defun replaced-element-p (document device elm)
  (values-list
   (cond ((member (sgml:gi elm) '(:IMG))
          ;;xxx (member (sgml:gi elm) '(:form :button :input :select :textarea :img))
          (cond ((eq (pt-%replacement elm :unset) :unset)
                 (multiple-value-bind (robj action-map) (build-replaced-element document device elm)
                   (setf (pt-%replacement elm) (list robj action-map) )))
                (t
                 (pt-%replacement elm))))
         (t
          nil))))

(defun build-replaced-element (document device elm)
  (case (sgml:gi elm)
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
  (case (sgml:gi elm)
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
      :children   (remove nil (mapcar #'parse-options (sgml:pt-children elm)))))
    (otherwise 
     (warn "The ~A element is not allowed with <SELECT>." (sgml:gi elm))
     nil)))

(defun make-option-menu-replacement (device elm)
  (gui::make-option-menu
   device 
   :pt         elm
   :options    (remove nil (mapcar #'parse-options (sgml:pt-children elm)))
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
  (pt-attr/enum pt slot default
                '(:top :middle :bottom :baseline)))

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
  (do ((q img-element (sgml:pt-parent q)))
      ((null q))
    (when (and (eq (sgml:gi q) :a)
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
           (setf (sgml:pt-children r)
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
  (assert (member (sgml:gi elm) '(:AREA :A)))
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
  (assert (eq (sgml:gi node) :MAP))
  (let ((res nil))
    (sgml:map-pt (lambda (x)
                   (when (member (sgml:gi x) '(:A :AREA))
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
                 (when (and (eq (sgml:gi node) :map)
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

(defmethod area-contains-point-p ((area imap-polygon) px py w h)
  (let ((new-point-seq nil))
    ;; resolve all coordinates and put then into points as list of conses
    (with-slots (point-seq) area
      (do ((q point-seq (cddr q)))
          ((or (endp q) (endp (cdr q))))
        (push (area-resolve-coordinate (cadr q) h) new-point-seq)
        (push (area-resolve-coordinate (car q) w) new-point-seq)))
    ;; the hard work is done in GU
    (gu:polygon-contains-point-p new-point-seq px py)))

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


;;;;

(defun handle-button (device pt)
  (let ((fake-pt (sgml::make-pt/low :name :div
                                    :parent pt ;;(sgml:pt-parent pt)
                                    :children (sgml:pt-children pt))))
    ;;(setf (sgml:pt-cache fake-pt) (sgml:pt-cache pt))
    ;;(setf (svref (sgml:pt-cache fake-pt) css:@display) :block)
    '(print (css:style-attr pt 'css:@display))
    (let ((max (pt-maximum-width *rcontext* fake-pt)))
      (incf max 0)
      ;;(print `(min = ,min max = ,max))
      (let ((w (ceiling (min max 600))))          ;arbitrary alert!
        '(print `(w = ,w))
        (let* ((new-rc (copy-rc *rcontext*))
               (*rcontext* new-rc))
          (setf (rc-y new-rc) 4
                (rc-x0 new-rc) 4
                (rc-x1 new-rc) (+ w 4)
                (rc-vertical-margins new-rc) nil
                (rc-vertical-margin-callbacks new-rc) nil
                (rc-left-floating-boxen new-rc) nil
                (rc-right-floating-boxen new-rc) nil)
          (let ((fake (make-bbox)))
            (let ((bbox (brender new-rc fake-pt fake)))
              (flush-vertical-margin new-rc)
              (make-instance 'clue-gui2::ro/drawn-button
                :width          (+ w 8)
                :height         (+ (rc-y new-rc) 4)
                :bbox           bbox
                :pt             pt
                :device         device
                :name           (pt-attr* pt :name)
                :initial-value  (pt-attr* pt :value (rod ""))
                :type           (pt-attr/enum pt :type :submit '(:submit :reset :button))
                :disabled-p     (pt-attr/boolean-flag pt :disabled) ))))))))

;;;;;;;;;;



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

(defmethod ro/resize ((self ro/image) w h)
  (with-slots (aimage url awidth aheight iwidth iheight) self
    (setf awidth (or w iwidth)
          aheight (or h iheight))
    (when aimage
      ;; **hack**
      (let ((drawable (xlib:screen-root (xlib:display-default-screen clue-gui2::*dpy*))))
        (with-slots (pixmap mask) self
          (unless pixmap
            (let ((q (clue-gui2::make-pixmap-from-aimage drawable aimage awidth aheight)))
              (setf pixmap (car q)
                    mask   (cadr q)))))))
    ))



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

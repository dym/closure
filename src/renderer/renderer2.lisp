(in-package :r2)

;;;; --------------------------------------------------------------------------------

;;;; Upon Incremental Changes

;; When something via :hover or DHTML changes, we can do it the
;; following way:
;;
;;   first find the set of all elements affected. that are all
;;   elements directly affected plus possible other elements affected
;;   because of geometry of things changed.
;;
;;   we need to keep a kind of cache of the rendering state for each
;;   element or more precisely some sort of callback which will
;;   rerender a given element.
;;

;; We can center all this around paragraphs again. A to be rendered
;; block box maps into a series of chunks, these chunks then map into
;; a series of lines. These lines are then drawn. A certain change of
;; an CSS rule results into possible changes at different levels and
;; different information can be retained.

;; The correct approach to this would be (and we once did that) to
;; formulate rendering in a functional fashion and use pretty vanilla
;; memo techniques. The hard part is to determine the exact set of CSS
;; properties which affect a certain stage of rendering.

;; We can start low and try to cache on a paragraph per paragraph
;; basis. => Each paragraph will be another output record.

;; When we keep a cache which maps block elements to their surrounding
;; rc, a mere A:hover we can handled by just finding the nearest
;; parent box element and just rerender that.

;; Or put the otherway round: Each element produces some output, this
;; output is retained in the output history together with the
;; appropriate rcontext object. Unless there is something in it which
;; changed or the rcontext changed we can keep the output history.

;; That is we build a hierachy of output records each containing:
;;  a. A description of the surrounding state
;;  b. A closure callable to regenerate the output.

;; This implies that we generally should change state as less as
;; possible and that using iteration is "interesting".

(import '(CSS::COOKED-STYLE-BORDER-TOP-STYLE
          CSS::COOKED-STYLE-PADDING-LEFT
          CSS::COOKED-STYLE-TEXT-TRANSFORM
          CSS::COOKED-STYLE-MARGIN-BOTTOM
          CSS::COOKED-STYLE-FONT-VARIANT
          CSS::COOKED-STYLE-POSITION
          CSS::COOKED-STYLE-LINE-HEIGHT
          CSS::COOKED-STYLE-FONT-WEIGHT
          CSS::COOKED-STYLE-CONTENT
          CSS::COOKED-STYLE-MARGIN-TOP
          CSS::COOKED-STYLE-FLOAT
          CSS::COOKED-STYLE-BACKGROUND-ATTACHMENT
          CSS::COOKED-STYLE-LEFT
          CSS::COOKED-STYLE-BACKGROUND-POSITION
          CSS::COOKED-STYLE-BACKGROUND-IMAGE
          CSS::COOKED-STYLE-BORDER-BOTTOM-WIDTH
          CSS::COOKED-STYLE-BACKGROUND-REPEAT
          CSS::COOKED-STYLE-HEIGHT
          CSS::COOKED-STYLE-VERTICAL-ALIGN
          CSS::COOKED-STYLE-COUNTER-INCREMENT
          CSS::COOKED-STYLE-MARGIN-LEFT
          CSS::COOKED-STYLE-TEXT-DECORATION
          CSS::COOKED-STYLE-MARGIN-RIGHT
          CSS::COOKED-STYLE-BORDER-BOTTOM-STYLE
          CSS::COOKED-STYLE-BORDER-RIGHT-WIDTH
          CSS::COOKED-STYLE-FONT-FAMILY
          CSS::COOKED-STYLE-MARKER-OFFSET
          CSS::COOKED-STYLE-LETTER-SPACING
          CSS::COOKED-STYLE-BORDER-LEFT-COLOR
          CSS::COOKED-STYLE-BORDER-TOP-WIDTH
          CSS::COOKED-STYLE-BORDER-LEFT-STYLE
          CSS::COOKED-STYLE-CLEAR
          CSS::COOKED-STYLE-QUOTES
          CSS::COOKED-STYLE-DISPLAY
          CSS::COOKED-STYLE-LIST-STYLE-IMAGE
          CSS::COOKED-STYLE-ORIG-WIDTH
          CSS::COOKED-STYLE-COLOR
          CSS::COOKED-STYLE-BORDER-BOTTOM-COLOR
          CSS::COOKED-STYLE-WHITE-SPACE
          CSS::COOKED-STYLE-BORDER-RIGHT-COLOR
          CSS::COOKED-STYLE-PADDING-TOP
          CSS::COOKED-STYLE-FONT-SIZE
          CSS::COOKED-STYLE-BACKGROUND-COLOR
          CSS::COOKED-STYLE-BORDER-LEFT-WIDTH
          CSS::COOKED-STYLE-LIST-STYLE-TYPE
          CSS::COOKED-STYLE-WORD-SPACING
          CSS::COOKED-STYLE-BOTTOM
          CSS::COOKED-STYLE-OVERFLOW
          CSS::COOKED-STYLE-PADDING-BOTTOM
          CSS::COOKED-STYLE-FONT-STYLE
          CSS::COOKED-STYLE-CLIP
          CSS::COOKED-STYLE-BORDER-RIGHT-STYLE
          CSS::COOKED-STYLE-TEXT-INDENT
          CSS::COOKED-STYLE-WIDTH
          CSS::COOKED-STYLE-TOP
          CSS::COOKED-STYLE-Z-INDEX
          CSS::COOKED-STYLE-BORDER-TOP-COLOR
          CSS::COOKED-STYLE-RIGHT
          CSS::COOKED-STYLE-LIST-STYLE-POSITION
          CSS::COOKED-STYLE-TEXT-ALIGN
          CSS::COOKED-STYLE-PADDING-RIGHT
          CSS::COOKED-STYLE-COUNTER-RESET
          CSS::COOKED-STYLE-BORDER-SPACING))

;;;;

(defparameter *hyphenate-p* nil
  "Whether we hyphenate.")

(defparameter *tex-mode-p* nil)

(defvar *canvas-width*)
(defvar *document-height* nil
  "XXX variable to pass the document height back to the GUI; mostly because OR is broken.")

;;;;

(defmacro defclass* (name super &rest slots)
  (cond ((member name '(black-chunk))
         `(progn
           (defstruct (,name (:constructor
                              ,(intern (format nil "CONS-~A" name))
                              (&key ,@(mapcar (lambda (slot)
                                                (let ((slot (if (consp slot) (car slot) slot)))
                                                  slot))
                                              slots))))
             ,@(mapcar (lambda (slot)
                        (let ((slot (if (consp slot) (car slot) slot))
                              (opts (if (consp slot) (cdr slot) nil)))
                          (list slot
                                (getf (if (consp slot) (cdr slot) nil) :initform))))
                      slots))
           ;;
           ))
        (t
         `(progn
           (defclass ,name ,super
             ,(mapcar (lambda (slot)
                        (let ((slot (if (consp slot) (car slot) slot))
                              (opts (if (consp slot) (cdr slot) nil)))
                          (cons slot
                                (append opts
                                        (list
                                         :initarg (intern (symbol-name slot) :keyword)
                                         :initform nil
                                         :accessor (intern (format nil "~A-~A" name slot)))))))
                      slots))
           ;;
           (defun ,(intern (format nil "CONS-~A" name))
               (&rest args)
             (apply #'make-instance ',name args))
           ;;
           (defun ,(intern (format nil "~A-P" name))
               (object)
             (typep object ',name))
           ;;
           (defun ,(intern (format nil "~A-MODIF" name))
               (.object. &key ,@(mapcar (lambda (slot)
                                          (let ((slot (if (consp slot) (car slot) slot)))
                                            (list slot nil (intern (format nil ".P.~A" slot)))))
                                        slots))
             (make-instance ',name
                            ,@(mapcan (lambda (slot)
                                        (let ((slot (if (consp slot) (car slot) slot)))
                                          (list (intern (symbol-name slot) :keyword)
                                                `(if ,(intern (format nil ".P.~A" slot))
                                                  ,slot
                                                  (slot-value .object. ',slot)))))
                                      slots))))) ))


(defvar *device*)
(defvar *document*)
(defvar *style-sheet*)

(defvar *baseline* nil
  "Kludge: When a line is actually rendered this variable unless
already set is set to the device y coordinate of the baseline. This is
used by the table renderer.")

(defclass* block-box ()
  ;; a block box
  style
  content               ;a list of other block, para, or marker boxen
  element
  content-output-record                 ;output record of content
  decoration-output-record              ;output record of decoration
  output-record)                        ;overall output record

(defclass* para-box ()
  items                 ;the main content of this paragraph
                        ; (a list of chunks)
  output-record
  genesis
  )

(defclass* bounding-chunk ()
  ;; common superclass for open/close chunks
  halfp                                 ;whether this is a half one
  style                                 ;its style
  pt)                                   ;its element

(defclass* open-chunk (bounding-chunk)
  height
  depth
  dy)

(defclass* close-chunk (bounding-chunk)
  )

(defclass* black-chunk ()
  style
  data
  %width)

(defclass* replaced-object-chunk ()
  element
  object)

(defclass* disc-chunk ()
  %before
  %after
  %here
  forcep)               ;whether this is a forced line break
                        ; (in which case 'here' may not apply)

(defmethod print-object ((object black-chunk) stream)
  (format stream "#{~S}" (rod-string (black-chunk-data object))))

(defmethod print-object ((object bounding-chunk) stream)
  (format stream "#{~A}" (chunk-debug-name object)))

(defmethod print-object ((object disc-chunk) stream)
  (format stream "#{disk:~I~_here=~S ~_before=~S ~_after=~S}"
          (disc-chunk-%here object)
          (disc-chunk-%before object)
          (disc-chunk-%after object)))

(defmethod print-object ((object block-box) stream)
  (format stream "#<~:2I~S ~S~{ ~_~S~}>"
          (type-of object)
          (element-gi (block-box-element object))
          (block-box-content object) ))

(defclass* kern-chunk ()
  amount)

(defclass* text-indent-chunk (kern-chunk)
  amount)

(defun text-indent-chunk-p (object)
  (typep object 'text-indent-chunk))

(defun kern-chunk-p (x)
  (typep x 'kern-chunk))

(defmethod chunk-width ((x kern-chunk))
  (slot-value x 'amount))

(defun make-kern-chunk (amount)
  (make-instance 'kern-chunk :amount amount))

(defun make-text-indent-chunk (amount)
  (make-instance 'text-indent-chunk :amount amount))

(defmethod disc-chunk-before (chunk)
  (let ((b (disc-chunk-%before chunk)))
    (if (functionp b)
        (setf (disc-chunk-%before chunk) (funcall b))
        b)))

(defmethod disc-chunk-after (chunk)
  (let ((b (disc-chunk-%after chunk)))
    (if (functionp b)
        (setf (disc-chunk-%after chunk) (funcall b))
        b)))

(defmethod disc-chunk-here (chunk)
  (let ((b (disc-chunk-%here chunk)))
    (if (functionp b)
        (setf (disc-chunk-%here chunk) (funcall b))
        b)))

(defmethod chunk-width ((chunk open-chunk))
  (if (bounding-chunk-halfp chunk)
      0
      (let ((style (bounding-chunk-style chunk)))
        (+ (cooked-style-border-left-width style)
           (cooked-style-padding-left style)
           (cooked-style-margin-left style)))))

(defmethod chunk-width ((chunk close-chunk))
  (if (bounding-chunk-halfp chunk)
      0
      (let ((style (bounding-chunk-style chunk)))
        (+ (cooked-style-border-right-width style)
           (cooked-style-padding-right style)
           (cooked-style-margin-right style)))))

(defmethod chunk-width ((chunk black-chunk))
  (declare (type black-chunk chunk))
  (or (black-chunk-%width chunk)
      (setf (black-chunk-%width chunk)
            (let* ((style (black-chunk-style chunk))
                   (fn (text-style-font (make-text-style-from-cooked-style style))))
              (loop for c across (black-chunk-data chunk)
                    sum (rune-width fn c)) ))))

(defmethod chunk-width ((chunk replaced-object-chunk))
  (ro/size (replaced-object-chunk-object chunk)))

;;;; Rendering (or dumping) a single line.

(defclass* line-fragment ()
  x1 x2                                 ;left and right margin
  block-style                           ;the style of the corresponding block
  chunks)                               ;their chunks

(defparameter *debug-tags* nil)

(defun chunk-debug-name (chunk)
  (format nil (if (bounding-chunk-halfp chunk)
                  "~(<~A~A>~)~A"
                  "<~A~A>~A")
          (if (typep chunk 'close-chunk) "/" "")
          (element-gi (bounding-chunk-pt chunk))
          (typecase (bounding-chunk-pt chunk)
            (before-pseudo-element ":before")
            (after-pseudo-element ":after")
            (first-line-pseudo-element ":first-line")
            (first-letter-pseudo-element ":first-letter")
            (t ""))))

(defclass link-presentation (CLIM:STANDARD-PRESENTATION)
  ())

#+NIL
(defmethod clim:highlight-output-record ((record link-presentation)
                                         stream
                                         mode)
  (let ((object (clim:presentation-object record)))
    ;;(tata mode)
    ))

(defun render-line (y fragments &aux (*package* (find-package "RENDERER")))
  ;; The very first thing we need to do is to care for vertical align.
  (multiple-value-bind (height depth) (vertically-align-line fragments)
    (incf y height)
    (unless *baseline* (setf *baseline* y)) ;this is for the table renderer
    ;; and yet another time traversing stuff.
    (dotimes (pass 2)
      (dolist (frag fragments)
        (let ((ss (list (line-fragment-block-style frag)))
              (x  (case (cooked-style-text-align (line-fragment-block-style frag))
                    ;; ### is this correct?
                    (:right
                     (- (line-fragment-x2 frag)
                        (reduce #'+ (mapcar #'chunk-width (line-fragment-chunks frag)))))
                    (:center
                     (/ (+ (line-fragment-x1 frag) (line-fragment-x2 frag)
                           (- (reduce #'+ (mapcar #'chunk-width (line-fragment-chunks frag)))))
                        2))
                    (otherwise
                     (line-fragment-x1 frag))))
              (ys nil)
              (dy 0))
          (labels ((walk-aux (chunks &aux text-seen-p);; -> cont
                     (do ()
                         ((null chunks)
                          (values nil nil text-seen-p))
                       (let ((chunk (pop chunks)))
                         (typecase chunk
                           (close-chunk
                            (return (values chunks chunk text-seen-p)))
                           (open-chunk
                            (let ((oc chunk)
                                  (replaced-object-p (replaced-object-chunk-p (car chunks))))
                              (incf x (chunk-width chunk))
                              (when *debug-tags*
                                (let ((q (format nil "~A~A"
                                                 (chunk-debug-name chunk)

                                                 (format nil "(~D)"
                                                         (open-chunk-dy chunk))
                                                 )))
                                  (when (eql pass 1)
                                    (clim:draw-text* clim-user::*medium* q x y))
                                  (incf x (clim:text-size clim-user::*medium* q))))
                              (push dy ys)
                              (setf dy (open-chunk-dy chunk))
                              (push (bounding-chunk-style chunk) ss)
                          ;;;
                              (let ((link (and (eql pass 1)
                                               (closure-protocol:element-imap closure-protocol:*document-language*
                                                                              *document*
                                                                              (bounding-chunk-pt chunk))))
                                    (x1 x))
                                (when (eql :img (element-gi (bounding-chunk-pt chunk)))
                                  (setf *dyn-elm* (bounding-chunk-pt chunk)))
                                (let (p q res.text-seen-p)
                                  (cond (link
                                         (clim:with-output-as-presentation
                                             (clim-user::*medium*
                                              (url:unparse-url
                                               (hyper-link-url (imap-area-link link)))
                                              'clim-user::url
                                              :record-type 'link-presentation)
                                           (setf (values p q res.text-seen-p)
                                                 (walk chunks))))
                                        (t
                                         (setf (values p q res.text-seen-p)
                                               (walk chunks))))
                                  (setf chunks p)
                                  (setf text-seen-p (or text-seen-p res.text-seen-p))
                                  (when q
                                    (when *debug-tags*
                                      (let ((q
                                             (format nil "~A~A"
                                                     (chunk-debug-name q)
                                                     "")))
                                        (when (eql pass 1)
                                          (clim:draw-text* clim-user::*medium* q x y))
                                        (incf x (clim:text-size clim-user::*medium* q))
                                        )))

                                  (pop ss)
                                  (when (eql pass 0)
                                    (let ((x1 (- x1 (cooked-style-padding-left (bounding-chunk-style oc))))
                                          (x  (+ x (if q (cooked-style-padding-right (bounding-chunk-style q))
                                                       0))))
                                      ;; See comment in VERTICALLY-ALIGN-LINE. Dimensions of
                                      ;; replaced objects are different to dimensions of regular
                                      ;; inline boxen.
                                      (cond (replaced-object-p
                                             (draw-box-decoration clim-user::*medium*
                                                                     x1 (- (+ y dy) (open-chunk-height oc)
                                                                           (cooked-style-padding-top (bounding-chunk-style oc))
                                                                           (- (cooked-style-padding-top (bounding-chunk-style oc)))
                                                                           (- (cooked-style-margin-top (bounding-chunk-style oc)))
                                                                           (- (cooked-style-border-top-width (bounding-chunk-style oc))))
                                                                     x  (+ (+ y dy) (open-chunk-depth oc)
                                                                           (cooked-style-padding-bottom (bounding-chunk-style oc))
                                                                           (- (cooked-style-padding-bottom (bounding-chunk-style oc)))
                                                                           (- (cooked-style-margin-bottom (bounding-chunk-style oc)))
                                                                           (- (cooked-style-border-bottom-width (bounding-chunk-style oc))))
                                                                     (bounding-chunk-style oc)
                                                                     :left-halfp (not (bounding-chunk-halfp oc))
                                                                     :right-halfp (not (bounding-chunk-halfp q))
                                                                     ))
                                            (t
                                             (draw-box-decoration clim-user::*medium*
                                                                     x1 (- (+ y dy) (open-chunk-height oc)
                                                                           (cooked-style-padding-top (bounding-chunk-style oc)))
                                                                     x  (+ (+ y dy) (open-chunk-depth oc)
                                                                           (cooked-style-padding-bottom (bounding-chunk-style oc)))
                                                                     (bounding-chunk-style oc)
                                                                     :left-halfp (not (bounding-chunk-halfp oc))
                                                                     :right-halfp (not (bounding-chunk-halfp q))
                                                                     )))))
                                  (setf dy (pop ys))
                                  ;; ### find out exact coordinates the text-deco should run over
                                  ;;     most probable: between the inner edges.
                                  (when (eql pass 1)
                                    (when res.text-seen-p
                                      (draw-text-decoration x1 (+ y dy) x
                                                            (cooked-style-text-decoration (bounding-chunk-style chunk))
                                                            (cooked-style-color (bounding-chunk-style chunk)))))
                                  (when q
                                    (incf x (chunk-width q)))))))

                           (kern-chunk
                            (incf x (chunk-width chunk)))

                           (black-chunk
                            (setf text-seen-p t)
                            (when (and ss (car ss))
                              (when (eql pass 1)
                                (setf (clim:medium-ink clim-user::*medium*)
                                      (css-color-ink (cooked-style-color (black-chunk-style chunk))))
                                (clim-draw-runes* clim-user::*medium*
                                                 x (+ dy y)
                                                 (black-chunk-data chunk)
                                                 0 (length (black-chunk-data chunk))
                                                 (make-text-style-from-cooked-style (car ss))))
                              (incf x (chunk-width chunk))))

                           (replaced-object-chunk
                            (let ((ro (replaced-object-chunk-object chunk)))
                              (when (eql pass 1)
                                (closure/clim-device::medium-draw-ro*
                                 clim-user::*medium*
                                 ro x (+ dy y)))
                              (incf x (ro/size ro))) )))))
                   ;;
                   (walk (chunks)
                     (walk-aux chunks))
                   )
            #+NIL
            (progn
              (fresh-line)
              (dolist (c (line-fragment-chunks frag))
                (when (typep c 'black-chunk)
                  (princ (map 'string #'code-char (black-chunk-data c))))))

            (let ((x1 x))
              (multiple-value-bind (rest-chunks some-chunk text-seen-p)
                  (walk (line-fragment-chunks frag))
                (when text-seen-p
                  ;; kludge!
                  (when (text-indent-chunk-p (car (line-fragment-chunks frag)))
                    (incf x1 (chunk-width (car (line-fragment-chunks frag)))))
                  (when (eql pass 1)
                    (draw-text-decoration x1 y x
                                          (cooked-style-text-decoration (line-fragment-block-style frag))
                                          (cooked-style-color (line-fragment-block-style frag)))))))
            ))))
      ;;
      (incf y depth) )
    y)

(defun cooked-style-effective-line-height (style)
  (let ((line-height (cooked-style-line-height style)))
    (if (and (consp line-height) (eq (car line-height) '*))
        ;;### what is with rounding here?
        (round (* (cdr line-height)
                  (cooked-style-font-size style)))
      line-height)))

(defun text-height-and-depth (style)
  (let* ((as (cooked-style-font-ascent style))
         (ds (cooked-style-font-descent style))
         (lh (cooked-style-effective-line-height style))
         (hl-1 (floor (- lh (+ as ds)) 2))) ;DEVRND
    (let* ((ds2 (max 0 (+ ds hl-1)))
           (as2 (- lh ds2)))
      (assert (= lh (+ as2 ds2)))
      (values as2 ds2))))

(defun replaced-object-chunk-p (x)
  (typep x 'replaced-object-chunk))

(defvar *tops* nil)
(defvar *btms* nil)

(defun vertically-align-line (fragments &optional no-line-height-mode)
  ;; Note: The logic in here is a kind of wicked. A line box solely
  ;; consisting out of replaced objects is assumed to have no ascent
  ;; or descent on its own. This is indicated by the parameter
  ;; 'no-line-height-mode' which when non-NIL tells this routine to
  ;; ignore contributions of the line-height of the line box. A first
  ;; run is attempted with no-line-height-mode turned off. After we
  ;; finished the flags 'text-seen-p' and 'replaced-object-seen-p'
  ;; tell us if we had seen the indicated kinds of entities. When
  ;; appropriate we rerun with 'no-line-height-mode' turned on.
  (let ((*tops* nil) (*btms* nil))
    (let ((total-height 0)
          (total-depth 0)
          (text-seen-p nil)
          (replaced-object-seen-p nil))
      (dolist (frag fragments)
        ;; this is a fragment, we would like to include its line-height into the
        ;; calculation:
        (multiple-value-bind (h d)
            (cond ((replaced-object-chunk-p (car (line-fragment-chunks frag)))
                   (let ((ro (replaced-object-chunk-object (car (line-fragment-chunks frag)))))
                     (values (+ (nth-value 1 (ro/size ro)) (nth-value 2 (ro/size ro))) 0)))
                  (no-line-height-mode
                   (values 0 0))
                  (t
                   (text-height-and-depth (line-fragment-block-style frag))))
          (maxf total-height h)
          (maxf total-depth d))
        (let ((frag-chunk (make-instance 'open-chunk :style (line-fragment-block-style frag))))
          (let ((chunk-stack (list frag-chunk))
                dys (cur-dy 0))
            (do ((q (line-fragment-chunks frag) (cdr q)))
                ((null q))
              (let ((chunk (car q)))
              (etypecase chunk
                (open-chunk
                 (multiple-value-bind (h d)
                     ;; Note that CSS makes a cruel distinction between inline replaced object
                     ;; and regular inline elements. The top and bottom of replaced object is at
                     ;; the outer edge, while the top and bottom of regular inline elements is
                     ;; at the inner edge. So we have to make a distinction here.
                     ;; Likewise we need to make a distinction in the line renderer.
                     (cond ((replaced-object-chunk-p (cadr q))
                            (setf replaced-object-seen-p t)
                            (let ((ro (replaced-object-chunk-object (cadr q)))
                                  (s  (bounding-chunk-style chunk)))
                              (values (+ (nth-value 1 (ro/size ro)) (nth-value 2 (ro/size ro))
                                         (cooked-style-margin-top s)
                                         (cooked-style-border-top-width s)
                                         (cooked-style-padding-top s))
                                      (+ (cooked-style-margin-bottom s)
                                         (cooked-style-border-bottom-width s)
                                         (cooked-style-padding-bottom s)))))
                           ;;
                           (no-line-height-mode
                            (values 0 0))
                           (t
                            (text-height-and-depth (bounding-chunk-style chunk))))
                   ;;
                   (setf (values (open-chunk-height chunk) (open-chunk-depth chunk))
                         (values h d))
                   (let ((dy (- (resolve-valign chunk (car chunk-stack)))))
                     (push cur-dy dys)
                     (push chunk chunk-stack)
                     (incf cur-dy dy)
                     (unless (member (cooked-style-vertical-align (bounding-chunk-style chunk))
                                     '(:top :bottom))
                       (maxf total-height (- h dy))
                       (maxf total-depth (+ d dy)))
                     (setf (open-chunk-dy chunk) cur-dy))))
                (close-chunk
                 (setf cur-dy (pop dys))
                 (pop chunk-stack))
                (replaced-object-chunk
                 ;; ###
                 )
                (kern-chunk
                 nil)
                (black-chunk
                 (setf text-seen-p t)
                 ;; nothing to do
                 nil)))))))
      ;;
      (cond ((and (not text-seen-p) replaced-object-seen-p (not no-line-height-mode))
             (vertically-align-line fragments t))
            (t
             ;; now after all care for top/bottom aligend stuff.
             (dotimes (i 2)
               (when *tops*
                 (dolist (k *tops*)
                   (setf (open-chunk-dy k) (- (open-chunk-height k) total-height))
                   (setf total-height (max total-height (- (open-chunk-height k) (open-chunk-dy k))))
                   (setf total-depth  (max total-depth (+ (open-chunk-depth k) (open-chunk-dy k)))) ))
               (when *btms*
                 (dolist (k *btms*)
                   (setf (open-chunk-dy k) (- total-depth (open-chunk-depth k)))
                   (setf total-height (max total-height (- (open-chunk-height k) (open-chunk-dy k))))
                   (setf total-depth  (max total-depth (+ (open-chunk-depth k) (open-chunk-dy k)))) )))
             ;;
             (values total-height total-depth) )))))

(defun cooked-style-font-ascent (style)
  (font-desc-ascent (text-style-font (make-text-style-from-cooked-style style))))

(defun cooked-style-font-descent (style)
  (font-desc-descent (text-style-font (make-text-style-from-cooked-style style))))

(defun cooked-style-font-xheight (style)
  (font-desc-x-height (text-style-font (make-text-style-from-cooked-style style))))

(defun resolve-valign (oc parent-oc)
  (let* ((style  (bounding-chunk-style oc))
         (valign (cooked-style-vertical-align style)))
    (cond ((realp valign)
           valign)
          ;;
          ;; Percentage values are relative to the line height of the element.
          ;; [Note: This should in theory be handled in css-properties.lisp, in practice
          ;;        line-height is special because it inherits differently].
          ;;
          ((and (consp valign) (eql (car valign) :%))
           (* (/ (cdr valign) 100)
              (cooked-style-effective-line-height style)))
          ;;
          (t
           (ecase valign
             (:BASELINE
              0)
             (:MIDDLE
              ;; align the vertical midpoint of the element (typically an image) with
              ;; the baseline plus half the x-height of the parent
              (if (null parent-oc)
                  (progn
                    (warn "Cannot align middle without a parent element.")
                    0)
                (/ (+ (- (open-chunk-depth oc) (open-chunk-height oc))
                      (cooked-style-font-ascent (bounding-chunk-style parent-oc)))
                   2)))
             (:IMG-MIDDLE
              (floor (+ (- (open-chunk-depth oc) (open-chunk-height oc))) 2))
             (:SUB
              (/ (- (cooked-style-font-xheight style)) 2))
             (:SUPER
              (/ (+ (cooked-style-font-xheight style)) 1))
             (:TEXT-TOP
              ;; align the top of the element with the top of the parent element's font
              (if (null parent-oc)
                  (progn
                    (warn "Cannot align 'text-top' without parent.")
                    0)
                (- (cooked-style-font-ascent (bounding-chunk-style parent-oc))
                   (open-chunk-height oc))))
             (:TEXT-BOTTOM
              ;; align the bottom of the element with the bottom of the parent element's font
              (if (null parent-oc)
                  (progn
                    (warn "Cannot align 'text-bottom' without parent.")
                    0)
                (- (open-chunk-depth oc)
                   (cooked-style-font-descent (bounding-chunk-style parent-oc)))))
             (:TOP
              (push oc *tops*)
              :top
              0)
             (:BOTTOM
              (push oc *btms*)
              :bottom
              0) )))))

(defun disc-chunk-p (x)
  (typep x 'disc-chunk))

;;; on floating boxen

;;; Notes:

;; CSS-2 9.5:
;; | A floated box must have an explicit width (assigned via the
;; | 'width' property, or its intrinsic width in the case of replaced
;; | elements).

;; | If there isn't enough horizontal room on the current line for the
;; | float, it is shifted downward, line by line, until a line has
;;                                  ^^^^^^^^^^^^
;;                                  This is what we did.
;; | room for it.

;; | When a block box overlaps, the background and borders of the
;; | block box are rendered behind the float and are only be visible
;; | where the box is transparent. The content of the block box is
;; | rendered in front of the float.
;;
;;   note that the 'floating box' is essentially before all block
;;   boxen background but behind all 'text'.

;;; Implementation

(defvar *floating-boxes*
  nil
  "A list of all currently mounted floating boxes as their floating-chunk.")

(defun mount-floating-box (chunk x y)
  "Mounts a floating chunk so that its outer top-left edge is at position x,y."
    (let ((x1 x)
          (x2 (+ x (floating-chunk-width chunk))))
      (multiple-value-bind (vmp vmn yy yy0 bm)
        (let ((*floating-boxes* nil)) ;### hmm
          (format-block (floating-chunk-content chunk) x1 x2
                    #|ss:|# nil
                    #|before-markers:|# nil
                    #|vmargins:|# 0 0
                    #|y|# y))
        (incf yy vmp)                    ;flush the vertical margin
        (incf yy vmn)                    ;flush the vertical margin
        (setf (floating-chunk-x1 chunk) x1
              (floating-chunk-x2 chunk) x2
              (floating-chunk-y1 chunk) y
              (floating-chunk-y2 chunk) yy)
        (push chunk *floating-boxes*) )))

(defun find-margins (x1 x2 y)
  "Find effective margin at vertical position /y/ considering all
mounted floating boxen."
  (dolist (fb *floating-boxes*)
    (multiple-value-bind (f.x1 f.y1 f.x2 f.y2 side)
        (values (floating-chunk-x1 fb) (floating-chunk-y1 fb)
                (floating-chunk-x2 fb) (floating-chunk-y2 fb)
                (cooked-style-float (floating-chunk-style fb)))
      (when (and (<= f.y1 y) (< y f.y2))                ;## might rethink this cond.
        (case side
          (:left
           (setf x1 (max x1 f.x2)))
          (:right
           (setf x2 (min x2 f.x1)))))))
  ;; it shouldn't happend that x1 < x2.
  (values x1 x2))

;;; vertical margin:

;; Vertical margin has only to be flushed when we really see some line with
;; something on it. The only complication is that we should assume vertical
;; margin to be flushed for finding out the x1, x2 coordinates.


(defun format-para (items pos-vertical-margin neg-vertical-margin x1 x2 yy ss block-style before-markers)
  ;; ### we want another special rule to force floating boxen which occur on otherwise empty lines.
  ;;     (i am still not sure if this is necessarily a good idea).
  (let ((clw 0)
        (cww 0)
        (cur-line nil)
        (cur-word nil)
        (word-fboxen nil)               ;(reversed) list of floating boxen
                                        ; attached to the current word.

        (line-fboxen nil)               ;(reversed) list of floating boxen
                                        ; attached to the current line.
        (ox1 x1)
        (ox2 x2))

    (when *hyphenate-p*
      (setf items (hyphenate-items items (- x2 x1))))

    (labels ((new-margins ()
               "finds the new margins, this takes the vertical margin into
                account without flushing it."
               (setf (values x1 x2)
                     (find-margins ox1 ox2
                                      (+ yy pos-vertical-margin neg-vertical-margin))))

             (flush-margin ()
               "actually flushes the vertical margin."
               (incf yy pos-vertical-margin)
               (incf yy neg-vertical-margin)
               (setf pos-vertical-margin 0)
               (setf neg-vertical-margin 0))

             (flush-line ()
               "called with the current line is about to be emitted. Has
                some extra 'safety' test to see if there is actually
                something on it."
               (cond ((or
                       (some #'(lambda (chunk)
                                (not (or
                                      ;; half chunks don't count
                                      (and (bounding-chunk-p chunk)
                                           (bounding-chunk-halfp chunk))
                                      ;; also first-line start end chunks don't count
                                      (and (bounding-chunk-p chunk)
                                           (typep (bounding-chunk-pt chunk) 'first-line-pseudo-element)))))
                            cur-line)
                       before-markers)
                      ;; okay there is something, build the line-fragments
                      (let ((fragments
                             (list*
                              (make-instance 'line-fragment :x1 x1 :x2 x2
                                                            :block-style block-style
                                                            :chunks (reverse cur-line))
                              ;; possible markers
                              (mapcar (lambda (marker)
                                        (make-instance 'line-fragment
                                                       :x1 (marker-box-x1 marker)
                                                       :x2 (marker-box-x2 marker)
                                                       :block-style block-style ;### correct?
                                                       :chunks
                                                       ;; ### hmm we assume a hell lot about this marker box.
                                                       (para-box-items (first (marker-box-content marker)))))
                                      before-markers))))
                        (setf before-markers nil) ;before markers are gone now
                        ;; render it
                        ;; but first flush the margin
                        (flush-margin)
                        (setf yy (render-line yy fragments))))
                     (t
                      ;; otherwise just forget the current line
                      ) )

               (setf cur-line nil
                     clw 0)
               (new-margins)
               ;; we need to decide what to do with the line-fboxen ...
               (consider-fboxen (prog1 (reverse line-fboxen)
                                  (setf line-fboxen nil)))
               (new-margins))

             (consider-fboxen (fboxen)
               "Consider all the floating boxen in fboxen in order and
                mount any that fit. Those that do not fit land up in line-fboxen."
               (let ((flag nil))
                 (dolist (k fboxen)
                   (cond ((and (not flag)
                               (<= (+ clw (floating-chunk-width k)) (- x2 x1))
                               (if (member (cooked-style-clear (floating-chunk-style k)) '(:left :both))
                                   (>= (+ yy pos-vertical-margin neg-vertical-margin)
                                       (clear-y-coordinate :left))
                                   t)
                               (if (member (cooked-style-clear (floating-chunk-style k)) '(:right :both))
                                   (>= (+ yy pos-vertical-margin neg-vertical-margin)
                                       (clear-y-coordinate :right))
                                   t))
                          ;; still fits, mount it
                          (mount-fbox k))
                         (t
                          ;; does not fit
                          (push k line-fboxen)
                          (setf flag t)))) ))

             (mount-fbox (chunk &optional (y (+ yy pos-vertical-margin neg-vertical-margin)))
               (mount-floating-box chunk
                                      (case (cooked-style-float (floating-chunk-style chunk))
                                        (:left  x1)
                                        (:right (- x2 (floating-chunk-width chunk))))
                                      y)
               (new-margins))

             (flush-word ()
               (tagbody
                re-consider
                  (when (> cww (- x2 x1))
                    ;; we have a change though
                    (flush-margin)
                    (unless (= (- x2 x1) (- ox2 ox1))
                      ;; some floating boxen must be in the way, move
                      ;; past them ...

                      ;; ### Question: although going in natural-line-height
                      ;; increments is fine it might be more correct to place
                      ;; floating boxen which take their priority at the highest
                      ;; possible position.

                      (let ((clear-y (reduce #'max (mapcar #'floating-chunk-y2 *floating-boxes*))))
                        (let ((natural-line-height (cooked-style-effective-line-height block-style)))
                          (do ((y (+ yy)
                                  (+ y natural-line-height)))
                              ((>= y clear-y)
                               (setf yy y))
                            (multiple-value-bind (x1 x2) (find-margins ox1 ox2 y)
                              (when (<= cww (- x2 x1))
                                (setf yy y)
                                (return))))))
                      (new-margins)
                      ;; now: floating boxen have priority
                      (consider-fboxen (prog1 (reverse line-fboxen)
                                         (setf line-fboxen nil)))
                      (new-margins)
                      (go re-consider))))

               (when (> cww (- x2 x1))
                 ;; situation persists: give a warning.
                 (warn "*** Overfull line box."))
               ;;
               (dolist (k (reverse cur-word)) (push k cur-line))
               (setf cur-word nil)
               (incf clw cww)
               (setf cww 0)
               (consider-fboxen (prog1
                                    (reverse word-fboxen)
                                  (setf word-fboxen nil))) )

             ;;
             (process (xs)
               (let ((x (car xs)))
                 (typecase x
                   (floating-chunk
                    (compute-floating-chunk-width x (- ox2 ox1))
                    ;; this is a special rule: when the word is otherwise
                    ;; empty we could as well try to mount the floating box
                    ;; now.
                    (cond ((= cww 0) ;; ### (null cur-word)
                           (push x word-fboxen)
                           (flush-word))
                          (t
                           (push x word-fboxen))))
                   (open-chunk
                    (push (bounding-chunk-style x) ss)
                    (incf cww (chunk-width x))
                    (push x cur-word))
                   (close-chunk
                    (pop ss)
                    (incf cww (chunk-width x))
                    (push x cur-word))
                   (disc-chunk
                    (cond ((disc-chunk-forcep x)
                           (flush-word)
                           (show (disc-chunk-before x))
                           (flush-word)
                           (flush-line)
                           (show (disc-chunk-after x)))
                          (t
                           (flush-word)
                           (let ((p (disc-chunk-here x))
                                 (ww 0))
                             (block zulu
                               (dolist (k p)
                                 (cond ((typep k 'disc-chunk)
                                        ;; before is missing.
                                        ;; actually this is a stranger situation ...
                                        (return-from zulu))
                                       ((typep k 'floating-chunk)
                                        nil) ;these don't count.
                                       (t
                                        (incf ww (chunk-width k))) ))
                               ;; disc exhausted continue with list
                               (dolist (k (cdr xs))
                                 (cond ((typep k 'disc-chunk)
                                        ;; before is missing.
                                        ;; actually this is a stranger situation ...
                                        (return-from zulu))
                                       ((typep k 'floating-chunk)
                                        nil)
                                       (t
                                        (incf ww (chunk-width k))))))
                             (cond ((<= (+ clw ww) (- x2 x1))
                                    (show p))
                                   (t
                                    (show (disc-chunk-before x))
                                    (flush-word)
                                    (flush-line)
                                    (show (disc-chunk-after x))))))))
                   (replaced-object-chunk
                    (incf cww (chunk-width x))
                    (push x cur-word))
                   (kern-chunk
                    (incf cww (chunk-width x))
                    (push x cur-word))
                   (black-chunk
                    (incf cww (chunk-width x))
                    (push x cur-word)) ))
               xs)
             ;;
             (show (seq)
               (do ((q seq (cdr q)))
                   ((null q))
                 (setf q (process q)) )) )

      ;; ### should text-indention be within the first-line pseudo element or
      ;;     outside?
      (let ((text-indent (cooked-style-text-indent block-style)))
        ;; now: this is art! text-indent
        (unless (zerop text-indent)
          (push (make-text-indent-chunk text-indent)
                items)))

      (new-margins)

      (show items)
      (flush-word)
      (flush-line)

      ;; after all there might be floating boxen still flying around. Mount
      ;; them where they fit but do not affect the yy coordinate.
      (when line-fboxen
        (let ((oyy yy)
              (oneg-vertical-margin neg-vertical-margin)
              (opos-vertical-margin pos-vertical-margin))

          (flush-margin)
          (new-margins)

          ;; ### hmm can it be that the 'not any higher' is for each side?
          ;;     look that up!

          (dolist (k (reverse line-fboxen)) ;one after another
            (block yseek
              (dolist (y (cons yy (sort (mapcar #'floating-chunk-y2 *floating-boxes*)
                                        #'<)))
                (setf yy y)
                (new-margins)
                (when (and (<= (floating-chunk-width k) (- x2 x1))
                           (if (member (cooked-style-clear (floating-chunk-style k)) '(:left :both))
                               (>= (+ yy pos-vertical-margin neg-vertical-margin)
                                   (clear-y-coordinate :left))
                               t)
                           (if (member (cooked-style-clear (floating-chunk-style k)) '(:right :both))
                               (>= (+ yy pos-vertical-margin neg-vertical-margin)
                                   (clear-y-coordinate :right))
                               t))
                  (mount-fbox k yy)
                  (return-from yseek)))
              ;; when we land here our y coordinates are exhausted, mount the
              ;; floating box never the less.
              (mount-fbox k yy) ))

          (setf yy oyy
                neg-vertical-margin oneg-vertical-margin
                pos-vertical-margin opos-vertical-margin) )))

    ;; ### now after all we _need_ to mount all floating boxen which still
    ;;     exist.

    (values pos-vertical-margin neg-vertical-margin ox1 ox2 yy ss block-style) ))

(defun render2 (device document pt selected-style)
  (declare (ignorable device document pt selected-style))
  ;;;
  (setf *floating-boxes* nil)
  (setf *device* device
        *document* document)
  (let* ((css::*device* device)
         (*style-sheet* (document-style-sheet document))
         (css::*style-sheet* *style-sheet*)
         (rc (make-rc :device device :y 0 :x0 0 :x1 *canvas-width*
                      :vertical-margins nil
                      :vertical-margin-callbacks nil
                      :first-line-tasks nil
                      :left-floating-boxen nil
                      :right-floating-boxen nil
                      :document document ))
         (*rcontext* rc))
    (setf *zzz* (po (flatten-pt pt) :dont-care))
    (setf *document-height*
          (nth-value 2
                     (format-block *zzz* 0 *canvas-width* nil nil 0 0 0) )) ))

(defun reflow ()
  (setf *document-height*
        (nth-value 2
                   (format-block *zzz* 0 *canvas-width* nil nil 0 0 0))))

(defvar *zzz* nil)
(defvar *dyn-elm* nil)

(defun tata (mode)
  (let ((clim-user::*medium* (clim:find-pane-named clim-user::*frame* 'clim-user::canvas))
        (closure-protocol:*document-language*
         (make-instance 'r2::html-4.0-document-language))
        (closure-protocol:*user-agent* nil))
    (multiple-value-bind (x c)
        (ignore-errors
          ;; first find the chunk
          (let ((offender *dyn-elm*)
                (the-pb nil))
            (block suche
              (labels ((walk (x)
                         (etypecase x
                           (marker-box)
                           (block-box
                            (mapc #'walk (block-box-content x)))
                           (para-box
                            (mapc #'(lambda (z) (walk-chunk x z)) (para-box-items x)))))
                       (walk-chunk (pb x)
                         (etypecase x
                           (floating-chunk)
                           (bounding-chunk
                            (setf (bounding-chunk-pt x) offender)
                            #+NIL
                            (when (eq (bounding-chunk-pt x) offender)
                              '(cond ((eql mode :highlight)
                                      (setf (slot-value (bounding-chunk-style x) 'css::border-left-width) 1
                                       (slot-value (bounding-chunk-style x) 'css::border-left-style) :solid
                                       (slot-value (bounding-chunk-style x) 'css::border-right-width) 1
                                       (slot-value (bounding-chunk-style x) 'css::border-right-style) :solid
                                       (slot-value (bounding-chunk-style x) 'css::border-top-width) 1
                                       (slot-value (bounding-chunk-style x) 'css::border-top-style) :solid
                                       (slot-value (bounding-chunk-style x) 'css::border-bottom-width) 1
                                       (slot-value (bounding-chunk-style x) 'css::border-bottom-style) :solid))
                                (t
                                 (setf (slot-value (bounding-chunk-style x) 'css::border-left-width) 0
                                  (slot-value (bounding-chunk-style x) 'css::border-left-style) :none
                                  (slot-value (bounding-chunk-style x) 'css::border-right-width) 0
                                  (slot-value (bounding-chunk-style x) 'css::border-right-style) :none
                                  (slot-value (bounding-chunk-style x) 'css::border-top-width) 0
                                  (slot-value (bounding-chunk-style x) 'css::border-top-style) :none
                                  (slot-value (bounding-chunk-style x) 'css::border-bottom-width) 0
                                  (slot-value (bounding-chunk-style x) 'css::border-bottom-style) :none)))
                              '(setf (slot-value (bounding-chunk-style x) 'css::background-color)
                                (if (eq mode :highlight)
                                    "#ccccff"
                                    :transparent))
                              '(setf (slot-value (bounding-chunk-style x) 'css::text-decoration)
                                (if (eq mode :highlight)
                                    (list :underline)
                                    :none))
                              ))
                           (kern-chunk)
                           (disc-chunk
                            (mapc #'(lambda (x) (walk-chunk pb x))
                                  (disc-chunk-here x))
                            (mapc #'(lambda (x) (walk-chunk pb x))
                                  (disc-chunk-after x))
                            (mapc #'(lambda (x) (walk-chunk pb x))
                                  (disc-chunk-before x)))
                           (black-chunk
                            '(setf (slot-value (black-chunk-style x) 'css::color)
                              (if (eq mode :highlight)
                                  "#ff0000"
                                  "#000000"))
                            )
                           (replaced-object-chunk
                            (when (typep (replaced-object-chunk-object x)
                                         'lazy-image)
                              (setf (replaced-object-chunk-object x)
                                    (replaced-element-p *document* *device* (replaced-object-chunk-element x)))
                              (setf the-pb pb)
                              (return-from suche nil))
                            ))))
                (walk *zzz*)))

            (dprint "@@@@@@@ offender = ~S." offender)
            (dprint "@@@@@@@ the-pb = ~S." the-pb)
            (when the-pb
              (let (
                    (papa (clim:output-record-parent (para-box-output-record the-pb))))
                (dprint "@@@@@@@ papa = ~S." papa)
                (clim:delete-output-record (para-box-output-record the-pb) papa)
                ;; now clim is so inherently broken ....
                (setf (para-box-output-record the-pb)
                      (clim:with-new-output-record (clim-user::*medium*)
                        (funcall (para-box-genesis the-pb)))))
              (tata mode))
            ))
      (when c
        (dprint "Error: ~A." c)))))

(defun format-block (item x1 x2 ss before-markers #||# pos-vertical-margin neg-vertical-margin yy)
  (let (res)
    (setf (block-box-output-record item)
          (clim:with-new-output-record
              (clim-user::*medium*) foo
              (setf res
                    (multiple-value-list
                        (case (cooked-style-display (block-box-style item))
                          (:table
                           (incf yy pos-vertical-margin)
                           (incf yy neg-vertical-margin)
                           (setf yy
                            (format-table item x1 x2 yy (block-box-style item)))
                           (values 0 0 yy))
                          (otherwise
                           (format-block-aux item
                                         x1 x2
                                         ss
                                         before-markers
                                         pos-vertical-margin
                                         neg-vertical-margin
                                         yy) ))))))
    (values-list res)))

(defun format-block-aux (block-box x1 x2 ss before-markers
                     #|in-out|# pos-vertical-margin neg-vertical-margin yy)

  ;; ###
  ;; now vertical should only be flushed in the following circumstances:
  ;; . a line is actually mounted.
  ;; . padding is 'mounted'
  ;; . a border is 'mounted'
  ;;

  (let* ((items (block-box-content block-box))
         (block-style (block-box-style block-box))
         (s block-style)
         (tm (cooked-style-margin-top s))
         (bm (cooked-style-margin-bottom s))
         (tp (cooked-style-padding-top s))
         (bp (cooked-style-padding-bottom s))
         (yy0 nil)      ;the inner top padding edge
                        ; NIL initially to indicate that we do not know it for now.
         (bg-record
          (clim:with-new-output-record (clim-user::*medium*)
            )))

    ;; remember the output record of the decoration
    (setf (block-box-decoration-output-record block-box)
          bg-record)

    (labels
        ((flush-margin ()
           "Flushes the vertical margin"
           (incf yy pos-vertical-margin)
           (incf yy neg-vertical-margin)
           (setf pos-vertical-margin 0)
           (setf neg-vertical-margin 0)))

      (multiple-value-bind (ml bl pl wd pr br mr)
          (values (if (eql (cooked-style-display s) :table-cell)
                      0                 ;table cells don't have margins  ### better way to do that?
                      (cooked-style-margin-left s))
                  (if (eql (cooked-style-display s) :table-cell)
                      0                 ;table cells don't have border (they are drawn by the table renderer)
                      (cooked-style-border-left-width s))
                  (cooked-style-padding-left s)
                  (cooked-style-width s)
                  (cooked-style-padding-right s)
                  (if (eql (cooked-style-display s) :table-cell)
                      0                 ;table cells don't have border (they are drawn by the table renderer)
                      (cooked-style-border-right-width s))
                  (if (eql (cooked-style-display s) :table-cell)
                      0                 ;table cells don't have margins  ### better way to do that?
                      (cooked-style-margin-right s)))
          (push s ss)

          ;; top margin
          (when (realp tm)
            (if (> tm 0)
                (maxf pos-vertical-margin tm)
                (minf neg-vertical-margin tm)))

    ;;; clear

          ;; Clear should work by increasing the top-margin of the box so that
          ;; the top border edge is below any offending floating.

          ;; Caution: this somehow magically also applies to floating boxen the
          ;; meaning of which in that context is unclear.

          ;; this formulation is clumspy with respect to collapsing margins, since
          ;; on a block box which has no border or padding the y-coordinate of the
          ;; border top edge is not known in advance.

          ;; we do the easy route and assert:

          ;;     clear-y = y-coordinate that is clear
          ;; yy + vertical-margin
          ;; = yy + max(pos-vertical-margin, margin-top) + neg-vertical-margin
          ;; = border-top-edge
          ;;
          ;; => max(pos-vertical-margin, margin-top)
          ;;    = clear-y - neg-vertical-margin - yy

          ;;     margin-top >= clear-y - neg-vertical-margin - yy
          ;; =>  max(pos-vertical-margin, margin-top) >= clear-y - neg-vertical-margin - yy
          ;; =>  max(pos-vertical-margin, margin-top) + neg-vertical-margin + yy = border-top-edge >= clear-y
          ;;

          (let ((clear (cooked-style-clear s)))
            (unless (eql :clear :none)
              (let ((clear-y 0))
                (when (member clear '(:left :both))
                  (setf clear-y (max clear-y (clear-y-coordinate :left))))
                (when (member clear '(:right :both))
                  (setf clear-y (max clear-y (clear-y-coordinate :right))))
                (setf pos-vertical-margin
                      (max pos-vertical-margin
                           (- clear-y neg-vertical-margin yy))))))

          ;; top border
          (unless (zerop (cooked-style-border-top-width s))
            (flush-margin)
            (unless yy0
              (setf yy0 yy))
            (incf yy (cooked-style-border-top-width s)))

          ;; top padding
          (when (/= tp 0)
            (flush-margin)
            (unless yy0
              (setf yy0 yy))
            (incf yy tp))

          (incf x1 (+ ml bl pl))
          (decf x2 (+ mr br pr))

          ;;(assert (= x2 (+ x1 wd)))
          
          (setf x2 (+ x1 wd))

          (dolist (item items)
            (etypecase item
              (para-box
               (let ((op pos-vertical-margin) (on neg-vertical-margin) (oy yy))

                 (let ((.*document* *document*))
                   (setf (para-box-genesis item)
                         (multiple-value-bind (pos-vertical-margin neg-vertical-margin x1 x2 yy ss block-style)
                             (values pos-vertical-margin neg-vertical-margin x1 x2 yy ss block-style)
                           (lambda ()
                             (let ((*document* .*document*))
                               (format-para (para-box-items item)
                                        pos-vertical-margin neg-vertical-margin x1 x2 yy ss block-style
                                        before-markers))))))

                 (setf (para-box-output-record item)
                       (clim:with-new-output-record (clim-user::*medium*)
                         (setf (values pos-vertical-margin neg-vertical-margin x1 x2 yy ss block-style)
                               (funcall (para-box-genesis item)))))

                 ;; EVIL: from the fact that pos and neg margin are now zero, we can
                 ;; deduce that the margin must have been flushed.
                 (when (and (null yy0)
                            (= pos-vertical-margin neg-vertical-margin 0))
                   (setf yy0 (+ oy op on)))
                 ;;
                 )
               (setf before-markers nil))

              (block-box
               ;; Here the block has to tell us, where it will flush the margin. Now
               ;; every block flushes its margin (eventually) but the exact amount
               ;; may not be known.
               (let (yy00)
                 (setf (values pos-vertical-margin neg-vertical-margin yy yy00)
                       (format-block item x1 x2 ss before-markers
                                 pos-vertical-margin neg-vertical-margin yy))
                 (unless yy0
                   (setf yy0 yy00)))
               (setf before-markers nil) )

              (marker-box
               ;; ### care for right markers
               ;; ### is this an after marker?
               (let (mx1 mx2)
                 ;; find out the margins we want to typeset into
                 (setf mx1 (- x1 pl bl ml)
                       mx2 (- x1 pl bl))
                 ;; find the width of the marker box and its marker offset for the
                 ;; definite marker box margins.
                 ;; ### what should padding, margin et al to do marker boxen?

                 ;; ### ouch: the containing block for this marker perhaps is not
                 ;; the containing block of the paragraph.

                 ;; ### now after all is the containing block of an element
                 ;; constant?

                 (let ((min (minmax-para (para-box-items(car(marker-box-content item))) block-style))
                       (offset (cooked-style-marker-offset (marker-box-style item))))
                   #+NIL
                   (dprint "Min width of ~S is ~S. offset = ~S"
                           (marker-box-content item) min
                           offset)
                   (setf offset 6) ;###
                   (setf mx1 (- mx2 min offset)))
                 (setf (marker-box-x1 item) mx1
                       (marker-box-x2 item) mx2)

                 (push item before-markers)
                 ))))

          ;; when there are any markers left, spill them now
          (when before-markers
            ;; ### evil: markers by (our) definition are always non-empty, so we can
            ;; flush the margin now.
            ;; ### now format-para is assumed to flush the margin if needed.
            (flush-margin)
            (unless  yy0
              (setf yy0  yy))
            (values pos-vertical-margin neg-vertical-margin x1 x2 yy ss block-style)
            (format-para nil pos-vertical-margin neg-vertical-margin x1 x2 yy ss block-style
                     before-markers))

          (unless yy0
            ;; ### now this is questionable, should we really force a margin flush?
            (flush-margin)
            (unless  yy0
              (setf yy0 yy)))

      ;;; height

          ;; It might happen that the user specified a height (which refers to
          ;; the content height of a box). The inner top edge is given by yy0.
          ;; Now specifying a height _forces_ the height to the given value
          ;; regardless of the fact that we might need more space. So we take a
          ;; brute force route and simply assert the appropriate y cursor
          ;; position.

          ;; oops yy0 is the border-top-edge

          (let ((height (cooked-style-height s)))
            ;; now it seems that css1 includes padding here
            (unless (eql :auto height)
              (flush-margin)              ;### should we actually do that?
              (setf yy (+ (+ yy0
                             (cooked-style-border-top-width s)
                             (cooked-style-padding-top s))
                          height))))

          ;; bottom padding
          (when (/= bp 0)
            (flush-margin))
          (incf yy bp)

          ;; bottom border
          (unless (zerop (cooked-style-border-bottom-width s))
            (flush-margin)
            (incf yy (cooked-style-border-bottom-width s)))

          ;; bottom margin
          (when (realp bm)
            (if (> bm 0)
                (maxf pos-vertical-margin bm)
                (minf neg-vertical-margin bm)))

          ;;
          (clim:with-output-recording-options (clim-user::*medium* :record t :draw nil)
            (let ((new-record
                   (clim:with-new-output-record (clim-user::*medium*)
                     ;;
                     (multiple-value-bind (x1 y1 x2 y2)
                         (values (- x1 pl) (+ yy0
                                              (cooked-style-border-top-width s)
                                              )
                                 (+ x2 pr) (- yy
                                              (cooked-style-border-bottom-width s)
                                              ))
                       (draw-box-decoration clim-user::*medium* x1 y1 x2 y2 block-style)
                       (incf y1 (cooked-style-padding-top s))
                       (decf y2 (cooked-style-padding-bottom s))
                       (when (realp (cooked-style-height s))
                         (unless (= (cooked-style-height s)
                                    (- y2 y1))
                           (error "Fubar")))
                       #+NIL
                       (unless (or (= x1 x2) (= y1 y2))
                         (clim:draw-rectangle* clim-user::*medium* x1 y1 x2 y2
                                               :ink clim:+red+
                                               :filled nil))
                       )
                     ;;
                     )))
              (clim:delete-output-record new-record (clim:output-record-parent new-record))
              (clim:add-output-record new-record bg-record)))

          (values pos-vertical-margin neg-vertical-margin yy yy0)))))




(defmethod print-object ((object para-box) stream)
  (format stream "#<PARA-BOX ~S>" (para-box-items object)))

(defmethod print-object ((object block-box) stream)
  (format stream "#<BLOCK-BOX ~S>" (block-box-content object)))

(defclass* marker-box ()
  ;; A marker box can occur as content of a block-box.

  ;; The formatting code is responsible for passing this marker box the first
  ;; paragraph that is applicable.
  x1 x2
  parent                ;the block box this marker-box emerged from.
  style                 ;the style of the pseudo-element
  side                  ;either :left or :right (the side it has to be rendered to)
  content)              ;the marker box content (a list of chunks)

(defun block-box-p (object)
  (typep object 'block-box))

(defun make-block-box (&key content style element)
  (unless (eql element (slot-value style 'css::%element))
    (warn "MAKE-BLOCK-BOX: style does match element (~S vs ~S)."
          (slot-value style 'css::%element) element))
  (make-instance 'block-box
                 :element element
                 :content content
                 :style style))

(defun marker-box-p (object)
  (typep object 'marker-box))

(defun make-marker-box (&rest args &key x1 x2 style side content)
  (declare (ignore x1 x2 style side content))
  (apply #'make-instance 'marker-box args))

(defun make-para-box (&key items)
  (make-instance 'para-box :items items))

(defun para-box-p (object)
  (typep object 'para-box))

(defun clear-y-coordinate (side)
  "Find smallest y coordinate clear of floating boxen on side 'side'."
  (let ((clear-y 0))
    (dolist (k *floating-boxes*)
      (when (eql side (cooked-style-float (floating-chunk-style k)))
        (setf clear-y (max clear-y (floating-chunk-y2 k)))))
    clear-y))

(defmethod replaced-object-dimensions ((object t) given-width given-height)
  "This method is reponsible to return a replaced object actual dimensions
under the situation that the user specified the width and height as
'given-width' and 'given-height'. Both dimensions can either a real number
or :auto meaning that the dimension was not explicitly given.

It is the responsibility of the individual replaced object to decide if it
wants to scale the other dimension if only one is given or if it wants to
retain its intrinsic value.

This method returns three values: width, ascent, descent. (Yes, replaced
objects have something like a baseline contrary to the believe of the W3C)."
  (cond ((and (realp given-width) (realp given-height))
         ;; both are given
         (values given-width given-height 0))
        ((and (eql given-width :auto) (eql given-height :auto))
         ;; both are auto
         (ro/intrinsic-size object))

        ((eql given-width :auto)
         ;; only width is auto, scale height as appropriate
         (values
          (round        ;xxx
           (if (zerop (+ (nth-value 1 (ro/intrinsic-size object))
                         (nth-value 2 (ro/intrinsic-size object))))
               0
               (* given-height (/ (nth-value 0 (ro/intrinsic-size object))
                                  (+ (nth-value 1 (ro/intrinsic-size object))
                                     (nth-value 2 (ro/intrinsic-size object)))))))
          given-height
          0))

        ((eql given-height :auto)
         (values
          (round given-width)
          (round (if (zerop (nth-value 0 (ro/intrinsic-size object)))
                     0
                     (* given-width (/ (+ (nth-value 1 (ro/intrinsic-size object))
                                          (nth-value 2 (ro/intrinsic-size object)))
                                       (nth-value 0 (ro/intrinsic-size object))))))
          0))

        (t
         (error "Bogus arguments."))))


(defun resolve-widthen (style cbwidth
                           &optional (margin-left        (cooked-style-margin-left style))
                                     (border-left-width  (cooked-style-border-left-width style))
                                     (padding-left       (cooked-style-padding-left style))
                                     (width              (cooked-style-width style))
                                     (padding-right      (cooked-style-padding-right style))
                                     (border-right-width (cooked-style-border-right-width style))
                                     (margin-right       (cooked-style-margin-right style)) )
  ;; -> margin-left padding-left width padding-right margin-right
  (let ()
    ;;
    ;; resolve possible percentage values
    ;;
    (setf margin-left   (maybe-resolve-percentage margin-left   cbwidth))
    (setf padding-left  (maybe-resolve-percentage padding-left  cbwidth))
    (setf width         (maybe-resolve-percentage width         cbwidth))
    (setf padding-right (maybe-resolve-percentage padding-right cbwidth))
    (setf margin-right  (maybe-resolve-percentage margin-right  cbwidth))
    ;;
    ;; deduct padding and border
    ;;
    (decf cbwidth (+ padding-left padding-right border-left-width border-right-width))

    ;;
    ;; now only margin and width are left
    ;;
    (let ((ml margin-left) (mr margin-right) (wd width) (width-left cbwidth))

      ;; Alle Faelle betrachten
      (cond
        ;; CSS1 says: If none of the properties are 'auto', the value
        ;; of 'margin-right' will be assigned 'auto'.
        ((and (neq ml :auto) (neq wd :auto) (neq mr :auto)) ; - - -
         (setq mr (- width-left ml wd)))

        ;; CSS1 says: If exactly one of 'margin-left', 'width' or 'margin-right'
        ;; is 'auto', the UA will assign that property a value that will make the
        ;; sum of the seven equal to the parent's width.

        ((and (neq ml :auto) (neq wd :auto) ( eq mr :auto)) ; - - A
         (setq mr (- width-left ml wd)))

        ((and (neq ml :auto) ( eq wd :auto) (neq mr :auto)) ; - A -
         (setq wd (- width-left ml mr)))

        ((and ( eq ml :auto) (neq wd :auto) (neq mr :auto)) ; A - -
         (setq ml (- width-left wd mr)))

        ;; CSS1: If more than one of the three is 'auto', and one of them is
        ;; 'width', than the others ('margin-left' and/or 'margin-right') will be
        ;; set to zero and 'width' will get the value needed to make the sum of
        ;; the seven equal to the parent's width.

        ((and (neq ml :auto) ( eq wd :auto) ( eq mr :auto)) ; - A A
         (setq mr 0)
         (setq wd (- width-left ml mr)))

        ((and ( eq ml :auto) ( eq wd :auto) (neq mr :auto)) ; A A -
         (setq ml 0)
         (setq wd (- width-left ml mr)))

        ((and ( eq ml :auto) ( eq wd :auto) ( eq mr :auto)) ; A A A
         (setq ml 0 mr 0)
         (setq wd (- width-left ml mr)))

        ;; Otherwise, if both 'margin-left' and 'margin-right' are 'auto', they will
        ;; be set to equal values. This will center the element inside its parent.

        ((and ( eq ml :auto) (neq wd :auto) ( eq mr :auto)) ; A - A
         (setq ml (ceiling (- width-left wd) 2)) ;DEVRND
         (setq mr (floor   (- width-left wd) 2)) ) ) ;DEVRND

      (values ml border-left-width padding-left wd padding-right border-right-width mr) )))

(defun flatten-pt (pt &optional s)
  (cond ((text-element-p pt)
         (list (list :data (element-text pt) pt)))
        (t
         (append (list (list :open pt))
                 (mapcan #'(lambda (x) (flatten-pt x s)) (element-children pt))
                 (list (list :close pt))))))

(defun my-setup-style (pt s containing-block-style)
  (assert (not (eq pt
                   (and containing-block-style
                        (slot-value containing-block-style 'css::%element)))))
  (css::setup-style-3
   *device* *document* *style-sheet* pt s containing-block-style))

;;;

(let ((memo (make-hash-table :test #'eq)))
  (defun make-text-style-from-cooked-style (cooked-style)
    (or (gethash cooked-style memo)
        (setf (gethash cooked-style memo)
              (make-text-style *device*
                               :font-family    (cooked-style-font-family cooked-style)
                               :font-weight    (cooked-style-font-weight cooked-style)
                               :font-size      (cooked-style-font-size cooked-style)
                               :font-style     (cooked-style-font-style cooked-style)
                               :font-variant   (cooked-style-font-variant cooked-style)
                               :letter-spacing (cooked-style-letter-spacing cooked-style)
                               :word-spacing   (cooked-style-word-spacing cooked-style))))))



;; | marker
;; |
;; |    This value declares generated content before or after a box to be a
;; |    marker. This value should only be used with :before and :after
;; |    pseudo-elements attached to block-level elements. In other cases,
;; |    this value is interpreted as 'inline'. Please consult the section
;; |    on markers for more information.


;;;; CSS2 Tables Model

;; CSS2 always assumes:

;;    <table> ::= <rowgroup>*
;; <rowgroup> ::= <row>*
;;      <row> ::= <cell>*
;;     <cell> ::=
;;
;; missing elements have to be infered.
;; colspan and rowspan is not defined by CSS2.
;;

;;; rowspan/colspan

;; rowspan and colspan for now is simply resolved by assigning row and
;; column indicies to cells.

;; "2. If the parent P of a 'table-cell' element T is not a
;; 'table-row', an object corresponding to a 'table-row' will be
;; generated between P and T. This object will span all consecutive
;; 'table-cell' siblings (in the document tree) of T."

;; White space?
;; this can be coded as:

;; The hard part about these rules is the requirement that consecutive
;; elements will be gathered into one table object. This implies that
;; this inference has to happen pretty early. So I guess we are
;; probably best of by doing this in the "popo" phase.


;; 17.5 Visual layout of table contents

;; | Cells may span several rows or columns. Each cell is thus a
;; | rectangular box, one or more grid cells wide and high. The top
;; | row of this rectangle is in the row specified by the cell's
;; | parent. The rectangle must be as far to the left as possible, but
;; | it may not overlap with any other cell box, and must be to the
;; | right of all cells in the same row that are earlier in the source
;; | document.

;; Hmm, is that generally true for HTML also?

;;; Collecting the table

;; Since as said in the spec the table model is row-centric, we first
;; collect the rowgroups and rows to the table. After that is done we
;; resolve rowspan and colspan by assigning a column and row index to
;; each cell. (OTH the row index should always be the row itself and
;; thus we'll need just a column index).

;; Now we now the number of columns and can actually start to gather
;; the columns and column groups. If these elements are not present in
;; the document tree we just generate anonymous elements.

;; Next step would be to derive the columns minimum and maxium
;; dimensions and assigning their actual size.

;; Actually rendering the table then is a bit awkward since we need to
;; align the table cells vertically also, which generally can only be
;; done after rendering said cell. Since we don't really support
;; incremental rendering we can as well rendering them with drawing
;; turned off and move the contents later on. This also implies that
;; we cannot simply use the block content renderer as it is for
;; rendering the cell content but need to advice it to skip generation
;; of background so that we later can easily move the emitted output.
;; In theory however shifting a cell vertically would be achieved by
;; modifying a cells vertical margins and have the incremental update
;; facility coping with it.

;;

(defclass* table-row-group ()
  style element
  rows)

(defclass* table-row ()
  style element
  cells)

(defclass* table-cell ()
  content                               ;a block box
  ;;
  (colspan :initform 1)
  (rowspan :initform 1)
  (col-index :initform 0
             :documentation "The index of the column this cell is part of.")
  ;; This cells min/max width
  ;; That is the cells minimum or maximum width including all padding
  ;; or margin or border.
  ;;
  ;; ### we should have the content width here and take a possible
  ;; 'width' value of the cell element into account.
  %minimum-width
  %maximum-width
  (minimum-height
   :documentation "what CSS calls the minimum height, that is the height of the cell content when rendered.")
  (baseline
   :documentation "The baseline of the rendered cell contents, relative coordinate from table-cell-y. This can as well be NIL incase the content really has no baseline.")
  (x :documentation "top-left device x coordinate either from outside the border (separate border model) or from the middle of the border (collapsing border model.")
  (y :documentation "see slot X")
  )

(defmethod table-cell-style ((cell table-cell))
  (block-box-style (table-cell-content cell)))

(defmethod table-cell-element ((cell table-cell))
  (block-box-element (table-cell-content cell)))

(defclass* table-column-group ()
  style
  element
  ;; hmm why do column groups bother with minmax?
  minimum-width
  maximum-width
  columns)

(defclass* table-column ()
  style
  element
  (minimum-width :initform 0)
  (maximum-width :initform 0))

(defclass* table ()
  style
  element
  decoration-output-record
  row-groups
  column-groups)

(defun make-anonymous-element ()
  (sgml::make-pt/low :name :%anon))

;;;;

;; The interesting thing is that HTML tables are more powerful than
;; CSS2 tables. So does CSS2 for instance lack COLSPAN and ROWSPAN
;; attributes, I expect that these will be added in CSS3 or CSS4.

;; Columns get min/max widthen also

;; they probably do so by first absorbing the min/max of colspan=1
;; then we would look for colspan=2 cells and distribute the lack
;; equally.

;; Likewise rows need to get dimensions also. This gets a little bit
;; more complicate since rows now also get a kind of baseline.

(defun table-column (table index)
  (loop for colgroup in (table-column-groups table) do
        (loop for col in (table-column-group-columns colgroup) do
              (when (zerop (prog1 index (decf index)))
                (return-from table-column col))))
  (error "Column number ~D is not existing." index))

(defun table-number-of-columns (table)
  (reduce #'+ (mapcar (lambda (colgroup)
                        (length (table-column-group-columns colgroup)))
                      (table-column-groups table))))

(defun table-number-of-rows (table)
  (reduce #'+ (mapcar (lambda (rowgroup)
                        (length (table-row-group-rows rowgroup)))
                      (table-row-groups table))))

(defun table-number-of-rows (table)
  (let ((res 0))
    (map-table table (lambda (cell ri ci)
                       (setf res (max res (+ ri (table-cell-rowspan cell))))))
    res))

;; ### now, this still is somewhat wrong ...

(defun table-number-of-rows (table)
  (max
   (reduce #'+ (mapcar (lambda (rowgroup)
                         (length (table-row-group-rows rowgroup)))
                       (table-row-groups table)))
   (let ((res 0))
     (map-table table (lambda (cell ri ci)
                        (setf res (max res (+ ri (table-cell-rowspan cell))))))
     res)))


(defvar *table-depth* -1)

(defparameter *table-depth-color*
  (list clim:+red+ clim:+blue+ clim:+green+ clim:+cyan+))

(defparameter *debug-table* nil)

;;; Vertical align in a table

;; When a cell needs to be made larger than otherwise this is thought
;; to be accomplished by adjusting the cells top or bottom padding.
;; Also aligning a cell works this way.

;; To minimize movements of output records we want to establish row
;; heights in a fashion that cells spaning more than one row are only
;; accounted for when we reach a row in which they do not participate.

(defun collect-table-2 (item)
  "Collects the table and also calculates the minima/maxima."
  (let ((table (collect-table item)))
    ;;
    (loop for span from 1 to (table-number-of-columns table)
          do
          (loop for row-group in (table-row-groups table) do
                (loop for row in (table-row-group-rows row-group) do
                      (loop for cell in (table-row-cells row) do
                            (let ((ci (table-cell-col-index cell)))
                              (when (= (table-cell-colspan cell) span)
                                ;; First, the cells extrema do not yet include the border.
                                ;; ### separate border modell
                                (let* ((border-space (+ (cooked-style-border-left-width  (table-cell-style cell))
                                                        (cooked-style-border-right-width (table-cell-style cell))))
                                       (min-cell (+ (table-cell-minimum-width cell)
                                                    border-space))
                                       (max-cell (+ (table-cell-maximum-width cell)
                                                    border-space))
                                       (have-border-spacing
                                        ;; amount of border-spacing gutter we can use
                                        (* (1- (table-cell-colspan cell))
                                           (table-horizontal-border-spacing table)))
                                       ;;
                                       (min-yet (+ (loop for i from ci below (+ ci (table-cell-colspan cell))
                                                         sum (table-column-minimum-width (table-column table i)))
                                                   have-border-spacing))
                                       (max-yet (+ (loop for i from ci below (+ ci (table-cell-colspan cell))
                                                         sum (table-column-maximum-width (table-column table i)))
                                                   have-border-spacing))
                                       ;; how much is missing?
                                       (min-lack (max 0 (- min-cell min-yet)))
                                       (max-lack (max 0 (- max-cell max-yet))))
                                  (dotimes (i span)
                                    (incf (table-column-minimum-width (table-column table (+ ci i)))
                                          (/ min-lack span))
                                    (incf (table-column-maximum-width (table-column table (+ ci i)))
                                          (/ max-lack span))) )))))))
    table))

(defun table-horizontal-border-spacing (table)
  "Shorthand accessor: Returns the table's horizontal inner cell gutter as defined by the CSS border-spacing property."
  (first (cooked-style-border-spacing (table-style table))))

(defun table-vertical-border-spacing (table)
  "Shorthand accessor: Returns the table's vertical inner cell gutter as defined by the CSS border-spacing property."
  (second (cooked-style-border-spacing (table-style table))))

;; The cell extrema are measured from within the border inner edges. The column
;; widthen though are measured from the outside of the border. The extrema do
;; not include the border because the table renderer assigns the borders (in the
;; collapsed border model).

;; ### in the collapsing border model it is said that the border is rendered
;;     straight on the grid lines. Also that the width of the table does not
;;     include this border. So should the border hang over? Guess not.

;; ### we'll further unify the border models by splitting the border array into
;;     two halfes. But be careful not to draw the border from two halfs.

;; ### fine now we only need to actually map the cellspacing and cellpadding to
;;     the appropriate css attributes.

(defun table-column-coordinates (table column-widths i &optional (span 1))
  "Return the left and right coordinates of the ith column of table, measured
from outside the border and not including any gutter from border-spacing. The
coordinates are measured relatively from the inner left of the table. Optionally
a span count can be specified, in which case possible extra gutter because of
border-spacing between the spaned columns is included."
  (let* ((x1 (+ (* (1+ i) (table-horizontal-border-spacing table)) ;off by one and one back
                (loop for j from 0 below i sum (elt column-widths j))))
         (x2 (+ x1
                (* (1- span) (table-horizontal-border-spacing table))
                (loop for j from i below (+ i span) sum (elt column-widths j)))))
    (values x1 x2)))

(defun table-row-coordinates (table row-heights i &optional (span 1))
  "Analog to TABLE-COLUMN-COORDINATES."
  (let* ((y1 (+ (* (1+ i) (table-vertical-border-spacing table)) ;off by one and one back
                (loop for j from 0 below i sum (elt row-heights j))))
         (y2 (+ y1
                (* (1- span) (table-vertical-border-spacing table))
                (loop for j from i below (+ i span) sum (elt row-heights j)))))
    (values y1 y2)))

(defun format-table (item x1 x2 yy style &aux (cur-depth *table-depth*))
  (or
   (with-simple-restart (skip "Skip rendering table at nesting depth ~D." (+ cur-depth 1))
     (setf (values x1 x2)
           (find-margins x1 x2 yy))
     (let ((*table-depth* (+ *table-depth* 1))
           (*baseline*-setp (not (null *baseline*)))) ;kludge
       ;;
       (let ((table (collect-table-2 item)))
         ;; now setup the column minimum/maximum widthen.

         ;;
         (let ((column-widths (allocate-table-columns table style x1 x2))
               (row-heights   (loop repeat (table-number-of-rows table) collect 0)))
           ;;
           (when *debug-table*
             (format *trace-output* "~&=== we have a table at depth ~D.~%" *table-depth*)
             (format *trace-output* "~&=== column minima: ~S.~%"
                     (loop for i below (table-number-of-columns table)
                           collect (table-column-minimum-width (table-column table i))))
             (format *trace-output* "~&=== column maxima: ~S.~%"
                     (loop for i below (table-number-of-columns table)
                           collect (table-column-maximum-width (table-column table i))))
             (format *trace-output* "=== space available: ~D.~%" (- x2 x1))
             (format *trace-output* "=== allocated space: ~S.~%" column-widths)
             (finish-output *trace-output*))

           ;; Now we can actually figure out our real margins
           (let ((actual-width (+ (* (1+ (table-number-of-columns table)) (table-horizontal-border-spacing table))
                                  (reduce #'+ column-widths))))
             (setf (values x1 x2)
                   (case (cooked-style-text-align style)
                     (:right
                      (values (- x2 actual-width) x2))
                     (:center
                      (values
                       (+ x1 (floor (- (- x2 x1) actual-width) 2))
                       (- x2 (ceiling (- (- x2 x1) actual-width) 2))))
                     (t
                      (values x1 (+ x1 actual-width))))))


           (let ((bg-record (clim:with-new-output-record (clim-user::*medium*))))
             (setf (table-decoration-output-record table) bg-record)
             (let ((yyy yy)
                   (dangling-cells nil)) ;a list of (rowspan total-rowspan cell) pairs of cells whose row span
                                        ; was larger than one. rowspan is decremented each row and
                                        ; every pair yielding a zero rowspan will be considered for
                                        ; row height calculation.
               (loop for row in (and (table-row-groups table) (table-row-group-rows (first (table-row-groups table))) )
                     for ri from 0 do
                     (loop for cell in (table-row-cells row) do
                           (let* ((ci (table-cell-col-index cell)))
                             (multiple-value-bind (xx1 xx2)
                                 (table-column-coordinates table column-widths ci (table-cell-colspan cell))
                               (let* ((x1 (+ x1 xx1))
                                      (w (- xx2 xx1)))
                                 ;; aha, i guess that we need to nuke style
                                 ;; here ... since width now is relative to
                                 ;; containig block.
                                 ;; ###
                                 (setf (slot-value (table-cell-style cell) 'css::width)
                                       (- w (cooked-style-padding-left (table-cell-style cell))
                                          (cooked-style-padding-right (table-cell-style cell))))

                                 (let ((*baseline* nil)
                                       fl)
                                   (multiple-value-bind (vm+ vm- yy yy0)
                                       (let ((*floating-boxes* nil)) ;### hmm
                                         (multiple-value-prog1 (format-block (table-cell-content cell)
                                                                         x1 (+ x1 w)
                                                                         nil ;ss
                                                                         nil ;before-markers
                                                                         0 0 ;vmargins
                                                                         yyy)
                                           (setf fl *floating-boxes*)))
                                     (incf yy vm+) ;hmm?
                                     (incf yy vm-) ;hmm?
                                     (setf yy (max yy (reduce #'max (mapcar #'floating-chunk-y2 fl) :initial-value yy)))
                                     (setf (table-cell-x cell) x1)
                                     (setf (table-cell-y cell) yyy)
                                     (setf (table-cell-baseline cell) *baseline*);;(if *baseline* (- *baseline* yyy) 0))
                                     (setf (table-cell-minimum-height cell) (- yy yyy))
                                     (cond ((= (table-cell-rowspan cell) 1)
                                            (setf (elt row-heights ri) (max (elt row-heights ri) (table-cell-minimum-height cell))))
                                           (t
                                            (push (list (table-cell-rowspan cell) (table-cell-rowspan cell) cell)
                                                  dangling-cells)))
                                     (when *debug-table*
                                       (unless (or (= x1 (+ x1 w))
                                                   (= yyy yy))
                                         #-NIL
                                         (clim:draw-rectangle* clim-user::*medium*
                                                               x1 yyy (+ x1 w) yy
                                                               :ink (elt *table-depth-color*
                                                                         (mod *table-depth* (length *table-depth-color*)))
                                                               :filled nil))))) ))))
                     (setf dangling-cells
                           (mapcan (lambda (x)
                                     (cond ((= 1 (car x))
                                            (let* ((have (loop for i from ri above (max 0 (- ri (second x))) sum (elt row-heights i)))
                                                   (want (table-cell-minimum-height (third x)))
                                                   (lack (max 0 (- want have))))
                                              (when (> want have)
                                                (loop for i from ri above (max 0 (- ri (second x))) do
                                                      (incf (elt row-heights i) (/ lack (second x))))))
                                            nil)
                                           (t
                                            (list (cons (- (car x) 1) (cdr x))))))
                                   dangling-cells))
                     ;;
                     (incf yyy (elt row-heights ri))
                     finally
                     ;; now some cells might be left
                     (mapcan (lambda (x)
                               (let* ((have (loop for i from ri above (max 0 (- ri (second x))) sum (elt row-heights i)))
                                      (want (table-cell-minimum-height (third x)))
                                      (lack (max 0 (- want have))))
                                 (when (> want have)
                                   (loop for i from ri above (max 0 (- ri (second x))) do
                                         (incf (elt row-heights i) (/ lack (second x)))))))
                             dangling-cells)
                     )
               ;; Grrff

               ;; Let us redo the row heights
               ;; [This is neccessary because the above is just a rough approximation]

               ;; Every cell has a minimum-height (actual height of its content) and a baseline.
               ;;
               (let ((row-baselines (loop repeat (table-number-of-rows table) collect 0)))
                 (setf row-heights  (loop repeat (table-number-of-rows table) collect 0))
                 ;; First establish the baseline of each row
                 (map-table table (lambda (cell ri ci)
                                    (when (and (eql :baseline (cooked-style-vertical-align (table-cell-style cell)))
                                               (table-cell-baseline cell))
                                      (setf (elt row-baselines ri)
                                            (max (elt row-baselines ri)
                                                 (table-cell-baseline cell))))))
                 ;; Establish row-heights. Take care of baseline aligned cells.
                 (loop for rs from 1 to (table-number-of-rows table) do
                       (map-table table (lambda (cell ri ci)
                                          (when (= (table-cell-rowspan cell) rs)
                                            (let* ((have (+ (loop for i from ri below (+ ri rs) sum (elt row-heights i))
                                                            (* (1- (table-cell-rowspan cell))
                                                               (table-vertical-border-spacing table))))
                                                   (want (cond ((and (eql :baseline (cooked-style-vertical-align (table-cell-style cell)))
                                                                     (not (null (table-cell-baseline cell))))
                                                                (+ (elt row-baselines ri)
                                                                   (- (table-cell-minimum-height cell)
                                                                      (table-cell-baseline cell))))
                                                               (t
                                                                (table-cell-minimum-height cell))))
                                                   (lack (max 0 (- want have))))
                                              (when (> want have)
                                                (loop  for i from ri below (+ ri rs) do
                                                       (incf (elt row-heights i) (/ lack rs)))))))))

                 (setf yyy (+ yy (reduce #'+ row-heights)
                              (* (1+ (length row-heights)) (table-vertical-border-spacing table))))

                 ;; Redo background and align cells
                 ;; But: This is not entirely correct!
                 (map-table table
                            (lambda (cell ri ci)
                              (let ((cell-record (block-box-output-record (table-cell-content cell)))
                                    (bg-record (block-box-decoration-output-record (table-cell-content cell))))
                                (multiple-value-bind (y1 y2)
                                    (table-row-coordinates table row-heights ri (table-cell-rowspan cell))
                                  (incf y1 yy)
                                  (incf y2 yy)
                                  ;;
                                  (let* ((rh (- y2 y1))
                                         (dy
                                          (case (cooked-style-vertical-align (table-cell-style cell))
                                            (:bottom
                                             (- rh (table-cell-minimum-height cell)))
                                            (:middle
                                             (floor (- rh (table-cell-minimum-height cell)) 2))
                                            (:baseline
                                             (cond ((not (null (table-cell-baseline cell)))
                                                    (- (elt row-baselines ri) (table-cell-baseline cell)))
                                                   (t
                                                    (warn "Funny, cell has baseline aligning, but no baseline.")
                                                    ;; threat this as top
                                                    0)))
                                            (otherwise
                                             0)))
                                         (y-soll ;where do we want this cell?
                                          (+ y1
                                             dy)))
                                    ;; CLIM makes no guarantees about an output records position, so
                                    ;; we only can move relative.
                                    (multiple-value-bind (x y) (clim:output-record-position cell-record)
                                      (setf (clim:output-record-position cell-record)
                                            (values x (+ y (- y-soll (table-cell-y cell)))))))
                                  ;;
                                  (clim:clear-output-record bg-record)
                                  (multiple-value-bind (xx1 xx2) (table-column-coordinates table column-widths ci (table-cell-colspan cell))
                                    (let ((new-record
                                           (clim:with-output-recording-options (clim-user::*medium* :record t :draw nil)
                                             (clim:with-new-output-record (clim-user::*medium*)
                                               (draw-box-decoration clim-user::*medium* (+ x1 xx1) y1 (+ x1 xx2) y2
                                                                       (block-box-style (table-cell-content cell)))))))
                                      (clim:delete-output-record new-record (clim:output-record-parent new-record))
                                      (clim:add-output-record new-record bg-record)))))))


                 ;; draw the tables background
                 (let* ((y1 yy)
                        (y2 yyy)
                        (x1 x1)
                        (x2 x2))
                   (let ((new-record
                          (clim:with-output-recording-options (clim-user::*medium* :record t :draw nil)
                            (clim:with-new-output-record (clim-user::*medium*)
                              (draw-box-decoration clim-user::*medium* x1 y1 x2 y2
                                                      (table-style table))))))
                     (clim:delete-output-record new-record (clim:output-record-parent new-record))
                     (clim:add-output-record new-record bg-record)))

                 ;; draw borders
                 '(multiple-value-bind (hborders vborders) (table-borders table)
                   ;; horizontal borders
                   (loop for i from 0 below (array-dimension hborders 0) do
                         (loop for j from 0 below (array-dimension hborders 1) do
                               (destructuring-bind (origin style width color) (aref hborders i j)
                                 (declare (ignore origin))
                                 ;; This is the border between row (i-1) and row i running across column j
                                 (unless (eql style :none)
                                   (multiple-value-bind (x1 x2) (table-column-coordinates table column-widths j)
                                     (let* (
                                            (y1 (+ yy (loop for k below i sum (elt row-heights k)))))
                                       (clim:draw-line* clim-user::*medium*
                                                        x1 y1 x2 y1
                                                        :ink (clim-user::parse-x11-color color)
                                                        :line-thickness width)))))))
                   ;; vertical borders
                   (loop for i from 0 below (array-dimension vborders 0) do
                         (loop for j from 0 below (array-dimension vborders 1) do
                               (destructuring-bind (origin style width color) (aref vborders i j)
                                 (declare (ignore origin))
                                 ;; This is the border between column (j-1) and column j running across row i
                                 (unless (eql style :none)
                                   (let* ((y1 (+ yy (loop for k below i sum (elt row-heights k))))
                                          (y2 (+ y1 (elt row-heights i)))
                                          (x1 (+ x1 (loop for k below j sum (elt column-widths k)))))
                                     (clim:draw-line* clim-user::*medium*
                                                      x1 y1 x1 y2
                                                      :ink (clim-user::parse-x11-color color)
                                                      :line-thickness width)))))) )
                 ;; Kludge, in our book a table also has a baseline. We set it up manually, since
                 ;; we moved the rendered output of table cells.
                 ;; ### only that this now spoils the telepolis site ;(
                 ;; ### i assume that tables have no baseline. so we really need to account for
                 ;;     cells which do not have a baseline and align them top.
                 ;; ### and should cells which are not baseline aligned be accounted for at all?
                 ;; #+NIL
                 (and row-baselines
                      (unless *baseline*-setp
                        (setf *baseline* (+ yy (elt row-baselines 0)))))
                 ;;
                 yyy)))))))
   yy))

(defun map-table (table continuation)
  "Applies 'continuation' to every table cell and its gird position"
  (loop for row-group in (table-row-groups table) do
        (loop for row in (table-row-group-rows row-group)
              for ri from 0 do
              (loop for cell in (table-row-cells row) do
                    (funcall continuation cell ri (table-cell-col-index cell))))))


(defun allocate-table-columns (table style x1 x2)
  (let ((table.width (slot-value style 'css::width))
        (ncols       (table-number-of-columns table)))
    (setf table.width (maybe-resolve-percentage table.width (- x2 x1)))
    (let* ((mins (loop for i below (table-number-of-columns table)
                       collect (table-column-minimum-width (table-column table i))))
           (maxs (loop for i below (table-number-of-columns table)
                       collect (table-column-maximum-width (table-column table i))))
           (min (reduce #'+ mins))
           (max (reduce #'+ maxs))
           (gutter (* (1+ (table-number-of-columns table)) (table-horizontal-border-spacing table))))
      ;;
      (setf table.width
            (cond
              ;; | 2. If the 'table' or 'inline-table' element has 'width: auto', the
              ;; |    computed table width is the greater of the table's containing block
              ;; |    width and MIN. However, if the maximum width required by the
              ;; |    columns plus cell spacing or borders (MAX) is less than that of the
              ;; |    containing block, use MAX.
              ;; ### now this somehow got lost
              ;; ### what really needs to be done: Our accessors
              ;;     compute the dimensions for us, so they should compute
              ;;     in this case too.
              ((eql table.width :auto)
               (format *trace-output* "~&Using auto layout (table.width = ~d) ~%" table.width)
               (if (< (+ max gutter) (- x2 x1))
                   (+ max gutter)
                   (max (- x2 x1)
                        min)))
              ;;
              ;; CSS-2 17.5.2:
              ;; | 1. If the 'table' or 'inline-table' element's 'width' property has a
              ;; |    specified value (W) other than 'auto', the property's computed
              ;; |    value is the greater of W and the minimum width required by all the
              ;; |    columns plus cell spacing or borders (MIN). If W is greater than
              ;; |    MIN, the extra width should be distributed over the columns.
              ((realp table.width)
               ;;(format *trace-output* "~&Using fixed layout~%")
               (max table.width
                    min) )
              (t
               (error "BARF"))))
      (decf table.width gutter)
      ;;
      (let* ((excess (- table.width min))
             (deltas (loop for min in mins for max in maxs collect (- max min)))
             (delta-sum (reduce #'+ deltas))
             (res
              (loop for delta in deltas
                    for min   in mins
                    for max   in maxs
                    collect
                    (if (zerop delta-sum)
                        (+ min (* excess (/ (length mins)))) ;### hmm
                        (+ min (* excess delta (/ delta-sum)))))))
        res))))

(defun map-table-cells (function table)
  "Apply function 'function' to each of the cells of the table 'table'."
  (dolist (row-group (table-row-groups table))
    (dolist (row (table-row-group-rows row-group))
      (dolist (cell (table-row-cells row))
        (funcall function cell)))))

(defun table-cell-minimum-width (cell)
  (with-slots (%minimum-width) cell
    (unless %minimum-width
      (table-cell-setup-min/max-width cell))
    %minimum-width))

(defun table-cell-maximum-width (cell)
  (with-slots (%maximum-width) cell
    (unless %maximum-width
      (table-cell-setup-min/max-width cell))
    %maximum-width))

(defun table-cell-setup-min/max-width (cell)
  (with-slots (%minimum-width %maximum-width) cell
    (setf (values %minimum-width %maximum-width)
          (minmax-block (table-cell-content cell)))))

(defun collect-table (item)
  (let ((row-groups nil)
        (col-groups nil))
    (assert (and (block-box-p item)
                 (eq (cooked-style-display (block-box-style item)) :table)))
    (let ((cur-col-group nil))
      (labels ((flush-cur-col-group ()
                 (when cur-col-group
                   (push (make-instance 'table-column-group
                                        :columns (reverse cur-col-group))
                         col-groups)
                   (setf cur-col-group nil))))
        (dolist (x (block-box-content item))
          (when (block-box-p x)
            (case (cooked-style-display (block-box-style x))
              (:table-row-group
               (push (collect-row-group x) row-groups))
              (:table-column-group
               (flush-cur-col-group)
               (push (collect-column-group x) col-groups))
              (:table-column
               (push (collect-column x) cur-col-group)))))
        (flush-cur-col-group)))

    ;;; Assign the col-index slots of cells
    (let ((row-allocation nil))         ;a list of integers indicating allocated rows.
                                        ; when a new cell with a certain rowspan r at
                                        ; column i is encountered we enter r at the i'th
                                        ; element of this list
      ;; Note: it feels a bit strange that cells can span across a I
      ;; expect that to be fixed by a later version; and check what
      ;; HTML does there.

      ;; ### if some guy passes in some pretty large colspan we lose.
      ;;     We can fight that by run-length encode (sic!) the
      ;;     row-allocation vector.
      (loop for row-group in row-groups do
            (loop for row in (table-row-group-rows row-group) do
                  ;; we enter a new row, so decrement the row-allocation
                  (setf row-allocation (mapcar (lambda (x) (max 0 (- x 1))) row-allocation))
                  ;;
                  (labels ((allocate-row (cells row-allocation col-index)
                             "Allocate cells, returns the new row allocation vector"
                             (cond ((null cells)
                                    row-allocation)
                                   ;; does the cell fit?
                                   ((every #'zerop (subseq row-allocation 0 (table-cell-colspan (car cells))))
                                    (let ((cell (car cells)))
                                      ;; It does, so store col-index and stuff the rowspan into the vector.
                                      (setf (table-cell-col-index cell) col-index)
                                      (append (make-list (table-cell-colspan cell) :initial-element (table-cell-rowspan cell))
                                              (allocate-row (cdr cells)
                                                            (subseq row-allocation (table-cell-colspan cell))
                                                            (+ col-index (table-cell-colspan cell))))))
                                   (t
                                    ;; It does not fit, try the next column
                                    (cons (car row-allocation)
                                          (allocate-row cells (cdr row-allocation) (+ col-index 1)))))))
                    ;;
                    (setf row-allocation
                          (allocate-row (table-row-cells row) row-allocation 0))))) )


    
    
    ;; we need now two sanity measures:
    ;; a. cut excessive colspan.
    ;; b. cut excessive rowspan.
    ;; c. make additional rows needed.
    ;; d. make additional columns needed.
    ;;
    (setf row-groups (reverse row-groups))
    (let ((need 0))
      (dolist (row-group row-groups)
        (dolist (row (table-row-group-rows row-group))
          (dolist (cell (table-row-cells row))
            (setf need (max need (+ (table-cell-col-index cell)
                                    (table-cell-colspan cell)))))))

    ;; Sanity check: We could the columns by column groups and by
    ;; colspan. When there is a difference => Report the error and fix
    ;; it.

      ;; Count the columns accounted for by existing column-groups
      (let ((have (reduce #'+ (mapcar #'length (mapcar #'table-column-group-columns col-groups)))))
        (cond ((< have need)
               ;; add an extra column group
               (unless (zerop have)
                 (warn "Columns and/or columns groups were explicitly specified yet ~
                        do not match actual number of columns."))
               (push (make-instance 'table-column-group
                                                   :columns
                                                   (loop repeat (- need have) collect
                                                         (make-instance 'table-column)))
                                    col-groups))
              ((> have need)
               (warn "Columns and/or columns groups were explicitly specified yet there actually less columns."))))

      (setf col-groups (reverse col-groups)))
    (make-instance 'table
                   :style (block-box-style item)
                   :element (block-box-element item)
                   :row-groups row-groups
                   :column-groups (reverse col-groups))))

(defun collect-row-group (item)
  (make-instance 'table-row-group
                 :style (block-box-style item)
                 :element (block-box-element item)
                 :rows (mapcar (lambda (x)
                                 (assert (and (block-box-p x)
                                              (eq (cooked-style-display (block-box-style x)) :table-row)))
                                 (collect-row x))
                               (block-box-content item))))

(defun collect-row (item)
  (make-instance 'table-row
                 :style (block-box-style item)
                 :element (block-box-element item)
                 :cells (mapcar (lambda (x)
                                 (assert (and (block-box-p x)
                                              (eq (cooked-style-display (block-box-style x)) :table-cell)))
                                  (collect-cell x))
                                (block-box-content item))))

(defun collect-cell (item)
  (make-instance 'table-cell
                 :colspan (pt-attr/integer (block-box-element item) :colspan 1)
                 :rowspan (pt-attr/integer (block-box-element item) :rowspan 1)
                 :content item))

(defun collect-column-group (item)
  (make-instance 'table-column-group
                 :style (block-box-style item)
                 :element (block-box-element item)
                 :columns (mapcar (lambda (x)
                                    (collect-column x))
                                  (block-box-content item))))

(defun collect-column (item)
  (assert (and (block-box-p item)
               (eq (cooked-style-display (block-box-style item)) :table-column)))
  (make-instance 'table-column
                 :style (block-box-style item)))


;;;;

(defun minmax-block (block-box)
  (minmax-block-content (block-box-content block-box) (block-box-style block-box)))

(defun minmax-block-content (items block-style)
  (let ((min 0)
        (max 0))
    (let* ((s block-style))
      (multiple-value-bind (ml bl pl wd pr br mr) (resolve-widthen s 0)
        ;; ### we need a more throughly analysis here.
        ;; ### also it is questionable if a block which specifies its
        ;;     width should have different min/max widthen altogether.

        ;; ### Now this was a stupid idea, since the sum of
        ;;     resolve-widthen is always 0 (what we gave as
        ;;     argument for containing block width).
        (setf ml 0 mr 0)                ;xxx (s.a.)

        ;; Further more if this happens to be a table cell, we do not include
        ;; the border of the cell as the border is set up by the table
        ;; typesetter. Yes, I know this is kludgey.
        (cond ((eql :table-cell (cooked-style-display block-style))
               (setf bl 0 br 0)))
        ;;
        (when *debug-table*
          (format *trace-output* "~&~S (~S): ml=~s, bl=~s, pl=~s, wd=~s, pr=~s, br=~s, mr=~s~%"
                  'minmax-block-content block-style ml bl pl wd pr br mr))
        (dolist (item items)
          (etypecase item
            (para-box
             (multiple-value-bind (mi ma) (minmax-para (para-box-items item) block-style)
               (setf min (max min mi))
               (setf max (max max ma))))
            (marker-box
             ;; ### hmm
             )
            (block-box
             (case (cooked-style-display (block-box-style item))
               (:table
                (multiple-value-bind (mi ma) (minmax-table item (block-box-style item))
                  (setf min (max min mi)
                        max (max max ma))))
               (t
                (multiple-value-bind (mi ma)
                    (minmax-block-content (block-box-content item) (block-box-style item))
                  (setf min (max min mi))
                  (setf max (max max ma))))))))

        (let ((explicit-width (slot-value block-style 'css::width)))
          (when (realp explicit-width)
            (setf min (max min explicit-width)
                  max (max min explicit-width))))

        (values (+ min ml bl pl pr br mr)
                (+ max ml bl pl pr br mr))))))

(defun minmax-table (item block-style)
  (let ((table (collect-table-2 item)))
    (multiple-value-bind (min max)
        (values
         (loop for i below (table-number-of-columns table)
               sum (table-column-minimum-width (table-column table i)))
         (loop for i below (table-number-of-columns table)
               sum (table-column-maximum-width (table-column table i))))
      (incf min (* (1+ (table-number-of-columns table)) (table-horizontal-border-spacing table)))
      (incf max (* (1+ (table-number-of-columns table)) (table-horizontal-border-spacing table)))
      (let ((explicit-width (slot-value block-style 'css::width)))
        (when (realp explicit-width)
          (setf min (max min explicit-width)
                max (max min explicit-width))))
      (values min max))))

(defun minmax-para (items block-style)
  (let ((clw 0)
        (cww 0)
        (min 0)
        (max 0)                         ;max is simply the sum.
        (cur-line nil)
        (cur-word nil)
        (ss (list block-style)))
    ;; ### we shouldn't borrow that blindly from format-para
    ;; ### replaced object might have percentage widthen
    (labels ((flush-line ()
               (setf min (max min (reduce #'+ (mapcar #'chunk-width cur-line))))
               (setf cur-line nil
                     clw 0))
             (flush-word ()
               (dolist (k (reverse cur-word)) (push k cur-line))
               (setf cur-word nil)
               (incf clw cww)
               (setf cww 0))
             ;;
             (process (xs)
               (let ((x (car xs)))
                 (typecase x
                   (open-chunk
                    (incf max (chunk-width x))
                    (push (bounding-chunk-style x) ss)
                    (incf cww (chunk-width x))
                    (push x cur-word))
                   (close-chunk
                    (incf max (chunk-width x))
                    (pop ss)
                    (incf cww (chunk-width x))
                    (push x cur-word))
                   (disc-chunk
                    ;; ### we assume a whole lot about disc-chunk-here
                    (incf max (reduce #'+ (mapcar #'chunk-width (remove-if #'disc-chunk-p (disc-chunk-here x))))) ;###
                    (flush-word)
                    (let ((p (disc-chunk-here x))
                          (ww 0))
                      (block zulu
                        (dolist (k p)
                          (cond ((typep k 'disc-chunk)
                                 ;; before is missing.
                                 ;; actually this is a stranger situation ...
                                 (return-from zulu))
                                ;; ###
                                ((typep k 'floating-chunk)
                                 nil)
                                (t
                                 (incf ww (chunk-width k))) ))
                        ;; disc exhausted continue with list
                        (dolist (k (cdr xs))
                          (cond ((typep k 'disc-chunk)
                                 ;; before is missing.
                                 ;; actually this is a stranger situation ...
                                 (return-from zulu))
                                ;; ###
                                ((typep k 'floating-chunk)
                                 nil)
                                (t
                                 (incf ww (chunk-width k))))))
                      (show (disc-chunk-before x))
                      (flush-word)
                      (flush-line)
                      (show (disc-chunk-after x))))
                   (replaced-object-chunk
                    (let ((w (chunk-width x)))
                      (when *debug-table*
                        (format *trace-output* "~&~S: ~S = ~S~%"
                                'minmax-para `(chunk-width ,x) (chunk-width x)))
                      (setf w (ro/size (replaced-object-chunk-object x)))
                      (incf max w)
                      (incf cww w)
                      (push x cur-word)))
                   (kern-chunk
                    (incf max (chunk-width x))
                    (incf cww (chunk-width x))
                    (push x cur-word))
                   (black-chunk
                    (incf max (chunk-width x))
                    (incf cww (chunk-width x))
                    (push x cur-word))
                   (floating-chunk
                    (compute-floating-chunk-width x 0)
                    (incf max (floating-chunk-width x))
                    (setf min (max min (floating-chunk-width x))))
                   ))
               xs)
             ;;
             (show (seq)
               (do ((q seq (cdr q)))
                   ((null q))
                 (setf q (process q)) )) )
      (show items)
      (flush-word)
      (flush-line))
    (values min max)) )

(defun draw-box-decoration (medium x1 y1 x2 y2 style
                               &key (left-halfp t) (right-halfp t))
  ;; die coordinaten sind ganz innen.
  (clim-draw-background medium
                        x1 y1 x2 y2
                        (cooked-style-background-color style))
  (clim-draw-border medium
                    x1 y1 x2 y2
                    (cooked-style-border-top-width style)
                    (cooked-style-border-top-style style)
                    (cooked-style-border-top-color style)
                    (if right-halfp (cooked-style-border-right-width style) 0)
                    (cooked-style-border-right-style style)
                    (cooked-style-border-right-color style)
                    (cooked-style-border-bottom-width style)
                    (cooked-style-border-bottom-style style)
                    (cooked-style-border-bottom-color style)
                    (if left-halfp (cooked-style-border-left-width style) 0)
                    (cooked-style-border-left-style style)
                    (cooked-style-border-left-color style)))


;;;;


(defclass* floating-chunk ()
  style                                 ;always the same as the (block-box-style (floating-chunk-content .))
  content                               ;always a single block-box
  %width                                ;width cache [outer width]
  x1 y1 x2 y2)                          ;the bounding rectangle of this floating box.
                                        ; (applicable if mounted).

(defun floating-chunk-width (chunk)
  (or (slot-value chunk '%width)
      (error "FLOATING-CHUNK has no width?")))

(defun compute-floating-chunk-width (chunk containing-block-width)
  (setf (slot-value chunk '%width)
        (let* ((style (floating-chunk-style chunk))
               (ml (cooked-style-margin-left  style))
               (pl (cooked-style-padding-left style))
               (bl (cooked-style-border-left-width style))
               (wd (cooked-style-width style))
               (br (cooked-style-border-right-width style))
               (pr (cooked-style-padding-right style))
               (mr (cooked-style-margin-right style)))
          (+
           ml pl bl wd br pr mr))))


;;;;

(defun flatten-code (form)
  (ecase (car form)
    ((case ecase)
     (let ((temp 'aux)
           (labels (mapcar (lambda (x) (gentemp "L.")) (cddr form)))
           (raus (gentemp "L.")))
       (values
        `(
          (setf ,temp ,(cadr form))
          ,@(mapcar (lambda (clause label)
                      (cond ((atom (car clause))
                             `(when (eql ,temp ',(car clause)) (go ,label)))
                            (t
                             `(when (member ,temp ',(car clause)) (go ,label))))
                      )
                    (cddr form) labels)
          ,@(mapcan (lambda (clause label)
                      (append (list label)
                              (copy-list (cdr clause))
                              (list `(go ,raus))))
                    (cddr form) labels)
          ,raus))))))


(defun cooked-style-block-element-p (cooked-style)
  (member (cooked-style-display cooked-style)
          '(:block :list-item
            :table :table-row :table-header :table-row-group :table-footer
            :table-column :table-column-group :table-cell
            :table-caption)))


(defun po (seq ignore)
  (popo-block seq nil nil))

(defun element-warn (element format &rest args)
  (declare (ignore element))
  (apply #'warn format args))

(defun popo-block (q ss cbss)
  ;; popo-block now returns a block-box
  (let (res nres me mes
            before-markers)
    ;; first off the first thing must be a block-open
    (unless (eq (caar q) :open)
      (error "Barf!"))
    (push (my-setup-style (cadar q) (car ss) cbss) ss)
    ;;
    (setf mes (car ss))
    (unless (cooked-style-block-element-p (car ss))
      (error "Barf!"))
    (setf me (cadar q))
    (pop q)

    ;; Look at a possible :before element.
    ;; ### look for a way to refactor that for the :after element also.
    ;; Q: can generated content float? I mean if it floats it must be of display:block;
    ;; Q: why is display:none; ruled out?
    ;;; Note

    ;; CSS2 has this funny restriction that a marker box can only emerge from
    ;; generated content. It would be better [for a possible typesetter] if we
    ;; allow marker boxen to emerge from real element trees so that they can
    ;; have more rich content.

    (unless (typep me 'pseudo-element)  ;pseudo elements itself have no generated content
      (let* ((pe (if (eql (cooked-style-display (car ss)) :list-item)
                     (make-list-item-marker-element me (car ss))
                     (make-instance 'before-pseudo-element :proxee me)))
             ;; ### what is the containing-block of such a pseudo-element?
             (pe-style (my-setup-style pe (car ss) cbss)))
        ;; ### now list items are supposed to map into such before/after elements also ...
        (unless (null (cooked-style-content pe-style))
          (let ((display (cooked-style-display pe-style)))
            (unless (member display '(:block :inline :marker))
              (element-warn "display property is ~S treated as ~S."
                            display :block)
              ;; xxx -- we should not modify a cooked-style.
              ;;        When we want such a rule then during cooking!
              (setf (cooked-style-display pe-style)
                    (setf display :block)))
            ;;
            (case display
              ((:block)
               ;; We do as when another block occurred here. Implementation: make
               ;; recursive call to popo-block. Watch out for possible double
               ;; application of before/after.
               )
              ((:inline)
               ;; We just do as when the appropriate inline element appeared here.
               ;; To achieve that we must somehow enter paragraph context and call
               ;; popo-para. Possible complication: white-space convention is
               ;; slightly off for generated content. and: avoid double
               ;; application of :before/:after. (Although X:before:before would
               ;; be logical ;-)

               ;; we achieve that by just pushing the generated content onto our
               ;; input.
               (push (list :close pe) q)
               ;; Note: due to an evil hack in popo-para, we do not add the
               ;; content itself since it will then be handled by an appropriate
               ;; clause in popo-para. Cleaner would be to add another
               ;; 'pre-chunk' stating this is content.
               (push (list :open pe) q) )
              ((:marker)
               ;; Markers are collected for the next paragraph that passes by
               ;; since they are then included into para-box so that the
               ;; paragraph breaking algorithm can see the marker too. This
               ;; has to be this way because the marker participates in the
               ;; line-height/vertical-alignment calculation of the first/last
               ;; line of the paragraph.
               ;; they are to be regarded as inline elements I guess ..
               ;; Q: Is there any first-line/first-letter stuff?
               ;; I guess not: It would simply not look right ...
               ;; oops it is indeed fine to attach them here, only that we are clueless
               ;; about the margin ... and it depends on the block we attached that to.
               ;; so alternate route: add markers to the block and handle them further up.

               ;; another alternative:
               ;; link from the marker box to its block.
               ;; when formatting we can peek there for the margin.

               ;; ### even if content is not-null popo-para might still return NIL.
               (let ((content (popo-para
                               (list (list :open pe) (list :close pe))
                               ss
                               me
                               cbss
                               :run :invariant ;we do not want first-line stuff applied here
                               )))
                 (cond ((null content)
                        ;; ### what does that mean?
                        )
                       (t
                        (push
                         (make-marker-box
                          :style pe-style
                          :side :left
                          :content content)
                         res))))))
            ;; we probably can cope with compact boxen in a similar way?
            ;; Now, a compact box that fits is like a marker box?
            ;; yes, they are.
            ))))

    ;;

    (tagbody
     L.0
       (when (null q)
         ;; stuff just ended
         (error "Barf 4") )

       (let* ((kind (caar q)) (data (cadar q))
              (s (and (member kind '(:open :close))
                      (if (eq data me)
                          mes
                          (my-setup-style data (car ss) mes)))))

         (cond ((and (eq kind :open)
                     (and (cooked-style-block-element-p s)
                          (eq (cooked-style-float s) :none)))
                ;; another block goes open
                (setf (values nres q) (popo-block q ss #| containing block style: |# mes))
                (push nres res)
                (go L.0))
               ;;
               ((and (eq kind :close)
                     (cooked-style-block-element-p s))
                ;; this must be our block

                (unless (eq (slot-value (car ss) 'css::%element)
                            me)
                  (error "buuz"))
                (return-from popo-block
                  (values (make-block-box
                           :element me
                           :style (car ss)
                           :content (reverse res))
                          (cdr q))))
               ;;
               ((and (eq kind :open)
                     (eq (cooked-style-display s) :none))
                (setf q (member-if (lambda (y) (eq (second y) data)) (cdr q))) )

               ((or (and (member kind '(:open :close))
                         (or (eq (cooked-style-display s) :inline)
                             (not (eq (cooked-style-float s) :none))))
                    (eq kind :data))
                (setf (values nres q) (popo-para q ss me mes))
                (dolist (k nres) (push k res))
                (go L.0))

               (t
                (error "Unexpected item: ~S." (car q))) ))

     L.continue
       (setf q (cdr q))
       (go L.0)) ))

(defun make-open-chunk (element style)
  (make-instance 'open-chunk :style style :pt element))

(defun make-close-chunk (element style)
  (make-instance 'close-chunk :style style :pt element))

(defun make-half-open-chunk (element style)
  (make-instance 'open-chunk :halfp t :style style :pt element))

(defun make-half-close-chunk (element style)
  (make-instance 'close-chunk :halfp t :style style :pt element))

(defun bounding-chunk-p (x)
  (typep x 'bounding-chunk))

(defun open-chunk-p (x)
  (typep x 'open-chunk))

(defun close-chunk-p (x)
  (typep x 'close-chunk))

;; for first char:

;; When we detect that we are about to insert the first character we
;; just insert the :first-char pseudo open chunk. When while
;; continuing to insert chunks we find a close chunk we need to adjust
;; our style stack accordingly. It might also be possible that an open
;; chunk gets inserted. Also tricky: although there might be an open
;; chunk, that does not neccessarily mean that it is to be included in
;; the :first-char stuff. But generally we can say: we have flags:
;;
;;   first-char-pseudo-element-inserted-p
;;      whether a first char pseudo element chunk was inserted.
;;
;;   first-char-p
;;      whether still in first char situation.
;;

;; We might however be better off by doing all this in a pre- or
;; post-analysis.

(defun make-black-chunk* (char style)
  (cons-black-chunk
   :style style
   :data (map '(simple-array (unsigned-byte 16) (*))
              #'identity
              (list char))))

;;; first-letter pseudo elements

;; Proper support for first-letter is not that easy because it is said
;; that leading punctation characters should be included in what is
;; considered the first letter which means that the first letter is
;; not neccessarily a single letter. The further implication of this
;; is that there might be other elements going open or close between
;; individual letters of the 'first letter' span.

;; Since at least individual chunks must be nested properly we need to
;; insert half-chunks.

;; To illustrate that consider the following input:

;;   "<B>we should check ...

;; The emited chunks now are:

;;   <P:first-letter>"(/P:first-letter)<B>(P:first-letter)w</P:first-letter>

;; Where chunks in round parens are half chunks. This has some
;; implications:

;;  a. When there is a first-letter element active, and other open or
;;     close chunks are due we first insert a half-close chunk for the
;;     first-letter, the other chunk and then reopen the first-letter
;;     by inserting a half-open chunk.

;;  b. This further implies that while in first-letter context the
;;     first-letter pseudo element is always last recently opened one.
;;     This is in the spirit of the CSS-2 specification.

;;  c. Although half-chunks no nest properly, the corresponding full
;;     chunks may no longer nest properly. That is the higher level
;;     paragraph layout and in particular the line drawing code has to
;;     be able to cope with that situation, which should case no
;;     particular problems.

;;  [although one might argue that the author force such situations,
;;  not caring for them would seriously reduce the usefulness of
;;  generated content like in:
;;
;;      Q:before { content: '"'; }
;;
;;      <Q>we should check for ...
;;
;;  ]


;;;

;; Now in case of a first-line element we need to actually process the
;; paragraph Once with the first-line element included and once with the
;; first line element not included. The disk-chunks of the first-line run
;; need to point to the appropriate stuff with their after link which just
;; is same as the after link of the not-first-line run.

;; For more precise: the after link of the nth disc-chunk in the first-line
;; run is the same as the after link of the nth disc-chunk in the
;; not-first-line run.

;; So our strategy now is to first do the not-first-line run and either keep
;; the disk-chunks. A second run is then done with the first-line element
;; included and the disk-chunks are then set up as outlined above.

;; What we want in the future:
;;
;;  a. only do two runs if the first-line element makes a difference all.
;;  b. perhaps if this ever gets a performance problem use lazy evaluation.

;; Further note: Although it is not noted in the "specification" i simply
;; assume that first-line style should not apply to floating boxen. It also
;; probably should not apply to blocks (as they always (at least should)
;; leave the first line condition).

;; Generally the so called specification is not that clear about where
;; exactly first line style has to apply.


;; we can probably refactor this into:

;; handle-black
;; handle-white
;; handle-break
;; handle-open
;; handle-close
;; handle-suspend       ;when the paragraph is to be suspended because of a block box
;; handle-resume        ;resume the paragraph.
;;


(defmacro %handle-data/normal/none (first-line-applicable-p
                                    first-letter-applicable-p
                                    letter-spacing-applicable-p
                                    white-space)
  `(progn
    ,(AND LETTER-SPACING-APPLICABLE-P
          '(setf
            letter-spacing (if (eql :normal letter-spacing) 0 letter-spacing) ;### hmm this shouldn't be neccessary.
            word-spacing (+
                          (if (eql :normal word-spacing) 0 word-spacing)
                          letter-spacing)))
    (let ((blacki 0))
      (loop
          for c across data
          for i fixnum from 0 do
          (cond
            ,@(AND (EQL :PRE WHITE-SPACE)
                   (list `((= c 10)
                           (let ((ocontext context))
                             ,(OR LETTER-SPACING-APPLICABLE-P
                                  '(unless (= blacki i)
                                    (push (cons-black-chunk :style (car ss)
                                           :data (subseq data blacki i))
                                     res)))
                             ,(AND (AND FIRST-LINE-APPLICABLE-P)
                                   `(when first-line-element
                                     ;; redo both style and context
                                     ;; first remove everything until first-line-element found
                                     ;; oops bug wrt to first-line ....
                                     (setf (values context ss) (frob context ss))
                                     (setf first-line-element nil)))
                            (push (make-instance
                                   'disc-chunk
                                   :%before (mapcar #'(lambda (k)
                                                        (make-half-close-chunk (bounding-chunk-pt k)
                                                                               (bounding-chunk-style k)))
                                                    ocontext)
                                   :%after (mapcar #'(lambda (k)
                                                       (make-half-open-chunk (bounding-chunk-pt k)
                                                                             (bounding-chunk-style k)))
                                                   (reverse context))
                                   :%here nil
                                   :forcep t)
                                  res)
                             (setf last-was-space-p t)
                             (setf blacki (+ i 1))) )))

            ((white-space-rune-p c)
             (let ((ocontext context) (ncontext context) (nss ss))
               (unless ,(IF (EQL :PRE WHITE-SPACE) 'nil 'last-was-space-p)
                 ,(OR LETTER-SPACING-APPLICABLE-P
                      `(unless (= blacki i)
                        (push (cons-black-chunk :style (car ss)
                               :data  (subseq data blacki i))
                         res)))
                 (setf last-was-space-p t)
                 ;; what is that supposed to do?
                 #+NIL
                 ,(AND (AND FIRST-LINE-APPLICABLE-P
                            (EQL WHITE-SPACE :NORMAL))
                       `(when first-line-element
                         ;; redo both style and context
                         ;; first remove everything until first-line-element found
                         ;; oops bug wrt to first-line ....
                         (setf (values ncontext nss) (frob context ss))
                         (setf first-line-element nil)))
                 ;;
                 ,(ECASE WHITE-SPACE
                         (:NORMAL
                          `(let ((context context))
                            ;; ### Now, this is too simplistic, this might as
                            ;; well be the very last white space in this block,
                            ;; in which case we do not want to have this disk
                            ;; chunk.
                            ;;
                            ;; When everything which follows is white or some
                            ;; open/close stuff we should skip this. But: we
                            ;; have to watch generated content.
                            ;;
                            ;; The implementation approach needed is: keep this
                            ;; disk chunk and spill it if something black comes
                            ;; That is when last-was-space-p turns nil again.
                            ;; ### white space convention?
                            #+NIL
                            (setf dangling-disc
                             (make-instance
                                   'disc-chunk
                                   :%before (mapcar #'(lambda (k)
                                                        (make-half-close-chunk (bounding-chunk-pt k)
                                                                               (bounding-chunk-style k)))
                                                    ocontext)
                                   :%after (mapcar #'(lambda (k)
                                                       (make-half-open-chunk (bounding-chunk-pt k)
                                                                             (bounding-chunk-style k)))
                                                   (reverse ncontext))
                                   :%here ,(IF LETTER-SPACING-APPLICABLE-P
                                               `(if (eql word-spacing :normal)
                                                    (list (make-black-chunk* 32 (car ss)))
                                                    (list (make-black-chunk* 32 (car ss))
                                                          (make-kern-chunk word-spacing)))
                                               `(list (make-black-chunk* 32 (car ss))))))
                            #-NIL
                            (push (make-instance
                                   'disc-chunk
                                   :%before (mapcar #'(lambda (k)
                                                        (make-half-close-chunk (bounding-chunk-pt k)
                                                                               (bounding-chunk-style k)))
                                                    ocontext)
                                   :%after (mapcar #'(lambda (k)
                                                       (make-half-open-chunk (bounding-chunk-pt k)
                                                                             (bounding-chunk-style k)))
                                                   (reverse ncontext))
                                   :%here ,(IF LETTER-SPACING-APPLICABLE-P
                                               `(if (eql word-spacing :normal)
                                                    (list (make-black-chunk* 32 (car ss)))
                                                    (list (make-black-chunk* 32 (car ss))
                                                          (make-kern-chunk word-spacing)))
                                               `(list (make-black-chunk* 32 (car ss)))))
                             res)))
                         ((:PRE)
                          `(progn
                            ,(IF LETTER-SPACING-APPLICABLE-P
                                 `(if (eql word-spacing :normal)
                                   (push (make-black-chunk* 32 (car ss)) res)
                                   (progn
                                     (push (make-black-chunk* 32 (car ss)) res)
                                     (push (make-kern-chunk word-spacing) res)))
                                 `(push (make-black-chunk* 32 (car ss)) res) )
                            (setf blacki (+ i 1))))
                         ((:NOWRAP)
                          `(progn
                            ,(IF LETTER-SPACING-APPLICABLE-P
                                 `(if (eql word-spacing :normal)
                                   (push (make-black-chunk* 32 (car ss)) res)
                                   (progn
                                     (push (make-black-chunk* 32 (car ss)) res)
                                     (push (make-kern-chunk word-spacing) res)))
                                 `(push (make-black-chunk* 32 (car ss)) res) )))))))

            (t
             ,(AND LETTER-SPACING-APPLICABLE-P
                   `(unless last-was-space-p
                     (push (make-kern-chunk (max letter-spacing
                                                 (- (chunk-width (make-black-chunk* c (car ss))))))
                      res)))
             (when last-was-space-p
               ;; ### spill the disk before or after the kern? Does it matter?
               (when dangling-disc
                 (push dangling-disc res)
                 (setf dangling-disc nil))
               (setf blacki i)
               (setf last-was-space-p nil))
             ,(AND FIRST-LETTER-APPLICABLE-P
                   `(when (and first-letter-p (not first-letter-element))
                     (first-letter-start)))
             ,(AND LETTER-SPACING-APPLICABLE-P
                   `(push (make-black-chunk* c (car ss)) res))
             ,(AND FIRST-LETTER-APPLICABLE-P
                   `(when first-letter-element
                     ,(OR LETTER-SPACING-APPLICABLE-P
                          `(push (make-black-chunk* c (car ss)) res))
                     (unless (punctation-character-p c)
                       (when first-letter-element
                         (first-letter-end)
                         (setf first-letter-element nil)
                         (setf first-letter-p nil)))
                     (setf blacki (+ i 1))))) ) )
      ;;
      (unless last-was-space-p
        ,(OR LETTER-SPACING-APPLICABLE-P
             `(unless (= blacki (length data))
               (push (cons-black-chunk
                      :style (car ss)
                      :data (subseq data blacki))
                res))))) ))

;; Text transform is now handled before anything else to keep code size
;; reasonable.

(defun transform-rod (rod transformation &optional (previous-rune #/.))
  (ecase transformation
    (:none
     rod)
    (:uppercase
     (glisp::register-rod (map 'rod #'rune-upcase rod)))
    (:lowercase
     (glisp::register-rod (map 'rod #'rune-downcase rod)))
    (:capitalize
     ;; more complicated
     (let ((res (make-rod (length rod))))
       (loop for c = previous-rune then d
             for d across rod
             for i from 0 do
             (setf (rune res i)
                   (cond ((glisp::rune-upper-case-letter-p c) d)
                         ((glisp::rune-lower-case-letter-p c) (rune-downcase d))
                         (t                                   (rune-upcase d)))))
       res))))

(defun popo-para (q ss block-element block-style
                  &key;; the kind of run we do
                       ;;  :not-first-line -- first pass
                       ;;  :first-line     -- second pass first-line to be applied
                       ;;  :invariant      -- first line not included and it makes no difference
                       (run :not-first-line)
                       )
  ;; ### we disabled apply-first-line-p since it breaks in case of
  ;;     the para-box entirely containing out of floating boxen ...
  ;;
  ;; ### in fact we introduce dangling white space, if the element happens to
  ;;     have some.
  ;;
  ;; ### also: we sometimes wind up with empty black chunks. Is that intentional?
  ;;
  ;; ### and: first line is not always going close? Does it matter?
  ;;
  ;; ### and: this badly needs refactoring.
  ;;
  ;; can't we still go with dangling-white somehow?
  ;; ss is current style stack (at entry)
  ;;(assert (eq (slot-value block-style 'css::%element) block-element))
  (setf run :invariant)

  ;; ### the no break at all path lacks a close element.
  
  (let* (res
         nres context
         (last-was-space-p t)
         (block-style (car ss))
         (white-space (cooked-style-white-space block-style))
         (first-letter-element nil)
         (first-letter-p t)             ;still in first-char condition?
         ;; the possible open first-line-element
         (first-line-element nil)
         (first-line-style nil)
         ;;
         (first-line-chunk nil)
         (para-start-q q)
         (para-start-ss ss)
         (previous-rune #/.)
         (dangling-disc nil)            ;dangling discretionary chunk in case last-was-space-p is t,
                                        ; to be spilled just before when last-was-space-p turns nil again.
                                        ; ### see if their spillment is correct!
         )
    (push '(:para-start) res)

    (labels
        ((frob (context style-stack)
           (cond ((eq (bounding-chunk-pt (car context)) first-line-element)
                  (values (cdr context) (cdr style-stack)))
                 (t
                  (multiple-value-bind (p q)
                      (frob (cdr context) (cdr style-stack))
                    (let ((style
                           (my-setup-style (bounding-chunk-pt (car context))
                                           (car q)
                                           block-style)))
                      (values
                       (cons (make-instance 'bounding-chunk
                                            :pt (bounding-chunk-pt (car context))
                                            :style style)
                             p)
                       (cons style q)))))))

         (add-content (content)
           ;; ### CSS2 is silent about the precise white space convention to be used.
           ;; ### handle all possible content variants
           (dolist (x content)
             (cond ((consp x)
                    (case (car x)
                      ((:string)
                       (handle-data (string-rod (cadr x))))
                      ((:url)
                       )
                      ((:counter)
                       )
                      ((:counters)
                       )
                      ((:attr)
                       )))))
           )

         (first-letter-start ()
           ;; ### what to do if the first letter ends up floating?
           ;;     we can collect the chunks as usual but after the
           ;;     element ended we need to stuff them into a floating
           ;;     box.
           (let* ((element (make-instance 'first-letter-pseudo-element :proxee block-element))
                  (style   (my-setup-style element (car ss) block-style)))
             (setf first-letter-element element)
             (handle-open first-letter-element style) ))

         (first-letter-end ()
           ;;
           (handle-close first-letter-element))

         ;; Some optimized variants of handle-data:

         (handle-data (data)
           (unless (eql (cooked-style-text-transform (car ss)) :normal)
             (setf data (transform-rod data (cooked-style-text-transform (car ss))
                                       previous-rune)))
           (unless (zerop (length data))
             (setf previous-rune (aref data (1- (length data)))))
           ;;
           ;; ### we need another whitespace convention for generated content
           (ecase white-space
             (:normal
              (let ((word-spacing   (cooked-style-word-spacing (car ss)))
                    (letter-spacing (cooked-style-letter-spacing (car ss))))
                (if (eql letter-spacing 0) (setf letter-spacing :normal)) ;###
                (if (and (eql word-spacing :normal)
                         (eql letter-spacing :normal))
                    (if first-line-element
                        (if first-letter-p
                            (%handle-data/normal/none t t nil :normal)
                            (%handle-data/normal/none t nil nil :normal))
                        (%handle-data/normal/none nil nil nil :normal))
                    (if first-line-element
                        (if first-letter-p
                            (%handle-data/normal/none t t t :normal)
                            (%handle-data/normal/none t nil t :normal))
                        (%handle-data/normal/none nil nil t :normal)))))
             (:nowrap
              (let ((word-spacing   (cooked-style-word-spacing (car ss)))
                    (letter-spacing (cooked-style-letter-spacing (car ss))))
                (when (eql letter-spacing :normal) (setf letter-spacing 0))
                (%handle-data/normal/none t t t :nowrap)
                #+NIL
                (if (and (eql word-spacing :normal)
                         (eql letter-spacing :normal))
                    (if first-line-element
                        (if first-letter-p
                            (%handle-data/normal/none t t nil :nowrap)
                            (%handle-data/normal/none t nil nil :nowrap))
                        (%handle-data/normal/none nil nil nil :nowrap))
                    (if first-line-element
                        (if first-letter-p
                            (%handle-data/normal/none t t t :nowrap)
                            (%handle-data/normal/none t nil t :nowrap))
                        (%handle-data/normal/none nil nil t :nowrap)))))
             (:pre
              (let ((word-spacing   (cooked-style-word-spacing (car ss)))
                    (letter-spacing (cooked-style-letter-spacing (car ss))))
                (when (eql letter-spacing :normal) (setf letter-spacing 0))
                (%handle-data/normal/none t t t :pre)
                #+NIL
                (if (and (eql word-spacing :normal)
                         (eql letter-spacing :normal))
                    (if first-line-element
                        (if first-letter-p
                            (%handle-data/normal/none t t nil :pre)
                            (%handle-data/normal/none t nil nil :pre))
                        (%handle-data/normal/none nil nil nil :pre))
                    (if first-line-element
                        (if first-letter-p
                            (%handle-data/normal/none t t t :pre)
                            (%handle-data/normal/none t nil t :pre))
                        (%handle-data/normal/none nil nil t :pre)))))))

         (handle-generated-content (class element)
           ;; ### we need to restrict and watch the possible display values.
           (let* ((pseudo-element (make-instance class :proxee element))
                  (pseudo-style   (my-setup-style pseudo-element (car ss) block-style))
                  (pseudo-content (cooked-style-content pseudo-style)))
             (unless (null pseudo-content)
               (handle-open pseudo-element pseudo-style)
               (add-content pseudo-content)
               (handle-close pseudo-element) )))

         (handle-open (element &optional (style (my-setup-style element (car ss) block-style)))
           (cond ((and first-letter-element (not (eq element first-letter-element)))
                  ;; ### the style might not be correct!
                  (assert (eq (bounding-chunk-pt (car context)) first-letter-element))
                  (let ((fl.style (bounding-chunk-style (car context))))
                    ;; half-close the first-letter-element
                    (push (make-half-close-chunk
                           first-letter-element
                           fl.style) res)
                    (pop context)
                    (pop ss)
                    (setf style (my-setup-style element (car ss) block-style))
                    ;; insert our open tag
                    (let ((chunk (make-open-chunk element style)))
                      (push chunk res)
                      (push chunk context))
                    (push style ss)
                    ;; half-open the flchunk again
                    (let* ((s (my-setup-style first-letter-element (car ss) block-style))
                           (c (make-half-open-chunk first-letter-element s)))
                      (push c res)
                      (push c context)
                      (push s ss))))
                 (t
                  (push style ss)
                  (let ((chunk (make-open-chunk element (car ss))))
                    (push chunk res)
                    (push chunk context) ))))

         (handle-close (element)
           (cond ((and first-letter-element (not (eq element first-letter-element)))
                  (assert (eq (bounding-chunk-pt (car context)) first-letter-element))
                  (let ((fl.style (bounding-chunk-style (car context))))
                    ;; half-close the first-letter-element
                    (push (make-half-close-chunk
                           first-letter-element
                           fl.style) res)
                    (pop context)
                    (pop ss)
                    ;; ### reset-up style?
                    (push (make-close-chunk element (car ss)) res)
                    (pop context)
                    (pop ss)
                    ;; half-open the flchunk again
                    (let* ((s (my-setup-style first-letter-element (car ss) block-style))
                           (c (make-half-open-chunk first-letter-element s)))
                      (push c res)
                      (push c context)
                      (push s ss))))
                 (t
                  (when (null ss)
                    (error "HANDLE-CLOSE [~A]on an empty style stack?"
                           (chunk-debug-name (make-instance 'close-chunk :pt element))))
                  (push (make-close-chunk element (car ss)) res)
                  (pop context)
                  (pop ss))))

         (clean-para (&aux non-halfp)
           ;; ### is this entirely right?
           (do ((q res (cdr q)))
               ((equal (car q) '(:para-start))
                (cond ((not non-halfp)
                       (setf res (cdr q))
                       nil)
                      (t
                       t)))
             (when (and (not (and (bounding-chunk-p (car q))
                                  (bounding-chunk-halfp (car q))))
                        (not (and (bounding-chunk-p (car q))
                                  (typep (bounding-chunk-pt (car q)) 'pseudo-element))))
               (setf non-halfp t))))

         (gather-para ()
           (let ((items))
             (do ((x (pop res) (pop res)))
                 ((equal x '(:para-start)))
               (push x items))
             (make-para-box :items items)))

         (stuff-para ()
           (cond ((clean-para)
                  ;; and here comes the magic:
                  (ecase run
                    (:not-first-line
                     ;; we need to call ourself again
                     (let ((my-para-box (gather-para))
                           (mirror-para-box
                            (popo-para para-start-q para-start-ss block-element block-style
                                       :run :first-line)))
                       ;; now simultaneously traverse both our and the mirror
                       ;; universe updating disk-links as we go
                       ;; ### what is with possible floating chunks?
                       ;;     we definitly want them only in non-first-line style
                       ;;     and skip building them in non-first-line style.
                       (let ((f (para-box-items mirror-para-box))
                             (o (para-box-items my-para-box)))
                         (loop
                             ;; walk both until we find either a disc-chunk or nil
                             (do () ((or (null f) (disc-chunk-p (car f)))) (setf f (cdr f)))
                             (do () ((or (null o) (disc-chunk-p (car o)))) (setf o (cdr o)))
                           (assert (or (and (null f) (null o))
                                       (and f o)))
                           (when (null f)
                             (return))
                           (setf (disc-chunk-%here (car f))
                                 (append (disc-chunk-here (car f)) (cdr f)))
                           (setf (disc-chunk-%after (car f))
                                 (append (disc-chunk-after (car o)) (cdr o)))
                           (psetf (cdr f) nil
                                  f (cdr f))
                           (setf o (cdr o))))
                       (push mirror-para-box res)
                       (setf run :invariant) ;the horror is over
                       ))
                    (:first-line
                     ;; the first line run, just return what we found
                     (when first-line-element (handle-close first-line-element))
                     ;; grrf this is still not 100% correct ;(
                     (let ((z (gather-para)))
                       '(format *trace-output* "~&;; first line run ended, we have:~%;; ~S."
                         z)
                       (return-from popo-para z) ))
                    (:invariant
                     ;; nothing special
                     (push (gather-para) res)) ))
                 (t
                  ;; we decided to skip this para, so be nice and reset para-start-*
                  (setf para-start-q q
                        para-start-ss ss) )) )

         (handle-suspend ()
           "This is called whenever the paragraph has to be 'suspended' because another
            block box came its way."
           ;; close the context
           (dolist (k context)
             (push (make-half-close-chunk (bounding-chunk-pt k)
                                          (bounding-chunk-style k))
                   res))
           ;; finish the current paragraph
           (stuff-para) )

         (handle-resume ()
           "This is called when a paragraph suspended because of a block-box is to be resumed again."
           ;; re-open the paragraph and re-open the context
           (push '(:para-start) res)
           (setf last-was-space-p t)
           (dolist (k (reverse context))
             (push (make-half-open-chunk (bounding-chunk-pt k)
                                         (bounding-chunk-style k))
                   res)) ) )

      (cond ((eql run :first-line)
             (setf first-line-element
                   (make-instance 'first-line-pseudo-element
                                  :proxee block-element))
             (setf first-line-style (my-setup-style first-line-element (car ss) block-style))

             (setf first-line-chunk
                   (make-instance
                    'bounding-chunk
                    :pt first-line-element
                    :style first-line-style))
             (handle-open first-line-element)
             ))

      (let ()
        (tagbody
         L.0
           (when (null q)
             ;; the paragraph must have ended.
             (stuff-para)
             (return-from popo-para
               (values (reverse res) q)))

           (let* ((kind (caar q))
                  (data (cadar q))
                  (s (and (member kind '(:open :close))
                          (if (eq data block-element)
                              block-style
                              ;; bang! this failes when this is the close
                              ;; element of the block around this para!
                              (my-setup-style data (car ss) block-style)))) )

             (cond
             ;;; regular block stuff.
               ((and (eq kind :open)
                     (cooked-style-block-element-p s))
                ;; This element can be on of:
                ;; . a regular block
                ;; . a floating box
                ;; . a replaced element
                ;; . a absolutely positioned regular block
                ;; . a absolutely positioned replaced block

                ;; When this is a replaced element we want can do:
                ;; construct a block with a para whose items then are
                ;;   <IMG> <RO/> </IMG>
                ;; so that the regular other stuff applies.



                ;; ### now this might as well be floating box ... in
                ;; which case we should just collect the block box as
                ;; usual but stuffing it into a floating-chunk.
                ;; ### also: keep proper track of the 'containing block'
                ;; for style (that is width and stuff).

                ;; ### and then we need the very same logic for floating
                ;; elements which are generated by possible block
                ;; content [above in popo-block].


                (let ((float (cooked-style-float s))
                      (replaced-element
                       (replaced-element-p *document* *device* data))
                      content)

                  #+NIL
                  (progn
                    ;; ### This variant is now quite right wrt to 'q'
                    ;;
                    ;; find the content, depends on replaced/non-replaced
                    (setf content (cond ((null replaced-element)
                                         ;; --- non-replaced
                                         (setf (values nres q) (popo-block q ss block-style)) ;###
                                         nres)
                                        (t
                                         ;; --- replaced
                                         ;; skip the whole rest
                                         (setf q (cdr (member-if (lambda (y) (eq (second y) data)) (cdr q))))
                                         (make-block-box
                                          :content
                                          (list
                                           (make-para-box
                                            :items
                                            (list (cons-replaced-object-chunk
                                                   :element data
                                                   :object replaced-element))))
                                          :style s
                                          :element data))))
                    ;; spill the content, depends on floating/non-floating
                    (cond ((eql float :none)
                           (handle-suspend)
                           (push content res)
                           (handle-resume))
                          (t
                           (push (make-instance 'floating-chunk
                                                :style s
                                                :content content)
                                 res))))

                  #-NIL
                  (cond
                  
                    ;; ---- non-floating, non-replaced block
                    ((and (eql float :none) (null replaced-element))
                     (handle-suspend)
                     ;; gather the block and push it onto our bag
                     ;; note that the containing block does not change
                     (setf (values nres q) (popo-block q ss block-style))
                     (push nres res)
                     (handle-resume))

                    ;; ---- non-floating, replaced block
                    ((and (eql float :none) (not (null replaced-element)))
                     (handle-suspend)
                     (push (make-block-box :content
                                           (list
                                            (make-para-box :items
                                                           (list (cons-replaced-object-chunk
                                                                  :element data
                                                                  :object replaced-element))))
                                           :style s
                                           :element data)
                           res)
                     ;; skip the whole rest
                     (setf q (cdr (member-if (lambda (y) (eq (second y) data)) (cdr q))))
                     (handle-resume))

                    ;; ---- floating
                    ((not (eql float :none))
                     (push (make-instance 'floating-chunk
                                          :style s
                                          :content
                                          (cond ((null replaced-element)
                                                 ;; --- non-replaced
                                                 (setf (values nres q) (popo-block q ss block-style)) ;###
                                                 nres)
                                                (t
                                                 ;; --- replaced
                                                 ;; skip the whole rest
                                                 (setf q (cdr (member-if (lambda (y) (eq (second y) data)) (cdr q))))
                                                 (make-block-box
                                                  :content
                                                  (list
                                                   (make-para-box
                                                    :items
                                                    (list (cons-replaced-object-chunk
                                                           :element data
                                                           :object replaced-element))))
                                                  :style s
                                                  :element data))))
                           res))
                  
                    ;; ---- floating, non-replaced block
                    ((and (not (eql float :none))
                          (null replaced-element))
                     ;; this is a floating box, it will not in
                     ;; anyway end the paragraph, we rather collect
                     ;; it into a floating-chunk.
                     ;; ### what is the containing block here?
                     (setf (values nres q) (popo-block q ss block-style)) ;###
                     (push (make-instance 'floating-chunk
                                          :style s
                                          :content nres)
                           res) )

                    ;; --- floating, replaced block
                    ((and (not (eql float :none))
                          (not (null replaced-element)))
                     ;; skip the whole rest
                     (setf q (cdr (member-if (lambda (y) (eq (second y) data)) (cdr q))))
                     (push (make-instance
                            'floating-chunk
                            :style s
                            :content (make-block-box
                                      :content
                                      (list
                                       (make-para-box
                                        :items
                                        (list (cons-replaced-object-chunk
                                               :element data
                                               :object replaced-element))))
                                      :style s
                                      :element data))
                           res) ) ))
                ;;
                (go L.0))
               ;;
               ((and (eq kind :close)
                     (cooked-style-block-element-p s))
                ;; the paragraph must have ended.
                (stuff-para)
                (return-from popo-para
                  (values (reverse res) q)))
               ;;
               ((and (eq kind :open)
                     (eq (cooked-style-display s) :none))
                (setf q (member-if (lambda (y) (eq (second y) data)) (cdr q))) )

               ((and (eq kind :open)
                     (member (cooked-style-display s) '(:marker :inline)))
                (when dangling-disc
                  (push dangling-disc res)
                  (setf dangling-disc nil))
                ;; ### hmm ...
                ;; Note: A before element can have a display of either :inline or :none
                ;; So we wind up with either
                ;;   open-chunk(before) context close-chunk(before)
                ;; or nothing.
                (assert s)
                (handle-open data s)
                (unless (typep data 'pseudo-element)
                  (handle-generated-content 'before-pseudo-element data))
                (when (typep data 'pseudo-element)
                  (add-content (cooked-style-content s)))

                (multiple-value-bind (re re-map)
                    (if nil ;;(eql :img (element-gi data))
                        (make-instance 'lazy-image)
                        (replaced-element-p *document* *device* data))
                  (when re
                    (when last-was-space-p
                      (when dangling-disc
                        (push dangling-disc res)
                        (setf dangling-disc nil))
                      (setf last-was-space-p nil))

                    (when first-letter-element
                      (first-letter-end)
                      (setf first-letter-element nil)
                      (setf first-letter-p nil))

                    (push (make-instance 'replaced-object-chunk
                                         :element data
                                         :object re)
                          res))) )

               ((and (eq kind :close)
                     (member (cooked-style-display s) '(:inline :marker)))
                ;; Note: An after element can have a display of either :inline or :none
                (unless (typep data 'pseudo-element)
                  (handle-generated-content 'after-pseudo-element data))
                (handle-close data))

               ((eq kind :data)
                (handle-data data))

               (t
                (error "Unexpected item: ~S." (car q))) ))

         L.continue
           (setf q (cdr q))
           (go L.0)))) ))




;;;;

(defun punctation-character-p (char)
  (member char
          '(#x0021                      ;EXCLAMATION MARK;Po;0;ON;;;;;N;;;;;
            #x0022                      ;QUOTATION MARK;Po;0;ON;;;;;N;;;;;
            #x0023                      ;NUMBER SIGN;Po;0;ET;;;;;N;;;;;
            #x0025                      ;PERCENT SIGN;Po;0;ET;;;;;N;;;;;
            #x0026                      ;AMPERSAND;Po;0;ON;;;;;N;;;;;
            #x0027                      ;APOSTROPHE;Po;0;ON;;;;;N;APOSTROPHE-QUOTE;;;;
            #x002A                      ;ASTERISK;Po;0;ON;;;;;N;;;;;
            #x002C                      ;COMMA;Po;0;CS;;;;;N;;;;;
            #x002E                      ;FULL STOP;Po;0;CS;;;;;N;PERIOD;;;;
            #x002F                      ;SOLIDUS;Po;0;ES;;;;;N;SLASH;;;;
            #x003A                      ;COLON;Po;0;CS;;;;;N;;;;;
            #x003B                      ;SEMICOLON;Po;0;ON;;;;;N;;;;;
            #x003F                      ;QUESTION MARK;Po;0;ON;;;;;N;;;;;
            #x0040                      ;COMMERCIAL AT;Po;0;ON;;;;;N;;;;;
            #x005C                      ;REVERSE SOLIDUS;Po;0;ON;;;;;N;BACKSLASH;;;;
            #x00A1                      ;INVERTED EXCLAMATION MARK;Po;0;ON;;;;;N;;;;;
            #x00B7                      ;MIDDLE DOT;Po;0;ON;;;;;N;;;;;
            #x00BF                      ;INVERTED QUESTION MARK;Po;0;ON;;;;;N;;;;;
            #x0374                      ;GREEK NUMERAL SIGN;Po;0;L;02B9;;;;N;GREEK UPPER NUMERAL SIGN;Dexia keraia;;;
            #x0375                      ;GREEK LOWER NUMERAL SIGN;Po;0;L;;;;;N;;Aristeri keraia;;;
            #x037E                      ;GREEK QUESTION MARK;Po;0;ON;003B;;;;N;;Erotimatiko;;;
            #x0387                      ;GREEK ANO TELEIA;Po;0;ON;00B7;;;;N;;;;;
            #x055A                      ;ARMENIAN APOSTROPHE;Po;0;L;;;;;N;ARMENIAN MODIFIER LETTER RIGHT HALF RING;;;;
            #x055B                      ;ARMENIAN EMPHASIS MARK;Po;0;L;;;;;N;;;;;
            #x055C                      ;ARMENIAN EXCLAMATION MARK;Po;0;L;;;;;N;;;;;
            #x055D                      ;ARMENIAN COMMA;Po;0;L;;;;;N;;;;;
            #x055E                      ;ARMENIAN QUESTION MARK;Po;0;L;;;;;N;;;;;
            #x055F                      ;ARMENIAN ABBREVIATION MARK;Po;0;L;;;;;N;;;;;
            #x0589                      ;ARMENIAN FULL STOP;Po;0;L;;;;;N;ARMENIAN PERIOD;;;;
            #x05BE                      ;HEBREW PUNCTUATION MAQAF;Po;0;R;;;;;N;;;;;
            #x05C0                      ;HEBREW PUNCTUATION PASEQ;Po;0;R;;;;;N;HEBREW POINT PASEQ;;;;
            #x05C3                      ;HEBREW PUNCTUATION SOF PASUQ;Po;0;R;;;;;N;;;;;
            #x05F3                      ;HEBREW PUNCTUATION GERESH;Po;0;R;;;;;N;;;;;
            #x05F4                      ;HEBREW PUNCTUATION GERSHAYIM;Po;0;R;;;;;N;;;;;
            #x060C                      ;ARABIC COMMA;Po;0;CS;;;;;N;;;;;
            #x061B                      ;ARABIC SEMICOLON;Po;0;R;;;;;N;;;;;
            #x061F                      ;ARABIC QUESTION MARK;Po;0;R;;;;;N;;;;;
            #x066A                      ;ARABIC PERCENT SIGN;Po;0;ET;;;;;N;;;;;
            #x066B                      ;ARABIC DECIMAL SEPARATOR;Po;0;AN;;;;;N;;;;;
            #x066C                      ;ARABIC THOUSANDS SEPARATOR;Po;0;AN;;;;;N;;;;;
            #x066D                      ;ARABIC FIVE POINTED STAR;Po;0;R;;;;;N;;;;;
            #x06D4                      ;ARABIC FULL STOP;Po;0;R;;;;;N;ARABIC PERIOD;;;;
            #x0964                      ;DEVANAGARI DANDA;Po;0;L;;;;;N;;;;;
            #x0965                      ;DEVANAGARI DOUBLE DANDA;Po;0;L;;;;;N;;;;;
            #x0970                      ;DEVANAGARI ABBREVIATION SIGN;Po;0;L;;;;;N;;;;;
            #x0E5A                      ;THAI CHARACTER ANGKHANKHU;Po;0;L;;;;;N;THAI ANGKHANKHU;;;;
            #x0E5B                      ;THAI CHARACTER KHOMUT;Po;0;L;;;;;N;THAI KHOMUT;;;;
            #x0F04                      ;TIBETAN MARK INITIAL YIG MGO MDUN MA;Po;0;L;;;;;N;;yik go dun ma;;;
            #x0F05                      ;TIBETAN MARK CLOSING YIG MGO SGAB MA;Po;0;L;;;;;N;;yik go kab ma;;;
            #x0F06                      ;TIBETAN MARK CARET YIG MGO PHUR SHAD MA;Po;0;L;;;;;N;;yik go pur shey ma;;;
            #x0F07                      ;TIBETAN MARK YIG MGO TSHEG SHAD MA;Po;0;L;;;;;N;;yik go tsek shey ma;;;
            #x0F08                      ;TIBETAN MARK SBRUL SHAD;Po;0;L;;;;;N;;drul shey;;;
            #x0F09                      ;TIBETAN MARK BSKUR YIG MGO;Po;0;L;;;;;N;;kur yik go;;;
            #x0F0A                      ;TIBETAN MARK BKA- SHOG YIG MGO;Po;0;L;;;;;N;;ka sho yik go;;;
            #x0F0B                      ;TIBETAN MARK INTERSYLLABIC TSHEG;Po;0;L;;;;;N;;tsek;;;
            #x0F0C                      ;TIBETAN MARK DELIMITER TSHEG BSTAR;Po;0;L;;;;;N;;tsek tar;;;
            #x0F0D                      ;TIBETAN MARK SHAD;Po;0;L;;;;;N;;shey;;;
            #x0F0E                      ;TIBETAN MARK NYIS SHAD;Po;0;L;;;;;N;;nyi shey;;;
            #x0F0F                      ;TIBETAN MARK TSHEG SHAD;Po;0;L;;;;;N;;tsek shey;;;
            #x0F10                      ;TIBETAN MARK NYIS TSHEG SHAD;Po;0;L;;;;;N;;nyi tsek shey;;;
            #x0F11                      ;TIBETAN MARK RIN CHEN SPUNGS SHAD;Po;0;L;;;;;N;;rinchen pung shey;;;
            #x0F12                      ;TIBETAN MARK RGYA GRAM SHAD;Po;0;L;;;;;N;;gya tram shey;;;
            #x0F85                      ;TIBETAN MARK PALUTA;Po;0;L;;;;;N;;;;;
            #x10FB                      ;GEORGIAN PARAGRAPH SEPARATOR;Po;0;L;;;;;N;;;;;
            #x2016                      ;DOUBLE VERTICAL LINE;Po;0;ON;;;;;N;DOUBLE VERTICAL BAR;;;;
            #x2017                      ;DOUBLE LOW LINE;Po;0;ON;<compat> 0020 0333;;;;N;SPACING DOUBLE UNDERSCORE;;;;
            #x2020                      ;DAGGER;Po;0;ON;;;;;N;;;;;
            #x2021                      ;DOUBLE DAGGER;Po;0;ON;;;;;N;;;;;
            #x2022                      ;BULLET;Po;0;ON;;;;;N;;;;;
            #x2023                      ;TRIANGULAR BULLET;Po;0;ON;;;;;N;;;;;
            #x2024                      ;ONE DOT LEADER;Po;0;ON;<compat> 002E;;;;N;;;;;
            #x2025                      ;TWO DOT LEADER;Po;0;ON;<compat> 002E 002E;;;;N;;;;;
            #x2026                      ;HORIZONTAL ELLIPSIS;Po;0;ON;<compat> 002E 002E 002E;;;;N;;;;;
            #x2027                      ;HYPHENATION POINT;Po;0;ON;;;;;N;;;;;
            #x2030                      ;PER MILLE SIGN;Po;0;ET;;;;;N;;;;;
            #x2031                      ;PER TEN THOUSAND SIGN;Po;0;ET;;;;;N;;;;;
            #x2032                      ;PRIME;Po;0;ET;;;;;N;;;;;
            #x2033                      ;DOUBLE PRIME;Po;0;ET;<compat> 2032 2032;;;;N;;;;;
            #x2034                      ;TRIPLE PRIME;Po;0;ET;<compat> 2032 2032 2032;;;;N;;;;;
            #x2035                      ;REVERSED PRIME;Po;0;ON;;;;;N;;;;;
            #x2036                      ;REVERSED DOUBLE PRIME;Po;0;ON;<compat> 2035 2035;;;;N;;;;;
            #x2037                      ;REVERSED TRIPLE PRIME;Po;0;ON;<compat> 2035 2035 2035;;;;N;;;;;
            #x2038                      ;CARET;Po;0;ON;;;;;N;;;;;
            #x203B                      ;REFERENCE MARK;Po;0;ON;;;;;N;;;;;
            #x203C                      ;DOUBLE EXCLAMATION MARK;Po;0;ON;<compat> 0021 0021;;;;N;;;;;
            #x203D                      ;INTERROBANG;Po;0;ON;;;;;N;;;;;
            #x203E                      ;OVERLINE;Po;0;ON;<compat> 0020 0305;;;;N;SPACING OVERSCORE;;;;
            #x2041                      ;CARET INSERTION POINT;Po;0;ON;;;;;N;;;;;
            #x2042                      ;ASTERISM;Po;0;ON;;;;;N;;;;;
            #x2043                      ;HYPHEN BULLET;Po;0;ON;;;;;N;;;;;
            #x3001                      ;IDEOGRAPHIC COMMA;Po;0;ON;;;;;N;;;;;
            #x3002                      ;IDEOGRAPHIC FULL STOP;Po;0;ON;;;;;N;IDEOGRAPHIC PERIOD;;;;
            #x3003                      ;DITTO MARK;Po;0;ON;;;;;N;;;;;
            #xFE30                      ;PRESENTATION FORM FOR VERTICAL TWO DOT LEADER;Po;0;ON;<vertical> 2025;;;;N;GLYPH FOR VERTICAL TWO DOT LEADER;;;;
            #xFE49                      ;DASHED OVERLINE;Po;0;ON;<compat> 203E;;;;N;SPACING DASHED OVERSCORE;;;;
            #xFE4A                      ;CENTRELINE OVERLINE;Po;0;ON;<compat> 203E;;;;N;SPACING CENTERLINE OVERSCORE;;;;
            #xFE4B                      ;WAVY OVERLINE;Po;0;ON;<compat> 203E;;;;N;SPACING WAVY OVERSCORE;;;;
            #xFE4C                      ;DOUBLE WAVY OVERLINE;Po;0;ON;<compat> 203E;;;;N;SPACING DOUBLE WAVY OVERSCORE;;;;
            #xFE50                      ;SMALL COMMA;Po;0;CS;<small> 002C;;;;N;;;;;
            #xFE51                      ;SMALL IDEOGRAPHIC COMMA;Po;0;ON;<small> 3001;;;;N;;;;;
            #xFE52                      ;SMALL FULL STOP;Po;0;CS;<small> 002E;;;;N;SMALL PERIOD;;;;
            #xFE54                      ;SMALL SEMICOLON;Po;0;ON;<small> 003B;;;;N;;;;;
            #xFE55                      ;SMALL COLON;Po;0;CS;<small> 003A;;;;N;;;;;
            #xFE56                      ;SMALL QUESTION MARK;Po;0;ON;<small> 003F;;;;N;;;;;
            #xFE57                      ;SMALL EXCLAMATION MARK;Po;0;ON;<small> 0021;;;;N;;;;;
            #xFE5F                      ;SMALL NUMBER SIGN;Po;0;ET;<small> 0023;;;;N;;;;;
            #xFE60                      ;SMALL AMPERSAND;Po;0;ON;<small> 0026;;;;N;;;;;
            #xFE61                      ;SMALL ASTERISK;Po;0;ON;<small> 002A;;;;N;;;;;
            #xFE68                      ;SMALL REVERSE SOLIDUS;Po;0;ON;<small> 005C;;;;N;SMALL BACKSLASH;;;;
            #xFE6A                      ;SMALL PERCENT SIGN;Po;0;ET;<small> 0025;;;;N;;;;;
            #xFE6B                      ;SMALL COMMERCIAL AT;Po;0;ON;<small> 0040;;;;N;;;;;
            #xFF01                      ;FULLWIDTH EXCLAMATION MARK;Po;0;ON;<wide> 0021;;;;N;;;;;
            #xFF02                      ;FULLWIDTH QUOTATION MARK;Po;0;ON;<wide> 0022;;;;N;;;;;
            #xFF03                      ;FULLWIDTH NUMBER SIGN;Po;0;ET;<wide> 0023;;;;N;;;;;
            #xFF05                      ;FULLWIDTH PERCENT SIGN;Po;0;ET;<wide> 0025;;;;N;;;;;
            #xFF06                      ;FULLWIDTH AMPERSAND;Po;0;ON;<wide> 0026;;;;N;;;;;
            #xFF07                      ;FULLWIDTH APOSTROPHE;Po;0;ON;<wide> 0027;;;;N;;;;;
            #xFF0A                      ;FULLWIDTH ASTERISK;Po;0;ON;<wide> 002A;;;;N;;;;;
            #xFF0C                      ;FULLWIDTH COMMA;Po;0;CS;<wide> 002C;;;;N;;;;;
            #xFF0E                      ;FULLWIDTH FULL STOP;Po;0;CS;<wide> 002E;;;;N;FULLWIDTH PERIOD;;;;
            #xFF0F                      ;FULLWIDTH SOLIDUS;Po;0;ES;<wide> 002F;;;;N;FULLWIDTH SLASH;;;;
            #xFF1A                      ;FULLWIDTH COLON;Po;0;CS;<wide> 003A;;;;N;;;;;
            #xFF1B                      ;FULLWIDTH SEMICOLON;Po;0;ON;<wide> 003B;;;;N;;;;;
            #xFF1F                      ;FULLWIDTH QUESTION MARK;Po;0;ON;<wide> 003F;;;;N;;;;;
            #xFF20                      ;FULLWIDTH COMMERCIAL AT;Po;0;ON;<wide> 0040;;;;N;;;;;
            #xFF3C                      ;FULLWIDTH REVERSE SOLIDUS;Po;0;ON;<wide> 005C;;;;N;FULLWIDTH BACKSLASH;;;;
            #xFF61                      ;HALFWIDTH IDEOGRAPHIC FULL STOP;Po;0;ON;<narrow> 3002;;;;N;HALFWIDTH IDEOGRAPHIC PERIOD;;;;
            #xFF64                      ;HALFWIDTH IDEOGRAPHIC COMMA;Po;0;ON;<narrow> 3001;;;;N;;;;;

            #x00AB                      ;LEFT-POINTING DOUBLE ANGLE QUOTATION MARK;Pi;0;ON;;;;;Y;LEFT POINTING GUILLEMET;;;;
            #x2018                      ;LEFT SINGLE QUOTATION MARK;Pi;0;ON;;;;;N;SINGLE TURNED COMMA QUOTATION MARK;;;;
            #x201B                      ;SINGLE HIGH-REVERSED-9 QUOTATION MARK;Pi;0;ON;;;;;N;SINGLE REVERSED COMMA QUOTATION MARK;;;;
            #x201C                      ;LEFT DOUBLE QUOTATION MARK;Pi;0;ON;;;;;N;DOUBLE TURNED COMMA QUOTATION MARK;;;;
            #x201F                      ;DOUBLE HIGH-REVERSED-9 QUOTATION MARK;Pi;0;ON;;;;;N;DOUBLE REVERSED COMMA QUOTATION MARK;;;;
            #x2039                      ;SINGLE LEFT-POINTING ANGLE QUOTATION MARK;Pi;0;ON;;;;;Y;LEFT POINTING SINGLE GUILLEMET;;;;

            #x00BB                      ;RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK;Pf;0;ON;;;;;Y;RIGHT POINTING GUILLEMET;;;;
            #x2019                      ;RIGHT SINGLE QUOTATION MARK;Pf;0;ON;;;;;N;SINGLE COMMA QUOTATION MARK;;;;
            #x201D                      ;RIGHT DOUBLE QUOTATION MARK;Pf;0;ON;;;;;N;DOUBLE COMMA QUOTATION MARK;;;;
            #x203A                      ;SINGLE RIGHT-POINTING ANGLE QUOTATION MARK;Pf;0;ON;;;;;Y;RIGHT POINTING SINGLE GUILLEMET;;;;

            )))

;;;; vertical margin

;; although we could go the route of another evil global variable, i would
;; prefer to keep it functional.

;;;; todo

;;; tables
;; ### HTML table props
;; ### table row dimensions
;; ### table border model
;; ### we need to test what happens in case the available horizontal
;;     space is too narrow to support the current word.


;;; asorted

;; ### auf heise.de gibt es wieder wsp glitch

;;;; marker boxen

;; now:

;;  - they _always_ render into a single line
;;  - a vertical-align property on the marker is meaningful
;;  - they render to the first / last line of the block's paragraph[s].
;;  - when the block has no paragraph ???

;;;; word-spacing and letter-spacing:

;; There are certain things to obey:

;;  a. a letter-spacing other than :normal forbids altering it for paragraph
;;     justification purposes.

;;  b. "When the resultant space between two characters is not the same as the
;;     default space, user agents should not use ligatures."
;;
;;     Which is just plain nuts!
;;
;;     In old German it was customary to increase letter-spacing for headers.
;;     ("sperren"). But ligatures were kept together like:
;;
;;                   N e tz k u l t u r
;;           S p a sz   o d e r   E r n s t ?
;;
;;     There therefore is no way to specify that.
;;
;; c. what exactly is "between text characters?" Between a letter and a white
;;    space also? Or just between letters?
;;

;;;;

;; After all we better do it again in two phases in popo-para.

;;;;

(defclass lazy-image ()
  ())

(defmethod ro/size ((object lazy-image))
  (values 20 20 0))

(defmethod closure/clim-device::medium-draw-ro* (medium (self lazy-image) x y)
  (clim:draw-rectangle* medium x y (+ x 20) (- y 20) :ink clim:+red+))

;;;; the containing block

;; Now we have a couple of style attributes which refer to the width of the
;; 'containing block' which is a well defined term in the CSS2 dream. We
;; want computation of this value in css-setup directly. However there are a
;; few complications:

;; - The width of the containing block is not always known at the time we
;;   set up style as for instance while collecting a table or computing its
;;   minimum/maximum. Also during min/max width calculation we want to
;;   assume 0 for each percentage value which refers to the width of the
;;   containing block.

;;   However, if we assume that we have a somewhat decent implementation of
;;   on the fly changing this property and having the change properagated
;;   down into 'child' styles quickly we can get away with just asserting
;;   zero for the width of the containing block first and just set it to the
;;   new value afterwards. Also by doing this we can easily implement window
;;   size changes by just flipping the width of the BODY element.

;; For now we just take a low tech approach and somehow store the containing
;; block with the cooked-style. Upon accessing e.g. width we simply
;; recompute in case of percentage values.




;;; all properties which refer to width of containing block

;; margin-*     width of containing block
;; padding-*    width of containing block
;; width        width of containing block

;; max-width    width of containing block
;; min-width    width of containing block

;; left         width of containing block
;; right        width of containing block
;; text-indent  width of containing block

;;; properties which refer to height of containing block

;; bottom       height of containing block
;; top          height of containing block
;; height       see prose
;; max-height   height of containing block
;; min-height   height of containing block

;;;

;; For now we just add the style of the containing block to a cooked style
;; and wrap methods around the readers to resolve the percentage widthen.
;; We need however look out for possible replaced elements.

;; Later we think about a kind of change propergation. So that we can simply
;; set a style attribute and have all the other style invalidated or
;; recomputed. The cooked-style object themself then inform the renderer
;; what ever.

(progn
  #.`(progn
      ,@(loop for foo in '(cooked-style-text-indent
                           cooked-style-padding-left
                           cooked-style-padding-right
                           cooked-style-padding-top
                           cooked-style-padding-bottom
                           cooked-style-margin-top
                           cooked-style-margin-bottom
                           cooked-style-max-width
                           cooked-style-min-width
                           cooked-style-left
                           cooked-style-right)
              collect
              `(defmethod ,foo :around ((style css::cooked-style))
                (let ((value (call-next-method)))
                  (if (css:percentage-p value)
                      (maybe-resolve-percentage
                       value
                       (cooked-style-width (slot-value style 'css::%containing-block)))
                      value))))))

;;; ### hmm we still wind up with superfluous cern-chunks ...
;;; ### this is because we still wind up with letter-spacing =0

;;; ### height

;;;; 10.3 Computing widths and margins

;; while we are at it we should compute left, right, top, bottom as well.

(defmethod cooked-style-margin-left :around ((style css::cooked-style))
  (with-slots (computed-margin-left) style
    (or computed-margin-left
        (progn (compute-widths-and-margins style)
               computed-margin-left))))

(defmethod cooked-style-margin-right :around ((style css::cooked-style))
  (with-slots (computed-margin-right) style
    (or computed-margin-right
        (progn (compute-widths-and-margins style)
               computed-margin-right))))

(defmethod cooked-style-width :around ((style css::cooked-style))
  (with-slots (computed-width) style
    (setf computed-width nil)           ;### for tables
    (or computed-width
        (progn (compute-widths-and-margins style)
               computed-width))))

(defmethod cooked-style-height :around ((style css::cooked-style))
  (with-slots (computed-height) style
    (or computed-height
        (progn (compute-widths-and-margins style)
               computed-height))))

(defun compute-widths-and-margins (style)
  "Given a cooked style compute the effective values for margins and width."
  #||
  (format *trace-output* "~&compute-widths-and-margins: we have ~s with containing block = ~s.~%"
          style (slot-value style 'css::%containing-block))
  ||#
  (let* ((containing-block-style
          (slot-value style 'css::%containing-block))
         (cb.width
          (if containing-block-style
              (cooked-style-width containing-block-style)
              *canvas-width*))     ;###
         (replaced-object (replaced-element-p *document* *device*
                                              (slot-value style 'css::%element)))
         (float (cooked-style-float style))
         (ml (slot-value style 'css::margin-left))
         (bl (slot-value style 'css::border-left-width))
         (pl (slot-value style 'css::padding-left))
         (wd (slot-value style 'css::width))
         (pr (slot-value style 'css::padding-right))
         (br (slot-value style 'css::border-right-width))
         (mr (slot-value style 'css::margin-right))
         (hd (slot-value style 'css::height)) )
    ;;
    (setf ml (maybe-resolve-percentage ml cb.width))
    (setf pl (maybe-resolve-percentage pl cb.width))
    (setf wd (maybe-resolve-percentage wd cb.width))
    (setf pr (maybe-resolve-percentage pr cb.width))
    (setf mr (maybe-resolve-percentage mr cb.width))

    (when (css:percentage-p hd)
      ;; | height: <percentage>
      ;; | Specifies a percentage height. The percentage is calculated with respect
      ;; | to the height of the generated box's containing block. If the height of
      ;; | the containing block is not specified explicitly (i.e., it depends on
      ;; | content height), the value is interpreted like 'auto'.
      ;;
      ;; I remember from HTML that a percentage attribute on an IMG element
      ;; should be treated relative to the canvas.
      ;;
      (let ((cb.hd (cooked-style-height containing-block-style)))
        (cond ((not (realp cb.hd))
               (setf hd :auto))
              (t
               (setf hd (maybe-resolve-percentage hd cb.hd))))))

    ;;
    (with-slots (computed-margin-left computed-width computed-margin-right computed-height) style

      (when replaced-object
        (multiple-value-bind (r.w r.h r.d)
            (replaced-object-dimensions replaced-object wd hd)
          (setf wd r.w
                hd (+ r.h r.d))
        ;; ### now this is bogus --- it does not belong here.
        (ro/resize replaced-object r.w (+ r.h r.d)) ))

      (cond ((cooked-style-block-element-p style)
             (cond ((eq float :none)
                    ;; 10.3.3 Block-level, non-replaced elements in normal flow
                    ;; 10.3.4 Block-level, replaced elements in normal flow

                    (setf (values ml bl pl wd pr br mr)
                          (resolve-widthen style cb.width
                                              ml bl pl wd pr br mr)) )

                   ;; 10.3.5 Floating, non-replaced elements
                   ((null replaced-object)
                    (when (eql wd :auto) (setf wd 0))
                    (when (eql ml :auto) (setf ml 0))
                    (when (eql mr :auto) (setf mr 0)))

                   ;; 10.3.6 Floating, replaced elements
                   (t
                    (when (eql ml :auto) (setf ml 0))
                    (when (eql mr :auto) (setf mr 0))) ))

            ;; 10.3.1 Inline, non-replaced elements
            ((null replaced-object)
             (when (eql ml :auto) (setf ml 0))
             (when (eql mr :auto) (setf mr 0)) )

            ;; 10.3.2 Inline, replaced elements
            (t
             (when (eql ml :auto) (setf ml 0))
             (when (eql mr :auto) (setf mr 0))) )

      (setf computed-margin-left ml
            computed-margin-right mr
            computed-width wd
            computed-height hd)
      (values ml wd mr)) ))

;; ### In case of floating boxen the vertical is properly still not handled
;;     correctly. This is because we handle floating objects in block context
;;     as a paragraph, which is not correct.

(defmethod cooked-style-margin-left :around ((style css::cooked-style))
  (with-slots (computed-margin-left) style
    (setf computed-margin-left nil)           ;### for tables
    (or computed-margin-left
        (progn (compute-widths-and-margins style)
               computed-margin-left))))

(defmethod cooked-style-margin-right :around ((style css::cooked-style))
  (with-slots (computed-margin-right) style
    (setf computed-margin-right nil)           ;### for tables
    (or computed-margin-right
        (progn (compute-widths-and-margins style)
               computed-margin-right))))

(defmethod cooked-style-width :around ((style css::cooked-style))
  (with-slots (computed-width) style
    (setf computed-width nil)           ;### for tables
    (or computed-width
        (progn (compute-widths-and-margins style)
               computed-width))))

(defmethod cooked-style-height :around ((style css::cooked-style))
  (with-slots (computed-height) style
    (or computed-height
        (progn (compute-widths-and-margins style)
               computed-height))))

;; ### Note: there is perhaps a design flaw: On one hand we pass x1,
;;     x2 down all the way on the other hand chunks know their width
;;

;;; We now need a change protocol ...

;; something like
;; update-block-box block-box
;; it should then figure out automatically what changed.

;; Better yet. A block box being dirty just means that its content just moves somehow.

(defun table-borders-lessp (b1.origin b1.style b1.width b2.origin b2.style b2.width)
  ;; | The following rules determine which border style "wins" in case of a conflict:
  (cond
    ;; | 1. Borders with the 'border-style' of 'hidden' take precedence over
    ;; |    all other conflicting borders. Any border with this value
    ;; |    suppresses all borders at this location.
    ((eql b1.style :hidden) nil)
    ((eql b2.style :hidden) t)
    ;; | 2. Borders with a style of 'none' have the lowest priority. Only if
    ;; |    the border properties of all the elements meeting at this edge are
    ;; |    'none' will the border be omitted (but note that 'none' is the
    ;; |    default value for the border style.)
    ((eql b1.style :none) t)
    ((eql b2.style :none) nil)
    ;; | 3. [...], then narrow borders are discarded in favor of wider
    ;; |    ones.
    ((< b1.width b2.width) t)
    ((> b1.width b2.width) nil)
    ;; |    If several have the same 'border-width' than styles are
    ;; |    preferred in this order: 'double', 'solid', 'dashed', 'dotted',
    ;; |    'ridge', 'outset', 'groove', and the lowest: 'inset'.
    ((> (position b1.style '(:double :solid :dashed :dotted :ridge :outset :groove :inset))
        (position b2.style '(:double :solid :dashed :dotted :ridge :outset :groove :inset)))
     t)
    ((< (position b1.style '(:double :solid :dashed :dotted :ridge :outset :groove :inset))
        (position b2.style '(:double :solid :dashed :dotted :ridge :outset :groove :inset)))
     nil)
    ;; | 4. If border styles differ only in color, then a style set on a
    ;; |    cell wins over one on a row, which wins over a row group, column,
    ;; |    column group and, lastly, table.
    ((> (position b1.origin '(:cell :row :row-group :column :column-group :table))
        (position b2.origin '(:cell :row :row-group :column :column-group :table)))
     t)
    ((> (position b1.origin '(:cell :row :row-group :column :column-group :table))
        (position b2.origin '(:cell :row :row-group :column :column-group :table)))
     nil)
    (t
     ;; pick one:
     (zerop (random 2))
     t)))

(defun combine-table-borders (b1.origin b1.style b1.width b1.color
                              b2.origin b2.style b2.width b2.color)
  (if (table-borders-lessp b1.origin b1.style b1.width b2.origin b2.style b2.width)
      (values b2.origin b2.style b2.width b2.color)
      (values b1.origin b1.style b1.width b1.color)))

(defun table-borders (table)
  "Returns two 2D arrays representing the borders existing in a table according to the collapsed border model."
  (let* ((nr (table-number-of-rows table))
         (nc (table-number-of-columns table))
         (hborders (make-array (list (1+ nr) nc)
                               :initial-element '(nil :none 0 nil)))
         (vborders (make-array (list nr (1+ nc))
                               :initial-element '(nil :none 0 nil))))
    (labels ((stuff-border (ri ci rs cs origin style)
               ;; top
               (loop for j from ci below (+ ci cs) do
                     (stuff-border-1 hborders ri j origin
                                     (cooked-style-border-top-style style)
                                     (cooked-style-border-top-width style)
                                     (cooked-style-border-top-color style)))
               ;; bottom
               (loop for j from ci below (+ ci cs) do
                     (stuff-border-1 hborders (+ ri rs) j origin
                                     (cooked-style-border-bottom-style style)
                                     (cooked-style-border-bottom-width style)
                                     (cooked-style-border-bottom-color style)))
               ;; left
               (loop for i from ri below (+ ri rs) do
                     (stuff-border-1 vborders i ci origin
                                     (cooked-style-border-left-style style)
                                     (cooked-style-border-left-width style)
                                     (cooked-style-border-left-color style)))
               ;; right
               (loop for i from ri below (+ ri rs) do
                     (stuff-border-1 vborders i (+ ci cs) origin
                                     (cooked-style-border-right-style style)
                                     (cooked-style-border-right-width style)
                                     (cooked-style-border-right-color style))))
             ;;
             (stuff-border-1 (array ri ci origin style width color)
               (setf (aref array ri ci)
                     (multiple-value-list (multiple-value-call #'combine-table-borders
                                            (values-list (aref array ri ci))
                                            (values origin style width color))))) )
      ;; Do the border of the table
      (stuff-border 0 0 nr nc :table (table-style table))
      ;; Do border of column groups and columns
      (let ((ci 0)
            (nrows (table-number-of-rows table)))
        (loop for col-group in (table-column-groups table) do
              (let ((ci0 ci))
                (loop for col in (table-column-group-columns col-group) do
                      (when (table-column-style col)
                        (stuff-border 0 ci nrows 1 :column (table-column-style col)))
                      (incf ci))
                (when (table-column-group-style col-group)
                  (stuff-border 0 ci0 nrows (- ci ci0) :column-group (table-column-group-style col-group))))))
      ;; Do border of row groups and rows
      (let ((ri 0)
            (ncols (table-number-of-columns table)))
        (loop for row-group in (table-row-groups table) do
              (let ((ri0 ri))
                (loop for row in (table-row-group-rows row-group) do
                      (when (table-row-style row)
                        (stuff-border ri 0 1 ncols :row (table-row-style row)))
                      (incf ri))
                (when (table-row-group-style row-group)
                  (stuff-border ri0 0 (- ri ri0) ncols :row-group (table-row-group-style row-group))))))
      ;; Do the border of the cells
      (map-table table
                 (lambda (cell ri ci)
                   (stuff-border ri ci (table-cell-rowspan cell) (table-cell-colspan cell)
                                 :cell (table-cell-style cell))))
      (values
       hborders
       vborders) )))

;;; Okay that is only half of the work needed. The other half is
;;; actually drawing these wicked borders.

;;; Drawing table borders

;;; Collapsing border model

;; In the collapsing border model a cell cannot have margins and its
;; border width is only accounted half for since it shares the border
;; with its neighbors. The table itself probably can have margins but
;; the same calculations wrt the border applies.

;; Uff we will also get floating tables and stuff ... But then doesn't
;; float force the display to block?

;;; Overview

;; In the first pass we cope with generated content, markers, run-in
;; and compact stuff and just generate a kind of formatting object
;; hierachy.

;; Tables also get collected here as well as whitespace handling and
;; text-transform and first-line, first-letter stuff.

;; where are the columns supposed to be

#||
(defun popo-table-stuff (q ss)
  ;; this is called when every we see some table element.
  (popo-collect-table q ss)
  )
||#

;;;; TABLE TODO

;; Collecting a table should happen much earlier. We want it in the
;; same stage we build the block-boxen and para-boxen.

;; Then we could also cope with situations like just having a few
;; table cells without having a table around it.

;; Then CSS-2's idea of caption is a. nuts and b. hopelessly
;; underspecified. c. somewhat incompatible with HTML-4.01.

;; The implication of the CSS table model is that cells have neigher
;; margin nor border.

;; Also my minmax should be more sensible to actually specified
;; margins.

;; ### we still have a probelm with width and margin et al. Our
;;     accessors are supposed to return the actual computed dimensions,
;;     yet dimensions of tables and their cells are are computed in the
;;     table type setter.
;;
;;     so either do it there for tables and table cells also. Or do as we
;;     previously did and compute the width and margins while typesetting.
;;     But:
;;
;; ### Since tables observe floating boxen in a strange way, we generally
;;     cannot compute width et al outside the renderer.
;;
;; So we might want to switch back. We introduced this because markers
;; have a containing block which is different to the containing block
;; of the line box they are rendered into. So we probably should
;; switch it back insofar as that the width attribute is not fully
;; computed and that percentage references to the containig block
;; refer this containing block not via a style-node but via a
;; block-box or something similar.


(defun sgml-unparse (element sink)
  (cond ((text-element-p element)
         (princ (rod-string (element-text element)) sink))
        (t
         (format sink "<~A" (element-gi element))
         (do ((q (sgml:pt-attrs element) (cddr q)))
             ((null q))
           (format sink " ~A=~S" (car q) (rod-string (cadr q))))
         (format sink "~%>")
         (dolist (child (element-children element))
           (sgml-unparse child sink))
         (unless (member (element-gi element) '(:img :br))
           (format sink "</~A>" (element-gi element))))))

;;; BUGS

;; ### Our handling of <BR><BR> is slightly off since netscape inserts a
;;     full blank line, we do not.

;; ### That is <BR> is mapped to a block level element instead of to BR:before{content:"\0a";}
;;     And then \0A is not handled correctly for content.

;; ### the table typesetter must acknowledge text-align.

;; ### background on table is not honoured.
;; ### parser incompatiblity:
;;     <td> <div> <td>    NS: <td> <div> </div> </td> <td>
;;                        we: something other
;;

;; ### incompat: NS still does not parse correctly.
;;     e.g. is <P>x<TABLE></TABLE> parsed as
;;             <P>x<TABLE></TABLE></P>
;;     instead of <P>x</P><TABLE></TABLE>
;;     same with opera.

;; ### we need a facility to find the baseline of a block-box
;;     [this is similar to the marker boxen issue]
;;

;; ### by accident i hit an incompatibility: NS apperently reads
;; <th<th> as <TH> while I'll read <TH><TH>

;; ### somebody is so kind and draws a border around our tables, which
;;     we do not want, since tables draw their borders themself. Guess tables
;;     are wraped into a superfluous block-box.

;; ### somehow the background and other style of the body gets lost ...

;; ### For no appearent reasons we fail the vertical margin test.
;;     --> we might revert to an evil global variable kludge.
;;         (although this is bad in the context of dynamic
;;         effects).
;;     I guess this is fixed now, but need to check.

;; ### "17.2.1 Anonymous table objects" is still not fully e.g. a
;; single table-cell element will not automatically infer the needed
;; stuff.

;; ### table-footer-group
;; ### table-header-group
;; ### table-caption
;; ### inline-table
;; ### 17.2.1 Anonymous table objects
;; ### visibility
;; ### iirc HTML has some abbreviation kludge to expand a COLGROUP into multiple columns.

;; ### For performance we definitely should optimize the case that there is no
;;     first-line pseudo element at all!

;; ### before/after on non-inline boxen
;; ### marker
;; ### run-in
;; ### compact

;; ### make sure setup-style is called _once_.

;; ### also from looking at the style stacks: first-line style is only applied to blocks within blocks -- art.

;; ### we need clear

;; ### since we might duplicate work anyway we should always recurse into blocks
;;     by means of po-para.

;; ### see if those pseudo elements are always closed.
;;     hmm ...

;; ### Q: where exactly should first-letter and first-line apply?

;; ### Does run-in alter the first-letter / first-line of an
;;     paragraph? That is: where exactly has a run-in to be inserted?

;; ### think about hyphenation -- but probably only hyphenate the
;;     words where that turns out neccessary.

;; ### We should think about performance. I imagine that all this
;;     first-letter and first-line stuff costs quite a bit of In 99%
;;     of the cases there is no first-line or first-letter style
;;     involved.

;; ### block level replaced elements.

;; ### systematically collect all properies about to be handled here
;;     and skim the code to make sure they are implemented properly.

;; ### white-space over a paragraph is constant, so optimize this case
;;     also.

;; ### text-indent: provide for it also in minmax
;; ### text-indent: only do it for the first-line of a paragraph
;;                  (use same definition as first-line).
;; ### text-indent: how to justify? guess we just include it.
;; ### text-indent: what exactly is its containing block?

;; ### text-shadow is just nuts!

;; ### now open chunks and stuff must either
;;     a. get their assigned values for width et al
;;     b. get a reference to the 'containing block'
;;     OR:
;;     c. we put resolving width into my-setup-style

;; ### handle all content variants

;; ### test markers attached to floating boxen

;; ### we have problems with para-boxen entirely consisting out of floating
;;     boxen as they shouldn't be considered exactly the same.
;;     [what would be a good solution to this?]
;;     so we need to reinstall the first-line stuff.
;;     so maybe just maybe we we need this dangling white-space thing again ;(

;; ### drawing order in case of floating boxen.

;;; popo pass

;; ### think about old hacks for making stuff lazy and on-demand.
;; ### possibly empty paras
;; ### possibly proper BR handling
;; ### dangling white space
;; ### first char condition should end with a white space.

;; ### in vertically-align-line haben wir wieder nicht-gepaarte chunks!
;;     z.b. www.w3.org ;-(
;;

;; ### justify

;; ### in PRE verschwinden leere zeilen.

;; ### in PRE: vernuenftige tab behandlung.

;; ### clear auf run-in, compact;

;; ### Question:
;;     when i have
;;     <p><span style='border: 1px solid'>foo<span style='display:block'></span>bar</span></p>
;;     how is that supposed to be rendered. should the border be open at the RHS of "foo" or not?
;;

;; BUG: word-spacing probably also has to apply to &nbsp;.

;; BUG: generally we flush too late, we should flush as soon any non
;; block content about to be rendered. Currently we solve that by
;; flushing in add-rune* which is quite expensive. We should post-pone
;; that until the first word is finished.

;; BUG: Also generally we should keep more track of the various
;; states like first line, first char, first word etc to spare the
;; possibly expensive test we have now.

;; BUGS

;; . The last white-spaces are not removed.
;;   => That is: a white space has only to be emitted when there is
;;      both black content before and after it.

;; ### <http://www.meyerweb.com/eric/css/edge/curvelicious/demo.html>
;;     probably a similiar drawing order issue. otherwise: okay.

;; ### <http://www.meyerweb.com/eric/css/edge/boxpunch/demo.html>
;;     drawing order issue.

;; ### <http://www.meyerweb.com/eric/css/edge/> does not look right.
;;     yes, abs positioned boxes are missing.

;; ### <http://lists.w3.org/Archives/Public/www-svg/2000Mar/0039.html> is fine now besides <PRE>

;; ### <http://research.yale.edu/lawmeme/modules.php?name=News&file=article> round borders are
;;     missing (background or something?)
;;     Yep, background on the TD cells that is.

;; ### we just came across the following:
;;
;;   #<LINE-FRAGMENT {488267D5}> is an instance of type LINE-FRAGMENT
;;      it has the following slots:
;;              CHUNKS: (#{<P>:first-line} #{<P>:first-letter} #{"T"}
;;                       #{</P>:first-letter} #{"he"} #{" "} #{"style"} #{" "}
;;                       #{"declarations"} #{" "} #{"which"} #{" "} #{"apply"}
;;                       #{" "} #{"to"} #{" "} #{"the"} #{" "} #{"text"} #{" "}
;;                       #{"below"} #{" "} #{"are:"})
;;         BLOCK-STYLE: #<COOKED-STYLE for P>
;;                  X2: 780
;;                  X1: 20
;; That is the line is probably closed. Especially the </P:firstline> is missing.
;; This is kind of "fixed".

;;; Solved

;; test44: a few problems with inline images:
;;             . vertical border and padding is not accounted for in line height calculation
;;             . border and image coördinates are out of synch vertically.
;;     => We need to talk again about geometry.
;;
;;     The "bottom" of a replaced object is the bottom including margin, padding and border.
;;     The "top" likewise.
;;
;;     Note: This is different to non replaced inline elements, where
;;     margins and border are not included in the line height calculation.
;;
;;     The generated fo sequence:
;;
;;     #<BLOCK-BOX :HTML
;;         #<BLOCK-BOX :BODY
;;             #<BLOCK-BOX :P
;;                 #<PARA-BOX (#{<IMG>} #<REPLACED-OBJECT-CHUNK {481EF905}>
;;                             #{</IMG>})>>>> 
;;
;;
;;     => It seems that we should not generate open and close chunks
;;     for IMG, but have that directly in the replaced object chunk
;;     for special threatment.
;;
;;     But then dis defeats block level replaced objects, which are
;;     now nicely represented by:
;;
;;     #<BLOCK-BOX :HTML
;;         #<BLOCK-BOX :BODY
;;             #<BLOCK-BOX :P
;;                 #<BLOCK-BOX :IMG
;;                     #<PARA-BOX (#<REPLACED-OBJECT-CHUNK {488BE5AD}>)>>>>>
;;
;;     and rendered almost correct.
;;

;; further clean up: e.g. popo-block always returns a list of one element
;; [this ripples up to e.g. floating-chunk-content etc]

;; in qc.html we still have a problem with vertical align.

;; <http://www.meyerweb.com/eric/css/inline-hades.html> seems to be fine now.

;; quite things happen:
;;   0: (COMPUTE-WIDTHS-AND-MARGINS #<CSS::COOKED-STYLE for BODY>)
;;    1: (COMPUTE-WIDTHS-AND-MARGINS #<CSS::COOKED-STYLE for BODY>)
;;      2: (COMPUTE-WIDTHS-AND-MARGINS #<CSS::COOKED-STYLE for HTML>)
;;
;; why does COMPUTE-WIDTHS-AND-MARGINS on BODY call
;; COMPUTE-WIDTHS-AND-MARGINS on BODY again?


(defun describe-table (table)
  (fresh-line)
  (with-slots (column-groups row-groups) table
    (format T "Column groups:~%")
    (dolist (cg column-groups)
      (format T "  Group~%")
      (dolist (col (slot-value cg 'columns))
        (format T "    Column [min ~D max ~D]~%"
                (slot-value col 'minimum-width)
                (slot-value col 'maximum-width))))
    (format T "Row groups:~%")
    (dolist (rg row-groups)
      (format T "  Rowgroup~%")
      (dolist (row (slot-value rg 'rows))
        (format T "    Row~%")
        (dolist (cell (slot-value row 'cells))
          (apply #'format T "      Cell [colspan ~D rowspan ~D col-index ~D]~%"
                 (mapcar (curry #'slot-value cell) '(colspan rowspan col-index))))))))

;;;;

(defvar *hyphenation-table* nil)

(defun hyphenation-table ()
  (or *hyphenation-table*
      (setf *hyphenation-table*
            (read-hyphen-table-file "/home/closure/closure/resources/patterns/english.ptn"))))

(defun hyphenate-items (items w)
  "This takes a chunk list and applies hyphenation it it."
  ;; The first thing we need to do is to identify words. Luckily words
  ;; are already separated by disk-chunks (white stuff).
  ;;
  ;; ### the proper half-chunks are missing.
  (let ((curword nil)
        (words nil))
    (labels ((spill-word ()
               (when curword
                 (push (reverse curword) words))
               (setf curword nil)))
      (loop for i from 0
            for x in items do
            (typecase x
              (black-chunk
               (let ((data (black-chunk-data x)))
                 (loop for j from 0
                       for c across data do
                       (cond ((or (<= #/a c #/z)
                                  (<= #/A c #/Z))
                              (push (list i j c) curword))
                             (t
                              (spill-word))))))
              (disc-chunk
               (spill-word))
              (otherwise
               )))
      (spill-word)
      '(dolist (word words)
        (let* ((s (map 'string (lambda (x) (code-char (third x))) word))
               (hps (hyphen-points (hyphenation-table) s)))
          (dolist (hp (reverse hps))
            (destructuring-bind (i j c) (elt word hp)
              (declare (ignore c))
              (let ((chunk (elt items i)))
                (setf items (append (subseq items 0 i)
                                    (list (cons-black-chunk :style (black-chunk-style chunk)
                                                            :data  (subseq (black-chunk-data chunk) 0 j))
                                          (make-instance 'disc-chunk
                                                         :%before (list
                                                                   (cons-black-chunk :style (black-chunk-style chunk)
                                                                                     :data  (rod "-")))
                                                         :%after nil
                                                         :%here nil)
                                          (cons-black-chunk :style (black-chunk-style chunk)
                                                            :data  (subseq (black-chunk-data chunk) j)))
                                    (subseq items (+ i 1)) )))))))
      (let ((res nil))
        (dolist (line (texpara::format-paragraph (items-to-tex-nodes items) w))
          (setf res (append res
                            (if (not (null res))
                                (list
                                 (make-instance 'disc-chunk
                                                :%before nil :%after nil :%here nil :forcep t))
                                nil)
                            (tex-line-to-items line))))
        res))))
      items)))

(defun items-to-tex-nodes (items)
  (map 'list (lambda (item)
               (etypecase item
                 (black-chunk
                  (cond ((equalp (black-chunk-data item) (rod " "))
                         (texpara::make-white-space-glue (chunk-width item)))
                        (t
                         (texpara::make-box :width (chunk-width item) :data item))))
                 ((or bounding-chunk replaced-object-chunk kern-chunk)
                  (texpara::make-box :width (chunk-width item) :data item))
                 (disc-chunk
                  (cond ((and (black-chunk-p (car (disc-chunk-here item)))
                              (null (cdr (disc-chunk-here item)))
                              (equalp (black-chunk-data (car (disc-chunk-here item))) (rod " ")))
                         (texpara::make-white-space-glue (chunk-width (car (disc-chunk-here item)))))
                        (t
                         (texpara::make-discretionary
                          :pre (items-to-tex-nodes (disc-chunk-before item))
                          :post (items-to-tex-nodes (disc-chunk-after item))
                          :no (items-to-tex-nodes (disc-chunk-here item))))))))
       items))

(defun tex-line-to-items (line)
  (map 'list (lambda (node)
               (etypecase node
                 (texpara::box
                  (texpara::box-data node))
                 (texpara::glue
                  (make-kern-chunk (+ (texpara::glue-width node) (texpara::glue-assigned node))))))
       line))




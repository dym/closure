(defpackage :texpara)

(in-package :texpara)

;;; High Quality Paragraph Typesetter

;; This is an alternate high quality typesetter inspirit by TeX's. (cf. The
;; TeXbook; chapter 14, and TeX source code section 813ff)

;;; Bugs:
;;; hyphenation:
;;;  - an #\- orginally present is lost
;;;  - will be confused by kerning
;;;  - slow
;;;

(defstruct node
  demerit)

(eval-when (compile eval load)
  (defstruct (glue (:include node))
    width
    shrink
    shrink-unit
    stretch
    stretch-unit
    assigned))

(defstruct (box (:include node))
  width
  data
  font)

(defstruct (discretionary (:include node))
  pre
  post
  no)

;; Parameters

(defparameter *line-penality* 10)
(defparameter *hyphen-penality* 50)
(defparameter *exhyphen-penality* 50)

(defparameter *pre-tolerance* 100)
(defparameter *tolerance* 200)

(defparameter *tolerance* 400)

(defconstant +badness-infinite+ 10000)

(defconstant +hfil-glue+
    ;; a \hfil fake
    (make-glue :width 0
               :shrink 0
               :shrink-unit 0
               :stretch 1e6
               :stretch-unit 0))

;(defconstant c/bullet #x2022)
(defconstant c/bullet #x6f)

(defstruct break-point
  position
  delta-width
  cache)                          ;from this to next

(defparameter *precision* 1)

(defun break-points (boxen)
  (let* ((res (cons (make-break-point :position -1) nil))
         (bpf res)
         (n (length boxen))
         (delta 0))
    (do ((i 0 (+ i 1)))
        ((= (the fixnum i) (the fixnum n))
         (setf (break-point-delta-width (car bpf)) delta)
         res)
      (let ((box (svref boxen i)))
        (cond ((or (glue-p box) (discretionary-p box))
               (setf (break-point-delta-width (car bpf)) delta
                     (cdr bpf) (cons (make-break-point :position i) nil)
                     bpf (cdr bpf)
                     delta 0))
              (t
               (incf delta (box-width box))))))))

(defmacro map-split-points (boxen bp cont)
  ;; bp is the break point just where the last break occurred
  `(let ((i (break-point-position (car ,bp)))
         (box)
         (ddw 0)
         (nw 0)                         ;natural width
         (w+ 0)                         ;stretchability
         (w- 0))                        ;shrinkability
     ;; The break just before the current line might have been a \discretionary
     ;; node; in case of that consider the post material
     (cond ((and (>= i 0) (discretionary-p (svref ,boxen i)))
            (mapc (lambda (box)
                    (cond ((box-p box)
                           (incf nw (box-width)))
                          (t
                           (error "Oops"))))
                  (discretionary-post (svref ,boxen i)))))
     ;; Now consider all other break points
     (loop
       (when (null (cdr ,bp))
         (return))
       ;; go to next break point
       (incf nw (break-point-delta-width (car ,bp)))
       (setf ,bp (cdr ,bp))
       (setf i (break-point-position (car ,bp)))
       (setf box (svref ,boxen i))
       ;; pretend we would break here
       (setf ddw 0)
       (cond ((discretionary-p box)
              (mapc (lambda (box) (incf ddw (box-width box))) (discretionary-pre box))))
       (incf nw ddw)

       (,cont ,bp nw w+ w-)

       ;; now pretend we would not break here
       (cond ((discretionary-p box)
              (decf nw ddw)             ;cancel effect of pre
              (mapc (lambda (box) (incf ddw (box-width box))) (discretionary-no box))))
       (cond ((glue-p box)
              (incf nw (glue-width box))
              (incf w+ (glue-stretch box))
              (incf w- (glue-shrink box)))) )))

(defmacro map-feasible-split-points (boxen bp width cont)
  `(block raus
     (map-split-points ,boxen ,bp
                       (lambda (bp nw w+ w-)
                          (let ((badness (badness2 nw w+ w- ,width)))
                            (when (> badness +badness-infinite+)
                              ;; overful box already
                              (return-from raus))
                            (when (< badness *tolerance*)
                              (,cont bp badness)))))))

(defun minimum-split (boxen width)
  (let ((memo nil))
    (mapcar (lambda (x)
              (break-point-position (caar x)))
            (minimum-split* memo boxen (break-points boxen) width))))

(defun minimum-split* (memo boxen bp width)
  (let (res)
    (cond ((null (cdr bp))
           (values nil 0))
          ((setf res (break-point-cache (car bp)));muessen wir noch modifizieren
           (values-list res))
          (t
           (values-list
            (setf (break-point-cache (car bp))
              (multiple-value-list
               (minimum-split** memo boxen bp width))))))))

(defun minimum-split** (memo line3 bp3 width3)
  (cond ((null bp3)
         (values nil 0))
        (t
         ;;(princ "@") (finish-output)
         (let ((best-bp nil)
               (best-bad nil)
               (best-pts nil)
               (best-d #.(expt +badness-infinite+ 2)))
           (map-feasible-split-points line3 bp3 width3
                                      (lambda (bp badness)
                                        (multiple-value-bind (points demerit)
                                            (minimum-split* memo line3 bp width3)
                                          (setf demerit (+ (expt (+ *line-penality* badness) 2)
                                                           demerit))
                                          (when (or (null best-d) (< demerit best-d))
                                            (setf best-d demerit
                                                  best-bp bp
                                                  best-bad badness
                                                  best-pts points)))))
           (values (and best-bad
                        (cons (cons best-bp best-bad) best-pts))
                   best-d)))))


;;;;;;;;;;

(defun map-line (fun boxen start end)
  "Map function to all elements, which would make up the line between start
   and end."
  (let ((end end))
    ;; forget leading glue
    (do ()
        ((not (and (glue-p (elt boxen start))
                   (< (1+ start) end))))
      (incf start))
    ;; forget dangling glue
    ;; don't do that when at end of paragraph though
    (unless (= end (length boxen))
      (do ()
          ((not (and (glue-p (elt boxen (1- end)))
                     (< start (1- end)))))
        (decf end)))
    ;; loop
    (do ((i start (+ i 1)))
        ((>= i end))
      (setq *i* i)
      (let ((box (elt boxen i)))
        (setq *i* (- i))
        (cond ((discretionary-p box)
               (cond ((= i start)
                      (mapc fun (discretionary-post box)))
                     ((= i (1- end))
                      (mapc fun (discretionary-pre box)))
                     (t
                      (mapc fun (discretionary-no box)))))
              ((funcall fun box))))))
  ;; Special case:
  ;; when at end of boxen think yourself a \hfil glue
  '(cond ((= end (length boxen))
         (funcall fun +hfil-glue+)) ))

(defun badness2 (nw w+ w- width)
  (let ((delta (- width nw)))
    (cond ((= delta 0)
           ;; perfect fit!
           0)
          ((< delta 0)
           (cond ((= w- 0)
                  +badness-infinite+)
                 ((> (- delta) w-)
                  ;; overful box
                  (* 2 +badness-infinite+))
                 (t
                  (min +badness-infinite+ (badness3 (- delta) w-)))))
          ((> delta 0)
           (cond ((= w+ 0)
                  +badness-infinite+)
                 (t
                  (min +badness-infinite+ (badness3 delta w+))))) )))

(defun badness3 (a b)
  (floor (* a (floor (* a (floor (* 100 a) b)) b)) b))

;;;;

;;; little interface

(defvar *display* nil)
(defvar *window* nil)
(defvar *gcontext* nil)
(defvar *font* nil)
(defvar *font-italic* nil)
(defvar *font-bold* nil)

(defun white-space-p (char)
  (member char '(#\space #\newline 32 10)))

(defun boxen-from-text-aux (font string)
  (let ((last-was-white-p t)
        (res nil))
    (labels ((add-char (font char)
               (cond ((white-space-p char)
                      (cond (last-was-white-p
                             nil)
                            (t
                             (setf last-was-white-p t)
                             (push (make-white-space-glue font) res))))
                     (t
                      (setf last-was-white-p nil)
                      (push (make-char-box font char) res))))
             
             (walk (font x)
               (cond ((listp x)
                      (let ((font (ecase (car x)
                                    (I *font-italic*)
                                    (B *font-bold*)
                                    (R *font*))))
                        (mapc (lambda (x) (walk font x)) (cdr x))))
                     ((stringp x)
                      (dotimes (i (length x))
                        (add-char font (char x i)))) )) )
      (walk font string) )
    (reverse res)))
  
(defun boxen-from-text (string &optional (font *font*))
  (append (boxen-from-text-aux font string)
          (list (copy-glue +hfil-glue+))))

(defun line-subseq (boxen start end)
  (let ((res nil))
    (map-line (lambda (x) (push x res)) boxen start end)
    (reverse res)))

(defun render-line (x y line start end)
  (loop for i from start to (1- end) do
        (let ((node (elt line i)))
          (incf x (render-node x y node)))) )

(defmethod render-node (x y (node box))
  (setf (xlib:gcontext-font *gcontext*) (box-font node))
  (xlib:draw-glyph *window* *gcontext* (round x) (round y) (box-data node))
  (box-width node))

(defmethod render-node (x y (node glue))
  x y
  (+ (glue-width node)
     (glue-assigned node)))


(defun assign-glue (boxen width)
  (let ((nw 0)
        (sha 0)
        (sta 0))
    (dolist (k boxen)
      (etypecase k
        (BOX (incf nw (box-width k)))
        (GLUE 
         (incf nw (glue-width k))
         (incf sha (glue-shrink k))
         (incf sta (glue-stretch k)))))
    (let ((delta (- width nw)))
      (cond ((= delta 0)
             (dolist (k boxen)
               (and (glue-p k)
                    (setf (glue-assigned k) 0))))
            ((< delta 0)
             ;; shrink
             (dolist (k boxen)
               (and (glue-p k)
                    (multiple-value-bind (a lack) 
                        (round (if (zerop sha)
                                   (glue-width k)
                                   (* delta (/ (glue-shrink k) sha)))
                               *precision*)
                      (setf a (* *precision* a))
                      (decf delta a)
                      (decf sha (glue-shrink k))
                      (setf (glue-assigned k) a)))))
            ((> delta 0)
             ;; shrink
             (dolist (k boxen)
               (and (glue-p k)
                    (multiple-value-bind (a lack) 
                        (round (if (zerop sta)
                                   (glue-width k)
                                   (* delta (/ (glue-stretch k) sta)))
                               *precision*)
                      (setf a (* *precision* a))
                      (decf delta a)
                      (decf sta (glue-stretch k))
                      (setf (glue-assigned k) a)))) )) )))

(defun introduce-dangling-punctation (boxen)
  (cond ((null boxen) nil)
        ((and (box-p (car boxen))
              (find (box-data (car boxen)) ".,"))
         (list* (make-discretionary
                 :pre (list
                       (car boxen)
                       (make-box :width (- (box-width (car boxen)))
                                 :data #\space
                                 :font *font*))
                 :post nil
                 :no (list (car boxen)))
                (introduce-dangling-punctation (cdr boxen))))
        ((cons (car boxen)
               (introduce-dangling-punctation (cdr boxen))))))

;;;;;

(defstruct hyphen-table
  lang
  patterns
  longest-pattern)

(defun slurp-patterns (filename &key (lang nil))
  (let ((res (make-hash-table :test #'equal)))
    (with-open-file (input filename)
      (do ((line (read-line input nil nil) (read-line input nil nil)))
	  ((null line) 
	   (make-hyphen-table :lang lang
			      :patterns res
			      :longest-pattern 8))  ;xxx
	(let ((j (position #\space line)))
	  (when j
	    (setf (gethash (subseq line 0 j) res)
	      (map 'vector #'(lambda (x) (- (char-code x) (char-code #\0))) (subseq line (+ j 1))))))))))

(defun hyphen-points (hyphen-table str)
  (let ((ht (hyphen-table-patterns hyphen-table))
	(lp (hyphen-table-longest-pattern hyphen-table)))
    (setq str (concatenate 'string "." (map 'string #'(lambda (x)
							(if (alpha-char-p x) (char-downcase x) #\.))
					    str) "."))
    (let ((vals (make-array (+ (length str) 1) :initial-element 0)))
      (dotimes (i (length str) vals)
	(dotimes (j (max lp (- (length str) i)))
	  (let ((vec (gethash (subseq str i (min (+ i j 1) (length str))) ht)))
	    (dotimes (k (length vec))
	      (setf (aref vals (+ i k)) (max (aref vals (+ i k)) (aref vec k)))))))
      (loop for i from 0 to (1- (length vals))
	  when (and (oddp (aref vals i))
		    (>= (1- i) 2)
		    (>= (- (- (length vals) 3) (1- i)) 3)
		    (not (char= (char str (- i 1)) #\.)))
	  collect (1- i)))))

(defun hyphenate (ht string)
  (maplist (lambda (hps)
             (subseq string (car hps) (or (cadr hps) (length string))))
           (cons 0 (hyphen-points ht string))))

(defun hyphenate2 (ht string)
  (format nil "~{~A~#[~:;-~]~}" (hyphenate ht string)))

(defvar *hyphen-table* nil)

(defun hyphenate3 (ht boxen)
  (let ((string (map 'string (lambda (x) (or (code-char x) #\X))
                     (mapcar (lambda (x)
                               (if (box-p x)
                                   (box-data x)
                                 (error "No box: ~S" x)))
                             boxen))))
    ;; XXX Puh! This is confused by kerning!
    (progn (butlast ;; xxx???
            (apply #'nconc
                   (maplist (lambda (hps)
                              (when (find #\- string)
                                '(print (list string (car hps)
                                         hps
                                         (ignore-errors (progn (char string (+ 0 (car hps)))))
                                         (ignore-errors (char= (char string (+ 0 (car hps))) #\-)) )))
                              (nconc
                               (subseq boxen (car hps) (or (cadr hps) (length boxen)))
                               (when t
                                 (cond ((ignore-errors (char= (char string (+ 0 (car hps))) #\-))
                                        nil)
                                       (t
                                        (list (make-discretionary
                                               :pre (let ((hyp (make-char-box (box-font (car boxen)) #xAD)))
                                                      (list
                                                       hyp
                                                       #+(OR) (make-box :width (- (box-width hyp)) 
                                                                        :data #\space 
                                                                        :font *font*)))
                                               :post (list
                                                      )
                                               :no nil))))) ))
                            (cons 0 (hyphen-points ht string))))))))

(defun init-hyp ()
  (setf *hyphen-table*
    (or *hyphen-table*
        (slurp-patterns "/export/potato/home/gilbert-neu/project/closure/Aqua/resources/patterns/english.ptn"))))

;;;;;

(defun insert-hyphens* (boxen)
  (cond ((null boxen) nil)
        ((glue-p (car boxen))
         (cons (car boxen)
               (insert-hyphens* (cdr boxen))))
        (t
         (let ((p1 (position-if #'glue-p boxen)))
           (cond ((null p1)
                  boxen)
                 (t
                  (nconc (hyphenate3 *hyphen-table* (subseq boxen 0 p1))
                         (insert-hyphens* (subseq boxen p1)))))))))

(defun make-kern (font dx)
  (make-box :width dx
            :data 32
            :font font))

(defun make-white-space-glue (w)
  (make-glue :width w
             :shrink (/ w 3)
             :shrink-unit 0
             :stretch (* w 2)
             :stretch-unit 0))

(defun make-char-box (font rune)
  (if (member rune '(8 9 10 11 12 13))
      (setf rune 32))
  (let ((w #+NIL (rune-width font rune)
           #-NIL 3                      ;xxx
           ))
    (when (null w)
      (error "No width for ~D in ~S?" rune font))
    (make-box :width w :data rune :font font)))

(defun format-paragraph (boxen width)
  (let ((res nil))
    (setq boxen
      (append
       boxen
       (list
        ;; a hfil fake
        (make-glue :width 0
                   :shrink 0
                   :shrink-unit 0
                   :stretch 1e6
                   :stretch-unit 0) )))
    (setq boxen (insert-hyphens* boxen))
    ;;(setq boxen (introduce-dangling-punctation boxen))
    (let ((sps ))
      (setf boxen (coerce boxen 'vector))
      (setf sps (minimum-split boxen width))
      (let ()
        (do ((p0 0 p1)
             (p1 (pop sps) (pop sps)))
            ((null p1))
          (let ((ln (line-subseq boxen p0 (min (length boxen) (+ p1 1)))))
            (assign-glue ln width)
            (push ln res)) )))
    (reverse res)))

;;; Some parameters


;;;

(defun dump-line (sink line x y)
  (dolist (k line)
    (etypecase k
      (GLUE
       (incf x (+ (glue-width k) (glue-assigned k))))
      (BOX
       (draw-rune sink x y (box-font k) (box-data k))
       (incf x (box-width k))))))

(defun dump-lines (sink lines x y dy)
  (dolist (k lines)
    (unless (every #'glue-p k)
      (dump-line sink k x y)
      (incf y dy)))
  y)

;;; Generic Device Interface

(defun init-x11 ()
  (unless *display*
    (setf *display*
      (xlib:open-display ""))
    (setf *window*
      (xlib:create-window
       :parent (xlib:screen-root (xlib:display-default-screen *display*))
       :x 0 :y 0 :width 600 :height 300
       :background (xlib:screen-white-pixel (xlib:display-default-screen *display*))
       :backing-store :always))
    (setf *font*
      (xlib:open-font *display* "-*-times-medium-r-*-*-*-110-100-100-*-*-iso8859-1"))
    (setf *font-bold*
      (xlib:open-font *display* "-*-times-bold-r-*-*-*-110-100-100-*-*-iso8859-1"))
    (setf *font-italic*
      (xlib:open-font *display* "-*-times-medium-i-*-*-*-110-100-100-*-*-iso8859-1"))
    (setf *gcontext* 
      (xlib:create-gcontext
       :drawable *window*
       :foreground (xlib:screen-black-pixel (xlib:display-default-screen *display*))
       :background (xlib:screen-white-pixel (xlib:display-default-screen *display*))
       :font *font*))
    (xlib:map-window *window*)
    (xlib:display-finish-output *display*) ))


(defun minimum-split (boxen width)
  (mapcar (lambda (x)
            (break-point-position (caar x)))
          (minimum-split* boxen (break-points boxen) width)))

(defun minimum-split* (boxen bp width)
  (let (res)
    (cond ((null (cdr bp))
           (values nil 0))
          ((setf res (break-point-cache (car bp)));muessen wir noch modifizieren
           (values-list res))
          (t
           (values-list
            (setf (break-point-cache (car bp))
              (multiple-value-list
               (minimum-split** boxen bp width))))))))

(defun minimum-split** (line3 bp3 width3)
  (cond ((null bp3)
         (values nil 0))
        (t
         ;;(princ "@") (finish-output)
         (let ((best-bp nil)
               (best-bad nil)
               (best-pts nil)
               (best-d #.(expt +badness-infinite+ 2)))
           (map-feasible-split-points line3 bp3 width3
                                      (lambda (bp badness)
                                        (multiple-value-bind (points demerit)
                                            (minimum-split* line3 bp width3)
                                          (setf demerit (+ (expt (+ *line-penality* badness) 2)
                                                           demerit))
                                          (when (or (null best-d) (< demerit best-d))
                                            (setf best-d demerit
                                                  best-bp bp
                                                  best-bad badness
                                                  best-pts points)))))
           (values (and best-bad
                        (cons (cons best-bp best-bad) best-pts))
                   best-d)))))


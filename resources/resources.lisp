(in-package :gluser)

;;; Ich habe das jetzt satt! Wir lesen das default style sheet jetzt
;;; im rahmen des compilierens, dann gibt es spaeter keine
;;; enttaeuschungen.

  
#+NIL  (defparameter *resources-base-directory*
    *load-truename*)

(let ((load-truename *load-truename*))
  (setf (url:url-logical-host-translator "closure")
        (lambda (url)
          (let ((res (url:copy-url url)))
            (setf (url:url-protocol res) "file"
                  (url:url-host res) "localhost"
                  (url:url-path res)
                  (append
                   (list :absolute)
                   (butlast (cdr (pathname-directory (truename load-truename))))
                   (cdr (url:url-path url))))
            res))))

(format T "~&;; Parsing default style sheet~%")
(setf r2::*default-style-sheet* 
      (css::parse-style-sheet-from-url (url:parse-url "file://closure/resources/css/default.css")
                                       :name "Closure Default Style Sheet"))

(format T "~&;; Parsing DTD~% ")
(sgml:slurp-catalog (url:parse-url "file://closure/resources/dtd/catalog"))
(setf user::*html-dtd* (sgml:parse-dtd '(:public "-//W3C//DTD HTML 4.0 Frameset//EN")))
(format T "~&;; done~%")



#||
(defclass uri ())

(defclass url (uri)
  ((protocol   :initarg       :protocol
               :initform      nil
               :reader        url-protocol
               :documentation "NIL or a string denoting the protocol")
   (host       :initarg       :host
               :initform      nil
               :reader        url-host
               :documentation "NIL or a string")
   (port       :initarg       :port
               :initform      nil
               :reader        url-port
               :documentation "NIL or an integer")
   (user       :initarg       :user
               :initform      nil
               :reader        url-user
               :documentation "NIL or a string")
   (password   :initarg       :password
               :initform      nil
               :reader        url-password
               :documentation "NIL or a string")
   (path       :initarg       :path
               :initform      nil
               :reader        url-path
               :documentation "NIL or a list ([:relative | :absolute] . components)")
   (parameters :initarg       :parameters
               :initform      nil
               :reader        url-parameters
               :documentation "NIL or a string")
   (query      :initarg       :query
               :initform      nil
               :reader        url-query
               :documentation "NIL or a string or a list of pairs (<field> . <value>)")
   (anchor     :initarg       :anchor
               :initform      nil
               :reader        url-anchor
               :documentation "NIL or a string")))

(defvar *uri-schemata*
  (make-hash-table :test #'equal)
  "A hash table mapping from URI schema names (as strings) to URI classes.")

(defmacro define-url-schema (name class-name superclasses &rest more-defclass-fodder)
  `(progn
    (setf (gethash ',name *uri-schemata*)
     ',class-name)
    (defclass ,class-name ,superclasses ,@more-defclass-fodder)
    ))

(define-url-schema "http" http-url (url))
(define-url-schema "file" file-url (url))
(define-url-schema "ftp"  ftp-url  (url))

;; Note that we have a problem now: URL are defined to be parsed by
;; first chopping off the anchor and only then seeking out for the
;; schema.

(defmethod parse-url ((input string) &rest options &key &allow-other-keys)
  ;; find the first 
  )

(defmethod parse-url ((input uri) &rest options &key &allow-other-keys)
  (declare (ignorable options))
  uri)

(defun make-url (&key protocol host port user password path parameters query anchor)
  (make-instance 'url
                 :protocol protocol
                 :host host
                 :port port
                 :user user
                 :password password
                 :path path
                 :parameters parameters
                 :query query
                 :anchor anchor))
||#


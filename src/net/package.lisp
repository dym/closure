(defpackage :ws/netlib
  (:nicknames :netlib)
  (:use :glisp :url)
  (:export #:*options/connection-timeout*
           #:open-document
           #:with-open-document
           #:*trace-http-p*
           #:*document-cache-dir*
           #:uncommit-document-cache-index
           #:commit-document-cache-index
           
           #:*proxy-host*
           #:*proxy-port*
           #:*use-http-proxy-p*
           #:*anonymous-ftp-password*
          
           #:parse-mime-content-type    ;### yet to be defined
           #:find-mime-type

           #:parse-http-link-field
           ))


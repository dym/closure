(in-package :r2)

;; evil

(import '(closure-protocol:element-p
          closure-protocol:element-parent
          closure-protocol:element-children
          closure-protocol:element-attribute
          closure-protocol:element-gi
          closure-protocol:text-element-p
          closure-protocol:element-text

          ;; css support protocol
          closure-protocol:element-css-class
          closure-protocol:element-css-id
          closure-protocol:pseudo-class-matches-p
          closure-protocol:element-style-cache
          closure-protocol:element-implicit-style
          closure-protocol:element-explicit-style
   
          ;; renderer support protocol
          closure-protocol:element-replaced-element-1
          closure-protocol:element-replaced-element
          closure-protocol:*user-agent*
          closure-protocol:*document-language*
          closure-protocol:element-base-url
          closure-protocol:element-imap
          closure-protocol:render
          closure-protocol:root-element-embedded-style
          ))

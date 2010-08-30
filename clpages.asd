(defsystem :clpages
  :name "clpages"
  :author "acieroid"
  :license "MIT"
  :depends-on (:cl-fad :cl-ppcre :html-template)
  :serial t
  :components ((:file "unescape")
               (:file "clpages")))
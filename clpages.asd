(defsystem :clpages
  :name "clpages"
  :author "acieroid"
  :license "MIT"
  :depends-on (:cl-fad :cl-ppcre :html-template :local-time)
  :serial t
  :components ((:file "unescape")
               (:file "clpages")))
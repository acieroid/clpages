(defsystem :clpages
  :name "clpages"
  :author "acieroid"
  :license "MIT"
  :depends-on (:cl-fad :cl-ppcre :html-template :local-time :xmls)
  :serial t
  :components ((:file "unescape")
               (:file "clpages")))
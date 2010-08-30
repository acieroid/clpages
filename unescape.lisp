;;;; Unescape-html -- This package *only* unescapes html symbols, if
;;;; you want to escape symbols, use CL-WHO and its escape-string
;;;; function
(defpackage :unescape-html
  (:use :cl :cl-ppcre)
  (:export :unescape-html :unescape-symbol))

(in-package :unescape-html)

(defvar *escape-table* (make-hash-table :test 'equal))
(defparameter *escaped-scanner* (create-scanner "&[#x]?[\\w\\d]+;"))

(defmacro defchar (name char)
  `(setf (gethash ,name *escape-table*) ,char))

(defchar "euro" #\euro_sign)
(defchar "nbsp" #\no-break_space)
(defchar "lt" #\<)
(defchar "gt" #\>)
(defchar "amp" #\&)
(defchar "quot" #\")
(defchar "ndash" #\en_dash)

(defun unescape-symbol (str)
  "Take as argument a string like &amp; and return a char like #\&"
  (let* ((code-p (char= (elt str 1) #\#))
         (hexa-p (and code-p (char= (elt str 2) #\x)))
         (symbol (subseq str (cond (hexa-p 3)
                                   (code-p 2)
                                   (t 1))
                         (1- (length str)))))
    (if (or hexa-p code-p)
        (code-char (parse-integer symbol
                                  :radix (if hexa-p 16 10)))
        (gethash symbol *escape-table*))))

(defun unescape-html (str)
  (regex-replace *escaped-scanner* str
                 (lambda (match start end from to v1 v2)
                   (declare (ignore start end v1 v2))
                   (let ((res (unescape-symbol (subseq match from to))))
                     (if res
                         (string res)
                         (error "Can't unescape: ~a" (subseq match from to)))))))

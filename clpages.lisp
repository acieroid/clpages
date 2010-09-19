(defpackage :clpages
  (:use :cl :cl-fad :cl-ppcre :html-template :unescape-html)
  (:export gen))

(in-package :clpages)

(defparameter *author* "Quentin Stievenart")
(defparameter *mail* "acieroid -at- awesom -dot- eu")
(defparameter *base-url* "http://awesom.eu/~acieroid/")
(defparameter *directory* #p"/home/quentin/articles/")
(defparameter *template* (merge-pathnames "template.tmpl"))
(defparameter *atom-template* (merge-pathnames "atom.tmpl"))

(defparameter *get-articles-fun* 'get-articles)
(defparameter *get-title-fun* 'get-title)
(defparameter *get-link-fun* 'get-link)
(defparameter *get-date-fun* 'get-date)
(defparameter *get-timestamp-fun* 'get-timestamp)

(defun is-html (file)
  (scan-to-strings ".html$" (file-namestring file)))

(defun get-articles ()
  (remove-if-not
   (lambda (file)
     (and (is-html file)
          (not (string= "index.html" (file-namestring file)))))
   (list-directory *directory*)))

(defun get-sorted-articles ()
  (sort (funcall *get-articles-fun*) #'> :key *get-timestamp-fun*))

(defun get-timestamp (article)
  (file-write-date article))

(defun get-title (article)
  (let ((title
         (with-open-file (stream article :direction :input)
           (loop for line = (read-line stream nil nil)
              while line do
              (multiple-value-bind (ok title)
                  (scan-to-strings "<title>(.*)</title>" line)
                (when ok
                  (return (unescape-html (aref title 0)))))))))
    (if title title "")))

(defun get-link (article)
  (file-namestring article))

(defmacro format-date (date)
  `(multiple-value-bind
         (second minute hour date month year day-of-week dst-p tz)
       ,date
     (declare (ignore day-of-week dst-p hour minute second tz))
     (format nil "~2,'0d-~2,'0d-~2,'0d" year date month)))

(defun get-date (article)
  (format-date (decode-universal-time (funcall *get-timestamp-fun* article))))

(defun actual-date ()
  (format-date (get-decoded-time)))

(defun gen ()
  (let ((values (list
                 :author *author* :mail *mail* :base-url *base-url*
                 :date (actual-date) :articles
                 (mapcar (lambda (article)
                           (list :link (funcall *get-link-fun* article)
                                 :title (funcall *get-title-fun* article)
                                 :date (funcall *get-date-fun* article)))
                         (get-sorted-articles)))))
    (with-open-file (stream (merge-pathnames "index.html" *directory*)
                            :direction :output
                            :if-exists :supersede)
      (fill-and-print-template *template* values :stream stream))
    (with-open-file (stream (merge-pathnames "atom.xml" *directory*)
                            :direction :output
                            :if-exists :supersede)
      (fill-and-print-template *atom-template* values :stream stream))))

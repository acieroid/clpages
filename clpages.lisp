(defpackage :clpages
  (:use :cl :cl-fad :cl-ppcre :html-template))

(in-package :clpages)

(defparameter *directory* #p"/home/quentin/articles/")
(defparameter *template* #p"/home/quentin/articles/template.tmpl")

(defun is-html (file)
  (scan-to-strings ".html$" (file-namestring file)))

(defun get-articles ()
  (remove-if-not
   (lambda (file)
     (and (is-html file)
          (not (string= "index.html" (file-namestring file)))))
   (list-directory *directory*)))

(defun get-title (article)
  (let ((title
         (with-open-file (stream article :direction :input)
           (loop for line = (read-line stream nil nil)
              while line do
              (multiple-value-bind (ok title)
                  (scan-to-strings "<title>(.*)</title>" line)
                (when ok
                  (return (aref title 0))))))))
    (if title title "")))

(defun get-link (article)
  (file-namestring article)
  #|(let ((title (get-title article)))
    (when (string= title "")
      (error "Missing title in article: ~a" article))
    (format nil "<a href=\"~a\">~a</a>" (file-namestring article) (get-title article)))|#)

(defun get-date ()
  (multiple-value-bind
	(second minute hour date month year day-of-week dst-p tz)
      (get-decoded-time)
    (declare (ignore day-of-week dst-p))
    (format nil "~d-~d-~d ~d:~d:~d (GMT~@d)"
            year date month
            hour minute second
            tz)))

(defun gen-index ()
  (with-open-file (stream (merge-pathnames "index.html" *directory*)
                          :direction :output
                          :if-exists :supersede)
    (fill-and-print-template
     *template*
     (list
      :date (get-date) :articles
      (mapcar (lambda (article)
                (list :link (get-link article)
                      :title (get-title article)))
              (get-articles)))
     :stream stream)))

(defpackage :clpages
  (:use :cl :cl-fad :cl-ppcre :html-template))

(in-package :clpages)

(defparameter *directory* #p"/home/quentin/articles/")
(defparameter *template* (merge-pathnames "template.tmpl"))

(defun is-html (file)
  (scan-to-strings ".html$" (file-namestring file)))

(defun get-articles ()
  (remove-if-not
   (lambda (file)
     (and (is-html file)
          (not (string= "index.html" (file-namestring file)))))
   (list-directory *directory*)))

(defun get-sorted-articles ()
  (sort (get-articles) #'> :key #'file-write-date))

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
  (file-namestring article))

(defmacro format-date (date)
  `(multiple-value-bind
         (second minute hour date month year day-of-week dst-p tz)
       ,date
     (declare (ignore day-of-week dst-p))
     (format nil "~2,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d (GMT~@d)"
             year date month
             hour minute second
             tz)))

(defun get-date (article)
  (format-date (decode-universal-time (file-write-date article))))

(defun actual-date ()
  (format-date (get-decoded-time)))

(defun gen-index ()
  (with-open-file (stream (merge-pathnames "index.html" *directory*)
                          :direction :output
                          :if-exists :supersede)
    (fill-and-print-template
     *template*
     (list
      :date (actual-date) :articles
      (mapcar (lambda (article)
                (list :link (get-link article)
                      :title (get-title article)
                      :date (get-date article)))
              (get-sorted-articles)))
     :stream stream)))

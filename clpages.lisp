(defpackage :clpages
  (:use :cl :cl-fad :cl-ppcre :html-template :unescape-html)
  (:export gen))

(in-package :clpages)

(defparameter *title* "acieroid's pages")
(defparameter *author* "Quentin Stievenart")
(defparameter *mail* "acieroid -at- awesom -dot- eu")
(defparameter *base-url* "http://awesom.eu/~acieroid/articles")
;; be sure to have a trailing slash for *directory*
(defparameter *directory* #p"/home/quentin/articles/")
(defparameter *template* (merge-pathnames "template.tmpl"))
(defparameter *atom-template* (merge-pathnames "atom.tmpl"))

(defparameter *get-articles-fun* 'get-articles)
(defparameter *get-title-fun* 'get-title)
(defparameter *get-content-fun* 'get-content)
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

(defun get-content (article)
  (let (in-body)
    (with-output-to-string (out)
      (with-open-file (stream article :direction :input)
        (loop for line = (read-line stream)
           while (not (scan-to-strings "</body>" line))
           when (scan-to-strings "<body(.*)>" line)
           do (progn (setf in-body t)
                     (setf line (regex-replace "<body(.*)>" line "")))
           when in-body
           do (progn (princ line out)
                     (terpri out)))))))

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
                 :page-title *title*
                 :author *author* :mail *mail* :base-url *base-url*
                 :date (actual-date)
                 :date-rfc3339 (local-time:format-rfc3339-timestring
                                nil (local-time:now))
                 :articles
                 (mapcar (lambda (article)
                           (list :link (funcall *get-link-fun* article)
                                 :title (funcall *get-title-fun* article)
                                 :date (funcall *get-date-fun* article)
                                 :date-rfc3339
                                 (local-time:format-rfc3339-timestring
                                  nil (local-time:universal-to-timestamp
                                       (funcall *get-timestamp-fun* article)))
                                 :content (funcall *get-content-fun* article)))
                         (get-sorted-articles)))))
    (with-open-file (stream (merge-pathnames "index.html" *directory*)
                            :direction :output
                            :if-exists :supersede)
      (fill-and-print-template *template* values :stream stream))
    (with-open-file (stream (merge-pathnames "atom.xml" *directory*)
                            :direction :output
                            :if-exists :supersede)
      (fill-and-print-template *atom-template* values :stream stream))))

;;; Secondary backend, use an "index.txt" file to get timestamps. The
;;; index.txt contains lines like "foo.html:1284909626", the first
;;; field is the file name, the second field is the *unix* timestamp
;;; (obtained with date +%s).
(defparameter *index-file* (merge-pathnames "index.txt" *directory*))
(defvar *articles* (make-hash-table))
(defconstant unix-to-universal-time 2208988800)

(defun get-articles-index ()
  (with-open-file (stream *index-file* :direction :input)
    (loop for line = (read-line stream nil nil) while line
         for article =
         (multiple-value-bind (ok datas)
             (scan-to-strings "^(.+):([0-9]+)$" line)
           (when ok
             (let ((title (aref datas 0))
                   (timestamp (aref datas 1)))
               (setf (gethash title *articles*)
                     (+ (parse-integer timestamp) unix-to-universal-time))
               title)))
       when article collect article)))

(defun get-timestamp-index (article)
  (gethash article *articles*))

;;; Uncomment this if you want to use the index.txt behaviour
(setf *get-articles-fun* 'get-articles-index)
(setf *get-timestamp-fun* 'get-timestamp-index)

;;;; bilibili/accu.lisp
;;;; Screen comments crawler.

(in-package #:breakds.corpus-accu)

(defvar *bili-workspace* "/home/breakds/tmp/bilibili/")
(defparameter *bili-comment-root* "http://comment.bilibili.tv/")
(defparameter *bili-root* "http://www.bilibili.com/")

(defvar *bili-catalog* (make-hash-table :test #'equal))

(defparameter *bili-category-spec*
  '(("movie-west" "video/movie_west_1.html")
    ("movie-japan" "video/movie_japan_1.html")
    ("movie-chinese" "video/movie_chinese_1.html")
    ("movie-chinese" "video/movie_chinese_1.html")
    ("movie-other" "video/movie-movie-1.html")
    ("tv-drama" "video/tv-drama-1.html")
    ("animation" "video/part-twoelement-1.html")
    ("documentary" "video/tech-popular-science-1.html")
    ("entertain" "video/ent-variety-1.html")))

(declaim (inline bili-page-url))
(defun bili-page-url (page-name &key (page-number nil))
  (if page-number
      (format nil "~a~a?page=~a" 
              *bili-root* page-name page-number)
      (format nil "~a~a" *bili-root* page-name)))

(def-struct-wrapper crawl-bili-video-cid
  ("body .z .player-wrapper .scontent script"
   :cid (lambda (node) 
          (awhen (nth-value 1 (ppcre:scan-to-strings "cid=\([0-9]+\)"
                                                     (get-content node)))
            (and (> (length it) 0) (aref it 0))))))

(def-struct-wrapper crawl-bili-video-entry
  (".l-item .w-info .dm" :comments (lambda (node) (or (get-content-int node) 0)))
  (".l-item .title" :link (lambda (node) (get-attribute node "href")))
  (".l-item .title" 
   :cid (lambda (node) 
          (let ((link (get-attribute node "href")))
            (handler-case 
                (awhen (crawl-bili-video-cid (html-from-uri (bili-page-url link)
                                                            :encoding :utf-8))
                  (getf it :cid))
              (t () nil)))))
  (".l-item .title" :name (lambda (node) (get-attribute node "title"))))

(def-list-wrapper crawl-bili-video-list
    "body .z .main-inner .container-body .list-custom-wrp .left .video-list .vd-list-cnt .vd-list li"
  (lambda (node)
    (let ((entry (crawl-bili-video-entry node)))
      (format t "~a, cid = ~a, comments = ~a~%"
              (getf entry :name)
              (getf entry :cid)
              (getf entry :comments))
      entry)))

(def-struct-wrapper crawl-bili-category-pages
  (#.(mkstr "body .z .main-inner .container-body .list-custom-wrp .left "
            ".video-list .vd-list-cnt .pagelistbox span")
     :pages (lambda (node)
              (handler-case
                  (or (parse-integer (ppcre:scan-to-strings "[0-9]+" (get-content node)))
                      0)
                (t () 0)))))

(def-path bili-catalog (category-name &optional (workspace *bili-workspace*))
  (merge-pathnames (format nil "catalog/~a.lisp" category-name)
                   workspace))


;; ---------- Operational Functions ----------

(defun clear-bili-catalog ()
  (clrhash *bili-catalog*))

(defun crawl-bili-category (category-name category-index 
                            &key (start-page 1))
  (let* ((index-page-url (bili-page-url category-index))
         (pages (getf (crawl-bili-category-pages (html-from-uri index-page-url
                                                                :encoding :utf-8))
                      :pages)))
    (with-open-file (output (bili-catalog-path category-name)
                            :direction :output
                            :if-does-not-exist :create
                            :if-exists :append)
      (loop 
         for page-number from start-page to pages
         do  
           (format t "[~a] page ~a/~a ...~%" category-name page-number pages)
           (let ((collected (crawl-bili-video-list 
                             (html-from-uri (bili-page-url category-index 
                                                           :page-number page-number)))))
             (loop 
                for entry in collected
                for cid = (getf entry :cid)
                unless (gethash cid *bili-catalog* nil)
                do (let ((complete-entry (list :category category-name
                                               entry)))
                     (setf (gethash cid *bili-catalog*)
                           complete-entry)
                     (fresh-line output)
                     (write complete-entry :stream output)))
             (finish-output output)
             (sleep 2))))))

(defun crawl-bili-specified-categories (category-specs &optional (start-pages nil))
  (clear-bili-catalog)
  (loop 
     for spec in category-specs
     for category-number from 0
     do 
       (format t "---------- Category ~a ----------~%" (first spec))
       (handler-case
           (crawl-bili-category (first spec) (second spec)
                                :start-page (or (and start-pages
                                                     (nth category-number start-pages))
                                                1))
         (t () (format t "[ERROR] Failed to process category ~a~%" 
                       (first spec))))))
       
         
       
       





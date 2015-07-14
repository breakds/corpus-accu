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

(def-path bili-comment-buffer (cid &optional (workspace *bili-workspace*))
  (merge-pathnames (format nil "buffer/~a.xml" cid)
                   workspace))

(def-path bili-raw (&optional (workspace *bili-workspace*))
  (merge-pathnames "input/raw.txt" workspace))

(declaim (inline bili-comment-url))
(defun bili-comment-url (cid)
  (format nil "~a~a.xml" *bili-comment-root* cid))

(defun curl-bili-comment-xml (cid)
  (let* ((buffer-path (bili-comment-buffer-path cid)))
    (sb-ext:run-program "/usr/bin/curl"
                        (list "--compressed" (bili-comment-url cid)
                              "-o" (format nil "~a" buffer-path)))
    (let ((xml (html-from-file buffer-path)))
      (sb-ext:run-program "/bin/rm" (list "-f" (format nil "~a" buffer-path)))
      xml)))

(def-list-wrapper crawl-bili-comment
    "body i d"
  (lambda (node) (get-content node)))

;; ---------- Operational Functions ----------

(defun clear-bili-catalog ()
  (clrhash *bili-catalog*))

(defun reload-bili-catalog (category-specs)
  (clear-bili-catalog)
  (loop for spec in category-specs
     do (with-open-file (input (bili-catalog-path (car spec))
                               :direction :input
                               :if-does-not-exist :error)
          (loop 
             for entry = (read input nil nil)
             for fixed-entry = (if (listp (third entry))
                                   (list* (first entry) (second entry)
                                          (third entry))
                                   entry)
             while entry
             do (setf (gethash (getf fixed-entry :cid) *bili-catalog*)
                      fixed-entry)))))

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
                do (let ((complete-entry (list* :category category-name
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


(defun crawl-bili-raw-comments (&key (start 0) (log t))
  (with-open-file (output (bili-raw-path)
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :append)
    (let ((seen-comments 0)
          (kept-comments 0))
      (loop 
         for cid being the hash-keys of *bili-catalog*
         for entry being the hash-values of *bili-catalog*
         for i from 0
         when (and (>= i start) cid (> (getf entry :comments 0)))
         do (handler-case
                (let ((comments (crawl-bili-comment (curl-bili-comment-xml cid)))
                      (crawled-comments 0))
                  (when (> (length comments) 0)
                    (loop 
                       for comment in comments
                       for pieces = (split-into-chinese-pieces comment)
                       ;; total length threshold
                       when (> (apply #'+ (mapcar #'length pieces)) 5)
                       do (progn (fresh-line output)
                                 (format output "~{~a~^ ~}~%" pieces)
                                 (incf crawled-comments)))
                    (incf seen-comments (length comments))
                    (incf kept-comments crawled-comments)
                    (format log "~5a: [~a] ~a: ~d/~d (~2$%) ---- ~d/~d (~2$%)~%" i
                            (getf entry :category) (getf entry :name)
                            crawled-comments (getf entry :comments)
                            (float (/ (* crawled-comments 100) (getf entry :comments)))
                            kept-comments seen-comments
                            (float (/ (* kept-comments 100) seen-comments)))
                    (format t ".")
                    (finish-output log)
                    (finish-output output)))
              (t () nil))))))
                    
                    
                

                   
               
                                
       



  
       
         
       
       





;;;; subhd/accu.lisp
;;;; Collect subtitle corpus from subhd.com.

(in-package #:breakds.corpus-accu)

(defvar *subhd-max-new-pages* 50)

(defvar *subhd-uploaders* nil)

;;; ---------- uploaders ----------

(def-list-wrapper crawl-uploaders
    "body .container .row .col-md-9 .box .lb_r b a[~target]"
  (lambda (node) (get-attribute node "href")))

(def-struct-wrapper crawl-uploader-info
  ("body .text-center span b" 
   :count (lambda (node)
            (get-content-int node))))
                              


(defun uploader-table-path ()
  "Get the path to the file that stores the uploader info."
  (let ((path (merge-pathnames "uploaders.lsp" *workspace*)))
    (ensure-directories-exist path)
    path))

(defun get-subhd-new-page (index)
  (html-from-uri (format nil "http://subhd.com/subs/new/page/~a"
                         index)))

(defun load-uploader-table ()
  (setf *subhd-uploaders* (make-hash-table :test #'equal))
  (when (probe-file (uploader-table-path))
    (with-open-file (in (uploader-table-path)
                        :direction :input)
      (loop for uploader in (read in)
         do (setf (gethash uploader *subhd-uploaders*) t))))
  (format t "~a uploaders loaded from ~a."
          (hash-table-count *subhd-uploaders*)
          (uploader-table-path))
  *subhd-uploaders*)

(defun update-uploader-table ()
  (setf *subhd-uploaders* (make-hash-table :test #'equal))
  (loop for index from 1 to *subhd-max-new-pages*
     do (let ((uploaders (crawl-uploaders (get-subhd-new-page index)))
              (added 0))
          (format t "Processing page ~a " index)
          (loop for uploader in uploaders
             do (if (null (gethash uploader *subhd-uploaders*))
                    (progn (setf (gethash uploader *subhd-uploaders*) t)
                           (incf added)
                           (format t "O"))
                    (format t ".")))
          (format t " added: ~a~%" added)))
  (format t "Total: ~a uploaders." 
          (hash-table-count *subhd-uploaders*))
  (with-open-file (out (uploader-table-path)
                       :direction :output
                       :if-exists :supersede)
    (write (loop for uploader being the hash-keys of *subhd-uploaders*
              collect uploader)
           :stream out))
  (format t "Saved to ~a." (uploader-table-path)))

    
             
             
  
    

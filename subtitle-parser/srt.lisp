;;;; srt.lisp
;;;; Description: parse srt subtitle syntax.

(in-package #:breakds.subtitle-parser)

(defun extract-chinese-corpus-from-srt (input-path output-path
                                        &optional (log-stream t))
  "Return the number of extracted chinese text pieces."
  (let (extracted)
    (with-open-file (input input-path
                           :direction :input)
      (handler-case 
          (loop for line = (read-line input nil nil)
             while line
             do (handler-case 
                    (loop for piece in (split-into-chinese-pieces line)
                       do (push piece extracted))
                  (t () (format log-stream "[WARNING] Error while parsing ~a: ~a~%"
                                input-path line) nil)))

        (t () (format log-stream "[ERROR] Corrupted file ~a." 
                      input-path) nil)))
    (with-open-file (output output-path
                            :direction :output
                            :if-exists :supersede)
      (format output "~{~a~^ ~}" (reverse extracted)))
    (length extracted)))
           
           
    

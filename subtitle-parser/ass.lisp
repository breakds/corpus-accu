;;;; ass.lisp
;;;; Description: parse ass subtitle syntax.

(in-package #:breakds.subtitle-parser)

(defparameter *ass-subtitle-body*
  (ppcre:create-scanner "Dialogue: [^,]*,[^,]*,[^,]*,[^,]*,[^,]*,[^,]*,[^,]*,[^,]*,[^,]*,(.*)")
  "The regexp to extract the body of subtitle events.")

(define-string-lexer ass-lexer
  ("{" (return :open-bracket))
  ("}" (return :close-bracket))
  ("\\\\" (return :backslash))
  ("([^{}\\s\\\\]+)" (return (values :text $1))))

(define-parser ass-parser
  (:start-symbol ass-subtitle)
  (:terminals (:open-bracket :close-bracket :backslash :text))
  (special-symbol (:backslash :text #2`(:special-symbol ,x2)))
  (special-wrapped (:text special-wrapped #2`,(cons x1 x2))
		   (special-symbol special-wrapped
				   #2`,(cons x1 x2))
		   (special-entity special-wrapped
				   #2`,(cons x1 x2))
		   ())
  (special-entity (:open-bracket special-wrapped :close-bracket
				 #3`,(list* :special-entity x2)))
  (ass-subtitle (special-symbol ass-subtitle #2`,(cons x1 x2))
		(:text ass-subtitle #2`,(cons x1 x2))
		(special-entity ass-subtitle #2`,(cons x1 x2))
		()))

(defun ass-lexer-from-file (filename)
  (with-open-file (input filename
			 :direction :input)
    (let ((seq (make-array (file-length input)
			   :element-type 'character
			   :fill-pointer t)))
      (setf (fill-pointer seq) (read-sequence seq input))
      (let ((lexer (ass-lexer seq)))
	lexer))))

(defun is-chinese-character (character)
  (and (char>= character #\u4e00)
       (char<= character #\u9fa5)))

(defun split-into-chinese-pieces (text)
  (remove-if #`,(= (length x1) 0)
	     (split-sequence:split-sequence-if-not #'is-chinese-character
						   text)))


(defun extract-chinese-corpus-from-ass (input-path output-path)
  "Return the number of extracted chinese text pieces."
  (let (extracted)
    (with-open-file (input input-path
			   :direction :input)
      (loop for line = (read-line input nil nil)
	 while line
	 do (multiple-value-bind (matched subtitle-body)
		(ppcre:scan-to-strings *ass-subtitle-body* line)
	      (when matched
		(handler-case 
		    (let* ((lexer (ass-lexer (aref subtitle-body 0)))
			   (parsed (parse-with-lexer lexer ass-parser)))
		      (loop for item in parsed
			 when (stringp item)
			 do (loop for piece in (split-into-chinese-pieces item)
			       do (push piece extracted))))
		  (t () (format t "[WARNING] Error while parsing ~a~%"
				(aref subtitle-body 0)) nil))))))
    (with-open-file (output output-path
			    :direction :output
			    :if-exists :supersede)
      (format output "~{~a~^ ~}" extracted))
    (length extracted)))

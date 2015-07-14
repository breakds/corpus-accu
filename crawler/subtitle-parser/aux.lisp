;;;; Aux.lisp
;;;; Auxiliary subroutines for subtitle parsers.

(in-package #:breakds.corpus-accu.aux)

(defun is-chinese-character (character)
  (and (char>= character #\u4e00)
       (char<= character #\u9fa5)))

(defun split-into-chinese-pieces (text)
  (remove-if #`,(= (length x1) 0)
	     (split-sequence:split-sequence-if-not #'is-chinese-character
						   text)))



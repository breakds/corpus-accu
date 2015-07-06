;;;; utils.lisp
;;;; Utilities

(in-package #:breakds.corpus-accu)

(defun string-trim-all (str)
  (string-trim '(#\space #\newline #\tab) str))

(defmacro def-path (name args &body body)
  (with-gensyms (path)
    `(defun ,(symb name '-path) ,args
       (let ((,path (progn ,@body)))
         (ensure-directories-exist ,path)
         ,path))))
  


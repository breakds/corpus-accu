;;;; monster-avengers.asd

(asdf:defsystem #:corpus-accu
    :serial t
    :depends-on (#:struct-wrapper
                 #:basicl
                 #:sqlite
                 #:split-sequence
		 #:yacc
		 #:cl-lex
                 #:cl-ppcre)
    :components ((:file "package")
                 (:file "utils")
		 (:file "subtitle-parser/ass")
                 (:file "subhd/accu")
                 (:file "zimuzu/accu")))

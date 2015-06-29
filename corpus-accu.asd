;;;; monster-avengers.asd

(asdf:defsystem #:corpus-accu
    :serial t
    :depends-on (#:struct-wrapper
                 #:basicl
                 #:sqlite
                 #:split-sequence
                 #:md5
                 #:cl-fad
		 #:yacc
		 #:cl-lex
                 #:cl-ppcre)
    :components ((:file "package")
                 (:file "utils")
		 (:file "subtitle-parser/aux")
		 (:file "subtitle-parser/ass")
		 (:file "subtitle-parser/srt")
                 (:file "subhd/accu")
                 (:file "zimuzu/accu")))

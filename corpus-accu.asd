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
    :components ((:file "crawler/package")
                 (:file "crawler/utils")
		 (:file "crawler/subtitle-parser/aux")
		 (:file "crawler/subtitle-parser/ass")
		 (:file "crawler/subtitle-parser/srt")
                 (:file "crawler/subhd/accu")
                 (:file "crawler/zimuzu/accu")))

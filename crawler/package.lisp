(defpackage #:breakds.corpus-accu.aux
  (:nicknames #:corpus-accu.aux)
  (:use #:cl)
  (:export #:is-chinese-character
           #:split-into-chinese-pieces))

(defpackage #:breakds.subtitle-parser
  (:nicknames #:subtitle-parser)
  (:use #:cl #:yacc #:cl-lex #:breakds.corpus-accu.aux)
  (:export #:extract-chinese-corpus-from-srt
           #:extract-chinese-corpus-from-ass))

(defpackage #:breakds.corpus-accu
  (:nicknames #:corpus-accu)
  (:use #:cl #:swiss-knife 
        #:struct-wrapper
        #:html-operation
        #:breakds.corpus-accu.aux)
  (:export #:*zimuzu-max-new-pages*
	   #:*zimuzu-catalog*
	   #:*zimuzu-root*
	   #:*zimuz-sleep-interval*
	   #:build-catalog
           #:pathname-ty))


	      

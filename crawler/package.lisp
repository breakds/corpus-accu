(defpackage #:breakds.subtitle-parser
  (:nicknames #:subtitle-parser)
  (:use #:cl #:yacc #:cl-lex)
  (:export #:extract-chinese-corpus-from-srt
           #:extract-chinese-corpus-from-ass))

(defpackage #:breakds.corpus-accu
  (:nicknames #:corpus-accu)
  (:use #:cl #:swiss-knife 
        #:struct-wrapper
        #:html-operation)
  (:export #:*zimuzu-max-new-pages*
	   #:*zimuzu-catalog*
	   #:*zimuzu-root*
	   #:*zimuz-sleep-interval*
	   #:build-catalog
           #:pathname-ty))


	      

;;;; monster-avengers.asd

(asdf:defsystem #:corpus-accu
    :serial t
    :depends-on (#:struct-wrapper
                 #:basicl
                 #:sqlite
                 #:split-sequence
                 #:stefil)
    :components ((:file "package")
                 (:file "utils")
                 (:file "subhd/accu")
                 (:file "zimuzu/accu")))

(asdf:defsystem #:pjs-utils
  :serial t
  :depends-on ("bordeaux-threads" "trivial-backtrace")
  :components ((:file "package")
               (:file "utils")
	       (:file "symbols")
	       (:file "collectors")))


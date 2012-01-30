(defpackage #:pjs-utils
  (:use :cl)
  (:export #:sconc
	   #:sconc*
           #:dovector
           #:1++
           #:aif
           #:awhen
	   #:it
           #:with-hash-table-lookup
	   #:with-self-eval
           #:flatten
	   #:null-string-p

	   #:[]
	   #:{}

           #:join
           #:join+
	   #:join*
           #:n-copies
           #:group
           #:begins-with-p
           #:ends-with-p
           #:until
           #:while
           #:binary-search
           #:only-one-of-p
           #:pair-equal
           #:pop-nth
	   #:pop-nth-helper
           #:file-length-for-path
           #:trim
           #:ndelete
           #:read-number-from-string
	   #:first-after
	   #:filter
	   #:map-chars
	   #:rest-and-keywords

	   #:symb
	   #:gensymb
	   #:make-keyword
	   #:symbol-name*

	   #:with-collector
	   #:dolist-c
	   #:dotimes-c
	   #:while-c
	   #:until-c
	   #:collect
	   
	   #:{}
	   #:[]))


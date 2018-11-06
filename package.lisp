(defpackage #:pjs-utils
  (:use :cl)
  (:export #:sconc
	   #:dovector
	   #:bind
	   #:maybe-wrap-body
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

           #:n-copies
           #:group
           #:begins-with-p
           #:ends-with-p
           #:until
           #:while
	   #:->
	   #:->>
	   #:megaexpand
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
	   #:explode
	   #:partition
	   #:range
	   
	   #:symb
	   #:gensymb
	   #:make-keyword
	   #:symbol-name*

	   #:with-collector*
	   #:with-collector
	   #:with-collectors
	   #:dolist-c
	   #:dotimes-c
	   #:dovector-c
	   #:while-c
	   #:until-c
	   #:collect
	   
	   #:{}
	   #:[]))


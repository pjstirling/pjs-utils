(in-package #:pjs-utils)

(defmacro until (test &body body)
  `(do ()
       (,test)
     ,@body))

(defmacro while (test &body body)
  `(until (not ,test)
     ,@body))

(defmacro with-collector* ((name) &body body)
  (let ((result (gensym "RESULT-"))
	(tail (gensym "TAIL-")))
    `(let* ((,result (cons nil nil))
	    (,tail ,result))
       (flet ((,name (&rest args)
		(if args
		    (dolist (arg args)
		      (let ((new (cons arg nil)))
			(setf (cdr ,tail) new
			      ,tail new)))
		    ;; else
		    (cdr ,result))))
	 ,@body))))

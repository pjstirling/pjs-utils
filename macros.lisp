(in-package #:pjs-utils)

(defmacro until (test &body body)
  `(do ()
       (,test)
     ,@body))

(defmacro while (test &body body)
  `(until (not ,test)
     ,@body))

(defmacro with-collector* ((name) &body body)
  (let ((result (gensym "RESULT"))
	(tail (gensym "TAIL"))
	(func (gensym (symbol-name name))))
    `(let* ((,result (cons nil nil))
	    (,tail ,result))
       (flet ((,func (arg)
		(let ((new (cons arg nil)))
		  (setf (cdr ,tail) new
			,tail new))))
	 (macrolet ((,name (&optional (arg nil arg-p))
		      (if arg-p
			  (list ',func arg)
			  ;; else
			  '(cdr ,result))))
	   ,@body)))))


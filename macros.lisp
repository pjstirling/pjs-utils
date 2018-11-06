(in-package #:pjs-utils)

(defmacro until (test &body body)
  `(do ()
       (,test)
     ,@body))

(defmacro while (test &body body)
  `(until (not ,test)
     ,@body))

#+nil
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

#+nil
(defmacro with-collector* ((name) &body body)
  (let ((func (gensym "FUNC-"))
	(result (gensym "RESULT-"))
	(tail (gensym "TAIL-")))
    `(let* ((,result (cons nil nil))
	    (,tail ,result))
       (flet ((,func (arg)
		(let ((new (cons arg nil)))
		  (setf (cdr ,tail) new
			,tail new))))
	 (macrolet ((,name (&rest args)
		      (let ((func ',func)
			    (result ',result))
			(cond
			  ((null args)
			   `(cdr ,result))
			  ((null (rest args))
			   `(,func ,(first args)))
			  (t
			   `(progn
			      ,@ (mapcar (lambda (arg)
					   `(,func ,arg))
					 args)))))))
	   ,@body)))))

(defmacro with-collector* ((&rest names) &body body)
  (flet ((gensymb (stem suffix)
	   (gensym (concatenate 'string
				stem
				(string-upcase suffix)))))
    (let ((names (mapcar (lambda (name)
			   (let ((s (symbol-name name)))
			     (list name
				   (gensymb s "-head")
				   (gensymb s "-tail")
				   (gensymb s "-func"))))
			 names))
	  (arg (gensym "ARG"))
	  (args (gensym "ARGS"))
	  (new (gensym "NEW"))) 
      `(let* (,@ (mapcan (lambda (name)
			   (destructuring-bind (name head tail func)
			       name
			     (declare (ignore name func))
			     (list `(,head (cons nil nil))
				   `(,tail ,head))))
			 names))
	 (flet (,@ (mapcar (lambda (name)
			     `(,(fourth name) (,arg)
			       (let ((,new (cons ,arg nil)))
				 (setf (rest ,(third name)) ,new
				       ,(third name) ,new))))
		     names))
	   (macrolet (,@ (mapcar (lambda (name)
				   (destructuring-bind (name head tail func)
				       name
				     (declare (ignore tail))
				     `(,name (&rest ,args)
					       (cond
						 ((null ,args)
						  (list 'rest ',head))
						 ((null (rest ,args))
						  (list ',func (first ,args)))
						 (t
						  (list* 'progn
							 (mapcar (lambda (,arg)
								   (list ',func ,arg))
								 ,args)))))))
			   names))
	     ,@body))))))

(defmacro -> (form &rest functions)
  "Thread FORM through each FUNCTION, in turn, as first parameter."
  (dolist (fn functions)
    (setf form
	  (if (listp fn)
	      `(,(first fn)
		,form
		,@ (rest fn))
	      ;; else
	      `(,fn ,form))))
  form)

(defmacro ->> (form &rest fns)
  "Thread form through each function in turn as last parameter."
  (dolist (fn fns)
    (setf form
	  (if (listp fn)
	      (append fn
		      (list form))
	      ;; else
	      (list fn form))))
  form)

(defun megaexpand (form env)
  "Expand form if it is either a macro or a compiler macro form"
  (flet ((expand (form)
	   (if (listp form)
	       (let ((expander (compiler-macro-function (first form)
							env)))
		 (if expander
		     (let ((expanded (funcall expander
					      form
					      env)))
		       (values expanded
			       (not (eq expanded
					form))))
		     ;; else
		     (macroexpand form env)))
	       ;; else
	       form)))
    (let ((expanded-p t))
      (loop :while expanded-p
	    :do (multiple-value-setq (form expanded-p)
		  (expand form))
	    :return form))))

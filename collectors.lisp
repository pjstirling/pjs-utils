(in-package #:pjs-utils)

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-introspect))

(defmacro with-collector ((name) &body body)
  `(with-collector* (,name)
     ,@body
     (,name)))

(defmacro with-collectors ((&rest names) &body body)
  (let ((body `(progn ,@body)))
    (dolist (name (reverse names))
     (setf body `(with-collector* (,name) ,body)))
    body))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun split-list (separator list &optional (test #'eq))
    (let ((index (position separator list :test test)))
      (if index
	  (values (subseq list 0 index)
		  (subseq list (1+ index)))
	  ;; else
	  (values list
		  nil))))

  (defun multi-split-list (separators list &optional (test #'eq))
    (let (result)
      (dolist (sep (reverse separators))
	(multiple-value-bind (prefix chunk) (split-list sep list test)
	  (push chunk result)
	  (setf list prefix)))
      (push list result)
      (values-list result)))

  (defun arg-names-for-macro-lambda-list (list)
    (when (< 1 (count '&environment list))
      (error "multiple &environment"))
    (when (< 1 (count '&whole list))
      (error "multiple &whole"))
    (awhen (position '&whole list)
      (unless (= it 0)
	(error "&whole not at start")))
    (let* ((env (first-after '&environment list))
	   (whole (first-after '&whole list))
	   (list (remove-if #'(lambda (form)
				(member form `(&whole ,whole
						      &environment ,env
						      &allow-other-keys)))
			    list))
	   result)
      (multiple-value-bind (required
			    optional
			    rest
			    body
			    key
			    aux)
	  (multi-split-list '(&optional &rest &body &key &aux) list)
	(declare (ignore aux))
	(when env
	  (push env result))
	(when whole
	  (push whole result))
	;; required variables
	(dolist (var required)
	  (if (listp var)
	      (setf result
		    (append result
			    (arg-names-for-macro-lambda-list var)))
	      ;; else
	      (push var result)))
	;; optional variables
	(dolist (var optional)
	  (if (listp var)
	      (let ((name (first var)))
		(if (listp name)
		    (setf result
			  (append result
				  (arg-names-for-macro-lambda-list name)))
		    ;; else
		    (push name result)))))
	(when rest
	  (push (first rest) result))
	(when body
	  (push (first body) result))
	;; keyword variables
	(dolist (var key)
	  (if (listp var)
	      (let ((name (first var)))
		(if (listp name)
		    (if (keywordp (first name))
			(setf result
			      (append result
				      (arg-names-for-macro-lambda-list (second name))))
			;; else
			(push (first name) result))
		    ;; else
		    (push name result)))
	      ;; else
	      (push var result))))
      result)))

#+sbcl
(defmacro define-collector (form-name)
  (let* ((args (sb-introspect:function-lambda-list form-name))
		    (arg-names (arg-names-for-macro-lambda-list args))
		    (whole (aif (first-after '&whole args)
				it
				;; else
				(gensym))))
	       `(defmacro ,(symb form-name "-c") (&whole ,whole ,@args)
		  (declare (ignore ,@(remove whole arg-names)))
		  `(with-collector (collect)
		     (,',form-name ,@(rest ,whole)))
		  (list 'with-collector '(collect)
			(list* ',form-name (rest ,whole))))))

#-sbcl
(defmacro define-collector (form)
  (let ((name (symb form "-c")))
    `(defmacro ,name (&rest args)
       (list 'with-collector '(collect)
	     (list* ',form args)))))

(defmacro define-collectors (&rest form-names)
  `(progn
     ,@(mapcar #'(lambda (form)
		   `(define-collector ,form))
	       form-names)))

(define-collectors while until dolist dotimes dovector)


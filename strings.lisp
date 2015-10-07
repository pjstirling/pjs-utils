(in-package #:pjs-utils)

(defun sconc (&rest args)
  "string joining with less typing !"
  (apply #'concatenate (cons 'string args)))

(define-compiler-macro sconc (&environment env &rest args)
  (compile-time-sconc env args))

(defun compile-time-sconc (env args)
  (let (args*
	constant
	(args (remove-if #'null-string-p
			 (mapcar (lambda (arg)
				   (macroexpand arg env))
				 args))))
    (flet ((emit-constant ()
	     (when constant
	       (push constant args*)
	       (setf constant nil))))
      (dolist (arg args)
	(if (or (stringp arg)
		(null arg))
	    (if constant
		(setf constant (concatenate 'string constant arg))
		;; else
		(setf constant arg))
	    ;; else
	    (progn
	      (emit-constant)
	      (push arg args*))))
      (if args*
	  (progn 
	    (emit-constant)
	    `(concatenate 'string ,@(nreverse args*)))
	  ;; else
	  (if constant
	      constant
	      ;; else
	      "")))))

(defun null-string-p (str)
  (or (null str)
      (equal str "")))


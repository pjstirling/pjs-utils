(in-package #:pjs-utils)

(defun mkstr (&rest args)
  (mkstr* args))

(defun mkstr* (args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (string-upcase (mkstr* args)))))

(defun make-keyword (&rest parts)
  (intern (string-upcase (mkstr* parts))
	  "KEYWORD"))

(defun gensymb (&rest args)
  (gensym (sconc (string-upcase (mkstr* args))
		 "-")))

(defun symbol-name* (sym)
  (string-downcase (symbol-name sym)))

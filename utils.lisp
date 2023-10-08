(in-package :pjs-utils)

(defun range (n &key (start 0))
  "Return a list of N elements starting at START."
  (with-collector* (collect)
    (dotimes (i n)
      (collect (+ i start)))
    (collect)))

(defmacro dohash ((k v hash) &body body)
  `(block nil
     (maphash (lambda (,k ,v)
		(declare (ignorable ,k ,v))
		,@body)
	      ,hash)))

;; why was this not included in cltl?
(defmacro dovector ((element-name vector &key (index-name (gensym)) result)
		    &body body)
  (let ((vec-name (gensym)))
    `(let ((,vec-name ,vector))
       (dotimes (,index-name (length ,vec-name) ,result)
	 (let ((,element-name (aref ,vec-name ,index-name)))
	   ,@body)))))

(defun maybe-wrap-body (sym body)
  "Wrap BODY (a list) in a SYM form, if it isn't a sole element."
  (if (rest body)
      `(,sym ,@body)
      ;; else
      (first body)))

(defmacro bind ((&rest bindings) &body body)
  (setf body (maybe-wrap-body 'locally body))
  (let (let-bindings)
    (labels ((add-let-binding (binding)
	       (push binding let-bindings))
	     (emit-let-bindings ()
	       (when let-bindings
		 (setf body
		       `(let* ,let-bindings
			  ,body))
		 (setf let-bindings nil)))
	     (ignored-var-p (sym)
	       (ends-with-p (symbol-name sym)
			    "-"))
	     (emit-multi-var (sym binding)
	       (destructuring-bind (type names value-form &optional declaration)
		   binding
		 (declare (ignore type))
		 (let ((ignored-names (remove-if-not #'ignored-var-p names)))
		   (setf body
			 `(,sym ,names ,value-form
				,@ (when ignored-names
				     `((declare (ignore ,@ignored-names))))
				,@(when declaration
				    (list declaration))
				,body))))))
      ;; we progressively wrap the body from the inner-most, (which is the last binding)
      ;; to the outer-most (which is the first). hence reverse
      (dolist (binding (reverse bindings))
	(if (listp binding)
	    (case (first binding)
	      (:mv
	       (emit-let-bindings)
	       (emit-multi-var 'multiple-value-bind binding))
	      (:db
	       (emit-let-bindings)
	       (emit-multi-var 'destructuring-bind binding))
	      (:progn
		;; not a binding at all :)
		(emit-let-bindings)
		(setf body
		      `(progn
			 ,@(rest binding)
			 ,body)))
	      (:slots
	       (emit-let-bindings)
	       (setf body
		     `(with-slots ,(second binding) ,(third binding)
			,body)))
	      (:symbols
	       (dolist (sym (rest binding))
		 (add-let-binding `(,sym (gensym ,(sconc (symbol-name sym) "-"))))))
	      (t
	       (add-let-binding binding)))
	    ;; else
	    (add-let-binding binding)))
      (emit-let-bindings)))
  body)

(defmacro 1++ (val)
  "c for dummies!"
  `(prog1
       ,val
     (setf ,val (1+ ,val))))

;; =============================================
;; idea from paul graham
;; anaphoric if
;; structured so that existing 'it' can be
;; used in expression before being re-bound
;; =============================================

(defmacro aif (expression then else)
  `(let ((it ,expression))
     (if it
	 ,then
	 ,else)))

;; =============================================
;; anaphoric when
;; =============================================

(defmacro awhen (expression &body then)
  `(let ((it ,expression))
     (when it
       ,@then)))

(defmacro with-hash-table-lookup ((name form) &body body)
  "Binds FORM (which should be a hash table) using NAME for both a
variable and a macro that invokes GETHASH (letting the hash table look
self-evaluating)."
  `(let ((,name ,form))
     (macrolet ((,name (key)
		  `(gethash ,key ,',name)))
       ,@body)))

;;(with-hash-table-lookup (foo (make-hash-table))
;;  (foo 1)
;;  (foo 3))

(defmacro with-self-eval ((name form type) &body body)
  (case type
    (:hash
     (let ((table-name (gensym)))
       `(let ((,table-name ,form))
	  (macrolet ((,name (key)
		       (list 'gethash key ',table-name)))
	    ,@body))))
    (:array
     (let ((array-name (gensym)))
       `(let ((,array-name ,form))
	  (macrolet ((,name (&rest subscripts)
		       (list* 'aref ',array-name subscripts)))
	    ,@body))))
    ((t)
     (error "bad type ~w" type))))

;(with-self-eval (foo bar :array)
;  (foo 5 8)
;  (bar))



;; =================================================
;; onlisp p. 49
;; =================================================

(defun flatten (x)
  (labels ((rec (x acc)
	     (cond ((null x) acc)
		   ((atom x) (cons x acc))
		   (t (rec (car x)
			   (rec (cdr x)
				acc))))))
    (rec x nil)))

;; ===============================================
;;
;; ===============================================

(define-symbol-macro []
    (make-array 10 :adjustable t :fill-pointer 0))

(define-symbol-macro {}
    (make-hash-table :test 'equal))

;; ===============================================
;;
;; ===============================================

(declaim (inline join))
(defun join (join-str &rest sequences)
  (run-time-join join-str sequences))

(defun run-time-join (join-str sequences)
  (let ((seq (mapcar (lambda (str)
		       (if (symbolp str)
			   (symbol-name str)
			   ;; else
			   str))
		     (remove-if #'null-string-p
				(flatten sequences)))))
    (if seq
	(let ((result (first seq)))
	  (dolist (it (rest seq))
	    (setf result 
		  (concatenate 'string result join-str it)))
	  result)
	;; else
	"")))

(defun compile-time-join (env join-str args)
  (let ((args (remove-if #'null-string-p
			 (mapcar (lambda (arg)
				   (megaexpand arg env))
				 args)))
	args*
	constant)
    (flet ((emit-constant ()
	     (when constant
	       (push constant args*)
	       (setf constant nil))))
      (dolist (arg args)
	(if (stringp arg)
	    (if constant
		(setf constant (concatenate 'string constant join-str arg))
		;; else
		(setf constant arg))
	    ;; else
	    (progn
	      (emit-constant)
	      (push arg args*))))
      (if args*
	  (progn
	    (emit-constant)
	    `(run-time-join ,join-str (list ,@(nreverse args*))))
	  ;; else
	  (if constant
	      constant
	      ;; else
	      "")))))

(define-compiler-macro join (&whole whole &environment env join-str &rest args)
  (let ((args (mapcar (lambda (form)
			(megaexpand form env))
		      args)))
    (cond
      ((null args)
       "")
      ((null (cdr args))
       (if (stringp (first args))
	   (first args)
	   ;; else
	   `(coerce ,(first args) 'string)))
      ((stringp join-str)
       (compile-time-join env join-str args))
      ;; else defer everything to run-time
      (t
       whole))))

;; =============================================
;; joins together forms with join-str, but assumes
;; that every form will evaluate to something
;; significant and safe to sconc
;; ==============================================

(defmacro join+ (join-str &rest forms)
  (let (list)
    (do ((x forms (cdr x)))
	((not x))
      (push (car x)
	    list)
      (when (cdr x)
	(push join-str list)))
  `(sconc ,@(reverse list))))

(defun n-copies (n val)
  (let (result)
    (dotimes (i n)
      (push val result))
    result))

(defun group (n seq)
  (if (zerop n)
      (error "0 length")
      (do ((result nil)
	   (chunk nil nil))
	  ((not seq) (nreverse result))
	(dotimes (i n)
	  (push (car seq) chunk)
	  (setf seq (cdr seq)))
	(push (nreverse chunk) result))))

(defun begins-with-p (str &rest patterns)
  (let ((start 0)
	(str-length (length str)))
    (dolist (pattern patterns)
      (let ((pattern-length (length pattern)))
	(when (> (+ pattern-length start)
		 str-length)
	  (return-from begins-with-p nil))

	(when (string/= str
			pattern
			:start1 start
			:end1 (+ start pattern-length))
	(return-from begins-with-p nil))
      (incf start pattern-length))))
  t)

(defun ends-with-p (str &rest patterns)
  (let ((start 0)
	(end (length str)))
    (dolist (pattern (reverse patterns))
      (let ((pattern-length (length pattern)))
	(when (< (- end pattern-length)
		 start)
	  (return-from ends-with-p nil))

	(when (string/= str 
			pattern
			:start1 (- end pattern-length)
			:end1 end)
	  (return-from ends-with-p nil))
	(decf end pattern-length)))
    t))


(defun make-comparator (greater-test equal-test lesser-test)
  (lambda (x y)
    (cond ((funcall greater-test x y)
	   1)
	  ((funcall equal-test x y)
	   0)
	  ((funcall lesser-test x y)
	   -1)
	  (t
	   (error "contract failure for make-comparator: (~a) (~a) not <, >, or =" x y)))))
	  
(defun number-comparator (x y)
  (- x y))

;; http://rosettacode.org/wiki/Binary_search#Common_Lisp
(defun binary-search (value array &key (test (make-comparator #'< #'= #'>)))
  (let ((low 0)
        (high (1- (length array))))
 
    (do ()
	((< high low) nil)
      (let* ((middle (floor (/ (+ low high) 2)))
	     (middle-value (aref array middle))
	     (test-result (funcall test
				   middle-value
				   value)))
	  (cond ((> 0 test-result)
		 (setf high (1- middle)))
		
		((= 0 test-result)
		 (return middle-value))

		((< 0 test-result)
		 (setf low (1+ middle))))))))

(defun only-one-of-p (variable values)
  (let (found)
    (dolist (v values)
      (when (member v variable)
	(if found
	    (return-from only-one-of-p nil)
	    ;; else
	    (setf found v))))
    found))

;; lazy pair-wise equal
(defmacro pair-equal (&rest args)
  `(and ,@(mapcar #'(lambda (x)
		      `(equal ,(first x) ,(second x)))
		  (group 2 args))))

;; delete and return the nth item from a list
(defun pop-nth-helper (n previous)
  (unless (< 0 n)
    (error "pop-nth-helper only works for 0 < n"))
  (do ()
      ((= n 1))
    (setf previous (cdr previous)
	  n (1- n)))
  (prog1
      (cadr previous)
    (setf (cdr previous) (cddr previous))))

(defmacro pop-nth (n place)
  (let ((counter (gensym)))
    `(let ((,counter ,n))
       (if (= ,counter 0)
	   (pop ,place)
	   ;; else
	   (pop-nth-helper ,counter ,place)))))

(defun test-pop-nth ()
  (dotimes (i 10)
    (let ((list (list 0 1 2 3 4 5 6 7 8 9)))
      (format t "~a ~w~%" (pop-nth i list) list))))

(defun file-length-for-path (path)
  (let ((pathname #+sbcl (sb-ext:parse-native-namestring path)
		  #-sbcl (make-pathname :name path)))
    (if (probe-file pathname)
      (with-open-file (s pathname :direction :input)
	(file-length s))
      ;; else
      (error "(~a) does not exist to take its length" path))))
		 
(defun trim (s)
  (when s
    (string-trim '(#\Space #\Newline #\Return #\Tab) s)))

(defmacro ndelete (item place)
  `(setf ,place (delete ,item ,place)))

(defun read-number-from-string (s)
  (let* ((*read-eval* nil)
	 (result (read-from-string s)))
    (when (numberp result)
      result)))
    

;; =========================================================
;;
;; =========================================================

(defmacro map-chars (key-form &body args)
  (flet ((first-char (s)
	   (if (stringp s)
	       (aref s 0)
	       s)))
    `(case ,key-form
       ,@(mapcar (lambda (char-case)
		   (list (first-char (first char-case))
			 (first-char (second char-case))))
		 (group 2 args)))))

#|
(map-chars foo
  "/" "_"
  "}" "{"
  "]" "["
  t (error "bleh"))
|#

(defun first-after (it list)
  (do ((p list (cdr p)))
      ((or (null p)
	   (eql (first p)
		it))
       (second p))))

(defun filter (fn list)
  (with-collector* (result)
    (dolist (el list)
      (awhen (funcall fn el)
	(result it)))
    (result)))

(defmacro rest-and-keywords ((rest &rest keywords) form &body body)
  "DWIM for destructuring bind with both rest and keywords.

file:///usr/share/doc/hyperspec/Body/03_dad.htm says that &key args
also appear in your &rest arg, if you mix them. The described
behaviour is retarded! This macro does the right thing by removing any
of the named keywords from the start of 'form'. 'form' should be the
name of your &rest arg, and normally you would use the same name for
'rest' so you can pretend that this isn't all necessary."
  (flet ((keyword-name (word)
	   (if (listp word)
	       (if (listp (first word))
		   (second (first word))
		   ;; else
		   (first word))
	       ;; else
	       word))
	 (keyword-keyword (word)
	   (if (listp word)
	       (if (listp (first word))
		   (first (first word))
		   ;; else
		   (intern (symbol-name (first word)) '#:keyword))
	       ;; else
	       (intern (symbol-name word) '#:keyword)))
	 (keyword-default (word)
	   (if (listp word)
	       (second word)
	       ;; else
	       nil))
	 (keyword-present (word)
	   (if (and (listp word)
		    (third word))
	       (third word)
	       ;; else
	       nil)))
    ;; end helpers
    (let ((quit-flag (gensym)))
      `(let ((,rest ,form)
	     ,@(mapcar (lambda (word)
			 (if (keyword-default word)
			     (list (keyword-name word) (keyword-default word))
			     ;; else
			     (keyword-name word)))
		       keywords)
	     ,@(remove-if #'null
			  (mapcar #'keyword-present keywords))
	     ,quit-flag)
	 (until (or (null ,rest)
		    ,quit-flag)
	   (if (keywordp (first ,rest))
	       (let ((first (first ,rest)))
		 (case first
		   ,@(mapcar (lambda (word)
			       `(,(keyword-keyword word)
				 (setf ,(keyword-name word)
				       (second ,rest))
				 (setf ,rest (cddr ,rest))
				 ;; mark it present
				 ,(awhen (keyword-present word)
				    `(setf ,it t))))
		      keywords)
		   (t
		    (setf ,quit-flag t))))
	       ;; else
	       (setf ,quit-flag t)))
	 ,@body))))

(defun explode (sep str)
  (with-collector* (collect)
    (let ((start 0)
	  (end (length str)))
      (while (< start end)
	(let ((index (search sep str :start2 start)))
	  (if index
	      (progn
		(collect (subseq str start index))
		(setf start (+ index (length sep))))
	      ;; else
	      (progn
		(collect (subseq str start))
		(setf start end)))))
      (collect))))

(defun partition (fn list)
  (with-collector* (yes)
    (with-collector* (no)
      (dolist (el list)
	(if (funcall fn el)
	    (yes el)
	    ;; else
	    (no el)))
      (values (yes) (no)))))

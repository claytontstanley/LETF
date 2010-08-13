(defclass mm-work-class (work-class) ())

(defmethod initialize-instance :after ((obj mm-work-class) &key)
  (setf (lines obj) (mapcar #'get-words (get-lines (file-string (workFilePath obj))))))

(defclass mm-collector-class (collector-class)
  ((out :accessor out :initarg :out :initform "mm_out.txt")))

(defmethod print-collector ((obj mm-collector-class))
  (with-open-file (out (out obj) :direction :output :if-exists :append :if-does-not-exist :create)
    (dotimes (i (length (cellElements obj)))
      (format out "~a " (cdr (nth i (cellElements obj)))))
    (dolist (key (keys obj))
      (aif (cdr (get-element key (collection obj) (gethash-ifhash key (collapseHash obj))))
	   (format out "~a " (coerce it 'double-float))))))

(defclass mm-process-output-str-class (process-output-str-class) ())

(defmethod print-collector ((obj mm-process-output-str-class))
  (format t "model unexpectedly quit... ~%~%here are the last ~a lines that were printed to stdout before the error~%~a~%"
	  (quot obj) (make-sentence (gethash "str" (collection obj)) :spaceDesignator #\Newline))
  (if (error-p obj) (format t "here's the error~%~a~%" (error-p obj))))

(defclass mm-run-collector-class (run-collector-class)
  ((out :accessor out :initarg :out :initform "mm_fraction_done.txt")))

(defmethod print-collector ((obj mm-run-collector-class))
  (with-open-file (out (out obj) :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format out "~a" (coerce (/ (quot (first (runs obj))) (quota (session (runProcess (first (runs obj)))))) 'double-float))))

(defun build-mm-session ()
  (build-session ;this is a macro
   :collector-instance (make-instance 'mm-collector-class)
   :work-instance (make-instance 'mm-work-class)
   :process-output-str-instance (make-instance 'mm-process-output-str-class)
   :run-collector-instance (make-instance 'mm-run-collector-class)))

(defun valid-function-name-p (name)
  "True if NAME denotes a function name that can be passed to MACRO-FUNCTION or FDEFINITION "
  (and (sb-int:valid-function-name-p name) t))

(defun function-lambda-list (function)
  "Describe the lambda list for the extended function designator FUNCTION.
Works for special-operators, macros, simple functions, interpreted functions,
and generic functions. Signals an error if FUNCTION is not a valid extended
function designator."
  (cond ((valid-function-name-p function)
         (function-lambda-list (or (and (symbolp function)
                                        (macro-function function))
                                   (fdefinition function))))
        ((typep function 'generic-function)
         (sb-pcl::generic-function-pretty-arglist function))
        #+sb-eval
        ((typep function 'sb-eval:interpreted-function)
         (sb-eval:interpreted-function-lambda-list function))
        (t
         (sb-kernel:%simple-fun-arglist (sb-kernel:%fun-fun function)))))

(defmacro html-color-start (&key (color 'yellow))
  `(fast-concatenate
    "~%htmlStart~%"
    "<FONT style=\"BACKGROUND-COLOR: " ,(string-downcase (string color)) "\">"))

(defmacro html-color-stop ()
  `(fast-concatenate
    "</FONT>~%"
    "htmlStop~%"))

(defmacro html-color (str &key (color 'yellow))
  `(fast-concatenate
    (html-color-start :color ,color)
    ,str
    (html-color-stop)))
   
(methods validate-entryFn
	 (((obj runprocess-class)))
	 (((obj run-class)))
	 (((obj session-class))
	  (let* ((IVKeys (IVKeys obj))
		 (modelProgram (modelProgram obj))
		 (arglst (function-lambda-list modelProgram))
		 (entryFnType (entryFnType obj)))
	    (when (equal entryFnType 'keys)
	      (let ((lst (mapcar (lambda (x) (format nil "~a" (car x))) (cdr arglst))))
		(assert (equalp (sort lst #'string<) (sort IVKeys #'string<)) nil
			(html-color "keys ~a for entry function ~a do not match IVs ~a in config file")
			lst modelProgram IVKeys)))
	    (when (equal entryFnType 'hash)
	      (assert nil nil (html-color "not allowing hash-table style entry functions for MM yet. Keep it simple..."))
	      (assert (equal (length arglst) 1) nil (html-color "problem with argument list ~a for the entry function ~a")
		      arglst modelProgram))
	    ;not doing any validation when the model is launched as a separate process yet
	    (when (equal entryFnType 'process)
	      nil))))

(methods validate-full-combinatorial
	 (((obj runProcess-class)))
	 (((obj run-class)))
	 (((obj session-class))
	  (with-pandoric (configFileWdLST) #'args
	     (dolist (line (get-matching-lines configFileWdLST "IV="))
	       (let ((nums (mapcar (lambda (x) 
				     (handler-case (eval (read-from-string x))
				       (error (condition) 
					 (assert nil nil (html-color "error \"~a\" when parsing line IV=~a") condition line))))
				   (rest (get-words line)))))
		 (mapc (lambda (x) (assert (numberp x) nil (html-color "~a not a number in line IV=~a") x line)) nums)
		 (assert (equal (length nums) 3) nil (html-color "not 3 numbers in line IV=~a") line)
		 (assert (< (first nums) (third nums)) nil (html-color "starting number ~a not less than ending number ~a in line IV=~a") (first nums) (third nums) line)
		 (assert (> (second nums) 0) nil (html-color "stepsize ~a not greater than zero in line IV=~a") (second nums) line)
		 (let ((cur (- (first nums) (second nums))))
		   (while (< (incf cur (second nums)) (third nums))
		     ())
		   (assert (equal cur (third nums)) nil (html-color "(~a-~a)/~a not a whole number in line IV=~a") (third nums) (first nums) (second nums) line))))
	     (dolist (line (get-matching-lines configFileWdLST "DV="))
	       (let ((name (get-words line)))
		 (assert (equal (length name) 1) nil (html-color "not 1 name in line DV=~a") line))))))

(methods print-unread-lines-html-color
	 (((obj runProcess-class)))
	 (((obj run-class)))
	 (((obj session-class))
	  (format *error-output* (html-color-start :color orange))
	  (print-unread-lines obj)
	  (format *error-output* (html-color-stop))))

(methods print-session-html-color
	 (((obj runProcess-class)))
	 (((obj run-class)))
	 (((obj session-class))
	  (format *error-output* (html-color-start :color orange))
	  (print-session obj)
	  (format *error-output* (html-color-stop))))


  
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

(methods validate-entryFn
	 (((obj runprocess-class)))
	 (((obj run-class)))
	 (((obj session-class))
	  (let* ((IVKeys (IVKeys obj))
		 (modelProgram (modelProgram obj))
		 (arglst (function-lambda-list modelProgram))
		 (entryFnType (entryFnType obj)))
	    (when (equal entryFnType 'keys) 
	      (mapc (let ((lst (mapcar (lambda (x) (format nil "~a" (car x))) (cdr arglst))))
		      (lambda (x) (assert (member x lst :test #'equalp) nil
					  "IV ~a not a member of the keys ~a for entry function ~a"
					  x arglst modelProgram))) 
		    IVKeys))
	    (when (equal entryFnType 'hash)
	      (assert nil nil "not allowing hash-table style entry functions for MM yet. Keep it simple...")
	      (assert (equal (length arglst) 1) nil "problem with argument list ~a for the entry function ~a" 
		      arglst modelProgram))
	    ;not doing any validation when the model is launched as a separate process yet
	    (when (equal entryFnType 'process)
	      nil))))


  
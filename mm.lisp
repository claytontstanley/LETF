;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;; 
;;; Author      : Clayton Stanley
;;; Address     : Air Force Research Laboratory
;;;             : Mesa, AZ 85212 USA
;;;             : clayton.stanley@wpafb.af.mil
;;; Filename    : mm.lisp
;;; Version     : 1.0
;;; 
;;; Description : MindModeling extensions to letf.lisp (lisp-based exploratory testing framework)
;;;               Enables using the LETF interface when running models on MM
;;;               mm.lisp acts as the mediator, abstracting away all of the MM details from the modeler
;;;               This allows models that have been interfaced with LETF to be portable (run on both HPCs and MM without altering)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass mm-work-class (work-class) 
  () 
  (:documentation "mm-work-class is responsible for storing the points to run; this is done by setting the 'lines' slot in the class"))

(defmethod initialize-instance :after ((obj mm-work-class) &key)
  "setting the 'lines' slot, and storing the points to run"
  (setf (lines obj) (mapcar #'get-objects (get-lines (file-string (workFilePath obj))))))

(defclass mm-collector-class (collector-class)
  ((out :accessor out :initarg :out :initform "mm_out.txt" 
	:documentation "extending the base class to hold the filename where all of the results will be printed"))
  (:documentation "mm-collector-class is responsible for printing the outputs of a collapsed set of runs"))

(let ((count 0))
  (defmethod print-collector ((obj mm-collector-class))
    "method will be called after each collapsed run; for the mm system, the results will be appended to mm_out.txt"
    (incf count)
    (with-open-file (out (out obj) :direction :output :if-exists (if (eq count 1) :supersede :append) :if-does-not-exist :create)
      ;print a fresh line
      (if (not (eq count 1)) (format out "~%"))
      (labels ((printIt (str &rest args)
		 (format out "~{~a~}" (append (list (if (numberp str) (coerce str 'double-float) str)) args))))
	;print the IVs with a tab after each
	(dotimes (i (length (cellElements obj)))
	  (printIt (cdr (nth i (cellElements obj))) (string #\Tab)))
	;print the DVs with a tab after all but the last
	(dotimes (i (length (keys obj)))
	  (let ((it (cdr (get-element (nth i (keys obj)) 
				      (collection obj) 
				      (gethash-ifhash (nth i (keys obj)) (collapseHash obj))))))
	    (if (eq i (- (length (keys obj)) 1))
		(printIt it)
		(printIt it (string #\Tab)))))))))

(defclass mm-process-output-str-class (process-output-str-class) 
  ()
  (:documentation "mm-process-output-str class is responsible for keeping track of the last N lines printed by the model"))

(defmethod print-collector ((obj mm-process-output-str-class))
  "method will be called if the model has died; will print the last lines outputted by the model (the model's last dying comments to stderr)"
  (format *error-output* "model unexpectedly quit... ~%~%here are the last ~a lines that were printed to stdout before the error~%~a~%"
	  (quot obj) (make-sentence (gethash "str" (collection obj)) :spaceDesignator #\Newline))
  (if (error-p obj) (format *error-output* "here's the error~%~a~%" (error-p obj))))

(defclass mm-run-collector-class (run-collector-class)
  ((out :accessor out :initarg :out :initform "mm_fraction_done.txt"
	:documentation "extending the base class to hold the file that will be touched after each run"))
  (:documentation "mm-run-collector-class is responsible for printing the outputs of a run"))

(defmethod print-collector ((obj mm-run-collector-class))
  "method will be called after each run; will touch the file and write the percent done"
  (with-open-file (out (out obj) :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format out "~a" (coerce (/ (quot (first (runs obj))) (quota (session (runProcess (first (runs obj)))))) 'double-float))))

(defun build-mm-session ()
  "top-level mm function called by letf that builds the session object"
  
  ;passing constructors to each of the classes extended above to customize the object for mm
  (build-session ;this is a macro
   :collector-instance (make-instance 'mm-collector-class)
   :work-instance (make-instance 'mm-work-class)
   :process-output-str-instance (make-instance 'mm-process-output-str-class)
   :run-collector-instance (make-instance 'mm-run-collector-class)))

(defmacro html-color-start (&key (color 'yellow))
  "returns a string that is html code to start tagging the text that follows in color"
  `(fast-concatenate
    "~%htmlStart~%"
    "<FONT style=\"BACKGROUND-COLOR: " ,(string-downcase (string color)) "\">"))

(defmacro html-color-stop ()
  "returns a string that is html code to stop tagging the text that follows in color"
  `(fast-concatenate
    "</FONT>~%"
    "htmlStop~%"))

(defmacro html-color (str &key (color 'yellow))
  "returns a string that is html code to tag 'str' in color 'color'"
  `(fast-concatenate
    (html-color-start :color ,color)
    ,str
    (html-color-stop)))

;wrapping html font tags around the text output from all assertions that fail
(sb-ext:without-package-locks
  (let ((fun (symbol-function 'sb-kernel:assert-error)))
    (setf (symbol-function 'sb-kernel:assert-error) 
	  (lambda (assertion places datum &rest arguments) 
	    (apply fun (append (list assertion places (html-color datum)) arguments))))))

(let ((DVs))
  ;define a pandoric function that stores (closes over) 'DVs'
  ;you can set/get the value of DVs using 'get-pandoric or 'with-pandoric
  (defpun DVs () (DVs)
    ()))

(defmacro send-DV (name% val)
  "interface for sending DVs evaluated by the model up to the wrapper"
  (let ((name (symbol-name name%)))
    ;store the DV name that the modeler is sending
    ;these DVs are stored at compile time; not run time
    (push name (get-pandoric #'DVs 'DVs))
     ;at run time, send the value of the DV back to the wrapper
    `(format t "~a=~a~%" ,name ,val)))

;generate all of the combinations of the lists inside of rangeList
;each list inside of rangeList consists of three elements: (start stepsize stop)
;each combination will be a list with a length of the number of elements in rangeList
;a function generated by comb returns a list where each element is a combination list
;try (funcall (comb) (list (list 0 1 2) (list 3 2 7)))
(defmacro comb (&body body)
  "generate all of the combinations of the lists inside of rangeList"
  
  ;macro is returning a function; the function is anaphoric, so you can recurse on it ('self') before it is defined
  ;this is the 'alambda' macro
  ;trail holds the history of where you have tracked
  ;rangelst holds the items that haven't been tracked
  ;this is a backtracking algorithm
  `(alambda (rangeList &optional (trail nil))
     (if rangeList
	 (do* ((out) 
	       (element (car rangeList))
	       ;increasing the point by the range after each iteration
	       (point (first element) (+ point (second element))) 
	       ;increasing the count after each iteraction
	       (count 0 (+ count 1)))
	      ;the check that stops the loop (if point is past the end point)
	      ((equal count (+ 1 (ceiling (- (third element) (first element)) (second element)))) out)
	   ;append the current solutions to the solutions that you generate by recursing; then, return the solutions
	   (setf out (append out (self (cdr rangeList) (append trail (list point))))))
	 ;base case
	 (if (consp trail)
	     ;here's the cool part about having this as a macro; if you body is empty, then this function will
	     ;just return all of the combinations; however, if you have something in body, then that code will
	     ;be executed instead of returning all of the combinations; for example, you can reroute each of the 
	     ;combinations to be outputted to a text file (see generate-full-combinatorial below)
	     ,(if body
		  `(progn ,@body)
		  `(list trail))))))

(methods validate-entryFn
	 (((obj runprocess-class)))
	 (((obj run-class)))
	 (((obj session-class))
	  "method will validate the parameters defined in the entry function against par names specified in the config file"
	  (let* ((IVKeys (IVKeys obj))
		 (DVKeys (DVKeys obj))
		 (modelProgram (modelProgram obj))
		 (arglst (sb-introspect:function-lambda-list modelProgram))
		 (entryFnType (entryFnType obj)))
	    (when (equal entryFnType 'keys)
	      (assert (> (length IVKeys) 0) nil "at least one IV is needed in config file")
	      (assert (> (length DVKeys) 0) nil "at least one DV is needed in config file")
	      (let ((lst (mapcar (lambda (x) (format nil "~a" (car x))) (cdr arglst))))
		(assert (equalp (sort lst #'string<) (sort IVKeys #'string<)) nil
			"keys ~a for entry function ~a do not match IVs ~a in config file"
			lst modelProgram IVKeys)))
	    (when (equal entryFnType 'hash)
	      ;this assert nil nil will throw an error; only a 'keys entryFnType is allowed on MM
	      ;for example (defun run-model (&key (x) (y)) ... is allowed, but
	      ;(defun run-model (hash) ... is not allowed
	      (assert nil nil "not allowing hash-table style entry functions for MM yet. Keep it simple...")
	      (assert (equal (length arglst) 1) nil "problem with argument list ~a for the entry function ~a"
		      arglst modelProgram))
	    ;not doing any validation when the model is launched as a separate process yet
	    (when (equal entryFnType 'process)
	      nil))))

(methods validate-DVs
	 (((obj runprocess-class)))
	 (((obj run-class)))
	 (((obj session-class))
	  "method will validate the DVs written in the model (using send-dv) against DV names specified in the config file"
	  (let ((necessary-DVs (necessaries (DVKeys obj) (DVHash obj)))
		(supplied-DVs (get-pandoric #'DVs 'DVs)))
	    (assert (equalp (sort necessary-DVs #'string<) (sort supplied-DVs #'string<)) nil
		    "DVKeys ~a sent using 'send-DVs' do not match necessary DVs ~a in config file"
		    supplied-DVs necessary-DVs))))

(methods validate-full-combinatorial
	 (((obj runProcess-class)))
	 (((obj run-class)))
	 (((obj session-class))
	  "method will check that the syntax for the 'start stepsize end' points specified for each IV in the config file is correct"
	  (with-pandoric (configFileWdLST) #'args
	    (dolist (line (get-matching-lines configFileWdLST "IV="))
	      (let ((nums (mapcar (lambda (x) 
				    (handler-case (eval (read-from-string x))
				      (error (condition) 
					(assert nil nil "error \"~a\" when parsing line IV=~a" condition line))))
				  (get-objects (make-sentence (rest (get-words line)))))))
		(mapc (lambda (x) (assert (numberp x) nil "~a not a number in line IV=~a" x line)) nums)
		(assert (equal (length nums) 3) nil "not 3 numbers in line IV=~a" line)
		(assert (< (first nums) (third nums)) nil "starting number ~a not less than ending number ~a in line IV=~a" (first nums) (third nums) line)
		(assert (> (second nums) 0) nil "stepsize ~a not greater than zero in line IV=~a" (second nums) line)
		(multiple-value-bind (q r) (ffloor (- (third nums) (first nums)) (second nums))
		  (declare (ignore q))
		  (assert (< (abs r) .000001) nil "(~a-~a)/~a not a whole number in line IV=~a" (third nums) (first nums) (second nums) line))))
	    (dolist (line (get-matching-lines configFileWdLST "DV="))
	      (let ((name (get-words line)))
		(assert (equal (length name) 1) nil "not 1 name in line DV=~a" line))))))

(methods generate-full-combinatorial
	 (((obj runProcess-class)))
	 (((obj run-class)))
	 (((obj session-class))
	  "method will generate all combinations of IVs in config file, and write the results (line by line) to file workFileName= in config file"
	  (with-pandoric (configFileWdLST) #'args
	    (let ((nums (mapcar (lambda (line) 
				  (mapcar (lambda (num) (eval (read-from-string num)))
					  (get-objects (make-sentence (rest (get-words line)))))) 
				(get-matching-lines configFileWdLST "IV=")))
		  (workFileName (eval
				 (read-from-string
				  (get-object
				   (get-matching-line configFileWdLST "workFileName=")))))
		  (lines 0))
	      (with-open-file (out workFileName :direction :output :if-exists :supersede :if-does-not-exist :create)
		;creating a lexical closure over the macro comb; instead of having comb do its default thing and return
		;all of the combinations, we are printing each combination (as a side effect) to the stream 'out' (defined above)
		;'trail' is defined in comb, and it's a single combination (of all the combinations)
		;this is what happens when you combine anaphoric macros (variable capture) with lexical closures (functions with memory)...
		(funcall (comb 
			  (incf lines) 
			  (format out "~{~,8f ~}~&" trail)) 
			 nums)
		(format *error-output* "wrote ~a lines to ~a using IV ranges ~a~%" lines workFileName nums))))))

(methods print-unread-lines-html-color
	 (((obj runProcess-class)))
	 (((obj run-class)))
	 (((obj session-class))
	  "an 'around' method that calls 'print-unread-lines' in letf, but prints the results in html color"
	  (format *error-output* (html-color-start :color orange))
	  (print-unread-lines obj)
	  (format *error-output* (html-color-stop))))

(methods print-session-html-color
	 (((obj runProcess-class)))
	 (((obj run-class)))
	 (((obj session-class))
	  "an 'around' method that calls 'print-session' in letf, but prints the results in html color"
	  (format *error-output* (html-color-start :color orange))
	  (print-session obj)
	  (format *error-output* (html-color-stop))))



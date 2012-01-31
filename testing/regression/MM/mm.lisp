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

(defun sandwich (item lst)
  "places item in between all elements of lst, but not to the left or right of lst"
  (expect (listp lst) "error; ~a not a list" lst)
  (cond ((null lst) nil)
	((eq (length lst) 1) lst)
	(t (cons (car lst) 
		 (cons item (sandwich item (cdr lst)))))))

(defclass bad-models-class ()
  ((num-runs-total :accessor num-runs-total :initarg :num-runs-total :initform nil)
   (num-runs-errored :accessor num-runs-errored :initarg :num-runs-errored :initform 0)
   (threshold :accessor threshold :initarg :threshold :initform .5))
  (:documentation "responsible for keeping track of the failed models, and deciding if there are enough failed models to exit with a non-zero code"))

(defmethod bad-models-p ((obj bad-models-class))
  "will return true if the number of failed models is greater than the threshold"
  (when (num-runs-total obj)
    (when (> (num-runs-total obj) 0)
      (> (/ (num-runs-errored obj) (num-runs-total obj)) (threshold obj)))))

(defmethod reset-collector ((obj base-collector-class))
  "removes all items from a collection"
  (setf (collection obj) (make-hash-table :test #'equalp))
  (setf (quot obj) 0))

;pandoric function that stores names for mm-specific variables and output files
(defpun mods () ((mm_out)
		 (mm_in)
		 (mm_fraction_done)
		 (mm_bold_out)
		 (mm_errors)
		 (fresh-errors-file-p)
		 (fresh-out-file-p)
		 (bad-models-collector))
	(setf mm_out "out.txt")
	(setf mm_in "in.txt")
	(setf mm_fraction_done "mm_fraction_done.txt")
	(setf mm_bold_out "mm_bold_out.txt")
	(setf mm_errors "errors.txt")
	(setf fresh-errors-file-p t)
	(setf fresh-out-file-p t)
	(setf bad-models-collector (make-instance 'bad-models-class)))

;initialize the variables
(mods)

(defclass mm-work-class (work-class) 
  () 
  (:documentation "mm-work-class is responsible for storing the points to run; this is done by setting the 'lines' slot in the class"))

(defmethod initialize-instance :after ((obj mm-work-class) &key)
  "setting the 'lines' slot, and storing the points to run"
  (setf (lines obj) (mapcar #'get-objects (get-lines (file-string (workFilePath obj))))))

(defclass mm-collector-class (collector-class)
  ((out :accessor out :initarg :out :initform (get-pandoric 'mods 'mm_out) 
	:documentation "extending the base class to hold the filename where all of the results will be printed"))
  (:documentation "mm-collector-class is responsible for printing the outputs of a collapsed set of runs"))

(defmethod print-collector ((obj mm-collector-class))
  "method will be called after each collapsed run; for the mm system, the results will be appended to out.txt"
  (with-slots (cellElements collection collapseHash keys) obj
    (with-pandoric (fresh-out-file-p) 'mods
		   (let ((DV-Elements (get-elements keys collection :collapseFns (mapcar (lambda (x) (gethash-ifHash x collapseHash)) keys))))
		     (when (notevery #'null (mapcar #'cdr DV-Elements)) ;if all DVs are nil, then the run errored, so don't print this run
		       (with-open-file (out (out obj) :direction :output :if-exists (if fresh-out-file-p :supersede :append) :if-does-not-exist :create)
			 (if fresh-out-file-p (setf fresh-out-file-p nil))
			 ;print the IVs & DVs, with a tab sandwiched in between them, followed by a newline
			 (let ((str (format nil "~{~a~}" (mapcar (lambda (x) (if (realp x) (coerce x 'double-float) x))
								 (sandwich #\Tab (append (mapcar #'cdr cellElements) (mapcar #'cdr DV-Elements)))))))
			   (format t "Writing IV/DV vector to output file:~a~a~%" #\Tab str)
			   (format out "~a~%" str))))))))


;keep track of the run object that is currently being executed
(defvar *run* nil)
(defmethod wrapper-execute :before ((obj run-class) &optional (process) (appetizers))  
  "before executing a run object, store the current run object in the *run* dynamic variable (global)"
  (declare (ignore process appetizers))
  (format t "Starting model with IV vector:~a~{~a~}~%" #\Tab (sandwich #\Tab (mapcar #'cdr (get-elements (cellKeys obj) (IVHash obj) :eval-val-p nil))))
  (setf *run* obj)
  (reset-collector (process-output-str (runProcess obj))))

(defmethod wrapper-execute :after ((obj run-class) &optional (process) (appetizers))
  "after executing a run object, print logging info to stdout"
  (declare (ignore process appetizers))
  (format t "Finished model with IV vector:~a~{~a~}~%~%" #\Tab (sandwich #\Tab (mapcar #'cdr (get-elements (cellKeys obj) (IVHash obj) :eval-val-p nil)))))

(defclass mm-process-output-str-class (process-output-str-class)
  ((out :accessor out :initarg :out :initform (get-pandoric 'mods 'mm_errors)))
  (:documentation "mm-process-output-str class is responsible for keeping track of the last N lines printed by the model"))

(defmethod print-collector ((obj mm-process-output-str-class))
  "method will be called if the model has died; will print the last lines outputted by the model (the model's last dying comments) to stderr
  an example output looks like this:
  ############################
  # x: 1
  # y: 2
  #
  # Error message: some kind of exception message caught by SBCL

  <last 200 lines of output>"
  (with-pandoric (fresh-errors-file-p) 'mods
		 (with-open-file (strm (out obj) :direction :output :if-exists (if fresh-errors-file-p :supersede :append) :if-does-not-exist :create)
		   (if fresh-errors-file-p (setf fresh-errors-file-p nil)) 
		   (format strm "##########################~%")
		   (with-slots (cellKeys IVHash) *run*
		     (dolist (element (get-elements cellKeys IVHash :eval-val-p nil))
		       (format strm "# ~a: ~a~%" (car element) (cdr element))))
		   (format strm "#~%")
		   (if (error-p obj) (format strm "# Error message: ~a~%" (error-p obj)))
		   (format strm "~%")
		   (format strm "~{~a~%~}" (gethash "str" (collection obj)))
		   (format strm "~%")
		   (incf (num-runs-errored (get-pandoric 'mods 'bad-models-collector)))))) ;remember that a point has crashed

(defclass mm-run-collector-class (run-collector-class)
  ((out :accessor out :initarg :out :initform (get-pandoric 'mods 'mm_fraction_done)
	:documentation "extending the base class to hold the file that will be touched after each run")
   (bold-out :accessor bold-out :initarg :bold-out :initform (get-pandoric 'mods 'mm_bold_out)
	     :documentation "holds the file that will contain the bold response data from each run"))
  (:documentation "mm-run-collector-class is responsible for printing the outputs of a run"))

(defmethod print-collector ((obj mm-run-collector-class))
  "method will be called after each run; will touch the file, write the percent done, and write out bold response"
  ;there is a race condition where the boinc wrapper may be reading from the percent done file right when the lisp process
  ;wants to overwrite it (and update its value). The simplest way around this is to wrap the lisp code in an attempt (try/catch) statement
  ;so that the lisp process doesn't crash if the race condition happens. In this case, the percent complete isn't updated for this point,
  ;but it will get updated correctly the next time a point finishes
  (attempt
    (with-open-file (out (out obj) :direction :output :if-exists :supersede :if-does-not-exist :create)
      (format out "~a" (coerce (/ (quot (first (runs obj))) (quota (session (runProcess (first (runs obj)))))) 'double-float))))
  ;write out bold response
  (with-pandoric (configFileWdLST) #'args
		 (dolist (line (get-matching-lines configFileWdLST "file2load="))
		   ;when an actr6 model
		   (when (get-matching-line (string-left-trim (list #\Space #\Tab) line) "actr6")
		     ;it's not mission critical that the bold response data is extracted, so don't kill lisp on an error
		     (attempt
		       ;when the bold response data is available
		       (when (and (meta-p-current-model (current-mp)) (get-module bold))
			 ;output the predicted bold response to bold-out
			 (with-open-file (out (bold-out obj) :direction :output :if-exists :supersede :if-does-not-exist :create)
			   (format out "~a" (with-output-to-string (*standard-output*)
					      (predict-bold-response))))))))))

(defun ls (dir &optional (recursive nil))
  "Returns information about the files listed in the directory"
  (let ((ret nil))
    (labels ((collect (x) (push-to-end (enough-namestring x) ret)))
      (if (cl-fad:directory-exists-p dir)
	(if recursive ;If recursive boolean flag is set, list directories recursively
	  (cl-fad:walk-directory dir #'collect :directories t)
	  (mapc #'collect (cl-fad:list-directory dir)))
	(format t "Directory ~a does not exists!" dir))
      ret)))

(defmethod wrapper-execute :before ((obj session-class) &optional (process) (appetizers))
  "executes before any runs are fired off; provides the total number of runs to the bad models collector"
  (declare (ignore process appetizers))
  ; Print out the contents of the slots and project directory 
  (format t "~%Slots dir:~%~%")
  (format t "~{~a~%~}" (ls ""))
  (format t "~%~%Projects dir:~%~%")
  (format t "~{~a~%~}" (remove-if-not (lambda (x) (search "MindModeling" x :test #'string-equal)) (ls "../../projects" t)))
  ; Print out the contents of the config file
  (format t "~%Config File:~%~%")
  (format t "~a~%~%" (get-pandoric #'args 'configFileStr))
  (with-pandoric (bad-models-collector) 'mods
		 (setf (num-runs-total bad-models-collector) (quota obj))))

(defmethod wrapper-execute :after ((obj session-class) &optional (process nil) (appetizers nil))
  "executes after all runs are fired off; determines how the lisp process will exit"
  (declare (ignore process appetizers))
  (with-pandoric (bad-models-collector) 'mods
		 (expect (not (bad-models-p bad-models-collector)) "too many runs crashed; exiting with non-zero status")))

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
;(sb-ext:without-package-locks
;  (let ((fun (symbol-function 'sb-kernel:assert-error)))
;    (setf (symbol-function 'sb-kernel:assert-error) 
;	  (lambda (assertion places datum &rest arguments) 
;	    (apply fun (append (list assertion places (html-color datum)) arguments))))))

;define a pandoric function that stores (closes over) 'DVs'
;you can set/get the value of DVs using 'get-pandoric or 'with-pandoric
(defpun DVs () ((DVs)))

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
		((eq count (+ 1 (round (- (third element) (first element)) (second element)))) out)
		;append the current solutions to the solutions that you generate by recursing; then, return the solutions
		(setf out (append out (self (cdr rangeList) (append trail (list point))))))
	      ;base case
	      (if (consp trail)
		;here's the cool part about having this as a macro; if your body is empty, then this function will
		;just return all of the combinations; however, if you have something in body, then that code will
		;be executed instead of returning all of the combinations; for example, you can reroute each of the 
		;combinations to be outputted to a text file (see generate-full-combinatorial below)
		,(if body
		   `(progn ,@body)
		   `(list trail))))))

(defmacro defmethod% (name pattern &body body)
  "works like defmethod, but fills in stub methods for the two classes not defined"
  (let* ((all-classes (list 'runprocess-class 'run-class 'session-class))
	 (stub-methods (set-difference all-classes (flatten pattern))))
    (expect (equal (length stub-methods) 2) "used defmethod% on an invalid class")
    `(methods ,name
	      ,@(mapcar (lambda (stub-method) `(((obj ,stub-method)))) stub-methods)
	      (,pattern ,@body))))

(defmethod% validate-entryFn ((obj session-class))
	    "method will validate the parameters defined in the entry function against par names specified in the config file"
	    (let* ((IVKeys (IVKeys obj))
		   (DVKeys (DVKeys obj))
		   (modelProgram (modelProgram obj))
		   (entryFnType (entryFnType obj)))
	      (when (equal entryFnType 'keys)
		(let ((arglst 
			#+SBCL (mapcar #'car (cdr (sb-introspect:function-lambda-list modelProgram)))
			#+CCL (mapcar #'symbol-name (cdr (arglist modelProgram)))))
		  (expect (> (length IVKeys) 0) (html-color "at least one IV is needed in config file"))
		  (expect (> (length DVKeys) 0) (html-color "at least one DV is needed in config file"))
		  (let ((lst (mapcar (lambda (x) (format nil "~a" x)) arglst)))
		    (expect (equalp (sort lst #'string<) (sort IVKeys #'string<))
			    (html-color "keys ~a for entry function ~a do not match IVs ~a in config file")
			    lst modelProgram IVKeys))))
	      (when (equal entryFnType 'hash)
		(let ((arglst
			#+SBCL (sb-introspect:function-lambda-list modelProgram)
			#+CCL (arglist modelProgram)))
		  ;this assert nil nil will throw an error; only a 'keys entryFnType is allowed on MM
		  ;for example (defun run-model (&key (x) (y)) ... is allowed, but
		  ;(defun run-model (hash) ... is not allowed
		  (expect nil (html-color "not allowing hash-table style entry functions for MM yet. Keep it simple..."))
		  (expect (equal (length arglst) 1) (html-color "problem with argument list ~a for the entry function ~a") arglst modelProgram)))
	      ;not doing any validation when the model is launched as a separate process yet
	      (when (equal entryFnType 'process)
		nil)))

(defmethod% validate-DVs ((obj session-class))
	    "method will validate the DVs written in the model (using send-dv) against DV names specified in the config file"
	    (let ((necessary-DVs (necessaries (DVKeys obj) (DVHash obj)))
		  (supplied-DVs (get-pandoric #'DVs 'DVs)))
	      (expect (equalp (sort necessary-DVs #'string<) (sort supplied-DVs #'string<))
		      (html-color "DVKeys ~a sent using 'send-DVs' do not match necessary DVs ~a in config file")
		      supplied-DVs necessary-DVs)))

(defmethod% validate-full-combinatorial ((obj session-class))
	    "method will check that the syntax for the 'start stepsize end' points specified for each IV in the config file is correct"
	    (with-pandoric (configFileWdLST) #'args
			   (dolist (line (get-matching-lines configFileWdLST "IV="))
			     (let ((nums (mapcar (lambda (x) 
						   (handler-case (eval (read-from-string x))
						     (error (condition) 
							    (expect nil (html-color "error \"~a\" when parsing line IV=~a") condition line))))
						 (get-objects (make-sentence (rest (get-words line)))))))
			       (mapc (lambda (x) (expect (numberp x) (html-color "~a not a number in line IV=~a") x line)) nums)
			       (expect (equal (length nums) 3) (html-color "not 3 numbers in line IV=~a") line)
			       (expect (< (first nums) (third nums)) (html-color "starting number ~a not less than ending number ~a in line IV=~a") (first nums) (third nums) line)
			       (expect (> (second nums) 0) (html-color "stepsize ~a not greater than zero in line IV=~a") (second nums) line)
			       (multiple-value-bind (q r) (fround (- (third nums) (first nums)) (second nums))
				 (declare (ignore q))
				 (expect (< (abs r) .000001) (html-color "(~a-~a)/~a not a whole number in line IV=~a") (third nums) (first nums) (second nums) line))))
			   (dolist (line (get-matching-lines configFileWdLST "DV="))
			     (let ((name (get-words line)))
			       (expect (equal (length name) 1) (html-color "not 1 name in line DV=~a") line)))))

(defmethod% generate-full-combinatorial ((obj session-class))
	    "method will generate the full combination of IVs in config file, and write results (line by line) to filename mapped to mm_in in 'mods"
	    (with-pandoric (configFileWdLST) #'args
			   (let ((nums (mapcar (lambda (line) (eval-objects (make-sentence (rest (get-words line)))))
					       (get-matching-lines configFileWdLST "IV=")))
				 (workFileName (get-pandoric 'mods 'mm_in))
				 (lines 0))
			     (with-open-file (out workFileName :direction :output :if-exists :supersede :if-does-not-exist :create)
			       (funcall (comb
					  (incf lines)
					  (format out "~{~a~a~}~&" (flatten (mapcar #'list trail (make-list (length trail) :initial-element #\Tab)))))
					nums))
			     (format *error-output* "wrote ~a lines to ~a using IV ranges ~a~%" lines workFileName nums))))

(defmethod% generate-header ((obj session-class))
	    "places the names of the IVs then DVs at the top of the output file, separated by tabs
	    also places the names of hte IVs at the top of the errors file, separated by tabs"
	    (with-slots (cellKeys DVKeys) obj
	      (with-open-file (out (get-pandoric 'mods 'mm_out) :direction :output :if-exists :supersede :if-does-not-exist :create)
		(format out "~{~a~}" (sandwich #\Tab (append cellKeys DVKeys))))
	      (with-open-file (out (get-pandoric 'mods 'mm_errors) :direction :output :if-exists :supersede :if-does-not-exist :create)
		(format out "~{~a~}" (sandwich #\Tab cellKeys)))))

(defmethod% print-unread-lines-html-color ((obj session-class))
	    "an 'around' method that calls 'print-unread-lines' in letf, but prints the results in html color"
	    (format *error-output* (html-color-start :color orange))
	    (print-unread-lines obj)
	    (format *error-output* (html-color-stop)))

(defmethod% print-session-html-color ((obj session-class))
	    "an 'around' method that calls 'print-session' in letf, but prints the results in html color"
	    (format *error-output* (html-color-start :color orange))
	    (print-session obj)
	    (format *error-output* (html-color-stop)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;; 
;;; Author      : Clayton Stanley
;;; Address     : Air Force Research Laboratory
;;;             : Mesa, AZ 85212 USA
;;;             : clayton.stanley@wpafb.af.mil
;;; Filename    : testMM.lisp
;;; Version     : 1.0
;;; 
;;; Description : Unit tests for mm.lisp
;;;               Uses unitTestFramework.lisp as the UTF 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;define a function called session-object that is a lexical closure that
;stores the mm object generated from calling the build-mm-session function
;this closure is pandoric, so we can access obj by using the with-pandoric macro
;this is so we only have to call build-mm-session once, without using a dynamic (global) variable
(let ((obj (build-mm-session)))
  (setf (symbol-function 'session-object)
	(plambda () (obj)
	  ())))

(setf *clean-exit-on-error* nil)

(defmacro! errors-p (form)
  `(handler-case
       (progn
	 ,form
	 nil)
     (error (,g!condition) ,g!condition)))

(defmacro! with-shadow ((fname fun) &body body)
  "shadow the function named fname with fun; any call to fname within body will use fun, instead of the default function for fname"
  (cond ((fboundp fname) ;if there is already a function with that name defined, then shadow it
	 `(let ((,g!fname-orig (symbol-function ',fname)))
	    (setf (symbol-function ',fname) ,fun)
	    ,@body
	    (setf (symbol-function ',fname) ,g!fname-orig)
	    nil))
	(t ;otherwise, define a new function with that name, and then undo the operation afterwards by unbinding that function
	 `(progn
	    (setf (symbol-function ',fname) ,fun)
	    ,@body
	    (fmakunbound ',fname)
	    nil))))

(deftest test-comb-default ()
  "unit tests for the comb macro, when using its default behavior that returns all combinations"
  (check
   (equal (funcall (comb) (list '(1 2 3) '(1 1 4))) 
	  (list '(1 1) '(1 2) '(1 3) '(1 4) '(3 1) '(3 2) '(3 3) '(3 4)))
   (equal (funcall (comb) nil) 
	  nil)
   (equal (funcall (comb) (list '(1 1 3)))
	  (list '(1) '(2) '(3)))
   ;end overshoots 12, but should round down to 12
   (equal (funcall (comb) (list '(10 1 12.00000000001)))
	  (list '(10) '(11) '(12)))
   ;end undershoots 12, but should round up to 12
   (equal (funcall (comb) (list '(10 1 11.99999999999)))
	  (list '(10) '(11) '(12)))
   (equal (funcall (comb) (list '(1 1 1))) (list '(1)))
   (equal (funcall (comb) (list '(1 1 2) '(2 1 3) '(3 1 4)))
	  (list '(1 2 3) '(1 2 4) '(1 3 3) '(1 3 4) '(2 2 3) '(2 2 4) '(2 3 3) '(2 3 4)))))

(deftest test-comb-anaphoric ()
  "unit tests for the comb macro, when altering its default behavior by injecting new code"
  (check
   (equal (funcall (comb (list (reverse trail))) (list '(1 2 3) '(1 1 4)))
	  (list '(1 1) '(2 1) '(3 1) '(4 1) '(1 3) '(2 3) '(3 3) '(4 3)))))

(deftest test-comb ()
  "unit tests for the comb macro"
  (combine-results
   (test-comb-default)
   (test-comb-anaphoric)))

(deftest test-validate-full-combinatorial ()
  "unit tests for validate-full-combinatorial"
  (macrolet ((deftest-vfc (configFileWdLSTPiece will-error)
	       `(progn
		  ;mocking up necessary objects/functions/variables
		  (with-pandoric (configFileWdLST) #'args
		    (setf configFileWdLST ,configFileWdLSTPiece))
		  (with-pandoric (obj) #'session-object
		    (let (result)
		      (handler-case (validate-full-combinatorial obj)
			(error (condition) (setf result condition)))
		      (check
		       (if ,will-error result (not result))))))))
    (deftest-vfc (list (list "IV=" "noise" "1" "1" "3")) nil) ;should not fail
    (deftest-vfc (list (list "IV=" "noise" "1" "1" "1")) t) ;should fail b/c min==max
    (deftest-vfc (list (list "IV=" "speed" "1" "2" "4")) t) ;should fail b/c min incrementing by range doesn't hit max
    (deftest-vfc (list (list "IV=" "speed" "3" ".1" "2")) t) ;should fail b/c min>max
    (deftest-vfc (list (list "IV=" "speed" "help" "1" "4")) t) ;should fail b/c min is not a number
    (deftest-vfc (list (list "IV=" "noise" ".9" ".1" "1.2")) nil) ;should not fail
    (deftest-vfc (list (list "IV=" "noise" "1" "1" "3") ; should not fail
		       (list "IV=speed" "1.0" "0.1" "2.0")) nil)
    (deftest-vfc (list (list "IV=" "noise" "1" "1/2" "2")) nil) ;should not fail; checking that code can read fractions
    ))

(deftest test-generate-header ()
  "unit tests for generate-header"
  (macrolet ((mac (cellKeys DVKeys)
	       `(with-pandoric (obj) #'session-object
		  (with-slots (cellKeys DVKeys) obj
		    (setf cellKeys ,cellKeys)
		    (setf DVKeys ,DVKeys))
		  (generate-header obj)
		  (check (equalp (flatten (mapcar #'get-words (get-lines (file-string (get-pandoric 'mods 'mm_out)))))
				 (append ,cellKeys ,DVKeys)))
		  (delete-file (get-pandoric 'mods 'mm_out))
		  (check (equalp (flatten (mapcar #'get-words (get-lines (file-string (get-pandoric 'mods 'mm_errors)))))
				 ,cellKeys))
		  (delete-file (get-pandoric 'mods 'mm_errors)))))
    (mac (list "IV1" "IV2") (list "DV1" "DV2"))
    (mac nil nil)
    (mac (list "testing") (list "one" "two" "three"))
    (mac (list "one" "two" "three") (list "testing"))))

(deftest test-generate-full-combinatorial ()
  "unit tests for generate-full-combinatorial"
  (macrolet ((deftest-vfc (IVPiece completedPoints workFileName outFileName combinationString)
	       `(progn
		  (with-pandoric (configFileWdLST) #'args
		    (setf configFileWdLST
			  (format nil "~a~%outFileName=\"~a\"~%DV=DV1~%DV=DV2~%" ,IVPiece ,outFileName))
		    (setf (get-pandoric 'mods 'mm_in) ,workFileName)
		    (if ,outFileName
			(with-open-file (out ,outFileName :direction :output :if-exists :supersede :if-does-not-exist :create)
			  (format out "~a~%" ,completedPoints)))
		    (with-pandoric (obj) #'session-object
		      (with-output-to-string (*error-output*)
			(generate-full-combinatorial obj)))
		    (check (equal
			    (sort (get-lines (file-string ,workFileName)) #'string<)
			    (sort (get-lines ,combinationString) #'string<)))
		    (delete-file ,workFileName)
		    (if ,outFileName (delete-file ,outFileName))))))
    (let ((workFileName "workFile.txt")
	  (outFileName "outFile.txt"))
      ;commented out tests are b/c the generate-full-combinatorial method is no longer generating a partial combinatorial,
      ;dependent on the completed runs; it just generates a full combinatorial, so tests on the partial capability are commented out

      ;checking a standard case, where a couple are finished "1 3~%1 5"
      ;(deftest-vfc (format nil "iv=noise 1 1 3~%iv=speed 3 1 5") (format nil "1 3~%1 5") workFileName outFileName
		   ;(format nil "~{~a ~a ~%~}" (flatten (list `(1 4) `(2 3) `(2 4) `(2 5) `(3 3) `(3 4) `(3 5)))))
      ;checking another standard case, this time spreading the finished ones out and stepping by twos
      ;(deftest-vfc (format nil "iv=noi 1 2 9") (format nil "3~%5~%7") workFileName outFileName
		   ;(format nil "~{~a~%~}" (list 9 1)))
      ;checking a fringe case, where there are no IVs or results
      (deftest-vfc "" "" workFileName outFileName "")
      ;checking the case where all are finished
      ;(deftest-vfc (format nil "iv=step 1 2 7") (format nil "1~%3~%5~%7") workFileName outFileName "")
      (deftest-vfc "" "" workFileName nil "")
      ;checking that multiple sets of IVs work correctly
      (deftest-vfc (format nil "IV=noise 1 2 3~%IV=speed 3 1 5") "" workFileName nil
	(format nil "~{~a~a~a~a~%~}" (flatten (list (list 1 #\tab 3 #\tab) (list 1 #\tab 4 #\tab) 
						    (list 1 #\tab 5 #\tab) (list 3 #\tab 3 #\tab) 
						    (list 3 #\tab 4 #\tab) (list 3 #\tab 5 #\tab)))))
      ;checking that one set of IVs works correctly
      (deftest-vfc (format nil "IV=noise 1 1 3") "" workFileName outFileName
		   (format nil "~{~a~a~%~}" (flatten (mapcar #'list (list 1 2 3) (list #\tab #\tab #\tab))))))))

(deftest test-validate-entryFn ()
  "unit tests for validate-entryFn"
  (macrolet ((deftest-vef (IVKeys DVKeys entryFn will-error)
	       `(progn
		  ;mocking up necessary objects/functions/variables
		  (with-pandoric (obj) #'session-object
		    (setf (modelProgram obj) (eval ',entryFn))
		    (setf (IVKeys obj) ,IVKeys)
		    (setf (DVKeys obj) ,DVKeys)
		    (let ((result))
		      (handler-case (validate-entryfn obj)
			(error (condition) (setf result condition)))
		      (check
		       (if ,will-error result (not result)))))
		  (setf (get-pandoric #'DVs 'DVs) nil))))
    ;should pass
    (deftest-vef (list "noise") (list "DV") (lambda (&key (noise)) (declare (ignore noise)) (send-dv dv 5)) nil)
    ;should fail; IV names not equal to parnames in function
    (deftest-vef (list "noise") (list "DV") (lambda (&key (nois)) (declare (ignore nois)) (send-dv dv 4)) t)
    ;should pass; lisp isn't case sensitive
    (deftest-vef (list "x" "y") (list "DV") (lambda (&key (X) (Y)) (declare (ignore x y)) (send-dv dv 3)) nil)
    ;should fail; IV names not equal to parnames in function
    (deftest-vef (list "a" "B") (list "DV") (lambda (&key (a) (b) (c)) (declare (ignore a b c)) (send-dv dv 2)) t)
    ;should fail; IV names not equal to parnames in function
    (deftest-vef (list "a" "b" "c") (list "DV") (lambda (&key (a) (b)) (declare (ignore a b)) (send-dv dv 5)) t)
    ;should fail; checking hard constraint that duplicate names cannot be used
    (deftest-vef (list "a" "a" "b") (list "DV") (lambda (&key (a) (b)) (declare (ignore a b)) (send-dv dv 3)) t)
    ;should fail; must have at least one IV= in the config file
    (deftest-vef () (list "DV") (lambda (&key (a) (b)) (declare (ignore a b)) (send-dv dv 8)) t)
    ;should fail; must have at least one DV= in the config file
    (deftest-vef (list "IV") () (lambda (&key (IV)) (declare (ignore IV)) (send-dv dv 2)) t)))

(deftest test-validate-dvs ()
  "unit tests for validate-dvs"
  (macrolet ((deftest-vef (IVKeys DVKeys entryFn will-error)
	       `(progn
		  ;mocking up necessary objects/functions/variables
		  (with-pandoric (obj) #'session-object
		    (setf (modelProgram obj) (eval ',entryFn))
		    (setf (IVKeys obj) ,IVKeys)
		    (setf (DVKeys obj) ,DVKeys)
		    (let ((result))
		      (handler-case (validate-dvs obj)
			(error (condition) (setf result condition)))
		      (check
		       (if ,will-error result (not result)))))
		  (setf (get-pandoric #'DVs 'DVs) nil))))
    ;should fail; dvs sent not equal to dvs specified in config file
    (deftest-vef (list "iv") (list "dvtypo") (lambda (&key (iv)) (declare (ignore iv)) (send-dv dv 2)) t)
    ;should pass; all ivs and dvs correct
    (deftest-vef (list "iv") (list "DV" "DV2") (lambda (&key (iv)) (declare (ignore iv)) (send-dv dv 5) (send-dv dv2 3)) nil)
    ;should fail; dvs sent not equal to dvs specified in config file
    (deftest-vef (list "iv") (list "DV" "dv2") (lambda (&key (iv)) (declare (ignore iv)) (send-dv dv1 1) (send-dv dv2 nil)) t)))

(deftest test-sandwich ()
  "unit tests for sandwich"
  (check (equal (sandwich 'a (list 5 4 3)) (list 5 'a 4 'a 3)))
  (check (equal (sandwich 'a nil) nil))
  (check (equal (sandwich #\Tab (list 5)) (list 5)))
  (check (equal (sandwich #\Tab (list "ha" #\g 'ksk)) (list "ha" #\Tab #\g #\Tab 'ksk)))
  (check (errors-p (sandwich #\Tab 5))))

(deftest test-wrapper-execute-johnny5-run-class ()
  "unit tests for wrapper-execute on the run class when short circuiting"
  (macrolet ((test ((expected returned) &body body)
	       `(progn
		  ;mock up the file-string function & set the configFilePath/workFilePath to the values normally returned by 'file-string 
		  ;(instead of the path) so that files aren't necessary to build a mm-session object
		  (with-pandoric (configFilePath workFilePath) 'args
		    (setf configFilePath
			  (format nil "~{~a~%~}" (cons "IV=FirstIV" (mapcar (lambda (x) (format nil "DV=~a" x)) ',expected))))
		    (setf workFilePath "0"))
                  ;build a mm-session object that has one johnny5-run-class object
		  (let ((obj))
		    (with-shadow (file-string #'identity) 
		      (args)
		      (mods)
		      (setf obj (build-mm-session)))
                    ;take that object, execute it, and evaluate body, which will make sure that the results are as expected
		    ;mock up get-DVs, so that an actual model doesn't have to be executed
		    (with-shadow (get-DVs (lambda (obj &optional (process) (apps))
					    (declare (ignore obj process apps))
					    (mapcar (lambda (x) (cons (string x) "0")) ',returned)))
			,@body))
		  ;cleanup
		  (delete-file (get-pandoric 'mods 'mm_out))
		  (delete-file (get-pandoric 'mods 'mm_fraction_done)))))
    (with-pandoric (mm_out) 'mods
      (test ((x y) (x y)) ;check that if returned DVs are exactly the ones expected...
	    (let ((str (with-output-to-string (*error-output*)
			 (check (not (errors-p (wrapper-execute obj))))))) ;no error is thrown after the run
	      (check (string-equal str "")) ;nothing is notated
	      (check (equalp (mapcar (lambda (x) (eval (read-from-string x))) ;and output file has no nil values
				     (get-words (file-string mm_out)))
			     (list 0 0 0)))))
      (test ((x z y hello) (x)) ;check that if not all expected DVs are returned...
	    (let ((str (with-output-to-string (*error-output*)
			 (check (not (errors-p (wrapper-execute obj))))))) ;no error is thrown
	      (check (search "(hello y z)" str :test #'string-equal)) ;missing DVs are notated
	      (check (equalp (mapcar (lambda (x) (eval (read-from-string x))) ;and output file has nil values for DVs not returned
				     (get-words (file-string mm_out))) 
			     (list 0 0 nil nil nil)))))
      (test ((x) (x y z hello)) ;check that if not all returned DVs are expected...
	    (let ((str (with-output-to-string (*error-output*)
			 (check (not (errors-p (wrapper-execute obj))))))) ;no error is thrown
	      (check (search "(hello y z)" str :test #'string-equal)) ;extra DVs are notated
	      (check (equalp (mapcar (lambda (x) (eval (read-from-string x))) ;and output file has values for only expected DVs
				     (get-words (file-string mm_out)))
			     (list 0 0))))))))

(deftest test-print-collector-mm-process-output-str-class ()
  "test that the printer when the model crashes works correctly
   header information and the last (up to) 200 lines printed by the model should be present"
  (macrolet ((test (IVs numModelLinesPrinted numModelLinesDisplayed)
	       `(progn
		  ;mock up the file-string function & set the configFilePath/workFilePath to the values normally returned by 'file-string
		  ;so that files are not necessary to build the mm-session object
		  (with-pandoric (configFilePath workFilePath) 'args
		    (setf configFilePath (format nil "~{~a~%~}" (append (mapcar (lambda (x) (format nil "IV=~a" x)) ',IVs) (list "DV=z"))))
		    (setf workFilePath (format nil "~{~a~}" (sandwich #\Tab (mapcar (lambda (x) (format nil "'~a-val" x)) ',IVs)))))
		  (let ((obj))
		    (with-shadow (file-string #'identity)
		      (args)
		      (mods)
		      (setf obj (build-mm-session)))
		    ;shadow the run-model function (the model entry function)
		    ;have the entry function print the number of lines specified by numModelLinesPrinted before crashing
		    (with-shadow (run-model (lambda (&key ,@(mapcar #'list IVs))
					      (declare (ignore ,@IVs))
					      (dotimes (i ,numModelLinesPrinted)
						(format t "line~a~%" i))
					      (error "I am a model; I crashed because of a divide by zero error")))
		      ;attempt to execute the entry function; should crash, so wrap execution in an errors-p call, and save model output to str
		      (let ((str))
			(setf str (with-output-to-string (*error-output*)
				    (attempt (wrapper-execute obj))))
			;the str must contain all "IV: 'IV-val" 's
			(dolist (IV (mapcar #'string ',IVs))
			  (check (search (format nil "~a: '~a-val" IV IV) str :test #'string-equal)))
			;the str must contain a header saying that the last N model lines follows
			(check (search (format nil "The last ~a lines that were printed by the model before the error" ,numModelLinesDisplayed)
				       str :test #'string-equal))
			;the str must contain all last numModelLinesDisplayed model lines
			(loop for i from (- ,numModelLinesPrinted 1) downto (- ,numModelLinesPrinted ,numModelLinesDisplayed) do
			     (check (search (format nil "line~a" i) str :test #'string-equal)))
			;the str must contain the string contained when the model crashed
			(check (search "I am a model; I crashed because of a divide by zero error" str :test #'string-equal))))))))
    (test (FirstIV secondIV thirdIV) 201 200) ;test that output is correct when model prints more than 200 lines before crashing
    (test (x) 2000 200) ;again, more than 200 lines, but this time much more
    (test (FirstIV secondIV) 0 0) ;test that output is correct in fringe case - when model prints no lines before crashing
    (test (FirstIV) 10 10))) ;test that output is correct when # model lines printed is less than 200

(deftest test-print-collector-mm-collector-class ()
  "test that the printer that writes output data to mm_out works correctly
   two main cases; when the model is successful, print tab-delimited IVs then DVs
   when the model crashes, print nothing"
  (macrolet ((test (IVs DVs outputted-DVs errors-p &body body)
	       `(progn
		  (with-pandoric (configFilePath workFilePath) 'args
		    (setf configFilePath (format nil "~{~a~%~}" (append (mapcar (lambda (x) (format nil "IV=~a" x)) ',IVs)
									(mapcar (lambda (x) (format nil "DV=~a" x)) ',DVs))))
		    (setf workFilePath (format nil "~{~a~}" (sandwich #\Tab (mapcar (lambda (x) (format nil "'~a-val" x)) ',IVs)))))
		  (let ((obj))
		    (with-shadow (file-string #'identity)
		      (args)
		      (mods)
		      (setf obj (build-mm-session)))
		    (with-shadow (run-model (lambda (&key ,@(mapcar #'list IVs))
					      (declare (ignore ,@IVs))
					      (dolist (DV ',outputted-DVs)
						(format t "~a='~a-val~%" DV DV))
					      (if ,errors-p (error "here"))))
		      (let ((str))
			(setf str (with-output-to-string (*error-output*)
				    (attempt (wrapper-execute obj))))
			,@body)))
		  ;cleanup
		  (attempt (delete-file (get-pandoric 'mods 'mm_out))))))
    (test (x y) (z) (z) nil ;test that output in file is correct for standard case; a few IVs and a DV
	  (check (string-equal (format nil "~{~a~}" (sandwich #\Tab (list "'x-val" "'y-val" "z-val")))
			       (file-string (get-pandoric 'mods 'mm_out)))))
    (test (x y) (z) (z) t ;test that output in file is correct for case where the model crashes
	  (format t "~a~%" str)
	  (check (not (string-equal str ""))) ;str should contain some output, since information about the crash is printed to stderr
	  (check (errors-p (file-string (get-pandoric 'mods 'mm_out))))) ;the output file shouldn't exist b/c nothing should have been written to it
    (test (x y) (z1 z2 z3) (z1 z3) nil ;test that output in file is correct for case where not all DVs are returned, but some are (model didn't crash)
	  (check (string-equal (format nil "~{~a~}" (sandwich #\Tab (list "'x-val" "'y-val" "z1-val" nil "z3-val"))) ;nil should be placed as value for z2
			       (file-string (get-pandoric 'mods 'mm_out)))))))

(deftest test-wrapper-execute-session-class ()
  "unit tests to make sure that the lisp process exits appropriately if at least one run of the model fails"
  (macrolet ((test (error-p &body body)
	       `(progn
		  (with-pandoric (configFilePath WorkFilePath) 'args
		    (setf configFilePath (format nil "iv=x~%dv=z"))
		    (setf workFilePath "0"))
		  (let ((obj))
		    (with-shadow (file-string #'identity)
		      (args)
		      (mods)
		      (setf obj (build-mm-session)))
		    (with-shadow (run-model (lambda (&key (x))
					      (declare (ignore x))
					      (if ,error-p (error "here"))))
		      ,@body)))))
    (test t (check (errors-p (wrapper-execute obj)))) ;if a run errors the lisp process should throw an exception
    (test nil (check (not (errors-p (wrapper-execute obj))))))) ;if the run doesn't error, then no exception should be thrown

(deftest test-expect ()
  "unit tests for the expect macro"
  (check (not (errors-p (expect t "this shouldn't be printed ~a" "he"))))
  (check (string-equal "test 5 4" (format nil "~a" (errors-p (expect nil "test ~a ~a" 5 4)))))
  (check (errors-p (expect nil ""))))

(defun testMM ()
  "unit tests for the mm.lisp code"
  (let ((result
	 (runtests 
	  (test-comb)
	  (test-validate-full-combinatorial)
	  (test-validate-entryFn)
	  (test-validate-dvs)
	  (test-generate-full-combinatorial)
	  (test-sandwich)
	  (test-generate-header)
	  (test-wrapper-execute-johnny5-run-class)
	  (test-expect)
	  (test-print-collector-mm-process-output-str-class)
	  (test-print-collector-mm-collector-class)
	  (test-wrapper-execute-session-class)
	  )))
    (format t "~%overall: ~:[FAIL~;pass~]~%" result)))

(require 'sb-cover)
(defun cover ()
  (testMM)
  (sb-cover:report "../docs/cover/"))





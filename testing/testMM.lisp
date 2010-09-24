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

(deftest test-comb-default ()
  "unit tests for the comb macro, when using its default behavior that returns all combinations"
  (check
   (equal (funcall (comb) (list '(1 2 3) '(1 1 4))) 
	  (list '(1 1) '(1 2) '(1 3) '(1 4) '(3 1) '(3 2) '(3 3) '(3 4)))
   (equal (funcall (comb) nil) 
	  nil)
   (equal (funcall (comb) (list '(1 1 3)))
	  (list '(1) '(2) '(3)))
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
    (deftest-vfc (list (list "IV=" "noise" "1" "1" "3") ; should not fail
		       (list "IV=speed" "1.0" "0.1" "2.0")) nil)
    (deftest-vfc (list (list "IV=" "noise" "1" "1/2" "2")) nil))) ;should not fail; checking that code can read fractions

(deftest test-generate-full-combinatorial ()
  "unit tests for generate-full-combinatorial"
  (macrolet ((deftest-gfc (IVWdLSTPiece workFileName combinationString)
	       `(progn
		  ;mocking up necessary objects/functions/variables
		  (with-pandoric (configFileWdLST) #'args
		    (setf configFileWdLST 
			  (append ,IVWdLSTPiece (list (list (fast-concatenate "workFileName=" "\"" ,workFileName "\""))))))
		  (with-pandoric (obj) #'session-object
		    (with-output-to-string (*error-output*)
		      (generate-full-combinatorial obj)))
		  (check (string-equal (file-string (string-trim (list #\") ,workFileName)) ,combinationString))
		  ;cleaning up after ourselves
		  (delete-file (string-trim (list #\") ,workFileName)))))
    (let ((workFileName "Test Full Combinatorial Output.txt"))
      ;checking that one set of IVs works correctly
      (deftest-gfc (list (list "IV=" "noise" "1" "1" "3")) workFileName (format nil "~{~,8f ~%~}" (list 1 2 3)))
      ;checking that multiple sets of IVs work correctly
      (deftest-gfc (list (list "IV=" "noise" "1" "2" "3") (list "IV=" "speed" "3" "1" "5")) workFileName
	(format nil "~{~,8f ~,8f ~%~}" (flatten (list (list 1 3) (list 1 4) (list 1 5) (list 3 3) (list 3 4) (list 3 5))))))))

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
		      (handler-case (validate-entryFn obj)
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
    (deftest-vef (list "IV") () (lambda (&key (IV)) (declare (ignore IV)) (send-dv dv 2)) t)
    ;should fail; dvs sent not equal to dvs specified in config file
    (deftest-vef (list "iv") (list "dvtypo") (lambda (&key (iv)) (declare (ignore iv)) (send-dv dv 2)) t)
    ;should pass; all ivs and dvs correct
    (deftest-vef (list "iv") (list "DV" "DV2") (lambda (&key (iv)) (declare (ignore iv)) (send-dv dv 5) (send-dv dv2 3)) nil)
    ;should fail; dvs sent not equal to dvs specified in config file
    (deftest-vef (list "iv") (list "DV" "dv2") (lambda (&key (iv)) (declare (ignore iv)) (send-dv dv1 1) (send-dv dv2 nil)) t)))

(defun testMM ()
  "unit tests for the mm.lisp code"
  (let ((result
	 (runtests 
	  (test-comb)
	  (test-validate-full-combinatorial)
	  (test-validate-entryFn)
	  (test-generate-full-combinatorial))))
    (format t "~%overall: ~:[FAIL~;pass~]~%" result)))





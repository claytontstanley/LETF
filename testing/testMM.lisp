(let ((obj (build-mm-session)))
  (setf (symbol-function 'session-object)
	(plambda () (obj)
	  ())))

(deftest test-comb-default ()
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
  (check
   (equal (funcall (comb (list (reverse trail))) (list '(1 2 3) '(1 1 4)))
	  (list '(1 1) '(2 1) '(3 1) '(4 1) '(1 3) '(2 3) '(3 3) '(4 3)))))

(deftest test-comb ()
  (combine-results
   (test-comb-default)
   (test-comb-anaphoric)))

(deftest test-validate-full-combinatorial ()
  (macrolet ((deftest-vfc (configFileWdLSTPiece will-error)
	       `(progn
		  (with-pandoric (configFileWdLST) #'args
		    (setf configFileWdLST ,configFileWdLSTPiece))
		  (with-pandoric (obj) #'session-object
		    (let (result)
		      (handler-case (validate-full-combinatorial obj)
			(error (condition) (setf result condition)))
		      (check
		       (if ,will-error result (not result))))))))
    (deftest-vfc (list (list "IV=" "noise" "1" "1" "3")) nil)
    (deftest-vfc (list (list "IV=" "noise" "1" "1" "1")) t)
    (deftest-vfc (list (list "IV=" "speed" "1" "2" "4")) t)
    (deftest-vfc (list (list "IV=" "speed" "3" ".1" "2")) t)
    (deftest-vfc (list (list "IV=" "speed" "help" "1" "4")) t)
    (deftest-vfc (list (list "IV=" "noise" "1" "1" "3")
		       (list "IV=speed" "1.0" "0.1" "2.0")) nil)
    (deftest-vfc (list (list "IV=" "noise" "1" "1/2" "2")) nil)))

(deftest test-generate-full-combinatorial ()
  (macrolet ((deftest-gfc (IVWdLSTPiece workFileName combinationString)
	       `(progn
		  (with-pandoric (configFileWdLST) #'args
		    (setf configFileWdLST 
			  (append ,IVWdLSTPiece (list (list (fast-concatenate "workFileName=" "\"" ,workFileName "\""))))))
		  (with-pandoric (obj) #'session-object
		    (with-output-to-string (*error-output*)
		      (generate-full-combinatorial obj)))
		  (check (string-equal (file-string (string-trim (list #\") ,workFileName)) ,combinationString))
		  (delete-file (string-trim (list #\") ,workFileName)))))
    (let ((workFileName "Test Full Combinatorial Output.txt"))
      (deftest-gfc (list (list "IV=" "noise" "1" "1" "3")) workFileName (format nil "~{~,8f ~%~}" (list 1 2 3)))
      (deftest-gfc (list (list "IV=" "noise" "1" "2" "3") (list "IV=" "speed" "3" "1" "5")) workFileName
	(format nil "~{~,8f ~,8f ~%~}" (flatten (list (list 1 3) (list 1 4) (list 1 5) (list 3 3) (list 3 4) (list 3 5))))))))

(deftest test-validate-entryFn ()
  (macrolet ((deftest-vef (IVKeys entryFn will-error)
	       `(progn
		  (with-pandoric (obj) #'session-object
		    (setf (modelProgram obj) ,entryFn)
		    (setf (IVKeys obj) ,IVKeys)
		    (let (result)
		      (handler-case (validate-entryFn obj)
			(error (condition) (setf result condition)))
		      (check
		       (if ,will-error result (not result))))))))
    (deftest-vef (list "noise") (lambda (&key (noise)) (declare (ignore noise))) nil)
    (deftest-vef (list "noise") (lambda (&key (nois)) (declare (ignore nois))) t)
    (deftest-vef (list "x" "y") (lambda (&key (X) (Y)) (declare (ignore x y))) nil)
    (deftest-vef (list "a" "B") (lambda (&key (a) (b) (c)) (declare (ignore a b c))) t)
    (deftest-vef (list "a" "b" "c") (lambda (&key (a) (b)) (declare (ignore a b))) t)
    (deftest-vef (list "a" "a" "b") (lambda (&key (a) (b)) (declare (ignore a b))) t)))

(defun testMM ()
  (let ((result
	 (runtests 
	  (test-comb)
	  (test-validate-full-combinatorial)
	  (test-validate-entryFn)
	  (test-generate-full-combinatorial))))
    (format t "~%overall: ~:[FAIL~;pass~]~%" result)))





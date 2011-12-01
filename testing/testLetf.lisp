(deftest test-mean ()
	 (check (equal (mean '(1 2 3 4)) 5/2))
	 (check (equal (mean '(1)) 1))
	 (check (equal (mean '(4 3 2 1)) 5/2))
	 (check (equal (mean nil) nil)))

(deftest test-median ()
	 (check (equal (median '(1 2 3 4 5)) 3))
	 (check (equal (median '(1 2 3 4)) 5/2))
	 (check (equal (median '(1)) 1))
	 (check (equal (median '(5 3 2 4 1)) 3))
	 (check (not (errors-p (median nil))))
	 (let ((lst '(5 4 3 2 1)))
	   (median lst)
	   ;if median is destructive, it may modify lst, but leave it in a state where (length lst) returns the 
	   ;correct length, but if you print the lst, it isn't the entire list. This at least happens in SBCL
	   ;so a workaround is to copy lst, and then check if the length of that list is equal to what is expected
	   (check (equal (length (copy-list lst)) 5))))

(deftest test-get-matching-lines ()
	 (let ((testStr (format nil "aLine= aLineVal~%bLine=bLineVal~%aLine= ALineVal2")))
	   (loop for (LHS RHS) in (list '("aLine=" ("aLineVal" "aLineVal2")) ;multiple matched lines; single matcher
					'("bLine=" ("bLineVal")) ;single matched line; single matcher
					'(("aLine=" "bline=") ("aLineVal" "bLineVal" "aLineVal2")) ;multiple matches lines; multiple matchers
					'("bline=" ("blineval")) ;not case sensitive
					'("notPresent=" ()) ; no match
					)
		 do (check (equalp (get-matching-lines testStr LHS)
				   RHS)))))

(deftest test-get-matching-lines-returning-linenum-lhs-rhs ()
	 (let ((testStr (format nil "aLine=aLineVal~%bLine=bLineVal~%aLine=aLineVal2")))
	   (loop for (LHS return-val) in (list '("aLine=" ( (0 "aline=" "alineVal") (2 "aline=" "alineVal2") ))
					       '("bLine=" ( (1 "bline=" "blineVal") ))
					       '( ("aLine=" "bLine=") ( (0 "aline=" "alineVal") (1 "bline=" "blineVal") (2 "aline=" "alineVal2") ))
					       )
		 do (check (equalp (get-matching-lines-returning-linenum-lhs-rhs testStr LHS)
				   return-val)))))

(deftest test-load-and-eval-commands ()
	 (let ((testConfigFile (format nil "runBeforeLoad=(print 1)~%file2load=2~%runWithinLoad=(print 3)~%file2load=4~%runAfterLoad=(print 5)"))
	       (configFilePath (get-pandoric #'args 'configFilePath)))
	   (setf (get-pandoric #'args 'configFilePath) testConfigFile)
	   (with-shadow (file-string #'identity)
			(args))
	   (check
	     (equal
	       (eval (read-from-string
		       (format nil "(list ~a)"
			       (capture-output t
					       (with-shadow (load-and-loaded (lambda (str)
									       (format t "~a " str)))
							    (load-and-eval-commands))))))
	       (list 1 2 3 4 5)))
	   (setf (get-pandoric #'args 'configFilePath) configFilePath)
	   (args)))

(defun test-letf ()
  (format t "~%overall: ~:[FAIL~;pass~]~%" (run-all-tests)))


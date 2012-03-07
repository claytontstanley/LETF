(defun make-test-session-object (&key (configFileStr) (workFileStr))
  "Takes strings for a config file and work file, and builds a session object"
  (let ((out) (orig-configFilePath) (orig-workFilePath))
    (with-pandoric (configFilePath workFilePath) 'args
      (setf orig-configFilePath configFilePath)
      (setf orig-workFilePath workFilePath)
      (setf configFilePath configFileStr)
      (setf workFilePath workFileStr)
      ; mock up the file-string function & set the configFilePath/workFilePath to the values normally returned by 'file-string
      ; (instead of the path) so that files aren't necessary to build a mm-session object
      (with-shadow (file-string #'identity)
        (args)
        (setf out (build-hpc-session)))
      (setf configFilePath orig-configFilePath)
      (setf workFilePath orig-workFilePath))
    (args)
    out))

(defmethod first-run-class ((obj session-class))
  (first (runs (first (runProcesses obj)))))

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

(deftest test-defaultIVStringFn ()
  (labels ((test (IV-lst)
             (let* ((keys (mapcar #'car IV-lst))
                    (vals (mapcar #'cdr IV-lst))
                    (obj (make-test-session-object
                           :configFileStr (format nil "~{IV=~a~%~}~%DV=z~%" keys)
                           :workFileStr (format nil "~{~a ~}~%" vals))))
               (check
                 (string-equal
                   (defaultIVStringFn (first-run-class obj))
                   (format nil "~{~a~^	~}" vals ))))))
    (test '(("IV1" . 5) ("IV2" . 4)))
    (test '(("IVa" . 'a)))
    (test '(("IV1" . 5) ("IV2" . 4) ("IV3" . 3)))))

(deftest test-get-matching-lines ()
  "Tests that get-matching-lines returns the correct values when parsing the testStr config file
         LHS is the str to look for in the config file
         RHS is the list of all matches to LHS in the config file"
         (let ((testStr (format nil "aLine= aLineVal~%bLine=bLineVal~%aLine= ALineVal2")))
           (loop for (LHS RHS) in (list '("aLine=" ("aLineVal" "aLineVal2")) ;multiple matched lines; single matcher
                                        '("bLine=" ("bLineVal")) ;single matched line; single matcher
                                        '(("aLine=" "bline=") ("aLineVal" "bLineVal" "aLineVal2")) ;multiple matches lines; multiple matchers
                                        '("bline=" ("blineval")) ;not case sensitive
                                        '("notPresent=" ()) ; no match
                                        )
                 do (check (equalp (get-matching-lines testStr LHS)
                                   RHS)))))

(deftest test-get-matching-lines-full ()
  "Analogous to test-get-matching-lines, except this one checks that the additional information
         for each match is included (i.e., the LHS for get-matching-lines-full)"
         (let ((testStr (format nil "aLine=aLineVal~%bLine=bLineVal~%aLine=aLineVal2")))
           (loop for (LHS return-val) in (list '("aLine=" ( ("aline=" "alineVal") ("aline=" "alineVal2") ))
                                               '("bLine=" ( ("bline=" "blineVal") ))
                                               '( ("aLine=" "bLine=") ( ("aline=" "alineVal") ("bline=" "blineVal") ("aline=" "alineVal2") ))
                                               )
                 do (check (equalp (get-matching-lines-full testStr LHS)
                                   return-val)))))

(deftest test-load-and-eval-commands ()
  "Tests that runBeforeLoad, file2load, runWithinLoad, and runAfterLoad commands in the config file get parsed and processed in the correct order"
  (let ((testConfigFile (format nil "runBeforeLoad=(print 1)~%file2load=2~%runWithinLoad=(print 3)~%file2load=4~%runAfterLoad=(print 5)"))
        (configFilePath (get-pandoric #'args 'configFilePath)))
    ;Load-and-eval-commands gets the configfile from the pandoric function #'args
    ;In order to mock the config file with testConfigFile here, I set the path to the config file to the actual text in the mocked config file,
    ;And then mock the file-string function , by having it return itself
    (setf (get-pandoric #'args 'configFilePath) testConfigFile)
    (with-shadow (file-string #'identity)
      (args))
    ;For the check, any call in the config file for file2load will try to load a file (by default). So I mock the loading function (load-and-loaded),
    ;and have it print the name of the file (instead of loading the file). Looking above, the name of the file is a number. The idea here is that if
    ;load and eval commands is working properly, the numbers 1-5 will be printed to stdout (in that order). So then I wrap what is
    ;outputted to stdout (should be 1 2 3 4 5) within a list, use the lisp reader to eval it, and then compare it to the list (1 2 3 4 5)
    (check
      (equal
        (eval (read-from-string
                (format nil "(list ~a)"
                        (capture-output t
                          (with-shadow (load-and-loaded #'print)
                            (load-and-eval-commands))))))
        (list 1 "2" 3 "4" 5)))
    ;Return the args pandoric function back to it's default state
    (setf (get-pandoric #'args 'configFilePath) configFilePath)
    (args)))

(defun test-letf ()
  (format t "~%overall: ~:[FAIL~;pass~]~%" (run-all-tests)))


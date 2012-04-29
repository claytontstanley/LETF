(defmethod first-run-class ((obj session-class))
  (first (runs (first (runProcesses obj)))))

(deftest-hpc test-mean ()
  (check (equal (mean '(1 2 3 4)) 5/2))
  (check (equal (mean '(1)) 1))
  (check (equal (mean '(4 3 2 1)) 5/2))
  (check (equal (mean nil) nil)))

(deftest-hpc test-median ()
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

(defclass dummy-process ()
  ((p-status :accessor p-status :initarg :p-status :initform :exited)
   (p-output :accessor p-output :initarg :p-output :initform (make-string-input-stream ""))
   (p-exit-code :accessor p-exit-code :initarg :p-exit-code :initform 0)))

(deftest-hpc test-get-DVs-sleep ()
  (let ((obj (funcall (compose #'first #'runs #'first #'runProcesses)
                      (make-test-session-object
                        :configfilestr (format nil "~{~a~%~}" (list "modelProgram=python" "iv=x" "dv=a"))
                        :workfilestr (format nil "1"))))
        (process (make-instance 'dummy-process)))
    (setf (no-dvs-sleep-time obj) 0)
    (labels ((test (p-output p-status expected)
               (setf (p-status process) p-status)
               (setf (p-output process) p-output)
               (let ((s (cl-ppcre:create-scanner 
                          expected
                          :case-insensitive-mode t 
                          :single-line-mode t)))
                 (check (cl-ppcre:scan s (capture-output t (get-DVs obj process)))))))
      (test (make-string-input-stream "") :running "did not find any DVs for running nonlisp model process.*sleeping")
      (test (make-string-input-stream "something") :running "^$")
      (test (make-string-input-stream "") :exited "^$")
      (test (make-string-input-stream "something") :exited "^$"))))

(deftest-hpc test-nonlisp-dv-parsing ()
  (labels ((test (dvs expected)
             (with-test-cleanup
               (let ((output)
                     (obj (make-test-session-object
                            :configfilestr (format nil "~{~a~%~}" (list "modelProgram=python" "iv=x" "iv=y" "dv=a" "dv=b" "runsPerProcess=2"))
                            :workfilestr (format nil "1 1~%2 2~%"))))
                 (with-shadow (get-process (lambda (obj input)
                                             (declare (ignore obj input))
                                             (make-instance 'dummy-process 
                                                            :p-output (make-string-input-stream (format nil dvs))))) 
                              (setf output (capture-output t (wrapper-execute obj))))
                 (let ((s (cl-ppcre:create-scanner 
                            expected
                            :case-insensitive-mode t 
                            :single-line-mode t)))
                   (check (cl-ppcre:scan s output))))))
           (dvs (&rest lst)
             (format nil "~{~a~%~}"
                     (mapcar (lambda (x)
                               (cond ((eq x 'run1) "IVs=1 1")
                                     ((eq x 'run2) "IVs=2 2")
                                     (t (format nil "~a=1" x))))
                             lst))))
    (test (dvs 'run1 'a 'b 'run2 'a 'b) ".*")
    (test (dvs 'a 'b 'c 'd) ".*a.*discarded.*b.*discarded.*c.*discarded.*d.*discarded")
    (test (dvs 'z 'run1 'a 'b 'a 'run2 'a) ".*z.*discarded.*extra.*a.*failed to send.*b")
    (test (dvs 'run1 'a 'b 'run2 'a 'b 'c) "evaluating.*sent extra dv.*(c.*).*evaluating")
    (test (dvs 'run1 'a 'b 'c 'run2 'b 'a) "sent extra dv.*(c.*).*evaluating.*evaluating")
    (test (dvs 'run1 'a 'b 'run2 'b) "failed to send.*(a).*left")
    (test (dvs 'run1 'a) "failed to send.*(b).*failed to send.*(a b)")
    (test (dvs 'run1 'a 'c 'b 'run2 'a 'b) "sent extra dv.*(c.*)")
    (test (dvs 'run1 'a 'run2 'a 'b) "sent trial start marker for next.*before sending all.*(b).*this")
    (check (errors-p (test (dvs 'run2 'before 'run1) ".*")))))

(deftest-hpc test-IV-string-fn ()
  (labels ((test (IV-lst)
             (let* ((keys (mapcar #'car IV-lst))
                    (vals (mapcar #'cdr IV-lst))
                    (obj (make-test-session-object
                           :configFileStr (format nil "~{IV=~a~%~}~%DV=z~%" keys)
                           :workFileStr (format nil "~{~a ~}~%" vals))))
               (check
                 (string-equal
                   (IV-string-fn (first-run-class obj))
                   (format nil "~{~a~^	~}" vals ))))))
    (test '(("IV1" . 5) ("IV2" . 4)))
    (test '(("IVa" . 'a)))
    (test '(("IV1" . 5) ("IV2" . 4) ("IV3" . 3)))))

(deftest-hpc test-get-matching-lines ()
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

(deftest-hpc test-get-matching-lines-full ()
  "Analogous to test-get-matching-lines, except this one checks that the additional information
   for each match is included (i.e., the LHS for get-matching-lines-full)"
   (let ((testStr (format nil "aLine=aLineVal~%bLine=bLineVal~%aLine=aLineVal2")))
     (loop for (LHS return-val) in (list '("aLine=" ( ("aline=" "alineVal") ("aline=" "alineVal2") ))
                                         '("bLine=" ( ("bline=" "blineVal") ))
                                         '( ("aLine=" "bLine=") ( ("aline=" "alineVal") ("bline=" "blineVal") ("aline=" "alineVal2") ))
                                         )
           do (check (equalp (get-matching-lines-full testStr LHS)
                             return-val)))))

(deftest-hpc test-load-and-eval-commands ()
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

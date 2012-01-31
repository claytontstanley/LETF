(defun run-model (&key (speed))
  (declare (ignore speed))
  (format t "rt1=\"hello~a\"~%" (random 20000))
  (format t "rt2=\"world~a\"~%" (random 20000)))

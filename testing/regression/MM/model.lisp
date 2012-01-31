
(defun run-model (&key (x 5) (y 5))  
     ;(format t "z=~a~%" (+ (- (/ (act-r-random 1000) 100) 5) (* x x) (* y y ))))
  (send-DV z (+ (- (/ (act-r-random 1000) 100) 5) (* x x ) (* y y))))



  

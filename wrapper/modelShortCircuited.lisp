(defun run-model (model)


  (format t " lat1= 23~a~%" (random (gethash "blend-temperature" model)))
  (format t "lat2 =43~d~%" (random (gethash "blend-temperature" model)))
  (format t "lat3=~d53~%" (random (gethash "blend-temperature" model)))
  (format t "lat4=63~d~%" (random (gethash "blend-temperature" model))) 
  (format t "lat5=73~d~%" (random (gethash "blend-temperature" model)))


  (format t "pos1=53~d~%" (random (gethash "blend-temperature" model)))
  (format t "pos2=6~d3~%" (random (gethash "blend-temperature" model)))
  (format t "pos3=73~d~%" (random (gethash "blend-temperature" model)))
  (format t "pos4=83~d~%" (random (gethash "blend-temperature" model)))
  (format t "pos5=83~d~%" (random (gethash "blend-temperature" model)))

  
  (format t "modrt = '~a~%" (list 
			     (if (> (random 30) -25) (random 1000)) 
			     (if (> (random 30) -25) (random 1000)) 
			     (if (> (random 30) -25) (random 1000)) 
			     (if (> (random 30) -25) (random 1000)) 
			     (if (> (random 30) -25) (random 1000))))  
  ;(format t "rt1=5~%")
  ;(format t "rt2=~a~%" (random 50))
  ;(format t "rt3=~a~%" (random 40))
  ;(format t "rt4=nil~%")
  ;(format t "rt5=nil~%")

#|
  (format t "rt1=nil~%") 
  (format t "rt2=2~d4~%" (random (gethash "blend-temperature" model)))
  (format t "rt3=37~d~%" (random (gethash "blend-temperature" model)))
  (format t "rt4=49~d~%" (random (gethash "blend-temperature" model)))
  (format t "rt5=52~d~%" (random (gethash "blend-temperature" model)))
|#

)

  
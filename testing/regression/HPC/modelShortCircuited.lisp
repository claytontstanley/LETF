(defparameter *num-runs* nil)
(defparameter *environ-wait* nil)
(defparameter *blend-temperature* nil)
(defparameter *bll* nil)
(defparameter *ans* nil)

(defparameter *temp* -1)

(defvar *not-sending-some-dvs-p* nil)

(defun run-model (model)

  (print-hash model)

  (format t "~%~%~%")

  (setf *num-runs* (gethash "num-runs" model)) 
  (setf *environ-wait* (gethash "environ-wait" model))
  (setf *blend-temperature* (gethash "blend-temperature" model))
  (setf *bll* (gethash "bll" model))
  (setf *ans* (gethash "ans" model))
  
  (setf *temp* -1)
  
  (dotimes (i 10) (format t "hello!!!!~%~%~%"))
  

  (format t " lat1= 23~a~%" (random (gethash "blend-temperature" model)))
  (format t "lat2 =43~d~%" (random (gethash "blend-temperature" model)))
  (format t "lat3=~d53~%" (random (gethash "blend-temperature" model)))
  (format t "lat4=63~d~%" (random (gethash "blend-temperature" model))) 
  (format t "lat5=73~d~%" (random (gethash "blend-temperature" model)))
  
 ; (random (+ (random 1) *temp*))

  
  (if *not-sending-some-dvs-p* 
      (if (equal (random 2) 1)
	  (format t "pos1=53~d~%" (random (gethash "blend-temperature" model)))
	  )
      (format t "pos1=53~d~%" (random (gethash "blend-temperature" model))))



  (format t "pos2=6~d3~%" (random (gethash "blend-temperature" model)))
  (format t "pos3=73~d~%" (random (gethash "blend-temperature" model)))
  (format t "pos4=83~d~%" (random (gethash "blend-temperature" model)))
  (format t "pos5=83~d~%" (random (gethash "blend-temperature" model)))


#|  (format t "rt1=5~%")
  (format t "rt2=~a~%" (random 50))
  (format t "rt3=~a~%" (random 40))
  (format t "rt4=6~%")
  (format t "rt5=7~%")|#

  (format t "modrt = '~a~%" (list 
			     (if (> (random 30) -25) (random 1000)) 
			     (if (> (random 30) -25) (random 1000)) 
			     (if (> (random 30) -25) (random 1000)) 
			     (if (> (random 30) -25) (random 1000)) 
			     (if (> (random 30) -25) (random 1000))))


#|  (format t "rt1=nil~%") 
  (format t "rt2=2~d4~%" (random (gethash "blend-temperature" model)))
  (format t "rt3=37~d~%" (random (gethash "blend-temperature" model)))
  (format t "rt4=49~d~%" (random (gethash "blend-temperature" model)))
  (format t "rt5=52~d~%" (random (gethash "blend-temperature" model))) |#

)

  

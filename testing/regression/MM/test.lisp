;returns a compiled lambda function that you can call to evaluate the 'key' in 'hash';
;similar to (get-elements key hash), except here, if you don't have an element yet to 
;evaluate that key (that is, the model still needs to run to return it), this lambda 
;function will require that parameter to be passed to it as an input when it's called
;I do this so that I can pre-compile all of the functions before actually having all of the 
;data that the model will return. The data in the functions here is the structure and data
;in the configuration file. What's left to be passed as an input are the model's results.
(defun function-for (key hash)
  (multiple-value-bind (necessaries str) (necessaries key hash)
    (compile nil `(lambda ,(mapcar (lambda (x) (intern (format nil "~:@(~A~)" x))) necessaries)
		    ,(read-from-string (car str))))))

(defclass test-collector-class (collector-class)
  ((functionHash :accessor functionHash :initarg :functionHash :initform nil)
   (necessariesHash :accessor necessariesHash :initarg :necessariesHash :initform nil)))

(defmethod print-collector ((obj test-collector-class))
  (format t "Evaluating: ")
  (dotimes (i (length (cellElements obj)))
    (if (equal i (- (length (cellElements obj)) 1))
	(format t "~a~%" (cdr (nth i (cellElements obj))))
	(format t "~a " (cdr (nth i (cellElements obj))))))
  (dolist (key (keys obj))
    (aif
     (apply (gethash key (functionHash obj))
	    (mapcar #'cdr (get-elements (gethash key (necessariesHash obj)) (collection obj) 
					:collapseFns (gethash-ifHash key (collapseHash obj)))))
     (format t "~a: ~a~%" key (coerce it 'double-float)))))

(defun build-test-session ()
  (build-session ;this is a macro
   :collector-instance (make-instance 'test-collector-class
				      :functionHash (merge-hash (mapcar (lambda (DVKey) (cons DVKey (function-for DVKey mergedHash))) DVKeys))
				      :necessariesHash (merge-hash (mapcar (lambda (DVKey) (cons DVKey (necessaries DVKey mergedHash))) DVKeys)))
   :work-instance (make-instance 'hpc-work-class)
   :process-output-str-instance (make-instance 'hpc-process-output-str-class)))
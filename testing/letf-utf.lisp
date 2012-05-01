(defmacro! with-test-cleanup-default (&body body)
  `(let ((,g!configFilePathOrig) 
         (,g!workFilePathOrig))
     (with-pandoric (configFilePath workFilePath) 'args
       (setf ,g!configFilePathOrig configFilePath)
       (setf ,g!workFilePathOrig workFilePath))
     (unwind-protect (progn ,@body)
       (with-pandoric (configFilePath workFilePath) 'args
         (setf configFilePath ,g!configFilePathOrig)
         (setf workFilePath ,g!workFilePathOrig))
       (args))))

(defun make-test-session-object-builder (session-builder-fn)
  (lambda (configFileStr workFileStr)
    "Takes strings for a config file and work file, and builds a session object"
    (let ((out))
      (with-pandoric (configFilePath workFilePath) 'args
                     (setf configFilePath configFileStr)
                     (setf workFilePath workFileStr)
                     ; mock up the file-string function & set the configFilePath/workFilePath to the values normally returned by 'file-string
                     ; (instead of the path) so that files aren't necessary to build a mm-session object
                     (with-shadow (file-string #'identity)
                                  (args)
                                  (setf out (funcall session-builder-fn))))
      out)))

(setf *clean-exit-on-error* nil)

(defun compose (&rest fns)
  "Paul Graham's compose, from 'OnLisp'
  The compose function returns a closure that calls each of the functions from last to first, passing on the result of each function call to the next.
  For example, (compose #'list #'round #'sqrt) is equivalent to (lambda (x) (list (round (sqrt x))))"
  (if fns
    (let ((fn1 (car (last fns)))
          (fns (butlast fns)))
      #'(lambda (&rest args)
          (reduce #'funcall fns
                  :from-end t
                  :initial-value (apply fn1 args))))
    #'identity))

(defmacro make-default-model (IVs DVs)
  `(lambda (&key ,@(mapcar #'list IVs))
     (declare (ignore ,@IVs))
     ,@(mapcar (lambda (DV) `(format t "~a=0~%" ',DV)) DVs)))

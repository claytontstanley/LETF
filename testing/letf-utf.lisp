(defmacro! with-test-cleanup-default (&body body)
  `(let ((,g!configFilePathOrig) 
         (,g!workFilePathOrig)
         (,g!ret))
     (with-pandoric (configFilePath workFilePath) 'args
       (setf ,g!configFilePathOrig configFilePath)
       (setf ,g!workFilePathOrig workFilePath))
     (setf ,g!ret (progn ,@body))
     (with-pandoric (configFilePath workFilePath) 'args
       (setf configFilePath ,g!configFilePathOrig)
       (setf workFilePath ,g!workFilePathOrig))
     (args)
     ,g!ret))

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

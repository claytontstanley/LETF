(setf *clean-exit-on-error* nil)

(defmacro with-test-cleanup (&body body)
  `(with-test-cleanup-default ,@body))

(defmacro deftest-hpc (name parameters &body body)
  `(deftest ,name ,parameters
     (with-test-cleanup
       ,@body)))

(defun make-test-session-object (&key (configFileStr) (workFileStr))
  (funcall (make-test-session-object-builder #'build-hpc-session) configFileStr workFileStr))

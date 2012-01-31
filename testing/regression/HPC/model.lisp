(defun run-the-loads (platform)
  ;load act-r
  (load (format nil "~a/actr6/load-act-r-6.lisp" platform))
  ;load the lisp file with the 'run-model entry function
  (load "modelShortCircuited.lisp"))

;/////////////////////////////////////////////
;you should not need to change the stuff below
;/////////////////////////////////////////////
(defmacro mklst (item)
  `(if ,item (if (not (listp ,item)) (setf ,item (list ,item)))))

(defmacro push-to-end (item place)
  `(setf ,place (nconc ,place (list ,item))))

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro awhen (test-form &body body)
  `(aif ,test-form
     (progn ,@body)))

(defun my-command-line ()
  (or 
   #+SBCL *posix-argv*
   #+LISPWORKS system:*line-arguments-list*
   #+CMU extensions:*command-line-strings*
   nil))

(defun get-arg ( from-right )
	(incf from-right)
	(nth (- (length (my-command-line)) from-right ) (my-command-line)))

(defun print-hash (hash)
  (loop for value being the hash-values of hash
     using (hash-key key)
     do (format t "~&~A -> ~A" key value)))

;returns lst of indeces where any of the characters in list chr is in string strng
(defun find-in-string (strng chr)
  (labels ((chars= (chr lst)
	     (dolist (item lst nil)
	       (if (equal item chr) (return-from chars= t)))))
    (mklst chr)
    (if strng (setf strng (string strng)))
    (let ((out nil))
      (dotimes (i (length strng) out)
	(if (chars= (char strng i) chr) 
	    (push-to-end i out))))))

;this function takes a string as input, and outputs a list with every line 
;in the string as an item in the list; analogous to 'cellstr in matlab
(defun get-lines (str &optional (lineDesignators (list #\Newline #\Return #\LineFeed)))
  (mklst lineDesignators)
  (assert lineDesignators)
  ;iterative routine; I chose not to use tail recursion due to speed/memory issues
  ;if there isn't a newline character at the end of the string, then add one
  (if (> (length str) 0) 
      (if (not (find-in-string (char str (- (length str) 1)) lineDesignators))
	  (setf str (concatenate 'string str (string (first lineDesignators))))))
  (let ((newLines (find-in-string str lineDesignators))
	(out nil))
    ;go through each line in the string, and push the line onto out
    (dotimes (i (length newLines) out)
      (let* ((newLinePast (if (equal i 0) -1 (nth (- i 1) newLines)))
	     (newLineCurrent (nth i newLines))
	     (curLine (subseq str (+ 1 newLinePast) newLineCurrent)))
	(unless (equal (length curLine) 0)
	  (push-to-end (format nil "~a" curLine) out))))))

;returns a list of the words in str
(defun get-words (str &optional (spaceDesignators (list #\Space #\Tab)))
  (let ((out))
    (mklst spaceDesignators)
    (assert spaceDesignators)
    (dolist (line (get-lines str) out)
      (let ((start 0)
	    (end 0)
	    (in-the-white t))
	(dotimes (i (length line) (if (not in-the-white) (push-to-end (subseq line start (length line)) out)))
	  (if (find-in-string (char line i) spaceDesignators)
	      (progn
		(setf end i)
		(if (not in-the-white)
		    (progn
		      (setf in-the-white t)
		      (push-to-end (subseq line start end) out)))
		(setf start i))
	      (progn
		(setf end i)
		(if in-the-white
		    (progn
		      (setf start i)
		      (setf in-the-white nil))))))))))

(defun get-word (str &optional (spaceDesignators (list #\Space #\Tab)))
  (mklst spaceDesignators)
  (assert spaceDesignators)
  (aif (get-words str spaceDesignators)
       (progn
	 (assert (equal (length it) 1)
		 nil "~d words in string; only 1 word allowed" (length it))
	 (first it))
       it))

;takes a list of strings, and returns a single string with single whitespaces between each word
(defun make-sentence (lst)
  (if lst
      (let ((out (nth 0 lst)))
	(dotimes (i (- (length lst) 1) out)
	  (setf out (concatenate 'string out " " (nth (+ 1 i) lst)))))))

(defmacro while (test &body body)
  `(do ()
       ((not ,test)) 
     ,@body))

(defun get-objects (str)
  "returns a list of the lisp objects in str; an object is anything that can be evaled by the lisp reader, e.g., 5, or (+ 3 2) or (+ (+ 1) 2)"
  (let ((startIndex 0)
	(out))
    (while (< startIndex (length str))
       (multiple-value-bind (it index) (read-from-string str nil nil :start startIndex :preserve-whitespace t)
	 (if it (push-to-end (subseq str startIndex index) out))
	 (setf startIndex index)))
    out))

(defparameter *input-lines* 
  (with-output-to-string (out)
    (while (listen *standard-input*)
      (write-string (string (read-char *standard-input*)) out))))



(map 'string #'(lambda (c) (print c)) *input-lines*)

(let ((IVs (list "t1" "t2" "blend-temperature")))
  (run-the-loads "darwin")

  (setf *not-sending-some-dvs-p* t)

  (dolist (inputLine (get-lines *input-lines*))
    (let ((tbl (make-hash-table :test 'equalp))
	  (i -1))
      (mapc 
       #'(lambda (str)
	   (incf i)
	   (setf (gethash (nth i IVs) tbl) 
		 (eval (read-from-string str))))
       (get-objects inputLine))
      (run-model tbl))))

(quit)
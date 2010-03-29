(setf *read-default-float-format* 'double-float) ;time to get serious...

; This function returns the command line parameters, accounting for differences across several
; lisp implementations.
(defun my-command-line ()
  (or 
   #+SBCL *posix-argv*
   #+LISPWORKS system:*line-arguments-list*
   #+CMU extensions:*command-line-strings*
   nil))

; This function returns the nth command line argument from right to left (note that this is the
; reverse of normal). 
(defun get-arg ( from-right )
	(incf from-right)
	(nth (- (length (my-command-line)) from-right ) (my-command-line)))

(defmacro push-to-end (item place)
  `(setf ,place (nconc ,place (list ,item))))
	    
(defmacro while (test &body body)
  `(do ()
       ((not ,test)) 
     ,@body))

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro mklst (item)
  `(if ,item (if (not (listp ,item)) (setf ,item (list ,item)))))

(defun meListp (lst)
  (if lst (listp lst)))

(defun flatten (lis)
  "Takes a nested list and makes in into a single-level list"
  (declare (list lis))
  (labels ((rec (lis acc)
             (cond ((null lis) acc)
                   ((atom lis) (cons lis acc))
                   (t (rec (car lis) (rec (cdr lis) acc))))))
    (rec lis nil)))

(defun all-equal (lst &key (test 'equal))
  (mklst lst)
  (dotimes (i (- (length lst) 1) t)
    (if (not (funcall test (nth i lst) (nth (+ 1 i) lst)))
	(return-from all-equal nil))))

(defun transpose (lst-source)
  (let ((lst (copy-tree lst-source)))
    (if (and lst (melistp lst) (melistp (car lst)))
	(let ((templst) (out))
	  (assert (all-equal (mapcar #'length lst)))
	  (while (if lst (car lst))
	    (setf templst nil)
	    (dotimes (i (length lst))
	      (push-to-end (car (nth i lst)) templst)
	      (setf (nth i lst) (cdr (nth i lst))))
	    (push-to-end templst out))
	  out)
	lst)))

(defun collapse (lst collapseFn)
  (if (not collapseFn)
      lst
      (if (and lst (melistp lst) (melistp (car lst)))
	  (let ((out))
	    (dolist (column (transpose lst) out)
	      (push-to-end (funcall collapseFn column) out)))
	  (funcall collapseFn lst))))
	  
(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part 
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos))) 

;returns lst of indeces where any of the characters in list chr is in string strng
(defun find-in-string (strng chr)
  (labels ((chars= (chr lst)
	     (dolist (item lst nil)
	       (if (equal item chr) (return-from chars= t)))))
    (mklst chr)
    (if strng (setf strng (format nil "~a" strng)))
    (let ((out nil))
      (dotimes (i (length strng) out)
	(if (chars= (char strng i) chr) 
	    (push-to-end i out))))))

(defun print-hash (hash)
  (labels ((hash-string (hash)
	     (if (not (hash-table-p hash))
		 (format nil "~a" hash)
		 (let ((out))
		   (loop for value being the hash-values of hash
		      using (hash-key key)
		      do
			(setf out (concatenate 
				   'string out (format nil "~%~a -> ~a" key (hash-string value)))))
		   out))))
    (format t "~a~%" (string-trim (list #\Newline #\Return #\LineFeed) (hash-string hash)))))

(defun copy-hash (hash)
  (if (hash-table-p hash)
      (let ((out (make-hash-table :test 'equalp)))
	(loop for value being the hash-values of hash
	   using (hash-key key)
	   do (setf (gethash key out) (copy-hash value)))
	out)
      hash))

(defun key-present (key hash)
  (multiple-value-bind (value flag) (gethash key hash)
    (declare (ignore value))
    flag))

(defun file-string (path)
  "Sucks up an entire file from PATH into a freshly-allocated string,
      returning two values: the string and the number of bytes read."
  (with-open-file (s path)
    (let* ((len (file-length s))
           (data (make-string len)))
      (values data (read-sequence data s)))))

(defun throwOutYerNils (&rest lsts)
  (let ((out))
    (if (meListp (car lsts))
	(dolist (column (transpose lsts) (setf out (transpose out)))
	  (if (not (member nil column))
	      (push-to-end column out)))
	(if (not (member nil lsts)) (setf out lsts)))
    (apply 'values (if out out (make-sequence 'list (length lsts) :initial-element nil)))))


(defmacro inLST (left right fName throwOutYerNils)
  `(if ,left (if (melistp (car ,left))
		 (let ((out))
		   (dolist (itm ,left)
		     (push-to-end (,fName itm ,right :throwOutYerNils ,throwOutYerNils) out))
		   (return-from ,fName (flatten out))))))

(defmacro inLSTs (left right fName throwOutYerNils)
  `(progn
     (inLST ,left ,right ,fName ,throwOutYerNils)
     (inLST ,right ,left ,fName ,throwOutYerNils)))

(defmacro assertEqualLengths (l1 l2)
  `(assert (equal (length ,l1) (length ,l2))
	   nil "length ~d not equal to length ~d" (length ,l1) (length ,l2)))
  
(defun MAD (l1 l2 &key (throwOutYerNils nil))
  (inLSTs l1 l2 MAD throwOutYerNils)
  (assertEqualLengths l1 l2)
  (if throwOutYerNils (multiple-value-setq (l1 l2) (throwOutYerNils l1 l2)))
  (if l1 (/ (apply '+ (mapcar (lambda (x y) (abs (- x y))) l1 l2)) (length l1))))

(defun correl (l1 l2 &key (throwOutYerNils nil))
  (inLSTs l1 l2 correl throwOutYerNils)
  (labels ((std (lst)
	     (assert (> (length lst) 1)
		     nil "must have at least 2 numbers to calculation std; only supplied ~d" (length lst))
	     (sqrt
	      (/
	       (apply '+ (funcall #'(lambda (x) (mapcar (lambda (y) (* (- y x) (- y x))) lst))
				  (/ (apply '+ lst) (length lst))))
	       (- (length lst) 1)))))
    (assertEqualLengths l1 l2)
    (ignore-errors
      (if throwOutYerNils (multiple-value-setq (l1 l2) (throwOutYerNils l1 l2)))
      (let ((out
	     (if (> (length l1) 1)
		 (/
		  (apply '+
			 (mapcar (lambda (x y) (* x y))
				 (funcall #'(lambda (x)
					      (mapcar (lambda (y) (- y x)) l1))
					  (/ (apply '+ l1) (length l1)))
				 (funcall #'(lambda (x)
					      (mapcar (lambda (y) (- y x)) l2))
					  (/ (apply '+ l2) (length l2)))))
		  (* (- (length l1) 1) (std l1) (std l2))))))
	(if out (if (not (or (> 0 out) (< 0 out) (equal 0 out))) (setf out nil)))
	out))))

(defun RMSE (l1 l2 &key (throwOutYerNils nil))
  (inLSTs l1 l2 RMSE throwOutYerNils)
  (assertEqualLengths l1 l2)
  (if throwOutYerNils (multiple-value-setq (l1 l2) (throwOutYerNils l1 l2)))
  (if l1 (sqrt (/ (apply '+ (mapcar (lambda (x y) (* (- x y) (- x y))) l1 l2)) (length l1)))))

(defun median (lst &key (throwOutYerNils nil))
  (mklst lst)
  (if throwOutYerNils (setf lst (throwOutYerNils lst)))
  (if lst
      (let ((len (length lst))
	    (sortedList (sort lst #'<)))
	(if (evenp len)
	    (/ (+ (nth (- (/ len 2) 1) sortedList)
		  (nth (/ len 2) sortedList))
	       2)
	    (nth (/ (- len 1) 2) sortedList)))))

(defun sum (lst &key (throwOutYerNils nil))
  (mklst lst)
  (if throwOutYerNils (setf lst (throwOutYerNils lst)))
  (if lst (apply '+ lst)))

(defun mean (lst &key (throwOutYerNils nil))
  (mklst lst)
  (if throwOutYerNils (setf lst (throwOutYerNils lst)))
  (if lst (/ (sum lst) (length lst))))

;returns a list of the words in str
(defun get-words (str &key (spaceDesignators (list #\Space #\Tab)) (includeSpaceDesignators nil))
  (if str (setf str (string-trim (list #\Space #\Tab) str))) ;yes, these should be hardcoded to space and tab
  (let ((out) (start) (in-the-white))
    (mklst spaceDesignators)
    (assert spaceDesignators)
    (if (equal (length str) 0) (return-from get-words nil))
    (setf start 0)
    (setf in-the-white (find-in-string (char str 0) spaceDesignators))
    (dotimes (i (length str) (if (not in-the-white) (push-to-end (subseq str start (length str)) out) out))
      (if (find-in-string (char str i) spaceDesignators)
	  (progn
	    (if (not in-the-white)
		(progn
		  (push-to-end (subseq str start i) out)
		  (setf in-the-white t)
		  (setf start i)))
	    (if includeSpaceDesignators (push-to-end (format nil "~a" (char str i)) out)))
	(progn
	  (if in-the-white
	      (progn
		(setf in-the-white nil)
		(setf start i))))))))

(defun get-word (str &key (spaceDesignators (list #\Space #\Tab)) (includeSpaceDesignators nil))
  (mklst spaceDesignators)
  (assert spaceDesignators)
  (aif (get-words str :spaceDesignators spaceDesignators :includeSpaceDesignators includeSpaceDesignators)
       (progn
	 (assert (equal (length it) 1) nil "~d words in string; only 1 word allowed" (length it))
	 (first it))))

(defun get-lines (str &key (lineDesignators (list #\Newline #\Return #\LineFeed)) (includeLineDesignators nil))
  (let ((out))
    (dolist (line (get-words str :spaceDesignators lineDesignators :includeSpaceDesignators includeLineDesignators) (reverse out))
      (push (string-trim (list #\Space #\Tab) line) out))))

;takes a list of strings, and returns a single string with single whitespaces between each word
(defun make-sentence (lst &key (spaceDesignator #\Space))
  (setf spaceDesignator (format nil "~a" spaceDesignator))
  (mklst lst)
  (if lst
      (let ((out))
	(dotimes (i (length lst) out)
	  (if (> (length (nth i lst)) 0)
	      (setf out (concatenate 'string out (format nil "~a" (if out spaceDesignator "")) (nth i lst))))))))

;like get-words, returns a list of the words in str
;however, here all words that are within brackets are lumped together as one word (i.e., item) in the list
;if there are no brackets in str, then lump-brackets is equivalent to get-words
(defun lump-brackets (str &key (desigs (cons #\[ #\])) (include-brackets t))
  (if (not desigs) (return-from lump-brackets (get-words str)))
  (assert (equal (length (flatten desigs)) 2))
  (let ((out) (in-bracket) (lump))
    (dolist (word (get-words str :spaceDesignators (flatten desigs) :includeSpaceDesignators t))
      (if (not in-bracket) (setf lump nil))
      (if (find-in-string (format nil "~a~a" (car desigs) (cdr desigs)) (char word 0))
	  (progn
	    (assert (find-in-string (format nil "~a" (if in-bracket (cdr desigs) (car desigs))) (char word 0))
		    nil "something is wrong with string ~a, possibly one bracket pair is nested within another bracket pair, which is not allowed" str)
	    (setf in-bracket (not in-bracket))
	    (if include-brackets (push-to-end word lump)))
	  (if in-bracket (push-to-end word lump)))
      (if (not in-bracket) (push-to-end (if lump (make-sentence lump :spaceDesignator "") (get-words word)) out)))
    (assert (not in-bracket) nil "something is wrong with string ~a, possibly a stray bracket somewhere" str)
    (flatten out)))

(defun remap-string (str hash &key (lambdas nil) (collapseFn "'mean") (inside-brackets nil) (key nil))
  (if inside-brackets
      ;remaps an expression surrounded by brackets by calling the hash table on each of the words in the expression
      ;each word in the expression should be a key that corresponds to an already-defined element in the hash table
      ;you can also supply a list of lambda functions to be evaluated, that take, as input, the current word in the 
      ;expression and the hash table; useful if you want to collect something other than what is returned by this 
      ;function see 'necessaries' or 'get-elements', or 'eval-hash' for examples 
      (let ((out) (shortIt!) (val))
	(if lambdas (if (not (melistp (car lambdas))) (setf lambdas (list lambdas))))
	(dolist (word (get-words str) (make-sentence out))
	  (setf shortIt! nil)
	  (dolist (lm lambdas) 
	    (if (equalp (car lm) "pre") 
		(if (equalp (funcall (cdr lm) word hash (gethash word hash)) "shortIt!") 
		    (setf shortIt! t)))) 
	  (setf val (if shortIt! (gethash word hash) 
			(remap-string (gethash word hash) hash 
				      :lambdas lambdas :collapseFn collapseFn 
				      :inside-brackets nil :key word)))
	  (setf shortIt! nil)
	  (dolist (lm lambdas)
	    (if (equalp (car lm) "post")
		(if (equalp (funcall (cdr lm) word hash val) "shortIt!")
		    (setf shortIt! t))))
	  (push-to-end (if shortIt! (gethash word hash) val) out)))
      ;remaps an expression, which may or may not have parts that are surrounded by brackets
      ;for each part that is surrounded by brackets, call remap-string with inside-brackets flagged to convert
      ;the bracketed expression to actual values
      (let ((out))
	(if (meListp str)
	    (progn
	      (dolist (item str)
		(push-to-end 
		 (remap-string item hash :lambdas lambdas :collapseFn collapseFn :inside-brackets nil :key nil)
		 out))
	      (setf out (make-sentence 
			 (append (list "(funcall 'collapse" "(list ") 
				 out 
				 (list 
				  ")" 
				  (make-sentence (if (hash-table-p collapseFn) 
						     (gethash key collapseFn) 
						     collapseFn))
				  ")")))))
	    (progn
	      (dolist (word (lump-brackets str))
		(push-to-end
		 (if (and (equal (char word 0) #\[) (equal (char word (- (length word) 1)) #\]))
		     (remap-string (string-trim "[]" word) hash :lambdas lambdas 
				   :collapseFn collapseFn :inside-brackets t :key nil)
		     word)
		 out))
	      (setf out (make-sentence out))))
	out)))

(defun traverse (keys hash &key (bool nil))
  (mklst keys)
  (let ((out) (words) (str)
	(traversed (make-hash-table :test #'equalp)))
    (dolist (key keys)
      (setf words nil)
      (setf str (remap-string
		 (concatenate 'string "[" key "]")
		 hash
		 :lambdas ;gotta love side effect
		 (cons "pre" 
		       #'(lambda (word hash val)
			   (declare (ignore val))
			   (if (key-present word traversed)
			       "shortIt!"
			       (progn
				 (setf (gethash word traversed) t)
				 (if (equal (key-present word hash) bool) (push-to-end word words))))))))
      (push-to-end words out))
    (sort (remove-duplicates (flatten out) :test #'equalp) 'string<)))

;returns the subset of keys that are needed to 'remap' the keys in 'keys' 
;that do not currently have values associated with them
(defun necessaries (keys hash)
  (traverse keys hash :bool nil))

;returns the subset of keys that are needed to 'remap' the keys in 'keys' 
;that already have values associated with them
(defun availables (keys hash)
  (traverse keys hash :bool t))

;evaluates all the stuff in the hash table that it can, given the current state of the hash table
(defmethod eval-hash ((hash hash-table))
  (labels ((toString (lst)
	     (let ((out))
	       (if (melistp lst)
		   (progn
		     (push-to-end "(list" out)
		     (dolist (item lst)
		       (push-to-end (toString item) out))
		     (push-to-end ")" out))
		   (push-to-end (format nil "~a" lst) out))
	       (make-sentence (flatten out)))))
    (let ((traversed (make-hash-table :test 'equalp)))
      (loop for key being the hash-keys of hash
	 do (if (and (not (key-present key traversed)) (not (necessaries key hash)))
		(remap-string
		 (concatenate 'string "[" key "]") hash
		 :lambdas
		 (cons "post"
		       #'(lambda (word hash val)
			   (if (not (key-present word traversed))
			       (let ((newVal))
				 (setf (gethash word traversed) t)
				 (ignore-errors
				   (multiple-value-bind (evaledVal lngth) (read-from-string val)
				     (if (equal lngth (length val))
					 (setf newVal (toString (eval evaledVal))))))
				 (if (and newVal 
					  (not (equal (- (length val) 
							 (length (find-in-string val (list #\space #\tab))))
						      (- (length newVal) 
							 (length (find-in-string newVal (list #\Space #\tab)))))))
				     (progn
				      ;(format t "~a -> ~a -> ~a~%" (gethash word hash) val newVal)
				       (setf (gethash word hash) newVal)
				       "shortIt!"))))))))))))

(defun bracket-expand (str &optional (inside-brackets nil))
  (labels ((num-indeces (str direction)
	     (assert (or (equalp direction "fromLeft") (equalp direction "fromRight") (equalp direction "both")))
	     (let ((out) (numIndeces) (index -1))
	       (map 'string #'(lambda (x) 
				(incf index)
				(if (numberp (read-from-string (format nil "~a" x)))
				    (push index numIndeces))
				x) str)
	       (if (equalp direction "both") (return-from num-indeces (reverse numIndeces))) 
	       (if (equalp direction "fromLeft") (setf numIndeces (reverse numIndeces)))
	       (setf index (if (equalp direction "fromLeft") 0 (- (length str) 1)))
	       (while (if numIndeces (equal (car numIndeces) index))
		 (push index out)
		 (if (equalp direction "fromLeft") (incf index) (decf index))
		 (setf numindeces (cdr numIndeces)))
	       (if (equalp direction "fromLeft") (reverse out) out))))
    (let ((out))
      (if (> (length str) 0)
	  (if (not inside-brackets)
	      (dolist (word (lump-brackets str))
		(push-to-end
		 (if (and (equal (char word 0) #\[) (equal (char word (- (length word) 1)) #\]))
		     (concatenate 'string "[" (bracket-expand (string-trim "[]" word) t) "]")
		     word)
		 out))
	      (if (not (find-in-string str #\:))
		  (push-to-end str out)
		  (let ((colon-words (get-words str :spaceDesignators #\:))
			(prev) (next) (wordPrev) (numPrev) (wordNext) (numNext) (cit))
		    (dotimes (i (length (find-in-string str #\:)) (push-to-end (rest cit) out))
		      (setf cit (get-words (nth i colon-words)))
		      (setf prev (car (last cit)))						    
		      (push-to-end (if (equal i 0) (butlast cit) (rest (butlast cit))) out)
		      (setf cit (get-words (nth (+ 1 i) colon-words)))
		      (setf next (first cit))
		      (setf wordPrev (subseq prev 0 (first (num-indeces prev "fromRight"))))
		      (setf numPrev (eval (read-from-string 
					   (subseq prev (first (num-indeces prev "fromRight")) (length prev)))))
		      (setf numNext (eval (read-from-string 
					   (subseq next 0 (+ 1 (car (last (num-indeces next "fromLeft"))))))))
		      (setf wordNext (subseq next (+ 1 (car (last (num-indeces next "fromLeft")))) (length next)))
		      (dotimes (j (+ 1 (- numNext numPrev)))
			(push-to-end 
			 (concatenate 'string wordPrev (format nil "~a" (+ numPrev j)) wordNext) 
			 out)))))))
      (make-sentence (flatten out)))))
  
;returns a list of RHS's of lines that start with 'key'
;where 'key' is any key in 'keys'
;the list will be ordered from left to right in the string
(defun get-matching-lines (str keys)
  (labels ((index= (words keys)
	     (let* ((word (first words))
		    (wordLength (length word)))
	       (dolist (key keys nil)
		 (if (equalp key (subseq word 0 (min wordLength (length key))))
		     (return-from index= (length key)))))))
    (mklst keys)
    (let ((words) (out))
      (dolist (line (if (melistp str) str (get-lines str)) out)
	(setf words (if (melistp line) line (get-words line)))
	(if words
	    (aif (index= words keys) 
		 (push-to-end 
		  (bracket-expand 
		   (string-trim 
		    '(#\Space #\tab) 
		    (subseq (make-sentence line) it (length (make-sentence line))))) 
		  out)))))))

(defun get-matching-line (str keys)
  (mklst keys)
  (aif (get-matching-lines str keys)
       (progn
	 (assert (equal (length it) 1) 
		 nil "~d matching lines with lhs=~a; only 1 line allowed" 
		 (length it)
		 (make-sentence keys))
	 (first it))))
      	  
;returns the list of elements (key . value) from the hash table 'hash' specified by 'keys'
;will evaluate each value before putting it in the list
(defun get-elements (keys hash &optional (collapseFn "'mean"))
  (mklst keys)
  (let ((out))
    (dolist (key keys out)
      (let ((val (remap-string
		  (concatenate 'string "[" key "]") hash
		  :lambdas (cons 
			    "pre" 
			    #'(lambda (word hash val)
				(declare (ignore val))
				(assert (key-present word hash) nil "key ~a not currently present in hash table" word)))
		  :collapseFn collapseFn)))
	;(format t "~a~%" val)
	(push-to-end (cons key (eval (read-from-string val))) out)))))
	    
;adds a list of elements (key . value) to the hash table 'hash' specified by 'keys'
;will not evaluate each value before putting it in the hash table
(defmethod add-elements ((hash hash-table) &optional (elements nil))
  (if elements (if (not (meListp (car elements))) (setf elements (list elements))))
  (dolist (element elements)
    (if (key-present (car element) hash)
	(assert (equalp (gethash (car element) hash) (cdr element))
		nil "'~a' key already present in hash table with value '~a', which is different than the value '~a' that you're trying to add now"
		(car element) (gethash (car element) hash) (cdr element))
	(setf (gethash (car element) hash) (cdr element)))))

;returns a list of keys (in order) that map to the values found in each line of the work file
(defun get-cell-keys (str)
  (let ((lines) (words) (out))
    (setf lines (get-matching-lines str (list "constant=" "iv=")))
    (dolist (line lines out)
      (setf words (get-words line))
      (assert (> (length words) 0)
	      nil "no rhs for lhs=(either contant= or iv=) in config file")
      (push-to-end (first words) out))))

;adds all cell elements for a single line in the work file to the hash table
(defmethod add-cell-elements ((hash hash-table) &optional (configFileStr nil) (cell-values nil))
  (let ((cell-keys (get-cell-keys configFileStr)))
    (assert (equal (length cell-keys) (length cell-values))
	    nil "number of cell keys (~d) does not equal number of cell values (~d)"
	    (length cell-keys) (length cell-values))
    (add-elements hash (mapcar #'cons cell-keys cell-values))))

(defmethod add-dependent-element ((hash hash-table) &optional (configFileStr nil) (lhs nil) (lambdas nil))
  (mklst lambdas)
  (let ((line) (words) (shortIt!))
    (setf line (if (melistp lhs) (cdr lhs) (get-matching-line configFileStr lhs)))
    (setf lhs (if (melistP lhs) (car lhs) lhs))
    (if line
	(progn
	  (add-elements hash (cons (subseq lhs 0 (- (length lhs) 1)) line))
	  (setf words (lump-brackets line))
	  (dolist (word words)
	    (if (and (equal (char word 0) #\[) (equal (char word (- (length word) 1)) #\]))
		(dolist (item (get-words (string-trim "[]" word)))
		  (setf shortIt! nil)
		  (dolist (lm lambdas)
		    (if (equalp (funcall lm item) "shortIt!") (setf shortIt! t)))
		  (if (not shortIT!)
		      (add-dependent-element hash configFileStr (concatenate 'string item "=") lambdas)))))))))

(defmethod add-dependent-elements ((hash hash-table) &optional (configFileStr nil) (lhs nil))
  (let ((lines) (words) (keys) (traversed (make-hash-table :test 'equalp)))
    (setf lines (get-matching-lines configFileStr lhs))
    (dolist (line lines)
      (setf words (get-words line))
      (assert words nil "no rhs for lhs=~a in config file" lhs)
      (push-to-end (first words) keys))
    (dolist (key keys)
      (add-dependent-element 
       hash 
       configFileStr 
       (concatenate 'string key "=")
       #'(lambda (word) 
	   (if (key-present word traversed) "shortIt!" (setf (gethash word traversed) t)))))))

(defclass work-class ()
  ((lines :accessor lines :initarg :lines :initform nil)))

(defclass session-class ()
  ((runs :accessor runs :initarg :runs :initform nil)))

(defclass process-output-str-class ()
  ((str :accessor str :initarg :str :initform nil)
   (quot :accessor quot :initarg :quot :initform 0)
   (quota :accessor quota :initarg :quota :initform 200)))

(defmethod collect ((obj process-output-str-class) str)
  (push-to-end str (str obj))
  (if (equal (quot obj) (quota obj))
      (setf (str obj) (rest (str obj)))
      (incf (quot obj))))

(defclass runProcess-class ()
  ((runs :accessor runs :initarg :runs :initform nil)
   (process :accessor process :initarg :process :initform nil)
   (modelProgram :accessor modelProgram :initarg :modelProgram :initform nil)
   (platform :accessor platform :initarg :platform :initform nil)
   (process-output-str :accessor process-output-str :initarg :process-output-str :initform (make-instance 'process-output-str-class))))

(defclass number5-runProcess-class (runProcess-class)
  ((sleepTime :accessor sleepTime :initarg :sleepTime :initform .2)))

(defclass johnny5-runProcess-class (runProcess-class)
  ((sleepTime :accessor sleepTime :initarg :sleepTime :initform 0)))

(defclass run-class ()
  ((IVHash :accessor IVHash :initarg :IVHash :initform nil)
   (DVHash :accessor DVHash :initarg :DVHash :initform nil)
   (IVKeys :accessor IVKeys :initarg :IVKeys :initform nil)
   (DVKeys :accessor DVKeys :initarg :DVKeys :initform nil)
   (cellKeys :accessor cellKeys :initarg :cellKeys :initform nil)
   (sleepTime :accessor sleepTime :initarg :sleepTime :initform 0)
   (collector :accessor collector :initarg :collector :initform nil)
   (runProcess :accessor runProcess :initarg :runProcess :initform nil)))

(defclass number5-run-class (run-class)
  ())

(defclass johnny5-run-class (run-class)
  ())

(defclass collector-class ()
  ((quota :accessor quota :initarg :quota :initform 1)
   (quot :accessor quot :initarg :quot :initform 0)
   (collection :accessor collection :initarg :collection :initform (make-hash-table :test 'equalp))
   (collapseHash :accessor collapseHash :initarg :collapseHash :initform nil)
   (keys :accessor keys :initarg :keys :initform nil)))
   
(defmethod print-collector :after ((obj collector-class))
  (setf (collection obj) nil))
  
(defmethod collect ((obj collector-class) (lst list))
  (dolist (item lst)
    (push-to-end
     (format nil "~a" (cdr item))
     (gethash (car item) (collection obj))))
  ;(format t "~a~%" (quot obj))
  (incf (quot obj))
  (assert (not (> (quot obj) (quota obj)))))

(defmethod collect :after ((obj collector-class) (lst list))
  (declare (ignore lst))
  (if (equal (quota obj) (quot obj))
      ;short-circuit to optimize recursion called later
      ;pass over all of the elements in the hash table, and
      ;if an element is a list and if all slots in that list are equal, then collapse the list
      (progn
	(loop for key being the hash-keys of (collection obj)	
	   using (hash-value val)
	   do (if (and (melistp val) (all-equal val :test #'equalp))
		  (setf (gethash key (collection obj)) (first val))))
	(print-collector obj))))

(defmethod collect ((obj collector-class) (DVHash hash-table))
  (let ((out))
    (maphash #'(lambda (key value) (push-to-end (cons key value) out)) DVHash)
    (collect obj out)))

(defun restructure (str)
  (let ((out) 
	(lineDesigs (list #\Newline #\Return #\LineFeed))
	(count -1)
	(strLength (length str)))
    (while (< (incf count) (- strLength 1))
      (if (if (find-in-string (char str count) #\\)
	      (find-in-string (char str (+ count 1)) lineDesigs))
	  (incf count)
	  (push (char str count) out)))
    (if (equal count (- strLength 1))
	(push (char str count) out))
    (coerce (reverse out) 'string)))

(defmethod mod-collapseHash ((hash hash-table) collapseFn &key (DVKeys nil) (ApplyToKeys nil))
  (labels ((keys (hash)
	     (let ((out))
	       (loop for value being the hash-values of hash
		  using (hash-key key)
		  do (push-to-end key out))
	       out)))
    (dolist (DVKey (aif DVKeys it (keys hash)))
      (assert (key-present DVKey hash) nil "DV=~a not present in collapse hash table" DVKey)
      (dolist (ApplyToKey (aif ApplyToKeys it (keys (gethash DVKey hash))))
	(if (not (key-present ApplyToKey (gethash DVKey hash)))
	    (assert (not DVKeys) nil "ApplyToKey=~a not present in DV=~a collapse hash table" ApplyToKey DVKey)
	    (setf (gethash ApplyToKey (gethash DVKey hash)) collapseFn))))))

;generates the session object from the config and work file string
(defmacro build-session (&key (collector-instance nil) (work-instance nil))
  `(progn
     (labels ((get-IV-hash (configFileStr cell-values)
		(let ((hash (make-hash-table :test 'equalp)))
		  (add-cell-elements hash configFileStr cell-values)
		  (add-dependent-elements hash configFileStr "input=")
		  hash))
	      (get-DV-hash (configFileStr)
		(let ((hash (make-hash-table :test 'equalp)))
       		  ;the analogous 'add-cell-elements for the dvs isn't called here, because these 
		  ;elements will be added during run-time, as the model is producing results
		  (add-dependent-elements hash configFileStr (list "DV=" "SDV="))
		  hash))
	      (get-IV-keys (configFileStr)
		(let ((out))
		  (aif (get-matching-lines configFileStr "input=")
		       (dolist (line it) (push-to-end (get-word line) out))
		       (setf out (get-cell-keys configFileStr)))
		  out))
	      (get-DV-keys (configFileStr)
		(let ((words) (out))
		  (dolist (line (get-matching-lines configFileStr (list "dv=" "sdv=")) out)
		    (setf words (get-words line))
		    (assert (> (length words) 0)
			    nil "no rhs on lhs=dv=; at least 1 word required")
		    (push-to-end (first words) out))))
	      (get-collapseHash (DVKeys DVHash configFileStr defaultCollapseFn)
		(let ((hash (make-hash-table :test 'equalp))
		      (tmpHash) (hashLST) (words) (tmpLn) (tmpLST))
		  (setf tmpLST (get-matching-lines configFileStr "collapseFn="))
		  (if (not tmpLST)
		      (return-from get-collapseHash defaultCollapseFn)
		      (if (and (equal (length tmpLST) 1) 
			       (equal (length (get-words (first tmpLST))) 1))
			  (return-from get-collapseHash (get-word (first tmpLST)))))
		  (dolist (DVKey DVKeys (add-elements hash hashLST))
		    (setf tmpHash (make-hash-table :test 'equalp))
		    (add-elements tmpHash (mapcar #'(lambda (x) (cons x defaultCollapseFn))
						  (append (necessaries DVKey DVHash) (availables DVKey DVHash))))
		    (push-to-end (cons DVKey tmpHash) hashLST))
		  (dolist (line (get-matching-lines configFileStr "collapseFn=") hash)
		    (setf words (get-words line :spaceDesignators (list #\; #\&)))
		    (setf tmpLn (make-sentence (rest words) :spaceDesignator #\Newline))
		    (assert (equal (length (rest words)) 
				   (length (get-matching-lines tmpLn (list "DV=" "SDV=" "ApplyTo="))))
			    nil "line ~a not valid" line)
		    (mod-collapseHash 
		     hash
		     (let ((hsh (make-hash-table :test 'equalp)))
		       (add-dependent-element hsh configFileStr (cons "collapseFn=" (first words)))
		       (remap-string (concatenate 'string "[" "collapseFn" "]") hsh))
		     :DVKeys (get-words (bracket-expand (get-matching-line tmpLn (list "DV=" "SDV=")) t))
		     :ApplyToKeys (get-words (bracket-expand (get-matching-line tmpLn "ApplyTo=") t)))))))
       (let ((session) (runProcess) (run) (lines) (line-index) (count) (iteration) (iterations)
	     (DVHash) (IVHash) (DVKeys) (IVKeys) (modelProgram) (cellKeys)
	     (quota) (collector) (quot) (collapseHash) (work)
	     (configFilePath (get-arg 1)) (workFilePath (get-arg 0)) (platform (get-word (get-arg 2)))
	     (configFileStr) (workFileStr) (runsPerProcess) (short-circuit-p) (configFileLnLST) (configFileWdLST))
	 (setf configFileStr (restructure (file-string configFilePath)))
	 (setf configFileLnLST (get-lines configFileStr))
	 (setf configFileWdLST (mapcar #'get-words configFileLnLST))
	 (setf workFileStr (file-string workFilePath))
	 (setf work ,work-instance)
	 (setf runsPerProcess (aif (get-matching-line configFileWdLST "runsPerProcess=")
				   (read-from-string (get-word it))
				   1))
	 (setf session (make-instance 'session-class))
	 (setf DVHash (get-dv-hash configFileWdLST))
	 (eval-hash DVHash)
	 (setf DVKeys (get-dv-keys configFileWdLST)
	       IVKeys (get-iv-keys configFileWdLST)
	       cellKeys (get-cell-keys configFileWdLST))
	 (setf collapseHash (get-collapseHash DVKeys DVHash configFileWdLST "'mean"))
	 (setf quota
	       (aif (get-matching-line configFileWdLST "collapseQuota=")
		    (eval (read-from-string (get-word it)))
		    1))
	 (setf modelProgram
	       (aif (get-matching-line configFileLnLST "modelProgram=")
		    (lump-brackets (replace-all it "$1" platform :test #'equalp) :desigs (cons #\" #\") :include-brackets nil)
		    (lump-brackets "'run-model" :desigs (cons #\" #\") :include-brackets nil)))
	 (setf short-circuit-p (or 
				(equal (subseq (first modelProgram) 0 1) "#")
				(equal (subseq (first modelProgram) 0 1) "'")))
	 (if short-circuit-p 
	     (setf 
	      modelProgram (eval (read-from-string (make-sentence modelProgram)))
	      runsPerProcess (read-from-string "inf")))
	 (setf runProcess 
	       (make-instance (if short-circuit-p 'johnny5-runProcess-class 'number5-runProcess-class) 
			      :modelProgram modelProgram :platform platform))
	 (setf iterations  (aif (get-matching-line configFileWdLST "iterations=")
				(eval (read-from-string (get-word it)))
				1)	
	       count 0
	       line-index -1)
	 (setf lines (lines work))
	 (while (< (incf line-index) (length lines))
	   (setf iteration -1)
	   (setf IVHash (get-iv-hash configFileWdLST (nth line-index lines)))
	   (if (equal line-index 0) 
	       (aif (necessaries IVKeys IVHash)
		    (assert nil nil "IVs '~a' that are necessary to evaluate the 'input=' lines are not present in the config file" it)))
	   (while (< (incf iteration) iterations)
	     (setf quot -1)
	     (setf collector ,collector-instance)
	     (while (< (incf quot) quota)
      	       ;build the run object for the current line in the work file
	       (setf run (make-instance 
			  (if short-circuit-p 'johnny5-run-class 'number5-run-class)
			  :IVHash (copy-hash IVHash)
			  :DVHash (copy-hash DVHash)
			  :IVKeys IVKeys
			  :DVKeys DVKeys
			  :cellKeys cellKeys
			  :collector collector
			  :runProcess runProcess))
	       ;push the run object onto the runProcess object
	       (push-to-end run (runs runProcess))
	       (incf count)
       	       ;if it's time to push the runProcess object onto the session object, do it
	       (if (not (equal 'inf runsPerProcess)) 
		   (if (equal (mod count runsPerProcess) 0)
		       (progn
			 (push-to-end runProcess (runs session))
			 (setf runProcess 
			       (make-instance (if short-circuit-p 'johnny5-runProcess-class 'number5-runProcess-class)
					      :modelProgram modelProgram :platform platform))))))))
	 (if (runs runProcess) (push-to-end runProcess (runs session)))
	 (aif (apply '+ (mapcar #'(lambda (x) (length (runs x))) (runs session)))
	      (assert (equal (* (length lines) iterations quota) it)
		      nil "number of linesxiterationsxquota in work file (~d) not equal to number of run objects (~d)"
		      (length lines) it))
	 session))))

;converts an output line of text sent by the model to a dotted pair
;discards if it's not a valid output line (handles when warning statements are printed to stdout)
(defun line2element (line)
  ;(format t "~a~%" line)
  (let ((equal-index) (key) (value))
    (setf equal-index (find-in-string line #\=))
    (if (equal (length equal-index) 1)
	(progn
	  (setf key (get-words (subseq line 0 (first equal-index))))
	  (setf value (get-words (subseq line (+ 1 (first equal-index)) (length line))))
	  (if (equal (length key) 1)
	      (progn
		;(format t "key=~a value=~a~%" (first key) (first value))
		(cons (first key) (make-sentence value))))))))

;capture all the input lines that the model has sent; 
;then, remap each line as a dotted pair (key . value)
(defmethod get-DVs ((obj number5-run-class) &optional (process nil) (appetizers nil)) 
  (assert process)
  (mklst appetizers)
  (let ((currentDVs) (line))
    (while (listen (process-output process))
      (setf line (read-line (process-output process) nil))
      (collect (process-output-str (runProcess obj)) line)
      (aif (line2element line) (push-to-end it currentDVs)))
    (assert (or (append appetizers currentDVs) (equalp (format nil "~a" (process-status process)) "running")) nil 
	    "model process unexpectedly quit... ~%~%here are the last ~a lines of the current process that were printed to stdout before the error~%~a" 
	    (quot (process-output-str (runProcess obj))) 
	    (make-sentence (str (process-output-str (runProcess obj))) :spaceDesignator #\Newline))
    (append appetizers currentDVs)))

(defmethod get-DVs ((obj johnny5-run-class) &optional (process nil) (appetizers nil))
  (mklst appetizers)  
  (let ((currentDVs) (fstr) (error-p)
	(tbl (make-hash-table :test 'equalp)))
    (mapc #'(lambda (x) (setf (gethash (car x) tbl) (cdr x))) (get-elements (IVKeys obj) (IVHash obj)))
    (setf fstr (make-array '(0) :element-type 'base-char
			     :fill-pointer 0 :adjustable t))
    (handler-case (with-output-to-string (*standard-output* fstr) (funcall process tbl))
      (error (condition) (setf error-p condition)))
    (dolist (line (get-lines fstr))
      (collect (process-output-str (runProcess obj)) line)
      (aif (line2element line) (push-to-end it currentDVs)))
    (assert (not error-p) nil
	    "model unexpectedly quit... ~%~%here are the last ~a lines that were printed to stdout before the error~%~a~%~%here's the error~%~a~%" 
	    (quot (process-output-str (runProcess obj))) 	    
	    (make-sentence (str (process-output-str (runProcess obj))) :spaceDesignator #\Newline) error-p)
    (append appetizers currentDVs)))
	     			 
(defmethod wrapper-execute ((obj run-class) &optional (process nil) (appetizers nil))
  (mklst appetizers)
  (let ((necessaryDVs) (currentDVs) (currentDV))
    (while (setf necessaryDVs (necessaries (DVKeys obj) (DVHash obj)))
     ;loop over the current DVs and add them to the DVHash table
      (setf currentDVs (get-DVs obj process appetizers))
      (while (and necessaryDVs currentDVs)
	(setf currentDV (car currentDVs))
	(setf currentDVs (cdr currentDVs))
	(assert (member (car currentDV) necessaryDVs :test #'equalp) nil 
		"sent ~a DV for the next trial, before sending all the DVs for this trial" (car currentDV)) 
	(add-elements (DVHash obj) currentDV)
	(setf necessaryDVs (remove (car currentDV) necessaryDVs :test #'equalp)))
      (sleep (sleepTime obj))
      (setf appetizers nil))
    currentDVs))

(defmethod wrapper-execute :after ((obj run-class) &optional (process nil) (appetizers nil))
  ;send the evaluated obj out
  (declare (ignore process appetizers))
  (collect (collector obj) (DVHash obj))
  (setf (DVHash obj) nil))

(defmethod wrapper-execute ((obj johnny5-runProcess-class) &optional (process nil) (appetizers nil))
  (assert (not appetizers))
  (assert (not process))
  (let ((leftovers))
    (dolist (run (runs obj) (assert (not leftovers) nil "should not be any leftovers after runProcess object finishes"))
      (setf leftovers (wrapper-execute run (modelProgram obj) leftovers))))
  (sleep (sleepTime obj)))
  
(defmethod wrapper-execute ((obj number5-runProcess-class) &optional (process nil) (appetizers nil))
  (assert (not appetizers))
  (assert (not process)) 
   ;launch the process
  (labels 
      ((get-iv-string (obj)
	 (let ((out) (elements) (runCount) (elementCount) (line))
	   (setf runCount 0)
	   (dolist (run (runs obj) out)
	     (setf line nil)
	     (incf runCount)
	     (setf elements (get-elements (IVKeys run) (IVHash run)))
	     (setf elementCount 0) 
	     (dolist (element elements)
	       (incf elementCount)
	       (setf line (concatenate 'string line (format nil "~a=~a" (car element) (cdr element))))
	       (if (not (equal elementCount (length elements)))
		   (setf line (concatenate 'string line (format nil ",")))))
	     (if (not (equal runCount (length (runs obj))))
		 (setf line (concatenate 'string line (format nil ";"))))
	     (setf out (concatenate 'string out line))))))
    (setf (process obj)
	  (run-program 
	   (first (modelProgram obj)) 
	   (append
	    (rest (modelProgram obj))
	    (list (get-IV-string obj))
	    (list (platform obj)))
	   :output :stream :wait nil)))
  (assert (equalp (format nil "~a" (process-status (process obj))) "running") nil "model process failed to start correctly")
  ;then execute each run
  (let ((leftovers))
    (dolist (run (runs obj)
	     (assert (not leftovers) nil "should not be any leftovers after runProcess object finishes"))
      (setf leftovers (wrapper-execute run (process obj) leftovers))))
  (sleep (sleepTime obj))
  ;a few assertions to make sure everything finished cleanly
  (assert (not (listen (process-output (process obj))))
	  nil "unprocessed lines remain in stdout stream of model after all DVs have been processed")
  (assert (equalp (format nil "~a" (process-status (process obj))) "exited")
	  nil "model process failed to quit after all DVs have been processed"))

(defmethod wrapper-execute ((obj session-class) &optional (process nil) (appetizers nil))
  ;execute each runProcess
  (assert (not process))
  (assert (not appetizers))
  (dolist (runProcess (runs obj))
    (wrapper-execute runProcess process appetizers))
  (assert 
   (equal 
    1
    (apply '* (flatten 
	       (mapcar #'(lambda (runProcess) 
			   (mapcar #'(lambda (run) (if (equal (quot (collector run)) 
							      (quota (collector run))) 1 0))
				   (runs runProcess)))
		       (runs obj)))))
   nil "not all collectors fully executed"))

;////////////////////////////////////////////
;hpc-specific classes, methods, and functions; all of this is to define the custom 'build-hpc-session
;function, and then call it by specifying "sessionBuilder='build-hpc-session" in the config file
(defclass hpc-work-class (work-class)
  ((workFilePath :accessor workFilePath :initarg :workFilePath :initform nil)))

(defmethod initialize-instance :after ((obj hpc-work-class) &key)
  (setf (lines obj) (mapcar #'get-words (get-lines (file-string (workFilePath obj))))))

(defclass hpc-collector-class (collector-class)
  ((cellElements :accessor cellElements :initarg :cellElements :initform nil)))

(defmethod print-collector ((obj hpc-collector-class))
  (format t "Evaluating: ")
  (dotimes (i (length (cellElements obj)))
    (if (equal i (- (length (cellElements obj)) 1))
	(format t "~a~%" (cdr (nth i (cellElements obj))))
	(format t "~a " (cdr (nth i (cellElements obj))))))
  ;(print-hash (collection obj))
  (dolist (DVKey (keys obj))
    (aif (cdr 
	  (first 
	   (get-elements 
	    DVKey 
	    (collection obj) 
	    (if (hash-table-p (collapseHash obj)) 
		(gethash DVKey (collapseHash obj)) 
		(collapseHash obj)))))
	 (format t "~a: ~a~%" DVKey (coerce it 'double-float)))))

(defun build-hpc-session ()
  (build-session ;yeah, this is a macro
   :collector-instance
   (make-instance 
    'hpc-collector-class
    :cellElements (get-elements cellKeys IVHash)
    :keys DVKeys
    :quota quota
    :collapseHash (copy-hash collapseHash))
   :work-instance
   (make-instance
    'hpc-work-class
    :workFilePath workFilePath)))
;////////////////////////////////////////////

;finally, we get to load and run stuff!
;load the extra lisp files
(dolist (line (get-matching-lines (restructure (file-string (get-arg 1))) "file2load="))
  (load (format nil "~a" (replace-all line "$1" (get-word (get-arg 2)) :test #'equalp))))
;run it!
(wrapper-execute 
 (funcall
  (aif (get-matching-line (restructure (file-string (get-arg 1))) "sessionBuilder=")
       (eval (read-from-string (make-sentence it)))
       (eval (read-from-string (make-sentence (list "'build-hpc-session")))))))
;kill it!	    
(quit)


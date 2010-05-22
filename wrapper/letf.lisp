;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;; 
;;; Author      : Clayton Stanley
;;; Address     : Air Force Research Laboratory
;;;             : Mesa, AZ 85212 USA
;;;             : clayton.stanley@wpafb.af.mil
;;; Filename    : LETF.lisp
;;; Version     : 1.0
;;; 
;;; Description : A Lisp-Based Exploratory Testing Framework for Computational Cognitive Models
;;; 
;;; Bugs        : ???
;;;
;;; ----- History -----
;;;
;;; 2010.04.22  : Creation.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setf *read-default-float-format* 'double-float) 

; returns the command line parameters
(defun my-command-line ()
  (or 
   #+SBCL *posix-argv*
   #+LISPWORKS system:*line-arguments-list*
   #+CMU extensions:*command-line-strings*
   nil))

; returns the nth command line argument from right to left (note that this is the
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

(defmacro awhen (test-form &body body)
  `(aif ,test-form
     (progn ,@body)))

;; Graham's alambda
(defmacro alambda (parms &body body)
  `(labels ((self ,parms ,@body))
     #'self))

(defmacro mklst (item)
  `(if (not (listp ,item)) (setf ,item (list ,item))))

(defmacro verbose (&rest lst)
  (mapc #'(lambda (x) (format *error-output* "~a~%" x)) lst)
  `(progn ,@lst))

(defmacro guard (fun &body body)
  `(let ((it (multiple-value-call #'list ,fun)))
     ,(if (not body)
	  `(assert (<= (length (car it)) 1) nil 
		   "function ~a returned sequence ~a; default guard failed" 
		   ,(format nil "~a" fun) it)
	  `(progn ,@body))
     (values-list it)))
     
;loads the file with pathname str
;keeps track of all the pathnames that have been sent 
;to this function; returns the list of those names
(let ((loaded))
  (defun load-and-loaded (str)
    (when str 
      (load str)
      (push-to-end str loaded))
    loaded))

;returns if a key is present in the hash table
(defun key-present (key hash)
  (multiple-value-bind (value flag) (gethash key hash)
    (declare (ignore value))
    flag))

;equivalent to #'symbol-function, just safer.
;converts a symbol to the function that the 
;symbol points to; if it can't convert the 
;symbol, it returns the symbol
(defun symbol-function-safe (x)
  (if (and (equal 'symbol (type-of x))
	   (fboundp x))
      (symbol-function x)
      x))

(defun gethash-ifHash (key hash)
  (if (hash-table-p hash)
      (progn
	(assert (key-present key hash))
	(gethash key hash))
      hash))

(defun flatten (lis)
  "Takes a nested list and makes in into a single-level list"
  (declare (list lis))
  (labels ((rec (lis acc)
             (cond ((null lis) acc)
                   ((atom lis) (cons lis acc))
                   (t (rec (car lis) (rec (cdr lis) acc))))))
    (rec lis nil)))

;converts the rows to columns (and vice versa) in a nested list
(defun transpose (lst)
  (if (and lst (consp lst) (consp (car lst)))
      (let ((templst) (out))
	(assert (equal (length (remove-duplicates (mapcar #'length lst) :test #'equal)) 1))
	(dotimes (j (length (car lst)) out)
	  (setf templst nil)
	  (dotimes (i (length lst) (push-to-end templst out))
	    (push-to-end (nth j (nth i lst)) templst))))
      lst))

;collapses across a nested list (returning a flat list)
;by calling collapseFn on each column in the list
(defun collapse (lst collapseFn)
  (if (not collapseFn)
      lst
      (if (and lst (consp lst) (consp (car lst)))
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

(defun print-hash (hash &key (strm t) (keys nil))
  (mklst keys)
  (labels ((hash-string (hash &key (keys nil))
	     (if (not (hash-table-p hash))
		 (format nil "~a" hash)
		 (let ((out))
		   (loop for value being the hash-values of hash using (hash-key key) do
			(if (or (not keys) (member key keys :test #'equalp))
			    (setf out (concatenate 'string out (format nil "~%~a -> ~a" key (hash-string value))))))
		   out))))
    (format strm "~a~%" (string-trim (list #\Newline #\Return #\LineFeed) (hash-string hash :keys keys)))))

(defun copy-hash (hash)
  (if (hash-table-p hash)
      (let ((out (make-hash-table :test #'equalp)))
	(loop for value being the hash-values of hash using (hash-key key) do 
	     (setf (gethash key out) (copy-hash value)))
	out)
      hash))

;copies all of the hash tables in the list lst, and merges them
(defun merge-hash (lst &key (toHash))
  (let* ((fun (lambda (out key value)
		(if (key-present key out) 
		    (assert (equalp (gethash key out) value) nil
			    "'~a' key already present in hash table with value '~a', which is different than the value '~a' that you're trying to add now"
			    key (gethash key out) value)
		    (setf (gethash key out) (copy-hash value)))))
	 (rec (alambda (lst out)
		       (if lst
			   (cond ((hash-table-p lst)
				  (loop for value being the hash-values of lst using (hash-key key) do 
				       (funcall fun out key value)))
				 ((or (hash-table-p (car lst)) (consp (car lst)))
				  (self (car lst) out)
				  (self (cdr lst) out))
				 (t (funcall fun out (car lst) (cdr lst)))))))
	 (out (aif toHash it (make-hash-table :test #'equalp))))
    (funcall rec lst out)
    out))

(defun file-string (path)
  "Sucks up an entire file from PATH into a freshly-allocated string,
      returning two values: the string and the number of bytes read."
  (with-open-file (s path)
    (let* ((len (file-length s))
           (data (make-string len)))
      (values data (read-sequence data s)))))

;if any column in the nested list is nil, it throws out the column
(defun throwOutYerNils (&rest lsts)
  (let ((out))
    (if (consp (car lsts))
	(dolist (column (transpose lsts) (setf out (transpose out)))
	  (if (not (member nil column))
	      (push-to-end column out)))
	(if (not (member nil lsts)) (setf out lsts)))
    (apply #'values (if out out (make-sequence 'list (length lsts) :initial-element nil)))))

;shorthand macro for recursively calling a function across 
;the lists in the nested lists left and right
(defmacro inLST (left right fName throwOutYerNils)
  `(if ,left (if (consp (car ,left))
		 (let ((out))
		   (dolist (itm ,left)
		     (push-to-end (,fName itm ,right :throwOutYerNils ,throwOutYerNils) out))
		   (return-from ,fName (flatten out))))))

(defmacro inLSTs (left right fName throwOutYerNils)
  `(progn
     (inLST ,left ,right ,fName ,throwOutYerNils)
     (inLST ,right ,left ,fName ,throwOutYerNils)))

(defmacro assertEqualLengths (l1 l2)
  `(assert (equal (length ,l1) (length ,l2)) nil "length ~d not equal to length ~d" (length ,l1) (length ,l2)))
  
(defun MAD (l1 l2 &key (throwOutYerNils nil))
  (inLSTs l1 l2 MAD throwOutYerNils)
  (assertEqualLengths l1 l2)
  (if throwOutYerNils (multiple-value-setq (l1 l2) (throwOutYerNils l1 l2)))
  (if l1 (/ (apply #'+ (mapcar (lambda (x y) (abs (- x y))) l1 l2)) (length l1))))

(defun correl (l1 l2 &key (throwOutYerNils nil))
  (inLSTs l1 l2 correl throwOutYerNils)
  (labels ((std (lst)
	     (assert (> (length lst) 1)
		     nil "must have at least 2 numbers to calculation std; only supplied ~d" (length lst))
	     (sqrt
	      (/
	       (apply #'+ (funcall #'(lambda (x) (mapcar (lambda (y) (* (- y x) (- y x))) lst))
				  (/ (apply #'+ lst) (length lst))))
	       (- (length lst) 1)))))
    (assertEqualLengths l1 l2)
    (ignore-errors
      (if throwOutYerNils (multiple-value-setq (l1 l2) (throwOutYerNils l1 l2)))
      (let ((out
	     (if (> (length l1) 1)
		 (/
		  (apply #'+
			 (mapcar (lambda (x y) (* x y))
				 (funcall #'(lambda (x)
					      (mapcar (lambda (y) (- y x)) l1))
					  (/ (apply #'+ l1) (length l1)))
				 (funcall #'(lambda (x)
					      (mapcar (lambda (y) (- y x)) l2))
					  (/ (apply #'+ l2) (length l2)))))
		  (* (- (length l1) 1) (std l1) (std l2))))))
	(if out (if (not (or (> 0 out) (< 0 out) (equal 0 out))) (setf out nil)))
	out))))

(defun RMSE (l1 l2 &key (throwOutYerNils nil))
  (inLSTs l1 l2 RMSE throwOutYerNils)
  (assertEqualLengths l1 l2)
  (if throwOutYerNils (multiple-value-setq (l1 l2) (throwOutYerNils l1 l2)))
  (if l1 (sqrt (/ (apply #'+ (mapcar (lambda (x y) (* (- x y) (- x y))) l1 l2)) (length l1)))))

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
  (if lst (apply #'+ lst)))

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
	    (when (not in-the-white)
	      (push-to-end (subseq str start i) out)
	      (setf in-the-white t)
	      (setf start i))
	    (if includeSpaceDesignators (push-to-end (format nil "~a" (char str i)) out)))
	  (when in-the-white
	    (setf in-the-white nil)
	    (setf start i))))))

;just like get words, but asserts that only one word can be found. Returns that word, and not
;a list of words, like get-words does
(defmacro get-word (&rest lst)
  `(car (guard (get-words ,@lst))))

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

(defmacro remap-string (&body body)
  `(alambda (str hash &key (collapseFn "#'mean") (inside-brackets nil) (key nil))
	    (if inside-brackets
               ;remaps an expression surrounded by brackets by calling the hash table on each of the words in the expression
               ;each word in the expression should be a key that corresponds to an already-defined element in the hash table
               ;you can also supply a list of lambda functions to be evaluated, that take, as input, the current word in the 
               ;expression and the hash table; useful if you want to collect something other than what is returned by this 
               ;function see 'necessaries' or 'get-elements', or 'eval-hash' for examples 
		(let ((out))
		  (dolist (word (get-words str) (make-sentence out))
		    ,(if (not body)
			 `(push-to-end (self (gethash word hash) hash 
					    :collapseFn collapseFn 
					    :inside-brackets nil :key word) out)
			 `(progn ,@body))))
                ;remaps an expression, which may or may not have parts that are surrounded by brackets
                ;for each part that is surrounded by brackets, call remap-string with inside-brackets flagged to convert
                ;the bracketed expression to actual values
		(let ((out))
		  (if (consp str)
		      (progn
			(dolist (item str)
			  (push-to-end 
			   (self item hash :collapseFn collapseFn :inside-brackets nil :key nil)
			   out))
			(setf out (make-sentence 
				   (append (list "(funcall #'collapse" "(list ") 
					   out (list ")" (make-sentence (gethash-ifHash key collapseFn)) ")")))))
		      (progn
			(dolist (word (lump-brackets str))
			  (push-to-end
			   (if (and (equal (char word 0) #\[) (equal (char word (- (length word) 1)) #\]))
			       (self (string-trim "[]" word) hash :collapseFn collapseFn :inside-brackets t :key nil)
			       word)
			   out))
			(setf out (make-sentence out))))
		  out))))

;traverses the 'keys' in the hash table 'hash', and recursively
;searches the other keys that each 'key' references. During the traversal
;checks if keys are present not-present, and builds a list of those keys
;if bool is t, returns the list of keys that are present; if bool is nil,
;returns the list of keys that aren't present
(defun traverse (keys hash &key (bool nil) (collapseFn "#'mean"))
  (mklst keys)
  (let* ((words) (str) 
	 (traversed (make-hash-table :test #'equalp))
	 (fun (remap-string
	       (when (not (key-present word traversed))
		 (if (equal (key-present word hash) bool) (push-to-end word words))		   
		 (setf (gethash word traversed)
		       (if (key-present word hash)
			   (self (gethash word hash) hash
				 :collapseFn collapseFn
				 :inside-brackets nil :key word)
			   word)))
	       (push-to-end (gethash word traversed) out))))
    (dolist (key keys)
      (push-to-end 
       (funcall fun (concatenate 'string "[" key "]") hash :collapseFn collapseFn) 
       str))
    (values (sort (flatten words) #'string<) str)))

;returns the subset of keys that are needed to 'remap' the keys in 'keys' 
;that do not currently have values associated with them
(defmacro necessaries (keys hash &rest args)
  `(traverse ,keys ,hash :bool nil ,@args))

;...that already have values associated with them
(defmacro availables (keys hash &rest args)
  `(traverse ,keys ,hash :bool t ,@args))

;evaluates all the stuff in the hash table that it can, given the current state of the hash table
(defmethod eval-hash ((hash hash-table))
  (labels ((toString (lst)
	     (let ((out))
	       (if (consp lst)
		   (progn
		     (push-to-end "(list" out)
		     (dolist (item lst)
		       (push-to-end (toString item) out))
		     (push-to-end ")" out))
		   (push-to-end (format nil "~a" lst) out))
	       (make-sentence (flatten out)))))
    (let* ((traversed (make-hash-table :test #'equalp))
	   (fun (remap-string
		 (if (not (key-present word traversed))
		     (let ((newVal) (val))
		       (setf val (self (gethash word hash) hash 
				       :collapseFn collapseFn 
				       :inside-brackets nil :key word))
		       (setf (gethash word traversed) val)
		       (ignore-errors
			 (multiple-value-bind (evaledVal lngth) (read-from-string val)
			   (if (equal lngth (length val))
			       (setf newVal (toString (eval evaledVal))))))
		       (when (and newVal (not (equal (- (length val) 
							(length (find-in-string val (list #\space #\tab))))
						     (- (length newVal) 
							(length (find-in-string newVal (list #\Space #\tab)))))))
			 ;(format t "~a -> ~a -> ~a~%" (gethash word hash) val newVal)
			 ;(format t "~a ~a~%" (incf count) word)
			 (setf (gethash word hash) newVal)
			 (setf (gethash word traversed) newVal))))
		 (push-to-end (gethash word traversed) out))))
      (loop for key being the hash-keys of hash do 
	   (if (not (necessaries key hash))
	       (funcall fun (concatenate 'string "[" key "]") hash))))))

;takes an expression, and expands the ':'s (similar to how matlab references arrays)
;for example: (bracket-expand "hello1:5") -> "hello1 hello2 hello3 hello4 hello5"
;(bracket-expand "1:5hello") -> "1hello 2hello 3hello 4hello 5hello"
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
;keeps track of the line numbers for all of the lines that have
;been returned from calling this function, and returns that list
;of numbers as it's second 'values value
(let ((traversed))
  (defun get-matching-lines (str keys)
    (labels ((index= (words keys)
	       (let* ((word (first words))
		      (wordLength (length word)))
		 (dolist (key keys nil)
		   (if (equalp key (subseq word 0 (min wordLength (length key))))
		       (return-from index= (length key)))))))
      (mklst keys)
      (let ((words) (out) (line) (lines))
	(setf lines (if (consp str) str (get-lines str)))
	(dotimes (i (length lines) (values out traversed))
	  (setf line (nth i lines))
	  (setf words (if (consp line) line (get-words line)))
	  (if words
	      (awhen (index= words keys)
		     (push-to-end i traversed)
		     (push-to-end 
		      (bracket-expand 
		       (string-trim 
			(list #\Space #\tab) 
			(subseq (make-sentence line) it (length (make-sentence line))))) 
		      out))))))))

;same as above, just asserts the expectation that only one (or zero) lines should be returned
(defmacro get-matching-line (&rest lst)
  `(car (guard (get-matching-lines ,@lst))))

;returns the list of elements (key . value) from the hash table 'hash' specified by 'keys'
;will evaluate each value before putting it in the list
(defun get-elements (keys hash &optional (collapseFns "#'mean"))
  (mklst keys)
  (if (not (consp collapseFns)) (setf collapseFns (make-list (length keys) :initial-element collapseFns)))
  (let ((out) (key) (collapseFn))
    (dotimes (i (length keys) out)
      (setf key (nth i keys))
      (setf collapseFn (nth i collapseFns))
      (multiple-value-bind (words val) 
	  (guard (necessaries key hash :collapseFn collapseFn)
		 (assert (equal (length (car it)) 0) nil
			 "necessaries ~a left over when calling get-elements; not allowed to have any necessaries here" (car it)))
	(declare (ignore words))
	(push-to-end (cons key (eval (read-from-string (first val)))) out)))))

(defmacro get-element (&rest lst)
  `(car (guard (get-elements ,@lst))))

;returns a list of keys (in order) that map to the values found in each line of the work file
(defun get-cell-keys (str)
  (let ((lines) (out))
    (setf lines (get-matching-lines str (list "constant=" "iv=")))
    (dolist (line lines out)
      (push-to-end
       (car (guard (get-words line) 
		   (assert (> (length (car it)) 0) nil
			   "no rhs for lhs=(either constant= or iv=) in config file")))
       out))))

;adds all cell elements for a single line in the work file to the hash table
(defmethod add-cell-elements ((hash hash-table) &optional (configFileStr nil) (cell-values nil))
  (let ((cell-keys (get-cell-keys configFileStr)))
    (assert (equal (length cell-keys) (length cell-values))
	    nil "number of cell keys (~d) does not equal number of cell values (~d)"
	    (length cell-keys) (length cell-values))
    (merge-hash (mapcar #'cons cell-keys cell-values) :toHash hash)))

;recursively adds all elements in the config file that are referenced in the 'lhs' line in the config file
(defmacro add-dependent-element (&body body)
  `(alambda (hash &optional (configFileStr nil) (lhs nil))
	    (let ((line) (words))
	      (setf line (if (consp lhs) (cdr lhs) (get-matching-line configFileStr lhs)))
	      (setf lhs (if (consp lhs) (car lhs) lhs))
	      (when line
		(merge-hash (cons (subseq lhs 0 (- (length lhs) 1)) line) :toHash hash)
		(setf words (lump-brackets line))
		(dolist (word words)
		  (if (and (equal (char word 0) #\[) (equal (char word (- (length word) 1)) #\]))
		      (dolist (item (get-words (string-trim "[]" word)))
			,(if (not body)
			     `(self hash configFileStr (concatenate 'string item "="))
			     `(progn ,@body)))))))))

;recursively adds all elements in the config file that are referenced in the list of 'lhs' lines in the config file
(defmethod add-dependent-elements ((hash hash-table) &optional (configFileStr nil) (lhs nil))
  (let* ((traversed (make-hash-table :test #'equalp))
	 (fun (add-dependent-element
	       (when (not (key-present item traversed))
		 (setf (gethash item traversed) t)
		 (self hash configFileStr (concatenate 'string item "="))))))
    (dolist (line (get-matching-lines configFileStr lhs))
      (funcall fun hash configFileStr 
	       (concatenate 
		'string 
		(car (guard (get-words line) 
			    ;3 b/c cell reads the dv lines for 3 elements; letf just cares about the 1st element
			    (assert (<= (length (car it)) 3) 
				    nil "length of rhs when adding dependent elements must be between 1-3; ~a not valid" (car it))))
		"=")))))

(defclass work-class ()
  ((lines :accessor lines :initarg :lines :initform nil)
   (workFilePath :accessor workFilePath :initarg :workFilePath :initform nil)))

;top-level class for session object that gets executed
(defclass session-class ()
  ((runProcesses :accessor runProcesses :initarg :runProcesses :initform nil)
   (quota :accessor quota :initarg :quota :initform nil)
   (traversed :accessor traversed :initarg :traversed :initform nil)
   (configFileLnLST :accessor configFileLnLST :initarg :configFileLnLST :initform nil)
   (statusPrinters :accessor statusPrinters :initarg :statusPrinters :initform nil)
   (collapseQuota :accessor collapseQuota :initarg :collapseQuota :initform nil)
   (iterations :accessor iterations :initarg :iterations :initform nil)
   (lines :accessor lines :initarg :lines :initform nil)
   (collapseHash :accessor collapseHash :initarg :collapseHash :initform nil)
   (DVHash :accessor DVHash :initarg :DVHash :initform nil)
   (DVKeys :accessor DVKeys :initarg :DVKeys :initform nil)
   (IVKeys :accessor IVKeys :initarg :IVKeys :initform nil)
   (cellKeys :accessor cellKeys :initarg :cellKeys :initform nil)
   (modelProgram :accessor modelProgram :initarg :modelProgram :initform nil)
   (entryFnType :accessor entryFnType :initarg :entryFnType :initform nil)))

;collector class pattern that can be extended to print results from letf in a specific way
(defclass base-collector-class ()
  ((quota :accessor quota :initarg :quota :initform 1)
   (quot :accessor quot :initarg :quot :initform 0)
   (collection :accessor collection :initarg :collection :initform (make-hash-table :test #'equalp))))

(defmethod collect ((obj base-collector-class) (lst list))
  (assert (not (> (quot obj) (quota obj))))
  (if lst (if (not (consp (car lst))) (setf lst (list lst))))
  (dolist (item lst)
    (push-to-end (cdr item) (gethash (car item) (collection obj))))
  (incf (quot obj)))

(defmethod collect ((obj base-collector-class) (DVHash hash-table))
  (let ((out))
    (maphash #'(lambda (key value) (push-to-end (cons key value) out)) DVHash)
    (collect obj out)))

(defmethod print-collector ((obj base-collector-class)) ())

(defmethod print-collector :after ((obj base-collector-class))
  (setf (collection obj) nil))

;extendable collector class for printing the last N lines that have been 
;returned by the model, if the model errors out
(defclass process-output-str-class (base-collector-class)
  ((error-p :accessor error-p :initarg :error-p :initform nil)))

(defmethod collect :after ((obj process-output-str-class) (lst list))
  (declare (ignore lst))
  (when (> (quot obj) (quota obj))
    (setf (gethash "str" (collection obj)) (rest (gethash "str" (collection obj))))
    (decf (quot obj))))

;extendable collector class for printing the collected results after the entire
;session is run
(defclass session-collector-class (base-collector-class)
  ((collectors :accessor collectors :initarg :collectors :initform nil)))

(defmethod collect :after ((obj session-collector-class) (lst list))
  (declare (ignore lst))
  (if (equal (quota obj) (quot obj))
      (print-collector obj)))

;extendable collector class for printing the collected results after each collapseQuota
;is reached
(defclass collector-class (base-collector-class)
  ((collapseHash :accessor collapseHash :initarg :collapseHash :initform nil)
   (keys :accessor keys :initarg :keys :initform nil)
   (cellElements :accessor cellElements :initarg :cellElements :initform nil)
   (run-collectors :accessor run-collectors :initarg :run-collectors :initform nil)
   (session-collector :accessor session-collector :initarg :session-collector :initform nil)))

(defmethod initialize-instance :after ((obj collector-class) &key)
  (assert (session-collector obj))
  (push-to-end obj (collectors (session-collector obj))))

(defmethod collect :after ((obj collector-class) (lst list))
  (declare (ignore lst))
  (when (equal (quota obj) (quot obj))
      ;short-circuit to optimize recursion called later
      ;pass over all of the elements in the hash table, and
      ;if an element is a list and if all slots in that list are equal, then collapse the list
    (loop for key being the hash-keys of (collection obj)	
       using (hash-value val)
       do (if (if (consp val) (equal 1 (length (remove-duplicates val :test #'equalp))))
	      (setf (gethash key (collection obj)) (first val))))
    (let ((elements (get-elements (keys obj) (collection obj) 
				  (mapcar (lambda (x) (gethash-ifHash x (collapseHash obj))) (keys obj)))))
      (print-collector obj)
      (collect (session-collector obj) elements))))

;extendable collector class for printing the collected results after each call to the entry function
(defclass run-collector-class (base-collector-class)
  ((runs :accessor runs :initarg :runs :initform nil)
   (collector :accessor collector :initarg :collector :initform nil)))

(defmethod initialize-instance :after ((obj run-collector-class) &key)
  (assert (collector obj))
  (push-to-end obj (run-collectors (collector obj))))

(defmethod collect :after ((obj run-collector-class) (lst list))
  (assert (equal (quot obj) 1))
  (assert (equal (quota obj) 1))
  (print-collector obj)
  (collect (collector obj) lst))
 
;class for a single process; if we're short-circuiting (i.e., calling a lisp-native model using an entry function)
;then there will only be 1 instance of this class; if we're launching the model as a separate process, then
;the number of instances will be (ceiling (total runs / runs per process))
(defclass runProcess-class ()
  ((runs :accessor runs :initarg :runs :initform nil)
   (session :accessor session :initarg :session :initform nil)
   (process :accessor process :initarg :process :initform nil)
   (modelProgram :accessor modelProgram :initarg :modelProgram :initform nil)
   (platform :accessor platform :initarg :platform :initform nil)
   (process-output-str :accessor process-output-str :initarg :process-output-str :initform nil)))

;class for a single process if we're not short circuiting
(defclass number5-runProcess-class (runProcess-class)
  ((sleepTime :accessor sleepTime :initarg :sleepTime :initform .2)))

;class for a single process if we're short circuiting
(defclass johnny5-runProcess-class (runProcess-class)
  ((sleepTime :accessor sleepTime :initarg :sleepTime :initform 0)))

;class for a single run
(defclass run-class ()
  ((IVHash :accessor IVHash :initarg :IVHash :initform nil)
   (DVHash :accessor DVHash :initarg :DVHash :initform nil)
   (IVKeys :accessor IVKeys :initarg :IVKeys :initform nil)
   (DVKeys :accessor DVKeys :initarg :DVKeys :initform nil)
   (quot :accessor quot :initarg :quot :initform nil)
   (cellKeys :accessor cellKeys :initarg :cellKeys :initform nil)
   (sleepTime :accessor sleepTime :initarg :sleepTime :initform 0)
   (run-collector :accessor run-collector :initarg :run-collector :initform nil)
   (runProcess :accessor runProcess :initarg :runProcess :initform nil)
   (entryFnType :accessor entryFnType :initarg :entryFnType :initform nil)))

(defmethod initialize-instance :after ((obj run-class) &key)
  (assert (run-collector obj))
  (assert (not (runs (run-collector obj))))
  (push-to-end obj (runs (run-collector obj))))

(defclass number5-run-class (run-class) ())

(defclass johnny5-run-class (run-class) ())

;reattaches lines that have been separated using the "\" character at the end of a line
(defun restructure (str)
  (let ((out) 
	(lineDesigs (list #\Newline #\Return #\LineFeed))
	(count -1)
	(strLength (length str)))
    (while (< (incf count) (- strLength 1))
      (if (if (find-in-string (char str count) #\\)
	      (find-in-string (char str (+ count 1)) lineDesigs))
	  (while (and (< (+ 1 count) (- strLength 1))
		      (find-in-string (char str (+ 1 count)) lineDesigs))
	    (incf count))
	  (push (char str count) out)))
    (if (equal count (- strLength 1))
	(push (char str count) out))
    (coerce (reverse out) 'string)))

;modifies the collapse hash table that keeps track of how letf will collapse each DV when
;collapseQuota is reached
(defmethod mod-collapseHash ((hash hash-table) collapseFn &key (DVKeys nil) (ApplyToKeys nil))
  (labels ((keys (hash)
	     (let ((out))
	       (loop for value being the hash-values of hash using (hash-key key) do 
		    (push-to-end key out))
	       out)))
    (dolist (DVKey (aif DVKeys it (keys hash)))
      (assert (key-present DVKey hash) nil "DV=~a not present in collapse hash table" DVKey)
      (dolist (ApplyToKey (aif ApplyToKeys it (keys (gethash DVKey hash))))
	(if (not (key-present ApplyToKey (gethash DVKey hash)))
	    (assert (not DVKeys) nil "ApplyToKey=~a not present in DV=~a collapse hash table" ApplyToKey DVKey)
	    (setf (gethash ApplyToKey (gethash DVKey hash)) collapseFn))))))

(defmacro upload-to (obj &rest vals)
  `(progn ,@(mapcar 
	     (lambda (x) 
	       (if (consp x)
		   `(setf (,(car x) ,obj) ,(cadr x))
		   `(setf (,x ,obj) ,x))) vals)))

;generates the code that generates the session object that will be executed
;this macro is configured by extending various pieces of the above
;object-oriented hierarchy and sending constructors to those new pieces
;as inputs to the 'build-session macro call
(defmacro build-session (&key 
			 (collector-instance `(make-instance 'collector-class)) 
			 (work-instance `(make-instance 'work-class))
			 (session-collector-instance `(make-instance 'session-collector-class))
			 (process-output-str-instance `(make-instance 'process-output-str-class))
			 (run-collector-instance `(make-instance 'run-collector-class)))
  (setf collector-instance (append collector-instance
				    '(:cellElements (get-elements cellKeys IVHash)
				      :keys DVKeys
				      :quota quota
				      :collapseHash (copy-hash collapseHash)
				      :session-collector session-collector)))
  (setf work-instance (append work-instance '(:workFilePath (get-arg 0))))
  (setf session-collector-instance (append session-collector-instance '(:quota (* (length (lines work)) iterations))))
  (setf run-collector-instance (append run-collector-instance '(:quota 1 :collector collector)))
  (setf process-output-str-instance (append process-output-str-instance '(:quota 200)))
  (let ((session-instance `(make-instance 'session-class))
	(run-process-instance `(make-instance (if short-circuit-p 'johnny5-runProcess-class 'number5-runProcess-class) 
					      :modelProgram modelProgram :platform platform
					      :process-output-str ,process-output-str-instance
					      :session session))
	(run-instance `(make-instance (if short-circuit-p 'johnny5-run-class 'number5-run-class)
				      :IVHash (copy-hash IVHash)
				      :DVHash (copy-hash mergedHash)
				      :IVKeys IVKeys
				      :DVKeys DVKeys
				      :quot (+ 1 count)
				      :cellKeys cellKeys
				      :run-collector ,run-collector-instance
				      :runProcess runProcess
				      :entryFnType entryFnType)))
    `(progn
       (labels ((get-IV-hash (configFileStr cell-values)
		  (let ((hash (make-hash-table :test #'equalp)))
		    (add-cell-elements hash configFileStr cell-values)
		    (add-dependent-elements hash configFileStr "input=")
		    hash))
		(get-DV-hash (configFileStr)
		  (let ((hash (make-hash-table :test #'equalp)))
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
		  (let ((out))
		    (dolist (line (get-matching-lines configFileStr (list "dv=" "sdv=")) out)
		      (push-to-end
		       (car (guard (get-words line)
				   (assert (> (length (car it)) 0) nil "no rhs on lhs=dv=; at least 1 word required")))
		       out))))
		(get-collapseHash (DVKeys DVHash configFileStr defaultCollapseFn)
		  (let ((hash (make-hash-table :test #'equalp)) 
			(words) (tmpLn) (tmpLST))
		    (setf tmpLST (get-matching-lines configFileStr "collapseFn="))
		    (if (not tmpLST)
			(return-from get-collapseHash defaultCollapseFn)
			(if (and (equal (length tmpLST) 1) 
				 (equal (length (get-words (first tmpLST) :spaceDesignators (list #\; #\&))) 1))
			    (return-from get-collapseHash (first tmpLST))))
		    (dolist (DVKey DVKeys)
		      (merge-hash 
		       (cons DVKey (merge-hash (mapcar #'(lambda (x) (cons x defaultCollapseFn))
						       (append (necessaries DVKey DVHash) (availables DVKey DVHash)))))
		       :toHash hash))
		    (dolist (line (get-matching-lines configFileStr "collapseFn=") hash)
		      (setf words (get-words line :spaceDesignators (list #\; #\&)))
		      (setf tmpLn (make-sentence (rest words) :spaceDesignator #\Newline))
		      (assert (equal (length (rest words)) (length (get-matching-lines tmpLn (list "DV=" "SDV=" "ApplyTo="))))
			      nil "line ~a not valid" line)
		      (mod-collapseHash 
		       hash
		       (let ((hsh (make-hash-table :test #'equalp)))
			 (funcall (add-dependent-element) hsh configFileStr (cons "collapseFn=" (first words)))
			 (funcall (remap-string) (concatenate 'string "[" "collapseFn" "]") hsh))
		       :DVKeys (get-words (bracket-expand (get-matching-line tmpLn (list "DV=" "SDV=")) t))
		       :ApplyToKeys (get-words (bracket-expand (get-matching-line tmpLn "ApplyTo=") t)))))))
	 (let ((session) (runProcess) (line-index) (count) (iteration) (iterations)
	       (DVHash) (IVHash) (DVKeys) (IVKeys) (modelProgram) (cellKeys) (mergedHash)
	       (quota) (collector) (quot) (collapseHash) (work) (session-collector) (entryFnType)
	       (configFilePath (get-arg 1)) (platform (get-word (get-arg 2)))
	       (configFileStr) (runsPerProcess) (short-circuit-p) (configFileLnLST) (configFileWdLST))
	   (setf configFileStr (restructure (file-string configFilePath)))
	   (setf configFileLnLST (get-lines configFileStr))
	   (setf configFileWdLST (mapcar #'get-words configFileLnLST))
	   (setf work ,work-instance)
	   (setf runsPerProcess (aif (get-matching-line configFileWdLST "runsPerProcess=")
				     (read-from-string (get-word it))
				     1))
	   (setf session ,session-instance)
	   (setf DVHash (get-dv-hash configFileWdLST))
	   (eval-hash DVHash)
	   (setf DVKeys (get-dv-keys configFileWdLST)
		 IVKeys (get-iv-keys configFileWdLST)
		 cellKeys (get-cell-keys configFileWdLST))
	   (setf collapseHash (get-collapseHash DVKeys DVHash configFileWdLST "#'mean"))
	   (setf quota (aif (get-matching-line configFileWdLST "collapseQuota=")
			    (eval (read-from-string (get-word it)))
			    1))
	   (setf modelProgram (aif (get-matching-line configFileLnLST "modelProgram=")
				   (lump-brackets (replace-all it "$1" platform :test #'equalp) :desigs (cons #\" #\") :include-brackets nil)
				   (lump-brackets "'run-model" :desigs (cons #\" #\") :include-brackets nil)))
	   (setf short-circuit-p (let ((it (subseq (first modelProgram) 0 1)))
				   (or (equal it "#") (equal it "'") (equal it "("))))
	   (if short-circuit-p
	       (setf modelProgram (symbol-function-safe (eval (read-from-string (make-sentence modelProgram))))
		     runsPerProcess (read-from-string "inf")))
	   (setf entryFnType (if short-circuit-p
				 (aif (get-matching-line configFileLnLST "entryFnType=")
				      (read-from-string (get-word it))
				      'keys)
				 'process))
	   (assert (member entryFnType (if short-circuit-p (list 'keys 'hash) (list 'process))) nil "invalid entryFnType ~a" entryFnType)
   	   (setf runProcess ,run-process-instance)
	   (setf iterations (aif (get-matching-line configFileWdLST "iterations=")
				 (eval (read-from-string (get-word it)))
				 1)	
		 count 0
		 line-index -1)
	   (setf session-collector ,session-collector-instance)
	   (while (< (incf line-index) (length (lines work)))
	     (setf iteration -1)
	     (setf IVHash (get-iv-hash configFileWdLST (nth line-index (lines work))))
	     (setf mergedHash (merge-hash (list DVHash IVHash)))
	     (if (equal line-index 0) 
		 (aif (necessaries IVKeys IVHash)
		      (assert nil nil "IVs '~a' that are necessary to evaluate the 'input=' lines are not present in the config file" it)))
	     (while (< (incf iteration) iterations)
	       (setf quot -1)
	       (setf collector ,collector-instance)
	       (while (< (incf quot) quota)
		 (push-to-end ,run-instance (runs runProcess))
		 (incf count)
		 ;if it's time to push the runProcess object onto the session object, do it
		 (if (not (equal 'inf runsPerProcess)) 
		     (when (equal (mod count runsPerProcess) 0)
		       (push-to-end runProcess (runProcesses session))
		       (setf runProcess ,run-process-instance))))))
	   (if (runs runProcess) (push-to-end runProcess (runProcesses session)))		
	   (upload-to session
		      (quota (* (length (lines work)) iterations quota))
		      (statusPrinters (mapcar (lambda (x) (symbol-function-safe (eval (read-from-string x)))) 
					      (get-matching-lines configFileWdLST "statusPrinter=")))
		      collapseHash DVHash DVKeys IVKeys cellKeys modelProgram iterations configFileLnLST entryFnType
		      (collapseQuota quota)
		      (lines (length (lines work)))
		      (traversed (multiple-value-bind (trash traversed) (get-matching-lines configFileWdLST nil)
				     (declare (ignore trash))
				     (sort (remove-duplicates traversed :test #'equal) #'<))))
	   (aif (apply #'+ (mapcar #'(lambda (x) (length (runs x))) (runProcesses session)))
		(assert (equal (quota session) it)
			nil "number of linesxiterationsxquota in work file (~d) not equal to number of run objects (~d)"
			(quota session) it))
	   session)))))

;converts an output line of text sent by the model to a dotted pair
;discards if it's not a valid output line (handles when warning statements are printed to stdout)
(defun line2element (line)
  (let ((equal-index) (key) (value))
    (setf equal-index (find-in-string line #\=))
    (when (equal (length equal-index) 1)
      (setf key (get-words (subseq line 0 (first equal-index))))
      (setf value (get-words (subseq line (+ 1 (first equal-index)) (length line))))
      (when (equal (length key) 1)
	;(format t "key=~a value=~a~%" (first key) (first value))
	(cons (first key) (make-sentence value))))))

;capture all the input lines that the model has sent; 
;then, remap each line as a dotted pair (key . value)
(defmethod get-DVs ((obj number5-run-class) &optional (process nil) (appetizers nil)) 
  (assert process)
  (mklst appetizers)
  (let ((currentDVs) (line))
    (while (listen (process-output process))
      (setf line (read-line (process-output process) nil))
      (collect (process-output-str (runProcess obj)) (cons "str" line))
      (aif (line2element line) (push-to-end it currentDVs)))
    (when (and (not (append appetizers currentDVs)) 
	     (not (equalp (format nil "~a" (process-status process)) "running")))
      (print-collector (process-output-str (runProcess obj))) 
      (assert nil))
    (append appetizers currentDVs)))

(defmethod get-DVs ((obj johnny5-run-class) &optional (process nil) (appetizers nil))
  (mklst appetizers)  
  (let ((currentDVs) (fstr) (error-p) (tbl))
    (cond ((equal (entryFnType obj) 'hash)
	   (setf tbl (make-hash-table :test #'equalp))
	   (mapc #'(lambda (x) (setf (gethash (car x) tbl) (cdr x))) (get-elements (IVKeys obj) (IVHash obj)))
	   (setf tbl (list tbl)))
	  ((equal (entryFnType obj) 'keys)
	   (setf tbl (eval (read-from-string
			    (concatenate 'string "(list "
					 (make-sentence
					  (mapcar #'(lambda (x) (format nil ":~a '~a" (car x) (cdr x))) (get-elements (IVKeys obj) (IVHash obj))))
					 ")"))))))
    (setf fstr (make-array '(0) :element-type 'base-char :fill-pointer 0 :adjustable t))
    (handler-case 
	(with-output-to-string (*standard-output* fstr) 
	  (with-output-to-string (*error-output* fstr)
	    (apply process tbl)))
      (error (condition) (setf error-p condition)))
    (dolist (line (get-lines fstr))
      (collect (process-output-str (runProcess obj)) (cons "str" line))
      (aif (line2element line) (push-to-end it currentDVs)))
    (when error-p 
      (setf (error-p (process-output-str (runProcess obj))) error-p)
      (print-collector (process-output-str (runProcess obj))) 
      (assert nil))
    (append appetizers currentDVs)))
	     			 
(defmethod wrapper-execute ((obj run-class) &optional (process nil) (appetizers nil))
  (mklst appetizers)
  (mapc #'(lambda (x) (funcall x obj)) (statusPrinters (session (runProcess obj))))
  (let ((necessaryDVs) (currentDVs) (currentDV))
    (while (setf necessaryDVs (necessaries (DVKeys obj) (DVHash obj)))
     ;loop over the current DVs and add them to the DVHash table
      (setf currentDVs (get-DVs obj process appetizers))
      (while (and necessaryDVs currentDVs)
	(setf currentDV (car currentDVs))
	(setf currentDVs (cdr currentDVs))
	(assert (member (car currentDV) necessaryDVs :test #'equalp) nil 
		"sent ~a DV for the next trial, before sending all the DVs for this trial" (car currentDV)) 
	(merge-hash currentDV :toHash (DVHash obj))
	(setf necessaryDVs (remove (car currentDV) necessaryDVs :test #'equalp)))
      (sleep (sleepTime obj))
      (setf appetizers nil))
    (collect (run-collector obj) (DVHash obj))
    (setf (DVHash obj) nil)
    currentDVs))

(defmethod wrapper-execute ((obj johnny5-runProcess-class) &optional (process nil) (appetizers nil))
  (assert (not appetizers))
  (assert (not process))
  (mapc #'(lambda (x) (funcall x obj)) (statusPrinters (session obj)))
  (let ((leftovers))
    (dolist (run (runs obj) (assert (not leftovers) nil "should not be any leftovers after runProcess object finishes"))
      (setf leftovers (wrapper-execute run (modelProgram obj) leftovers))))
  (sleep (sleepTime obj)))
  
(defmethod wrapper-execute ((obj number5-runProcess-class) &optional (process nil) (appetizers nil))
  (assert (not appetizers))
  (assert (not process))
  (mapc #'(lambda (x) (funcall x obj)) (statusPrinters (session obj)))
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
	   :output :stream :error :output :wait nil)))
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
  ;print information about the session to the terminal
  (mapc #'(lambda (x) (funcall x obj)) (statusPrinters obj))
  ;execute each runProcess
  (assert (not process))
  (assert (not appetizers))
  (dolist (runProcess (runProcesses obj))
    (wrapper-execute runProcess process appetizers))
  (assert 
   (equal 1 (apply #'* (flatten 
		       (mapcar 
			#'(lambda (runProcess) 
			    (mapcar 
			     #'(lambda (run) 
				 (if (and (equal (quot (run-collector run)) (quota (run-collector run)))
					  (equal (quot (collector (run-collector run))) (quota (collector (run-collector run))))
					  (equal (quot (session-collector (collector (run-collector run)))) 
						 (quota (session-collector (collector (run-collector run))))))
				     1 0)) (runs runProcess))) (runProcesses obj)))))
   nil "not all collectors fully executed"))

(defmacro methods (name &rest args)
  `(progn ,@(mapcar (lambda (x) `(defmethod ,name ,@x)) args)))

(methods print-unread-lines 
	 (((obj runprocess-class)) nil)
	 (((obj run-class)) nil)
	 (((obj session-class))
	  (if (not (apply #'< (traversed obj)))
	      (setf (traversed obj) (sort (remove-duplicates (traversed obj) :test #'equal) #'<)))
	  (let ((strm *error-output*))
	    (format strm "~%~a~%" "printing uncommented lines in configuration file that were not read by letf:")
	    (mapc (lambda (x) (format strm "~a~%" x))
		  (flatten 
		   (mapcar 
		    (let ((i -1) (j 0))
		      #'(lambda (x)
			  (if (equal (incf i) (nth j (traversed obj)))
			      (progn (incf j) nil)
			      (if (and (> (length x) 0) (not (equal (char x 0) #\#))) x))))
		    (configFileLnLST obj))))
	    (format strm "~%"))))

(methods print-session 
	 (((obj runprocess-class)))
	 (((obj run-class)))
	 (((obj session-class))
	  (let ((strm *error-output*))
	    (format strm "~%printing session status:~%")
	    (format strm "#####entry function: ~a~%" (modelProgram obj))
	    (format strm "#####total calls to entry function: ~a~%" (quota obj))
	    (format strm "#####number of lines in the work file: ~a~%" (lines obj))
	    (format strm "#####number of times to run each line in the work file (iterations=): ~a~%" (iterations obj))
	    (format strm "#####quota before collapsing (collapseQuota=): ~a~%" (collapseQuota obj))
	    (format strm "#####extra lisp files loaded (file2load=): ~a~%" (make-sentence (load-and-loaded nil)))
	    (if (hash-table-p (collapseHash obj))
		(format strm "#####collapse hash table (collapseFn=): ~%~a" (print-hash (collapseHash obj) :strm nil))
		(format strm "#####collapse function (collapseFn=): ~a~%" (collapseHash obj)))
	    (format strm "#####available elements in the DV hash table:~%")
	    (print-hash (DVHash obj) :keys (availables (DVKeys obj) (DVHash obj)))
	    (format strm "#####DVs that will be collected:~%")
	    (mapc (lambda (x) (format strm "~a~%" x)) (DVKeys obj))
	    (format strm "#####IVs that will be read from the work file:~%")
	    (mapc (lambda (x) (format strm "~a~%" x)) (cellKeys obj))
	    (format strm "#####elements that will be sent to the entry function:~%")
	    (mapc (lambda (x) (format strm "~a~%" x)) (IVKeys obj))
	    (format strm "#####necessary elements for the entry function to return:~%")
	    (mapc (lambda (x) (format strm "~a~%" x)) (necessaries (DVKeys obj) (DVHash obj)))
	    (format strm "~%"))))

;////////////////////////////////////////////
;hpc-specific classes, methods, and functions; all of this is to define the custom 'build-hpc-session
;function, and then call it by specifying "sessionBuilder='build-hpc-session" in the config file
(defclass hpc-work-class (work-class) ())

(defmethod initialize-instance :after ((obj hpc-work-class) &key)
  (setf (lines obj) (mapcar #'get-words (get-lines (file-string (workFilePath obj))))))

(defclass hpc-collector-class (collector-class) ())

(defmethod print-collector ((obj hpc-collector-class))
  (format t "Evaluating: ")
  (dotimes (i (length (cellElements obj)))
    (if (equal i (- (length (cellElements obj)) 1))
	(format t "~a~%" (cdr (nth i (cellElements obj))))
	(format t "~a " (cdr (nth i (cellElements obj))))))
  (dolist (key (keys obj))
    (aif (cdr (get-element key (collection obj) (gethash-ifHash key (collapseHash obj))))
	 (format t "~a: ~a~%" key (coerce it 'double-float)))))

(defclass hpc-process-output-str-class (process-output-str-class) ())

(defmethod print-collector ((obj hpc-process-output-str-class))
  (format *error-output* "model unexpectedly quit... ~%~%here are the last ~a lines that were printed to stdout before the error~%~a~%"
	  (quot obj) (make-sentence (gethash "str" (collection obj)) :spaceDesignator #\Newline))
  (if (error-p obj) (format *error-output* "here's the error~%~a~%" (error-p obj))))

(defun build-hpc-session ()
  (build-session ;this is a macro
   :collector-instance (make-instance 'hpc-collector-class)
   :work-instance (make-instance 'hpc-work-class)
   :process-output-str-instance (make-instance 'hpc-process-output-str-class)))
;////////////////////////////////////////////

;load the extra lisp files
(dolist (line (get-matching-lines (restructure (file-string (get-arg 1))) "file2load="))
  (load-and-loaded (format nil "~a" (replace-all line "$1" (get-word (get-arg 2)) :test #'equalp))))
;run it!
(let ((configFile (restructure (file-string (get-arg 1)))))
  (aif (get-matching-line configFile "albumBuilder=")
       (funcall (eval (read-from-string it)))
       (wrapper-execute 
	(funcall 
	 (eval 
	  (read-from-string 
	   (aif (get-matching-line configFile "sessionBuilder=")  it "#'build-hpc-session")))))))

;kill it!	    
(quit)



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

;;;usage: <launch common lisp image> <load letf> <pass three arguments to letf: <platform> <configFile> <workFile>>
;;;e.g.,  ./sbcl --core ./sbcl.core --noinform --noprint --disable-debugger --load letf.lisp darwin configFile.txt workFile.txt
;;;       ./sbcl --core ./sbcl.core --noinform --noprint --disable-debugger --load letf.lisp nil configFile.txt workFile.txt
;;;       ./sbcl --core ./sbcl.core --noinform --noprint --disable-debugger --load letf.lisp nil configFile.txt nil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setf *read-default-float-format* 'double-float)

;////////////////////////////////////////////////////////////
;////////////////////////////////////////////////////////////
;everything below is from Doug Hoyte's 'Let over Lambda' book (lol.lisp)
;some of which is originally from Paul Graham's 'On Lisp' book

(defmacro aif (test-form then-form &optional else-form)
  "anaphoric version of if' 'it' is the anaphor"
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro awhen (test-form &body body)
  "anaphoric version of when; 'it' is the anaphor"
  `(aif ,test-form
     (progn ,@body)))

(defmacro alambda (parms &body body)
  "Graham's anaphoric version of lambda; 'self' is the anaphor"
  `(labels ((self ,parms ,@body))
     #'self))

(defun flatten (lis)
  "Takes a nested list and makes in into a single-level list"
  (declare (list lis))
  (labels ((rec (lis acc)
             (cond ((null lis) acc)
                   ((atom lis) (cons lis acc))
                   (t (rec (car lis) (rec (cdr lis) acc))))))
    (rec lis nil)))

(defun mkstr (&rest args)
  "concatenates and converts a list of things to an uppercase string"
  (if args (format nil "~:@(~{~a~}~)" args)))

(defun symb (&rest args)
  "concatenates and converts a list of things to an interned symbol"
  (values (intern (apply #'mkstr args))))

(defun g!-symbol-p (s)
  "returns true if symbol s starts with g!"
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string= (symbol-name s)
                "G!"
                :start1 0
                :end1 2)))

(defmacro defmacro/g! (name args &body body)
  "gensym (g!) capabilities added to defmacro"
  (let ((syms (remove-duplicates
                (remove-if-not #'g!-symbol-p
                               (flatten body)))))
    (multiple-value-bind (forms decls doc) 
      #+:SBCL (sb-int:parse-body body)
      #-:SBCL (values body nil nil)
      `(defmacro ,name ,args
         ,doc
         ,@decls
         (let ,(mapcar
                (lambda (s)
                  `(,s (gensym ,(subseq
                                  (symbol-name s)
                                  2))))
                syms)
           ,@forms)))))

(defun o!-symbol-p (s)
  "returns true if symbol s starts with an o!"
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string= (symbol-name s)
                "O!"
                :start1 0
                :end1 2)))

(defun o!-symbol-to-g!-symbol (s)
  "converts an o! symbol to a g! symbol"
  (symb "G!"
        (subseq (symbol-name s) 2)))

(defmacro defmacro! (name args &body body)
  "once only (o!) and gensym (g!) capabilities added to defmacro"
  (let* ((os (remove-if-not #'o!-symbol-p args))
         (gs (mapcar #'o!-symbol-to-g!-symbol os)))
    (multiple-value-bind (forms decls doc)
      #+:SBCL (sb-int:parse-body body)
      #-:SBCL (values body nil nil)
      `(defmacro/g! ,name ,args
         ,doc
         ,@decls
         `(let ,(mapcar #'list (list ,@gs) (list ,@os))
            ,(progn ,@forms))))))

(defmacro! dlambda (&rest ds)
  "macro that returns a function that calls different user-defined functions depending on the input supplied"
  `(lambda (&rest ,g!args)
     (case (car ,g!args)
       ,@(mapcar
           (lambda (d)
             `(,(if (eq t (car d))
                  t
                  (list (car d)))
                (apply (lambda ,@(cdr d))
                       ,(if (eq t (car d))
                          g!args
                          `(cdr ,g!args)))))
           ds))))

(defun |#`-reader| (stream sub-char numarg)
  (declare (ignore sub-char))
  (unless numarg (setq numarg 1))
  `(lambda ,(loop for i from 1 to numarg
                  collect (symb 'a i))
     ,(funcall
        (get-macro-character #\`) stream nil)))

(set-dispatch-macro-character
  #\# #\` #'|#`-reader|)

(defun pandoriclet-get (letargs)
  "low-level getter function for pandoric macros"
  `(case sym
     ,@(mapcar #`((,(car a1)) ,(car a1))
               letargs)
     (t (error
          "Unknown pandoric get: ~a"
          sym))))

(defun pandoriclet-set (letargs)
  "low-level setter function for pandoric macros"
  `(case sym
     ,@(mapcar #`((,(car a1))
                  (setq ,(car a1) val))
               letargs)
     (t (error
          "Unknown pandoric set: ~a"
          sym))))

(declaim (inline get-pandoric))

(defun get-pandoric (box sym)
  "opens a pandoric closure 'box' and returns the value for 'sym'"
  (funcall box :pandoric-get sym))

(defsetf get-pandoric (box sym) (val)
         "opens a pandoric closure 'box' and sets the value for 'sym' to 'val'"
         `(progn
            (funcall ,box :pandoric-set ,sym ,val)
            ,val))

;these two macros are just awesome when you use a lot of lexical closures
;they are compliments of each other; you use 'plambda
;when writing your lexical closure (instead of lambda)
;and then when you want to process variables that the closure has
;closed over (maybe print their current value, or set
;their value to something else), you wrap your 
;section of code with a 'with-pandoric
(defmacro with-pandoric (syms box &body body)
  "opens a pandoric closure 'box' and places variables 'syms' within the scope of 'body'"
  (let ((g!box (gensym "box")))
    `(let ((,g!box ,box))
       (declare (ignorable ,g!box))
       (symbol-macrolet
         (,@(mapcar #`(,a1 (get-pandoric ,g!box ',a1))
                    syms))
         ,@body))))

(defmacro plambda (largs pargs% &body body)
  "defines a pandoric lambda that is a lexical closure over 'pargs'"
  (let ((pargs (mapcar #'list (mapcar (lambda (parg) (if (consp parg) (car parg) parg)) pargs%))))
    `(let ,(remove-if-not #'consp pargs%)
       (let (this self)
         (setq
           this (lambda ,largs ,@body)
           self (dlambda
                  (:pandoric-get (sym) ,(pandoriclet-get pargs))
                  (:pandoric-set (sym val) ,(pandoriclet-set pargs))
                  (t (&rest args) (apply this args))))))))
;////////////////////////////////////////////////////////////
;///////////////////////////////////////////end lol.lisp

;////////////////////////////////////////////////////////////
;////////////////////////////////////////////////////////////
;everything below is from Paul Graham's On Lisp

(defmacro! acond (&rest clauses)
  "works just like cond, but stores the value of each condition as 'it', which is accessable in the code following the condition"
  (if clauses
    (let ((cl1 (car clauses)))
      `(let ((,g!sym ,(car cl1)))
         (if ,g!sym
           (let ((it ,g!sym)) 
             (declare (ignorable it)) 
             ,@(cdr cl1))
           (acond ,@(cdr clauses)))))))

(defmacro aand (&rest args) 
  (cond ((null args) t)
        ((null (cdr args)) (car args)) 
        (t `(aif ,(car args) (aand ,@(cdr args))))))

(defmacro while (test &body body)
  "loops through body, evaluating test each time until test returns false"
  `(do ()
     ((not ,test)) 
     ,@body))

(defmacro push-to-end (item place)
  "analogous to the push macro; just places 'item' at the end of 'place', instead of the front"
  `(setf ,place (nconc ,place (list ,item))))

(defmacro mklst (item)
  "makes a list out of item, if necessary; does not alter the empty list 'nil'"
  `(if (not (listp ,item)) (setf ,item (list ,item))))

;////////////////////////////////////////////////////////////
;///////////////////////////////////////////end of 'on lisp'

;TODO; need to look at internals of defun, to try and mimick more of defun here
(defmacro defpun (name largs pargs &body body)
  "defines a pandoric function; syntax similar to defun"
  `(setf (symbol-function ',name)
         (plambda ,largs ,pargs 
           ,@body)))

(defvar *clean-exit-on-error* t)
(defmacro expect (test-form datum &rest arguments)
  `(unless ,test-form
     (cond (*clean-exit-on-error*
             (format *error-output* ,datum ,@arguments)
             (format *error-output* "~%")
             #+SBCL (quit :unix-status 1)
             #+CCL (quit 1))
           (t
            (error ,datum ,@arguments)))))

(defmacro! fast-concatenate (&rest lst)
  "equivalent to writing (concatenate 'string ...), but ~5x faster"
  `(with-output-to-string (,g!stream)
     ,@(mapcar (lambda (x) `(write-string ,x ,g!stream)) lst)))

(defmacro attempt (form &key (on-error `(format t "error: ~a~%" condition)))
  "anaphoric macro for attemping to evaluate form; default is to print error to screen on error"
  `(handler-case ,form (error (condition) 
                              (declare (ignorable condition))
                              ,on-error)))

(defmacro verbose (&rest lst)
  "echoes the unevaluated forms in lst to *error-output* before evaluating them"
  (mapc #'(lambda (x) (format *error-output* "~a~%" x)) lst)
  `(progn ,@lst))

(defmacro guard (fun &body body)
  "evaluates 'fun'; before returning, evaluates the guards in body (which can access fun through the anaphor 'it')"
  `(let ((it (multiple-value-call #'list ,fun)))
     ,(if (not body)
        `(expect (<= (length (car it)) 1)
                 "function ~a returned sequence ~a; default guard failed" 
                 ,(format nil "~a" fun) it)
        `(progn ,@body))
     (values-list it)))

;loads the file with pathname str
;keeps track of all the pathnames that have been sent 
;to this function; returns the list of those names
(defpun load-and-loaded (str) ((loaded))
  (push-to-end str loaded)
  (load str))

(defun key-present (key hash)
  "returns if a key is present in the hash table"
  (multiple-value-bind (value flag) (gethash key hash)
    (declare (ignore value))
    flag))

(defun symbol-function-safe (x)
  "equivalent to #'symbol-function, just safer; converts a symbol to the function that the 
  symbol points to; if it can't convert the symbol, it returns the symbol"
  (if (and (equal 'symbol (type-of x))
           (fboundp x))
    (symbol-function x)
    x))

(defun gethash-ifHash (key hash)
  "gets value of key in hash table, if hash is a hash table; otherwise, return hash"
  (if (hash-table-p hash)
    (guard (gethash key hash) (expect (key-present key hash) "key ~a not present in hash table" key))
    hash))

(defun transpose (lst)
  "converts the rows to columns (and vice versa) in a nested list"
  (if (and lst (consp lst) (consp (car lst)))
    (let ((templst) (out))
      (expect (equal (length (remove-duplicates (mapcar #'length lst) :test #'equal)) 1) "should equal 1 here")
      (dotimes (j (length (car lst)) out)
        (setf templst nil)
        (dotimes (i (length lst) (push-to-end templst out))
          (push-to-end (nth j (nth i lst)) templst))))
    lst))

(defun collapse (lst collapseFn)
  "collapses across a nested list (returning a flat list) by calling collapseFn on each column in the list"
  (if (not collapseFn)
    lst
    (if (and lst (consp lst) (consp (car lst)))
      (let ((out))
        (dolist (column (transpose lst) out)
          (push-to-end (funcall collapseFn column) out)))
      (funcall collapseFn lst))))

(defun replace-all (string part replacement &key (test #'char-equal))
  "Returns a new string in which all the occurences of the part is replaced with replacement."
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

(defun find-in-string (strng chr)
  "returns lst of indeces where any of the characters in list chr is in string strng"
  (mklst chr)
  (if (and strng (not (stringp strng))) 
    (setf strng (string strng)))
  (let ((out))
    (dotimes (i (length strng) (reverse out))
      (if
        (block check
               (dotimes (j (length chr) nil)
                 (if (char-equal (nth j chr) (char strng i))
                   (return-from check t))))
        (push i out)))))

(defun print-hash (hash &key (strm t) (keys nil))
  "recursively prints the key values in hash table hash"
  (mklst keys)
  (labels ((hash-string (hash &key (keys nil))
             (if (not (hash-table-p hash))
               (format nil "~a" hash)
               (with-output-to-string (out)
                 (loop for value being the hash-values of hash using (hash-key key) do
                       (if (or (not keys) (member key keys :test #'equalp))
                         (write-string (fast-concatenate (string #\newline) (format nil "~a" key) " -> " (hash-string value)) out)))))))
    (format strm "~a~%" (string-trim (list #\Newline #\Return #\LineFeed) (hash-string hash :keys keys)))))

(defun copy-hash (hash)
  "recursively copies the hash table hash; returns the copied hash"
  (if (hash-table-p hash)
    (let ((out (make-hash-table :test #'equalp)))
      (loop for value being the hash-values of hash using (hash-key key) do 
            (setf (gethash key out) (copy-hash value)))
      out)
    hash))

(defun merge-hash (lst &key (toHash))
  "copies all of the hash tables in the list lst, and merges them"
  (let* ((fun (lambda (out key value)
                (if (key-present key out) 
                  (expect (equalp (gethash key out) value)
                          "'~a' key already present in hash table with value '~a', which is different than the value '~a' that you're trying to add now"
                          key (gethash key out) value))
                (if (not (key-present key out))
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
  "Sucks up an entire file from PATH into a freshly-allocated string returning two values: the string and the number of bytes read."
  (if path
    (with-open-file (s path)
      (let* ((len (file-length s))
             (data (make-string len)))
        (values data (read-sequence data s))))))

(defun throwOutYerNils (&rest lsts)
  "if any column in the nested list is nil, it throws out the column"
  (let ((out))
    (if (consp (car lsts))
      (dolist (column (transpose lsts) (setf out (transpose out)))
        (if (not (member nil column))
          (push-to-end column out)))
      (if (not (member nil lsts)) (setf out lsts)))
    (apply #'values (if out out (make-sequence 'list (length lsts) :initial-element nil)))))

(defmacro inLST (left right fName throwOutYerNils)
  "shorthand macro for recursively calling a function across the lists in the nested lists left and right"
  `(if ,left (if (consp (car ,left))
               (let ((out))
                 (dolist (itm ,left)
                   (push-to-end (,fName itm ,right :throwOutYerNils ,throwOutYerNils) out))
                 (return-from ,fName (flatten out))))))

(defmacro inLSTs (left right fName throwOutYerNils)
  "shorthand macro for recursively calling a function across the lists in the nested lists left and right"
  `(progn
     (inLST ,left ,right ,fName ,throwOutYerNils)
     (inLST ,right ,left ,fName ,throwOutYerNils)))

(defmacro assertEqualLengths (l1 l2)
  "asserts that l1 and l2 are equal in length"
  `(expect (equal (length ,l1) (length ,l2)) "length ~d not equal to length ~d" (length ,l1) (length ,l2)))

(defun median (lst% &key (throwOutYerNils nil))
  "returns the median of lst, throwing out the 'nils' in list if flag is true"
  (mklst lst%)
  (if throwOutYerNils (setf lst% (throwOutYerNils lst%)))
  (if lst%
    (let* ((lst (copy-list lst%))
           (len (length lst))
           (sortedList (sort lst #'<)))
      (if (evenp len)
        (/ (+ (nth (- (/ len 2) 1) sortedList)
              (nth (/ len 2) sortedList))
           2)
        (nth (/ (- len 1) 2) sortedList)))))

(defun sum (lst &key (throwOutYerNils nil))
  "returns the sum of lst, throwing out the 'nils' in list if flag is true"
  (mklst lst)
  (if throwOutYerNils (setf lst (throwOutYerNils lst)))
  (if lst (apply #'+ lst)))

(defun mean (lst &key (throwOutYerNils nil))
  "returns the mean of lst, throwing out the 'nils' in list if flag is true"
  (mklst lst)
  (if throwOutYerNils (setf lst (throwOutYerNils lst)))
  (if lst (/ (apply #'+ lst) (length lst))))

(defun MAD (l1 l2 &key (throwOutYerNils nil))
  "returns the mean absolute deviation of l1 and l2, throwing out the l1/l2 element pairs if either is 'nil', if flag is true"
  (inLSTs l1 l2 MAD throwOutYerNils)
  (assertEqualLengths l1 l2)
  (if throwOutYerNils (multiple-value-setq (l1 l2) (throwOutYerNils l1 l2)))
  (if l1 (/ (apply #'+ (mapcar (lambda (x y) (abs (- x y))) l1 l2)) (length l1))))

(defun RMSE (l1 l2 &key (throwOutYerNils nil))
  "returns the root mean squared deviation of l1 and l2, throwing out the l1/l2 element pairs if either is 'nil', if flag is true"
  (inLSTs l1 l2 RMSE throwOutYerNils)
  (assertEqualLengths l1 l2)
  (if throwOutYerNils (multiple-value-setq (l1 l2) (throwOutYerNils l1 l2)))
  (if l1 (sqrt (/ (apply #'+ (mapcar (lambda (x y) (* (- x y) (- x y))) l1 l2)) (length l1)))))

(defun correl (l1 l2 &key (throwOutYerNils nil))
  "returns the correlation of l1 and l2, throwing out the l1/l2 element pairs if either is 'nil', if flag is true"
  (inLSTs l1 l2 correl throwOutYerNils)
  (labels ((std (lst)
             (sqrt (/ (apply #'+ (mapcar 
                                   (let ((x (/ (apply #'+ lst) (length lst))))
                                     (lambda (y) (* (- y x) (- y x))))
                                   lst))
                      (- (length lst) 1)))))
    (assertEqualLengths l1 l2)
    (ignore-errors
      (if throwOutYerNils (multiple-value-setq (l1 l2) (throwOutYerNils l1 l2)))
      (let ((out
              (if (> (length l1) 1)
                (/ (apply #'+ (mapcar #'* 
                                      (mapcar (let ((x (/ (apply #'+ l1) (length l1))))
                                                (lambda (y) (- y x))) 
                                              l1)
                                      (mapcar (let ((x (/ (apply #'+ l2) (length l2))))
                                                (lambda (y) (- y x))) 
                                              l2)))
                   (* (- (length l1) 1) (std l1) (std l2))))))
        (if out (if (not (or (> 0 out) (< 0 out) (equal 0 out))) (setf out nil)))
        out))))

(defun get-words (str &key (spaceDesignators (list #\Space #\Tab)) (includeSpaceDesignators nil))
  "returns a list of the words in str"
  (if str (setf str (string-trim (list #\Space #\Tab) str))) ;yes, these should be hardcoded to space and tab
  (let ((out) (start) (in-the-white))
    (mklst spaceDesignators)
    (expect spaceDesignators "must be some spaceDesignators")
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
          (if includeSpaceDesignators (push-to-end (string (char str i)) out)))
        (when in-the-white
          (setf in-the-white nil)
          (setf start i))))))

(defmacro get-word (&rest lst)
  "just like get words, but asserts that at most one word can be found. Returns that word, and not a list of words"
  `(car (guard (get-words ,@lst))))

(defun get-objects (str)
  "returns a list of the lisp objects in str; an object is anything that can be evaled by the lisp reader, e.g., 5, or (+ 3 2) or (+ (+ 1) 2)"
  (let ((startIndex 0)
        (out))
    (while (< startIndex (length str))
      (multiple-value-bind (it index) (read-from-string str nil nil :start startIndex :preserve-whitespace t)
        (if it (push-to-end (subseq str startIndex index) out))
        (setf startIndex index)))
    out))

(defmacro get-object (&rest lst)
  "just like get-objects, but asserts that at most one object can be found. Returns that object, and not a list of objects"
  `(car (guard (get-objects ,@lst))))

(defun eval-objects (str)
  "evals all of the lisp objects in str"
  (mapcar (lambda (x) (eval (read-from-string x)))
          (get-objects str)))

(defmacro eval-object (&rest lst)
  "just like eval-objects, but asserts that at most one object can be evaled. Returns that evaled object, and not a list of objects"
  `(car (guard (eval-objects ,@lst))))

(defun get-lines (str &key (lineDesignators (list #\Newline #\Return #\LineFeed)) (includeLineDesignators nil))
  "returns a list of the lines in str"
  (let ((out))
    (dolist (line (get-words str :spaceDesignators lineDesignators :includeSpaceDesignators includeLineDesignators) (reverse out))
      (push (string-trim (list #\Space #\Tab) line) out))))

(defun make-sentence (lst &key (spaceDesignator " "))
  "takes a list of strings, and returns a single string with single whitespaces between each word"
  (if (not (stringp spaceDesignator))
    (setf spaceDesignator (string spaceDesignator)))
  (mklst lst)
  (if lst
    (with-output-to-string (out)
      (let ((flag))
        (dolist (item lst)
          (when (> (length item) 0)
            (if flag
              (write-string spaceDesignator out)
              (setf flag t))
            (write-string item out)))))))

(defun lump-brackets (str &key (desigs (list #\[ #\])) (include-brackets t))
  "like get-words, returns a list of the words in str however, all words that are within brackets are lumped together as one word;
  if there are no brackets in str, then lump-brackets is equivalent to get-words"
  (if (not desigs) (return-from lump-brackets (get-words str)))
  (expect (equal (length desigs) 2) "should be 2 brackets here")
  (let ((out) (in-bracket) (lump))
    (dolist (word (get-words str :spaceDesignators desigs :includeSpaceDesignators t))
      (if (not in-bracket) (setf lump nil))
      (if (find-in-string (char word 0) desigs)
        (progn
          (expect (char-equal (char word 0) (if in-bracket (second desigs) (first desigs)))
                  "something is wrong with string ~a, possibly one bracket pair is nested within another bracket pair, which is not allowed" str)
          (setf in-bracket (not in-bracket))
          (if include-brackets (push-to-end word lump)))
        (if in-bracket (push-to-end word lump)))
      (if (not in-bracket) (push-to-end (if lump (make-sentence lump :spaceDesignator "") (get-words word)) out)))
    (expect (not in-bracket) "something is wrong with string ~a, possibly a stray bracket somewhere" str)
    (flatten out)))

(defun toString (lst)
  "converts a list of lisp objects to string representations that can be evaled to return the objects"
  (cond ((consp lst)
         (with-output-to-string (out)
           (write-string "(list " out)
           (dolist (item lst)
             (write-string (toString item) out)
             (write-string " " out))
           (write-string ") " out)))
        ((stringp lst)
         (fast-concatenate "\"" lst "\""))
        ((keywordp lst)
         (fast-concatenate ":" (string lst)))
        ((symbolp lst)
         (fast-concatenate "'" (string lst)))
        (t
         (format nil "~a" (coerce lst 'double-float)))))

(defmacro remap-string (&body body)
  "expands the string expression in body, by inserting all hash keynames written inside brackets [] in body to their values"
  `(alambda (str hash &key (collapseFn "#'mean") (inside-brackets nil) (key nil))
     (if inside-brackets
       ;remaps an expression surrounded by brackets by calling the hash table on each of the words in the expression
       ;each word in the expression should be a key that corresponds to an already-defined element in the hash table
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
             (setf out (fast-concatenate
                         "(funcall #'collapse (list " (make-sentence out) " ) " (make-sentence (gethash-ifHash key collapseFn)) " )")))
           (progn
             (dolist (word (lump-brackets str))
               (push-to-end
                 (if (and (equal (char word 0) #\[) (equal (char word (- (length word) 1)) #\]))
                   (self (string-trim "[]" word) hash :collapseFn collapseFn :inside-brackets t :key nil)
                   word)
                 out))
             (setf out (make-sentence out))))
         out))))

(defun traverse (keys hash &key (bool nil) (collapseFn "#'mean"))
  "traverses the 'keys' in the hash table 'hash', and recursively
  searches the other keys that each 'key' references. During the traversal,
  checks if keys are present not-present, and builds a list of those keys
  if bool is t, returns the list of keys that are present; if bool is nil,
  returns the list of keys that aren't present"
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
        (funcall fun (fast-concatenate "[" key "]") hash :collapseFn collapseFn) 
        str))
    (values (sort (flatten words) #'string<) str)))

(defmacro necessaries (keys hash &rest args)
  "returns the subset of keys that are needed to 'remap' the keys in 'keys' that do not currently have values associated with them"
  `(traverse ,keys ,hash :bool nil ,@args))

(defmacro availables (keys hash &rest args)
  "returns the subset of keys that are needed to 'remap' the keys in 'keys' that already have values associated with them"
  `(traverse ,keys ,hash :bool t ,@args))

(defmethod eval-hash ((hash hash-table))	     
  "evaluates all the stuff in the hash table that it can, given the current state of the hash table"
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
            (funcall fun (fast-concatenate "[" key "]") hash)))))

(defun bracket-expand (str &optional (inside-brackets nil))
  "takes an expression, and expands the ':'s (similar to how matlab references arrays)
  for example: (bracket-expand \"hello1:5\") -> \"hello1 hello2 hello3 hello4 hello5\"
  (bracket-expand \"1:5hello\") -> \"1hello 2hello 3hello 4hello 5hello\""
  (labels ((num-indeces (str direction)
             (expect (or (string-equal direction "fromLeft") (string-equal direction "fromRight") (string-equal direction "both"))
                     "invalid direction ~a" direction)
             (let ((out) (numIndeces) (index -1))
               (dotimes (i (length str))
                 (if (numberp (read-from-string (string (char str i))))
                   (push i numindeces)))
               (if (string-equal direction "both") (return-from num-indeces (reverse numIndeces))) 
               (if (string-equal direction "fromLeft") (setf numIndeces (reverse numIndeces)))
               (setf index (if (string-equal direction "fromLeft") 0 (- (length str) 1)))
               (while (if numIndeces (equal (car numIndeces) index))
                 (push index out)
                 (if (string-equal direction "fromLeft") (incf index) (decf index))
                 (setf numindeces (cdr numIndeces)))
               (if (string-equal direction "fromLeft") (reverse out) out))))
    (let ((out))
      (if (> (length str) 0)
        (if (not inside-brackets)
          (dolist (word (lump-brackets str))
            (push-to-end
              (if (and (equal (char word 0) #\[) (equal (char word (- (length word) 1)) #\]))
                (fast-concatenate "[" (bracket-expand (string-trim "[]" word) t) "]")
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
                    (fast-concatenate wordPrev (write-to-string (+ numPrev j)) wordNext) 
                    out)))))))
      (make-sentence (flatten out)))))

(defmacro process-matching-lines (&body body)
  "Returns a function that can be used to do some sort of processing on lines in str matched by keys
  See the two functions below for examples on how currying is used to define different types of processing
  that can be done on matched lines"
  `(lambda (str keys)
     (mklst keys)
     (let ((words) (out) (line) (lines))
       (setf lines (if (consp str) str (get-lines str)))
       (dotimes (i (length lines) out)
         (setf line (nth i lines))
         (setf words (if (consp line) line (get-words line)))
         (if words
           (awhen (block index=
                         (dolist (key keys nil)
                           (if (string-equal key (subseq (first words) 0 (min (length (first words)) (length key))))
                             (return-from index= (length key)))))
                  (let ((RHS
                          (bracket-expand 
                            (string-trim 
                              (list #\Space #\tab) 
                              (subseq (make-sentence line) it (length (make-sentence line)))))))
                    ;Section where currying is used to change the behavior of the function that will be returned
                    ,(if body 
                       `(progn ,@body)
                       `(push-to-end RHS out))))))))) ;default behavior is to push the RHS match onto out, and return out

;Keep track of the line numbers for all of the lines that have been
;returned from calling the functions below
;Do this by closing over the traversed variable, and making the two functions pandoric
(let ((traversed))

  (defpun get-matching-lines (str keys) (traversed)
    "Returns a list of RHS's of lines that start with 'key', where 'key' is any key in 'keys', ordered left to right in the string"
    (funcall (process-matching-lines
               (push-to-end i traversed)
               (push-to-end RHS out))
             str keys))

  (defpun get-matching-lines-full (str keys) (traversed)
    "Similar to the function above, except this one returns a few extra items for each match"
    (funcall (process-matching-lines
               (push-to-end i traversed)
               (push-to-end (list (subseq (make-sentence line) 0 it)
                                  RHS)
                            out))
             str keys)))

(defun get-first-word-from-matching-lines (str keys)
  "gets the RHS from lines in string 'str' where the LHS matches a key in 'keys'" 
  (mklst keys)
  (let ((out))
    (dolist (line (get-matching-lines str keys) out)
      (push-to-end
        (car (guard (get-words line) 
                    (expect (> (length (car it)) 0) "no rhs for line in config file using keys ~a" keys)))
        out))))

(defmacro get-matching-line (&rest lst)
  "same as get-matching-lines, just asserts the expectation that only one (or zero) lines should be returned"
  `(car (guard (get-matching-lines ,@lst))))

(defun get-elements (keys hash &key (collapseFns "#'mean") (eval-val-p t))
  "returns the list of elements (key . value) from the hash table 'hash' specified by 'keys'
  will evaluate each value before putting it in the list"
  (mklst keys)
  (if (not (consp collapseFns)) (setf collapseFns (make-list (length keys) :initial-element collapseFns)))
  (let ((out) (key) (collapseFn))
    (dotimes (i (length keys) out)
      (setf key (nth i keys))
      (setf collapseFn (nth i collapseFns))
      (multiple-value-bind (words val) 
        (guard (necessaries key hash :collapseFn collapseFn)
               (expect (equal (length (car it)) 0)
                       "necessaries ~a left over when calling get-elements; not allowed to have any necessaries here" (car it)))
        (declare (ignore words))
        (push-to-end (cons key (if eval-val-p (eval (read-from-string (first val))) (first val))) out)))))

(defmacro get-element (&rest lst)
  "same as get-elements, but asserts that at most one element can be returned; returns that element (or nil), and not a list"
  `(car (guard (get-elements ,@lst))))

(defmacro add-dependent-element (&body body)
  "recursively adds all elements in the config file that are referenced in the 'lhs' line in the config file"
  `(alambda (hash &optional (configFileStr nil) (lhs nil) (rhs nil))
     (let ((line) (words))
       (setf line (aif rhs it (get-matching-line configFileStr lhs)))
       (when line
         (merge-hash (cons (subseq lhs 0 (- (length lhs) 1)) line) :toHash hash)
         (setf words (lump-brackets line))
         (dolist (word words)
           (if (and (equal (char word 0) #\[) (equal (char word (- (length word) 1)) #\]))
             (dolist (item (get-words (string-trim "[]" word)))
               ,(if (not body)
                  `(self hash configFileStr (fast-concatenate item "="))
                  `(progn ,@body)))))))))

(defmethod add-dependent-elements ((hash hash-table) &optional (configFileStr nil) (lhs nil))
  "recursively adds all elements in the config file that are referenced in the list of 'lhs' lines in the config file"
  (let* ((traversed (make-hash-table :test #'equalp))
         (fun (add-dependent-element
                (when (not (key-present item traversed))
                  (setf (gethash item traversed) t)
                  (self hash configFileStr (fast-concatenate item "="))))))
    (dolist (word (get-first-word-from-matching-lines configFileStr lhs))
      (funcall fun hash configFileStr (fast-concatenate word "=")))))

(defclass work-class ()
  ((lines :accessor lines :initarg :lines :initform nil
          :documentation "stores all of the IV configs to run in a nested list; each line is an IV config")
   (workFilePath :accessor workFilePath :initarg :workFilePath :initform nil
                 :documentation "stores the path to the work file as a string"))
  (:documentation "work-class is responsible for loading and storing the points to run"))

(defclass session-class ()
  ((runProcesses :accessor runProcesses :initarg :runProcesses :initform nil
                 :documentation "each of the runProcess objects for the session")
   (quota :accessor quota :initarg :quota :initform nil
          :documentation "quota of the session; that is, total number of runs that the session will execute")
   (traversed :accessor traversed :initarg :traversed :initform nil
              :documentation "lines in the config file that were touched to build the job")
   (configFileLnLST :accessor configFileLnLST :initarg :configFileLnLST :initform nil
                    :documentation "config file converted to a list of strings, where each item in the list is a line in the config")
   (statusPrinters :accessor statusPrinters :initarg :statusPrinters :initform nil
                   :documentation "all of the functions/methods that will be called before the session object is executed")
   (collapseQuota :accessor collapseQuota :initarg :collapseQuota :initform nil
                  :documentation "number of runs (with same IV config) before the result is collapsed; handles stochastic models")
   (iterations :accessor iterations :initarg :iterations :initform nil
               :documentation "number of times each collapsed set of runs will be executed")
   (lines :accessor lines :initarg :lines :initform nil
          :documentation "number of lines in the work file")
   (collapseHash :accessor collapseHash :initarg :collapseHash :initform nil
                 :documentation "stores all information related to collapsing functions for each DV")
   (DVHash :accessor DVHash :initarg :DVHash :initform nil
           :documentation "stores all information related to the way that each DV will be calculated")
   (DVKeys :accessor DVKeys :initarg :DVKeys :initform nil
           :documentation "stores names for DVs")
   (IVKeys :accessor IVKeys :initarg :IVKeys :initform nil
           :documentation "stores names for IVs")
   (cellKeys :accessor cellKeys :initarg :cellKeys :initform nil
             :documentation "stores names for columns in work file; usually, this will match IVKeys unless you are remapping the inputs")
   (modelProgram :accessor modelProgram :initarg :modelProgram :initform nil
                 :documentation "entry function that will get executed to run the model")
   (runsPerProcess :accessor runsPerProcess :initarg :runsPerProcess :initform nil
                   :documentation "if the model is launched as a separate process, determines number of runs that will be sent every time the process is spawned")
   (entryFnType :accessor entryFnType :initarg :entryFnType :initform nil
                :documentation "if the model is lisp-based, determines the interface for the entry function")
   (IVStringFn :accessor IVStringFn :initarg :IVStringFn :initform nil
               :documentation "if the model is launched as a separate process, determines the interface for the entry function"))
  (:documentation "session-class is responsible for storing all information related to the job, and executing it"))

(defclass base-collector-class ()
  ((quota :accessor quota :initarg :quota :initform 1
          :documentation "quota of the collector; when quot reaches quota, the print method on the collector will fire")
   (quot :accessor quot :initarg :quot :initform 0
         :documentation "current number of items that the collector has collected")
   (collection :accessor collection :initarg :collection :initform (make-hash-table :test #'equalp)))
  (:documentation "base class for all tasks involving collecting and printing results"))

(defmethod collect ((obj base-collector-class) (lst list))
  "pushes all values (cdrs lst) on the base collector, using keys (cars lst)"
  (expect (not (> (quot obj) (quota obj))) "quot ~a is higher than quota ~a" (quot obj) (quota obj))
  (if lst (if (not (consp (car lst))) (setf lst (list lst))))
  (dolist (item lst)
    (push-to-end (cdr item) (gethash (car item) (collection obj))))
  (incf (quot obj)))

(defmethod collect ((obj base-collector-class) (DVHash hash-table))
  "pushes all values (values hash) on teh base collector, using keys (keys hash)"
  (let ((out))
    (maphash #'(lambda (key value) (push-to-end (cons key value) out)) DVHash)
    (collect obj out)))

(defmethod print-collector ((obj base-collector-class)) 
  "stub method; will be overridden by any extension of the base-collector-class"
  ())

(defmethod print-collector :after ((obj base-collector-class))
  "method to clear out the collection (uses garbage collection to reallocate memory)"
  (setf (collection obj) (make-hash-table :test #'equalp)))

(defclass process-output-str-class (base-collector-class)
  ((error-p :accessor error-p :initarg :error-p :initform nil
            :documentation "stores the error object, if the model errors out"))
  (:documentation "extendable collector class for printing the last N lines that have been returned by the model, if the model errors out"))

(defmethod collect :after ((obj process-output-str-class) (lst list))
  "method to keep track of the last N lines that have been returned by the model"
  (declare (ignore lst))
  (when (> (quot obj) (quota obj))
    (setf (gethash "str" (collection obj)) (rest (gethash "str" (collection obj))))
    (decf (quot obj))))

(defclass session-collector-class (base-collector-class)
  ((collectors :accessor collectors :initarg :collectors :initform nil
               :documentation "stores all instantiated collector objects that are part of this session-object"))
  (:documentation "exendable collector class for printing the collected results after the entire session is run"))

(defmethod collect :after ((obj session-collector-class) (lst list))
  "method to fire the print-collector method after the quota has been reached"
  (declare (ignore lst))
  (if (equal (quota obj) (quot obj))
    (print-collector obj)))

(defclass collector-class (base-collector-class)
  ((collapseHash :accessor collapseHash :initarg :collapseHash :initform nil
                 :documentation "stores a hash table that determines how the items in the collection will be collapsed")
   (keys :accessor keys :initarg :keys :initform nil
         :documentation "stores the keynames for each DV in the config file")
   (cellElements :accessor cellElements :initarg :cellElements :initform nil
                 :documentation "stores the values for each column in a single row of the work file")
   (run-collectors :accessor run-collectors :initarg :run-collectors :initform nil
                   :documentation "stores all instantiated run-collector objects that are part of this collector-object")
   (session-collector :accessor session-collector :initarg :session-collector :initform nil
                      :documentation "stores the session-collector object that this collector-object is part of"))
  (:documentation "extendable collector class for printing the collected results after each collapseQuota is reached"))

(defmethod initialize-instance :after ((obj collector-class) &key)
  "registers this collector object in its parent session-collector object" 
  (expect (session-collector obj) "should have a session-collector here")
  (push-to-end obj (collectors (session-collector obj))))

(defmethod collect :after ((obj collector-class) (lst list))
  "method to fire the print-collector method after the quota has been reached;
  will collapse the lists in the collector (using collapseHash) before calling print-collector;
  also fires the collect method on the session-collector object"
  (declare (ignore lst))
  (when (equal (quota obj) (quot obj))
    ;short-circuit to optimize recursion called later
    ;pass over all of the elements in the hash table, and
    ;if an element is a list and if all slots in that list are equal, then collapse the list
    (loop for key being the hash-keys of (collection obj)	
          using (hash-value val)
          do (if (and (consp val) 
                      (equal 1 (length (remove-duplicates val :test #'equalp))))
               (setf (gethash key (collection obj)) (first val))))
    (let ((elements (get-elements (keys obj) (collection obj) 
                                  :collapseFns (mapcar (lambda (x) (gethash-ifHash x (collapseHash obj))) (keys obj)))))
      (print-collector obj)
      (collect (session-collector obj) elements))))

;TODO; where is the 'runs' slot here used?
(defclass run-collector-class (base-collector-class)
  ((runs :accessor runs :initarg :runs :initform nil)
   (collector :accessor collector :initarg :collector :initform nil
              :documentation "stores the collector-object that this run-collector object is part of"))
  (:documentation "extendable collector class for printing the collected results after each call to the entry function"))

(defmethod initialize-instance :after ((obj run-collector-class) &key)
  "registers this run-collector object in its parent session-collector object"
  (expect (collector obj) "should have a collector here")
  (push-to-end obj (run-collectors (collector obj))))

(defmethod collect :after ((obj run-collector-class) (lst list))
  "method will fire the print-collector method after the quota has been reached;
  also fires the collect method on the collector object"
  (expect (equal (quot obj) 1) "should only collect once; quot is ~a" (quot obj))
  (expect (equal (quota obj) 1) "quota should be one; quota is ~a" (quota obj))
  (print-collector obj)
  (collect (collector obj) lst))

(defclass runProcess-class ()
  ((runs :accessor runs :initarg :runs :initform nil
         :documentation "stores all of the run objects for this run process")
   (session :accessor session :initarg :session :initform nil
            :documentation "stores the session object that this runProcess object is part of")
   (process :accessor process :initarg :process :initform nil
            :documentation "stores a pointer to the process that is spawned, if running the model by launching a separate process")
   (modelProgram :accessor modelProgram :initarg :modelProgram :initform nil
                 :documentation "stores the call to run the entryFn for the model")
   (platform :accessor platform :initarg :platform :initform nil
             :documentation "stores the platform that the program is running on")
   (process-output-str :accessor process-output-str :initarg :process-output-str :initform nil
                       :documentation "stores the process-output-str object that is keeping track of the last N lines printed by the process (model)"))
  (:documentation 
   "class for a single process; if we're short-circuiting (i.e., calling a lisp-native model using an entry function)
    then there will only be 1 instance of this class; if we're launching the model as a separate process, then
    the number of instances will be (ceiling (total runs / runs per process))"))

(defclass number5-runProcess-class (runProcess-class)
  ((sleepTime :accessor sleepTime :initarg :sleepTime :initform .2
              :documentation "amount of time (in seconds) the program will sleep between checking the strm for model output (using a polling solution)"))
  (:documentation "class for a single process if we're not short circuiting"))

(defclass johnny5-runProcess-class (runProcess-class)
  ((sleepTime :accessor sleepTime :initarg :sleepTime :initform 0
              :documentation "should be 0s when short circuiting b/c all of the model's output will be on the strm on the first check of strm"))
  (:documentation "class for a single process if we're short circuiting"))

(defclass run-class ()
  ((IVHash :accessor IVHash :initarg :IVHash :initform nil
           :documentation "stores a hash table containing all IV key->values for the run")
   (DVHash :accessor DVHash :initarg :DVHash :initform nil
           :documentation "stores a hash table containing all DV key->calues for the run")
   (IVKeys :accessor IVKeys :initarg :IVKeys :initform nil
           :documentation "stores a list of keynames for the IVs")
   (DVKeys :accessor DVKeys :initarg :DVKeys :initform nil
           :documentation "stores a list of keynames for the DVs")
   (quot :accessor quot :initarg :quot :initform nil
         :documentation "counter for the number of runs contained in the class (should equal 1 after the run finishes)")
   (cellKeys :accessor cellKeys :initarg :cellKeys :initform nil
             :documentation "stores a list of keynames for the raw IVs in the work file")
   (sleepTime :accessor sleepTime :initarg :sleepTime :initform 0
              :documentation "amount of time to sleep after the run is completed (should be zero)")
   (run-collector :accessor run-collector :initarg :run-collector :initform nil
                  :documentation "stores the run-collector object that this run-class is part of")
   (runProcess :accessor runProcess :initarg :runProcess :initform nil
               :documentation "stores the runProcess object that this run-class is part of")
   (entryFnType :accessor entryFnType :initarg :entryFnType :initform nil
                :documentation "either 'keys or 'hash; defines the interface between the model & this wrapper"))
  (:documentation "base class for a single run"))

(defmethod initialize-instance :after ((obj run-class) &key)
  "registers this run-class object in its parent run-collector object"
  (expect (run-collector obj) "should have a run-collector here")
  (expect (not (runs (run-collector obj))) "should not have any run in the run-collector yet")
  (push-to-end obj (runs (run-collector obj))))

(defclass number5-run-class (run-class) 
  ()
  (:documentation "class for a single run, if not short circuiting"))

(defclass johnny5-run-class (run-class) 
  ()
  (:documentation "class for a single run, if short circuiting"))


(defclass process-class ()
  ((process :accessor process :initarg :process :initform nil)))

(defmethod p-status ((obj process-class))
  (funcall
    #+CCL #'external-process-status
    #-CCL #'process-status 
    (process obj)))

(defmethod p-output ((obj process-class))
  (funcall
    #+CCL #'external-process-output-stream
    #-CCL #'process-output 
    (process obj)))

(defmethod p-exit-code ((obj process-class))
  (funcall
    #+CCL #'external-process-status
    #-CCL #'process-exit-code 
    (process obj)))

(defmethod get-process ((obj number5-runProcess-class) input)
  (let ((process (make-instance 'process-class)))
    ;Launch the process
    (setf (process process)
          (run-program 
            (first (modelProgram obj)) 
            (rest (modelProgram obj))
            :input (make-string-input-stream input)
            :output :stream 
            :error :output
            #+SBCL :search #+SBCL t
            :wait nil))
    (expect (equal (p-status process) :running) "model process failed to start correctly")
    process))

;////helper functions and macros for build-session macro////

(defun restructure (str)
  "reattaches lines that have been separated using the \ character at the end of a line"
  (let ((lineDesigs (list #\Newline #\Return #\LineFeed))
        (count -1)
        (strLength (length str)))
    (with-output-to-string (out)
      (while (< (incf count) (- strLength 1))
        (if (and (find-in-string (char str count) #\\)
                 (find-in-string (char str (+ count 1)) lineDesigs))
          (while (and (< (+ 1 count) (- strLength 1))
                      (find-in-string (char str (+ 1 count)) lineDesigs))
                 (incf count))
          (write-string str out :start count :end (+ 1 count))))
      (if (equal count (- strLength 1))
        (write-string str out :start count :end (+ 1 count))))))

(defmethod mod-collapseHash ((hash hash-table) collapseFn &key (DVKeys nil) (ApplyToKeys nil))
  "modifies the collapse hash table that keeps track of how letf will collapse each DV when collapseQuota is reached"
  (labels ((keys (hash)
             (let ((out))
               (loop for value being the hash-values of hash using (hash-key key) do 
                     (push-to-end key out))
               out)))
    (dolist (DVKey (aif DVKeys it (keys hash)))
      (expect (key-present DVKey hash) "DV=~a not present in collapse hash table" DVKey)
      (dolist (ApplyToKey (aif ApplyToKeys it (keys (gethash DVKey hash))))
        (if (not (key-present ApplyToKey (gethash DVKey hash)))
          (expect (not DVKeys) "ApplyToKey=~a not present in DV=~a collapse hash table" ApplyToKey DVKey)
          (setf (gethash ApplyToKey (gethash DVKey hash)) collapseFn))))))

(defun get-collapseHash (DVKeys DVHash configFileStr defaultCollapseFn)
  "builds the collapse hash table by parsing the config file"
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
        (cons DVKey (merge-hash 
                      (mapcar #'(lambda (x) (cons x defaultCollapseFn)) 
                              (append (necessaries DVKey DVHash) (availables DVKey DVHash)))))
        :toHash hash))
    (dolist (line (get-matching-lines configFileStr "collapseFn=") hash)
      (setf words (get-words line :spaceDesignators (list #\; #\&)))
      (setf tmpLn (make-sentence (rest words) :spaceDesignator #\Newline))
      (expect (equal (length (rest words)) (length (get-matching-lines tmpLn (list "DV=" "SDV=" "ApplyTo="))))
              "line ~a not valid" line)
      (mod-collapseHash 
        hash
        (let ((hsh (make-hash-table :test #'equalp)))
          (funcall (add-dependent-element) hsh configFileStr "collapseFn=" (first words))
          (funcall (remap-string) (fast-concatenate "[" "collapseFn" "]") hsh))
        :DVKeys (get-words (bracket-expand (get-matching-line tmpLn (list "DV=" "SDV=")) t))
        :ApplyToKeys (get-words (bracket-expand (get-matching-line tmpLn "ApplyTo=") t))))))

(defmacro upload-to (obj &body vals)
  "uploads vals to obj by setting the slots in obj (named vals) to the value of vals"
  `(progn ,@(mapcar 
              (lambda (x) 
                (if (consp x)
                  `(setf (,(car x) ,obj) ,(cadr x))
                  `(setf (,x ,obj) ,x))) vals)))

(defun my-command-line ()
  "This function returns the command line parameters, accounting for differences across several lisp implementations"
  (or 
    #+SBCL *posix-argv*
    #+LISPWORKS system:*line-arguments-list*
    #+CMU extensions:*command-line-strings*
    #+CCL *unprocessed-command-line-arguments*
    nil))

(defun get-arg ( from-right )
  "This function returns the nth command line argument from right to left (note that this is the reverse of normal)"
  (nth (- (length (my-command-line)) (+ 1 from-right)) (my-command-line)))

;defines the lexical closure 'args that stores all of the information that was passed to letf using command-line arguments 
;'args is pandoric, so you can access its state using 'with-pandoric
(defpun args () ((platform (get-word (get-arg 2)))
                 (configFilePath (get-arg 1))
                 (configFileStr)
                 (configFileLnLST)
                 (configFileWdLST)
                 (workFilePath (unless (string-equal (get-arg 0) "nil") (get-arg 0))))
        (setf configFileStr (restructure (file-string configFilePath)))
        (setf configFileLnLST (get-lines configFileStr))
        (setf configFileWdLST (mapcar #'get-words configFileLnLST)))

(args) ;initialize configFile Str/LnLST/WdLST to defaults

; This macro is configured by extending various pieces of the above
; object-oriented hierarchy and sending constructors to those new pieces
; as inputs to the 'build-session macro call 
(defmacro build-session (&key 
                          (collector-instance `(make-instance 'collector-class)) 
                          (work-instance `(make-instance 'work-class))
                          (session-collector-instance `(make-instance 'session-collector-class))
                          (process-output-str-instance `(make-instance 'process-output-str-class))
                          (run-collector-instance `(make-instance 'run-collector-class))
                          (johnny5-runProcess-instance `(make-instance 'johnny5-runProcess-class))
                          (number5-runProcess-instance `(make-instance 'number5-runProcess-class))
                          (johnny5-run-instance `(make-instance 'johnny5-run-class))
                          (number5-run-instance `(make-instance 'number5-run-class)))
  "generates the code that generates the session object that will be executed"
  (setf collector-instance (append collector-instance
                                   '(:cellElements (get-elements cellKeys IVHash :eval-val-p nil)
                                     :keys DVKeys
                                     :quota quota
                                     :collapseHash (copy-hash collapseHash)
                                     :session-collector session-collector)))
  (setf work-instance (append work-instance `(:workFilePath (get-pandoric #'args 'workFilePath))))
  (setf session-collector-instance (append session-collector-instance '(:quota (* (length (lines work)) iterations))))
  (setf run-collector-instance (append run-collector-instance '(:quota 1 :collector collector)))
  (setf process-output-str-instance (append process-output-str-instance '(:quota 200)))
  (let* ((session-instance `(make-instance 'session-class))
         (run-process-default-initargs `(:modelProgram modelProgram :platform platform
                                         :process-output-str ,process-output-str-instance
                                         :session session))
         (run-process-instance `(if short-circuit-p
                                  ,(append johnny5-runProcess-instance run-process-default-initargs)
                                  ,(append number5-runProcess-instance run-process-default-initargs)))
         (run-default-initargs `(:IVHash (copy-hash IVHash)
                                 :DVHash (copy-hash mergedHash)
                                 :IVKeys IVKeys
                                 :DVKeys DVKeys
                                 :quot (+ 1 count)
                                 :cellKeys cellKeys
                                 :run-collector ,run-collector-instance
                                 :runProcess runProcess
                                 :entryFnType entryFnType))
         (run-instance `(if short-circuit-p
                          ,(append johnny5-run-instance run-default-initargs)
                          ,(append number5-run-instance run-default-initargs))))
    `(progn
       (let ((session) (runProcess) (line-index) (count) (iteration) (iterations)
             (DVHash) (IVHash) (DVKeys) (IVKeys) (modelProgram) (cellKeys) (mergedHash)
             (quota) (collector) (quot) (collapseHash) (work) (session-collector) (entryFnType) (runsPerProcess) (short-circuit-p))
         (with-pandoric (platform configFileStr configFileLnLST configFileWDLST) #'args
           (setf work ,work-instance)
           (setf runsPerProcess (aif (get-matching-line configFileWdLST "runsPerProcess=")
                                  (read-from-string (get-word it))
                                  1))
           (setf session ,session-instance)
           (setf DVHash (make-hash-table :test #'equalp))
           (add-dependent-elements DVHash configFileWdLST (list "DV=" "SDV="))
           (eval-hash DVHash)
           (setf DVKeys (get-first-word-from-matching-lines configFileWdLST (list "dv=" "sdv="))
                 IVKeys (aif (get-first-word-from-matching-lines configFileWdLST "input=")
                          it
                          (get-first-word-from-matching-lines configFileWdLST (list "constant=" "iv=")))
                 cellKeys (get-first-word-from-matching-lines configFileWdLST (list "constant=" "iv=")))
           (setf collapseHash (get-collapseHash DVKeys DVHash configFileWdLST "#'mean"))
           (setf quota (aif (get-matching-line configFileWdLST "collapseQuota=")
                         (eval (read-from-string (get-word it)))
                         1))
           (setf modelProgram (aif (get-matching-line configFileLnLST "modelProgram=")
                                (lump-brackets (replace-all it "$1" platform :test #'string-equal) :desigs (list #\" #\") :include-brackets nil)
                                (lump-brackets "'run-model" :desigs (list #\" #\") :include-brackets nil)))
           (setf short-circuit-p (let ((it (subseq (first modelProgram) 0 1)))
                                   (or (equal it "#") (equal it "'") (equal it "("))))
           (if short-circuit-p
             (setf modelProgram (eval (read-from-string (make-sentence modelProgram)))
                   runsPerProcess (read-from-string "inf")))
           (setf (IVStringFn session)
                 (if (not short-circuit-p)
                   (aif (get-matching-line configFileWdLST "IVStringFn=")
                     (symbol-function-safe (eval (read-from-string it)))
                     #'defaultIVStringFn)))
           (setf entryFnType (if short-circuit-p
                               (aif (get-matching-line configFileWdLST "entryFnType=")
                                 (read-from-string (get-word it))
                                 'keys)
                               'process))
           (expect (member entryFnType (if short-circuit-p (list 'keys 'hash) (list 'process))) "invalid entryFnType ~a" entryFnType)
           (setf runProcess ,run-process-instance)
           (setf iterations (aif (get-matching-line configFileWdLST "iterations=")
                              (eval (read-from-string (get-word it)))
                              1)	
                 count 0
                 line-index -1)
           (setf session-collector ,session-collector-instance)
           (while (< (incf line-index) (length (lines work)))
             (setf iteration -1)
             (setf IVHash (make-hash-table :test #'equalp))
             (let ((cell-values (nth line-index (lines work))))
               (merge-hash
                 (mapcar #'cons
                         (guard (get-first-word-from-matching-lines configFileWdLST (list "constant=" "iv="))
                                (expect (equal (length (car it)) (length cell-values))
                                        "number of cell keys (~d) does not equal number of cell values (~d)" 
                                        (length (car it)) (length cell-values)))
                         cell-values)
                 :toHash IVHash))
             (add-dependent-elements IVHash configFileWdLST "input=")
             (setf mergedHash (merge-hash (list DVHash IVHash)))
             (if (equal line-index 0) 
               (guard (necessaries IVKeys IVHash)
                      (expect (not (car it))
                              "IVs '~a' that are necessary to evaluate the 'input=' lines are not present in the config file" (car it))))
             (while (< (incf iteration) iterations)
               (setf quot -1)
               (setf collector ,collector-instance)
               (while (< (incf quot) quota)
                 (push-to-end ,run-instance (runs runProcess))
                 (incf count)
                 (if (not (equal 'inf runsPerProcess)) 
                   (when (equal (mod count runsPerProcess) 0)
                     (push-to-end runProcess (runProcesses session))
                     (setf runProcess ,run-process-instance))))))
           (if (runs runProcess) (push-to-end runProcess (runProcesses session)))	
           (upload-to session
             (quota (* (length (lines work)) iterations quota))
             (statusPrinters (mapcar (lambda (x) (symbol-function-safe (eval (read-from-string x)))) 
                                     (get-matching-lines configFileWdLST "statusPrinter=")))
             collapseHash DVHash DVKeys IVKeys cellKeys modelProgram iterations configFileLnLST entryFnType runsPerProcess
             (collapseQuota quota)
             (lines (length (lines work)))
             (traversed (with-pandoric (traversed) #'get-matching-lines 
                          (sort (remove-duplicates traversed :test #'equal) #'<))))
           (guard (apply #'+ (mapcar #'(lambda (x) (length (runs x))) (runProcesses session)))
                  (expect (equal (quota session) (car it))
                          "number of linesxiterationsxquota in work file (~d) not equal to number of run objects (~d)"	
                          (quota session) (car it)))
           session)))))

(defun line2element (line)
  "converts an output line of text sent by the model to a dotted pair;
  discards if it's not a valid output line (handles when warning statements are printed to stdout)"
  (let ((equal-index) (key) (value))
    (setf equal-index (find-in-string line #\=))
    (when (equal (length equal-index) 1)
      (setf key (get-words (subseq line 0 (first equal-index))))
      (setf value (get-words (subseq line (+ 1 (first equal-index)) (length line))))
      (when (equal (length key) 1)
        ;(format t "key=~a value=~a~%" (first key) (first value))
        (cons (first key) (make-sentence value))))))

(defmethod get-DVs ((obj number5-run-class) &optional (process nil) (appetizers nil)) 
  "capture all the input lines that the model has sent; 
  then, remap each line as a dotted pair (key . value)"
  (expect process "should be a process here")
  (mklst appetizers)
  (let ((currentDVs) (line))
    (while (listen (p-output process))
      (setf line (read-line (p-output process) nil))
      (collect (process-output-str (runProcess obj)) (cons "str" line))
      (aif (line2element line) (push-to-end it currentDVs)))
    (append appetizers currentDVs)))

(defmethod get-DVs ((obj johnny5-run-class) &optional (process nil) (appetizers nil))
  "capture all the input lines that the model has sent; 
  then, remap each line as a dotted pair (key . value)"
  (mklst appetizers)  
  (let ((currentDVs) (fstr) (error-p) (tbl))
    (cond ((equal (entryFnType obj) 'hash)
           (setf tbl (make-hash-table :test #'equalp))
           (mapc #'(lambda (x) (setf (gethash (car x) tbl) (cdr x))) (get-elements (IVKeys obj) (IVHash obj)))
           (setf tbl (list tbl)))
          ((equal (entryFnType obj) 'keys)
           (setf tbl (flatten (mapcar (lambda (x) (list (eval (read-from-string (fast-concatenate ":" (car x)))) (cdr x)))
                                      (get-elements (IVKeys obj) (IVHash obj)))))))
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
      (error "failed to get DVs; model crashed"))
    (append appetizers currentDVs)))

(defmethod wrapper-execute ((obj johnny5-run-class) &optional (process nil) (appetizers nil))
  (mklst appetizers)
  (mapc #'(lambda (x) (funcall x obj)) (statusPrinters (session (runProcess obj))))
  (with-slots (DVKeys DVHash run-collector sleepTime) obj
    (let* ((necessaryDVs (necessaries DVKeys DVHash))
           (currentDVs (if necessaryDVs (attempt (get-DVs obj process appetizers)))))
      ;note the expected DVs that were not returned for this trial, and set their value to nil
      (awhen (sort (set-difference necessaryDVs (mapcar #'car currentDVs) :test #'string-equal) #'string<)
        (format *error-output* "failed to send all DVs for this trial; missing ~a~%" it)
        (merge-hash (mapcar (lambda (missingDV) (cons missingDV "nil")) it) :toHash DVHash))
      ;note the returned DVs for this trial that were not expected
      (awhen (sort (set-difference (mapcar #'car currentDVs) necessaryDVs :test #'string-equal) #'string<)
        (format *error-output* "sent extra DVs ~a for this trial~%" it))
      ;push all currentDVs that are in necessaryDVs onto DVHash
      (merge-hash
        (remove-if-not (lambda (x) (member (car x) necessaryDVs :test #'string-equal)) currentDVs)
        :toHash DVHash)
      (collect run-collector DVHash)
      (setf DVHash nil)
      (sleep sleepTime)
      ;not returning any leftovers
      nil)))

(defmethod wrapper-execute ((obj number5-run-class) &optional (process) (appetizers))
  (mklst appetizers)
  (mapc #'(lambda (x) (funcall x obj)) (statusPrinters (session (runProcess obj))))
  (with-slots (DVKeys DVHash run-collector sleepTime) obj
    (let ((necessaryDVs) (currentDVs) (currentDV) (process-alive-p t))
      (while (and (setf necessaryDVs (necessaries DVKeys DVHash))
                  process-alive-p)
             (if (not (equal (p-status process) :running))
               (setf process-alive-p nil))
             (setf currentDVs (get-DVs obj process appetizers))
             (setf appetizers nil)
             (while (and necessaryDVs currentDVs)
               (setf currentDV (pop currentDVs))
               (if (not (member (car currentDV) necessaryDVs :test #'string-equal))
                 (progn
                   ;log it
                   (format *error-output* "sent ~a DV for the next trial, before sending all ~a DVs left for this trial~%" (car currentDV) necessaryDVs)
                   ;make sure currentDVs is correct (undo the pop)
                   (push currentDV currentDVs)
                   ;merge nils for all necessaryDVs to the dvhash
                   (merge-hash (mapcar (lambda (missingDV) (cons missingDV "nil")) necessaryDVs) :toHash DVHash)
                   ;set necessaryDVs to nil
                   (setf necessaryDVs nil))
                 (progn
                   (merge-hash currentDV :toHash DVHash)
                   (setf necessaryDVs (remove (car currentDV) necessaryDVs :test #'string-equal))))))
      (awhen necessaryDVs
        ;it should be really difficult not to throw an error here
        (when (equal (p-exit-code process) 1)
          (print-collector (process-output-str (runProcess obj)))       
          (expect nil ""))
        (format *error-output* "failed to send all ~a DVs left for this trial~%" it)
        (merge-hash (mapcar (lambda (missingDV) (cons missingDV "nil")) it) :toHash DVHash))
      (collect run-collector DVHash)
      (setf DVHash nil)
      (sleep sleepTime)
      currentDVs)))

(defmethod wrapper-execute ((obj johnny5-runProcess-class) &optional (process nil) (appetizers nil))
  "execute the runProcess-class object, if short circuiting"
  (expect (not appetizers) "should not have any appetizers here; have ~a" appetizers)
  (expect (not process) "should not have a process here; have ~a" process)
  (mapc #'(lambda (x) (funcall x obj)) (statusPrinters (session obj)))
  (let ((leftovers))
    (dolist (run (runs obj))
      (setf leftovers (wrapper-execute run (modelProgram obj) leftovers)))
    (expect (not leftovers) "should not be any leftovers after runProcess object finishes"))
  (sleep (sleepTime obj)))


(defmethod get-input-line ((obj number5-run-class))
  (funcall (IVStringFn (session (runProcess obj))) obj))

(defmethod get-process-inputs ((obj number5-runProcess-class))
  "Returns two values: [1] Any appetizers (cached-results) and [2] the input string to send to the launched process' stdin"
  (values nil (format nil "~{~a~%~}" (mapcar #'get-input-line (runs obj)))))

(defmethod wrapper-execute ((obj number5-runProcess-class) &optional (process nil) (appetizers nil))
  "execute the runProcess-class object, if we're not short circuiting"
  (expect (not appetizers) "should not have any appetizers here; have ~a" appetizers)
  (expect (not process) "should not have a process here; have ~a" process)
  (mapc #'(lambda (x) (funcall x obj)) (statusPrinters (session obj)))
  (let ((appetizers) (input-string))
    (multiple-value-setq (appetizers input-string)
      (get-process-inputs obj))
    (setf (process obj)
          (get-process obj input-string))
    (dolist (run (runs obj))
      (setf appetizers (wrapper-execute run (process obj) appetizers)))
    (expect (not appetizers) "should not be any leftover results after runProcess object finishes"))
  (sleep (sleepTime obj))
  ;a few assertions to make sure everything finished cleanly
  (expect (not (listen (p-output (process obj))))
          "unprocessed lines remain in stdout stream of model after all DVs have been processed")
  (expect (equal (p-status (process obj)) :exited)
          "model process failed to quit after all DVs have been processed"))

(defmethod wrapper-execute ((obj session-class) &optional (process nil) (appetizers nil))
  "execute the top-level session-class object"
  ;print information about the session to the terminal
  (mapc #'(lambda (x) (funcall x obj)) (statusPrinters obj))
  ;execute each runProcess
  (expect (not appetizers) "should not have any appetizers here; have ~a" appetizers)
  (expect (not process) "should not have a process here; have ~a" process)
  (dolist (runProcess (runProcesses obj))
    (wrapper-execute runProcess process appetizers))
  (expect
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
    "not all collectors fully executed"))

(defmacro methods (name &body args)
  "shorthand for defining multiple methods that are taking advantage of dynamic dispatching;
  that is, they all have the same name, they just operate on different classes"
  ;adding a 'stub method' line to the documentation, if defining an empty stub method
  (mapc (lambda (x) (if (not (cdr x)) (setf (cdr x) `("stub method")))) args)
  `(progn ,@(mapcar (lambda (x) `(defmethod ,name ,@x)) args)))

(methods print-unread-lines 
  (((obj runprocess-class)) nil)
  (((obj run-class)) nil)
  (((obj session-class))
   "print to terminal, uncommented lines in config file that were not read"
   (if (not (apply #'< (traversed obj)))
     (setf (traversed obj) (sort (remove-duplicates (traversed obj) :test #'equal) #'<)))
   (let ((strm *error-output*))
     (format strm "~%~a~%" "printing uncommented lines in configuration file that were not read:")
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
   "print to terminal, information about the top-level session-class object"
   (let ((strm *error-output*))
     (format strm "~%printing session status:~%")
     (format strm "#####entry function: ~a~%" (if (listp (modelProgram obj))
                                                (make-sentence (modelProgram obj))
                                                (modelProgram obj)))
     (format strm "#####total calls to entry function: ~a~%" (quota obj))
     (format strm "#####calls to entry function per model process (runsPerProcess=): ~a~%" (runsPerProcess obj))
     (format strm "#####number of lines in the work file: ~a~%" (lines obj))
     (format strm "#####number of times to run each line in the work file (iterations=): ~a~%" (iterations obj))
     (format strm "#####quota before collapsing (collapseQuota=): ~a~%" (collapseQuota obj))
     (format strm "#####extra lisp files loaded (file2load=): ~a~%" (make-sentence (get-pandoric #'load-and-loaded 'loaded)))
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

(defmethod defaultIVStringFn ((obj run-class))
  "defines how an IV vector will look when sent across stdin, when the model is launched as a separate process"
  (format nil "~{~a~^	~}"
          (mapcar #'cdr (get-elements 
                          (IVKeys obj) 
                          (IVHash obj) 
                          :eval-val-p nil))))

;////////////////////////////////////////////
;hpc-specific classes, methods, and functions; all of this is to define the custom 'build-hpc-session
;function, and then call it by specifying "sessionBuilder='build-hpc-session" in the config file
(defclass hpc-work-class (work-class) 
  ()
  (:documentation "hpc-work-class is responsible for storing the points to run; this is done by setting the 'lines' slot in the class"))

(defmethod initialize-instance :after ((obj hpc-work-class) &key)
  "setting the lines slot, and storing the points to run"
  (setf (lines obj) (mapcar #'get-objects (get-lines (file-string (workFilePath obj))))))

(defclass hpc-collector-class (collector-class) 
  ()
  (:documentation "hpc-collector-class is responsible for printing the outputs of a collapsed set of runs"))

(defmethod print-collector ((obj hpc-collector-class))
  "method will be called after each collapsed run; for the hpcs, the results will be printed to stdout"
  (format t "Evaluating: ")
  (dotimes (i (length (cellElements obj)))
    (if (equal i (- (length (cellElements obj)) 1))
      (format t "~a~%" (cdr (nth i (cellElements obj))))
      (format t "~a " (cdr (nth i (cellElements obj))))))
  (dolist (key (keys obj))
    (aif (cdr (get-element key (collection obj) :collapseFns (gethash-ifHash key (collapseHash obj))))
      (format t "~a: ~a~%" key (if (realp it) (coerce it 'double-float) it)))))

(defclass hpc-process-output-str-class (process-output-str-class) 
  ()
  (:documentation "hpc-process-output-str class is responsible for keeping track of the last N lines printed by the model"))

(defmethod print-collector ((obj hpc-process-output-str-class))
  "method will be called if the model has died; will print the last lines outputted by the model (the model's last dying comments to stderr)"
  (format *error-output* "model unexpectedly quit... ~%~%here are the last ~a lines that were printed to stdout before the error~%~a~%"
          (quot obj) (make-sentence (gethash "str" (collection obj)) :spaceDesignator #\Newline))
  (if (error-p obj) (format *error-output* "here's the error~%~a~%" (error-p obj))))

(defun build-hpc-session ()
  "top-level hpc function called by letf that builds the session object"
  (build-session ;this is a macro
    :collector-instance (make-instance 'hpc-collector-class)
    :work-instance (make-instance 'hpc-work-class)
    :process-output-str-instance (make-instance 'hpc-process-output-str-class)))
;////////////////////////////////////////////

(defun load-and-eval-commands ()
  (with-pandoric (platform configFileWdLST) #'args
    ;run the pre-processing commands
    (mapc (lambda (str) (eval-object str)) (get-matching-lines configFileWdLST "runBeforeLoad="))
    ;load the extra lisp files, and run any processing commands weaved within the loads
    (loop for (lhs rhs) in (get-matching-lines-full configFileWdLST '("runWithinLoad=" "file2Load="))
          do (cond ((string-equal lhs "runWithinLoad=") 
                    (eval-object rhs))
                   ((string-equal lhs "file2Load=") 
                    (load-and-loaded (replace-all rhs "$1" platform :test #'string-equal)))
                   (t (expect nil "function not working as expected"))))
    ;run the post-processing commands
    (mapc (lambda (str) (eval-object str)) (get-matching-lines configFileWdLST "runAfterLoad="))))

(load-and-eval-commands)

(with-pandoric (platform configFileWdLST) #'args
  ;run it!
  (aif (get-matching-line configFileWdLST "albumBuilder=")
    (funcall (eval (read-from-string it)))
    (wrapper-execute
      (funcall 
        (eval 
          (read-from-string 
            (aif (get-matching-line configFileWdLST "sessionBuilder=")  it "#'build-hpc-session")))))))
;kill it!	    
(quit)



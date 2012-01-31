(defmacro square (x)
  `(* ,x ,x))

(defun mean2 (sequence)
  (/ (reduce #'+ sequence) (length sequence)))

#|(defun correl2 (xs ys &key (throwOutYerNils nil))
  (inLSTs xs ys correl2 throwOutYerNils)
  (let ((x-bar (mean2 xs))
	(y-bar (mean2 ys)))
    (/ (reduce #'+ (mapcar #'(lambda (xi yi) (* (- xi x-bar) (- yi y-bar)))
			   xs ys))
       (sqrt (* (reduce #'+ (mapcar #'(lambda (xi) (square (- xi x-bar)))
				    xs))
		(reduce #'+ (mapcar #'(lambda (yi) (square (- yi y-bar)))
				    ys)))))))|#

(defun correl2 (l1 l2 &key (throwOutYerNils nil))
  (inLSTs l1 l2 correl2 throwOutYerNils)
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


(defun fooPOW ()
  (list 1.2 2.2 3.3 4.4 5.5))

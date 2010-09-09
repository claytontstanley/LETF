(defmacro square (x)
  `(* ,x ,x))

(defun mean2 (sequence)
  (/ (reduce #'+ sequence) (length sequence)))

(defun correl2 (xs ys &key (throwOutYerNils nil))
  (inLSTs xs ys correl2 throwOutYerNils)
  (let ((x-bar (mean2 xs))
	(y-bar (mean2 ys)))
    (/ (reduce #'+ (mapcar #'(lambda (xi yi) (* (- xi x-bar) (- yi y-bar)))
			   xs ys))
       (sqrt (* (reduce #'+ (mapcar #'(lambda (xi) (square (- xi x-bar)))
				    xs))
		(reduce #'+ (mapcar #'(lambda (yi) (square (- yi y-bar)))
				    ys)))))))


(defun fooPOW ()
  (list 1.2 2.2 3.3 4.4 5.5))

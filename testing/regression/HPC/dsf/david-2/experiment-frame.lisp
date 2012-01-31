;(defparameter *top-object* nil)
;(defparameter *pname* (directory-namestring *load-truename*)) ;has a '/' on the end of it already
;(defparameter *data* nil) ;stores observed data, if loading observed data from lisp
;(defparameter *data-dv-acc* nil) ;stores dv accessor associated with observed data
(defparameter *num-trials* 100)
;note that *hpc-object* must be defined in model code

#|(defclass top-class ()
  ((run-objects :accessor run-objects :initarg :run-objects :initform nil)
   (transducer-class :accessor transducer-class :initarg :transducer-class :initform 'experiment-transducer)
   ;probably update these 7 if instantiating a top-class 
   (dv-acc :accessor dv-acc :initarg :dv-acc :initform nil)
   (runFun :accessor runFun :initarg :runFun :initform nil)
   (IVs :accessor IVs :initarg :IVs :initform nil)
   (conditions :accessor conditions :initarg :conditions :initform nil)
   (num-runs :accessor num-runs :initarg :num-runs :initform 1)
   (path-to-data-inner :accessor path-to-data-inner :initarg :path-to-data-inner :initform nil)
   (path-to-data-outer :accessor path-to-data-outer :initarg :path-to-data-outer :initform nil)
   (mergeFun :accessor mergeFun :initarg :mergeFun :initform nil)
   ;end update these
   (pname :accessor pname :initarg :pname :initform *pname*)
   (dsf-port :accessor dsf-port :initarg :dsf-port :initform 9548)
   (dsf-host :accessor dsf-host :initarg :dsf-host :initform "127.0.0.1")
   (merged-transducer :accessor merged-transducer :initarg :merged-transducer :initform nil)))|#


#|(defclass run-class ()
  ((experiment-objects :accessor experiment-objects :initarg :experiment-objects :initform nil)
   (appended-transducer :accessor appended-transducer :initarg :appended-transducer :initform nil)))|#

(defclass experiment-class ()
  ((generator :accessor generator :initarg :generator :initform nil)
   (transducer :accessor transducer :initarg :transducer :initform nil)
   (ccondition :accessor ccondition :initarg :ccondition :initform nil)
   (goal :accessor goal :initarg :goal :initform nil)
   (initial-value :accessor initial-value :initarg :initial-value :initform nil)))

(defclass experiment-generator ()
  ((ui :accessor ui :initarg :ui :initform nil)
   (uo :accessor uo :initarg :uo :initform nil)
   (ei :accessor ei :initarg :ei :initform nil)
   (eo :accessor eo :initarg :eo :initform nil)))

(defclass experiment-transducer ()
  ((deviation :accessor deviation :initarg :deviation :initform nil)
   (abs-deviation :accessor abs-deviation :initarg :abs-deviation :initform nil) 
   (dsfstate :accessor dsfstate :initarg :dsfstate :initform nil)
   (ctime :accessor ctime :initarg :ctime :initform nil)
   (goal :accessor goal :initarg :goal :initform nil)
   (amount :accessor amount :initarg :amount :initform nil)))

#|(defclass stat-transducer ()   
   ;these will only be filled if loading observed data from lisp
   ;(i.e., if path-to-data-inner/outer in top-object is specified)
   ;the one that will be filled is specified by the 'dv-acc slot of the top-object
  ((MAD :accessor MAD :initarg :MAD :initform nil)
   (RMSE :accessor RMSE :initarg :RMSE :initform nil)
   (correl :accessor correl :initarg :correl :initform nil)))|#

;gotta love multiple inheritance, when you only kind of need it...
#|(defclass experiment-transducer (base-transducer stat-transducer)
  ())|# 

(defclass lininc (experiment-generator) ())

(defclass lindec (experiment-generator) ())

(defclass nonlininc (experiment-generator) ())

(defclass nonlindec (experiment-generator) ())

(defclass del2 (experiment-generator) ())

(defclass del3 (experiment-generator) ())

(defclass seq2 (experiment-generator) ())

(defclass seq2nos (experiment-generator) ())

(defclass seq4 (experiment-generator) ())

(defmethod initialize-instance :after ((obj experiment-class) &key)
  (if (or
       (equal (ccondition obj) 'lininc)
       (equal (ccondition obj) 'lindec)
       (equal (ccondition obj) 'nonlininc)
       (equal (ccondition obj) 'nonlindec))
      (progn
	(setf (goal obj) 4)
	(setf (initial-value obj) 2))
      (progn
	(setf (goal obj) 6)
	(setf (initial-value obj) 4))))

#+:sbcl
(defun class-slot-names (class-name)
  (mapcar #'sb-mop:slot-definition-name 
	  (sb-mop:class-slots (find-class class-name))))

#+(not :sbcl)
(defun class-slot-names (class-name)
  (mapcar #'slot-definition-name
	  (class-slots (find-class class-name))))

(defun slot-of (slot-symbol class-symbol)
  (if (member (symbol-name slot-symbol) 
	      (mapcar (lambda (x) (symbol-name x)) (class-slot-names class-symbol))
	      :test #'equalp)
      t nil))

#|(defun merge-objects (mergeFun &rest objs)
  (if (listp (car objs)) (setf objs (car objs)))
  (let ((accs (class-slot-names (class-name (class-of (first objs)))))
	(out (make-instance (class-name (class-of (first objs)))))
	(temp-lst))
    (dolist (acc accs)
      (dotimes (i (length (slot-value (first objs) acc)))
	(setf temp-lst nil)
	(dolist (obj objs)
	  (push (nth i (slot-value obj acc)) temp-lst))       
	(push
	 (if (or (not mergeFun) (equal (class-name (class-of (first temp-lst))) 'symbol))
	     (if (equal (length temp-lst) 1) (car temp-lst) temp-lst)
	     (eval (push mergeFun temp-lst)))
	 (slot-value out acc)))
      (setf (slot-value out acc) (reverse (slot-value out acc))))
    out))|#

#|(defun append-objects (objs)
  (if (not (listp objs)) (setf objs (list objs))) 
  (let ((accs (class-slot-names (class-name (class-of (first objs)))))
	(out (make-instance (class-name (class-of (first objs))))))
    (dolist (acc accs out)
      (dolist (obj objs)
	(setf (slot-value out acc)
	      (append (slot-value out acc) (slot-value obj acc)))))))|#

(defmethod post-process-generator ((obj experiment-generator))
  (dolist (acc (class-slot-names (class-name (class-of obj))))
    (when (slot-value obj acc)
      (assert (> (length (slot-value obj acc)) 0)) 
      (setf (slot-value obj acc) (reverse (slot-value obj acc))))))

(defmethod post-process-transducer ((obj experiment-transducer))
  (dolist (acc (class-slot-names (class-name (class-of obj))))
    (when (slot-value obj acc)
      (assert (> (length (slot-value obj acc)) 1))
      (setf (slot-value obj acc) (rest (reverse (slot-value obj acc)))))))

(defmethod post-process-object ((obj experiment-class))
  (post-process-generator (generator obj))
  (post-process-transducer (transducer obj)))

;returns lst of indeces of the lst offset by start-pt
#|(defun make-indexed-list (length start-pt)
  (reverse
   (labels ((make-reversed-indexed-list (length2 start-pt)
	      (if (not (equal length2 -1))
		  (cons (+ length2 start-pt) 
			(make-reversed-indexed-list (- length2 1) start-pt)))))
     (make-reversed-indexed-list (- length 1) start-pt))))|#

;works just like (nth lst) except that you can specify a list of indeces to reference
#|(defun nth-indeces (indeces lst)
  (if indeces 
      (cons (nth (car indeces) lst) 
	    (nth-indeces (cdr indeces) lst))))|#

;dummy method; override if you want
(defmethod update-transducer ((obj experiment-transducer)) ())


#|(defmethod append-statistic ((obj experiment-transducer) &key (path-to-data nil) (statistic nil))
  (let ((cInd 0))
    (dolist (cPath path-to-data (setf (slot-value obj statistic) (reverse (slot-value obj statistic))))
      (load (format nil cPath))
      (push
       (funcall
	statistic
	*data*
	(flatten
	 (funcall
	  #'(lambda (obj length start-pt) 
	      (mapcar (lambda (y) (nth-indeces (make-indexed-list length start-pt) (slot-value obj y)))
		      *data-dv-acc*))
	  obj
	  (length *data*)
	  cInd)))
       (slot-value obj statistic))
      (setf cInd (+ cInd (length *data*))))))|#
		  	 
#|(defun std (lst)
  (assert (> (length lst) 1))
  (sqrt
   (/
    (eval
     (cons '+ 
	   (funcall #'(lambda (x)
			(mapcar (lambda (y) (* (- y x) (- y x))) lst))
		    (/ (eval (cons '+ lst)) (length lst)))))
    (- (length lst) 1))))|#

#|(defun MAD (l1 l2)
  (assert (equal (length l1) (length l2)))
  (/ (eval 
      (cons '+ (mapcar (lambda (x y) (abs (- x y))) l1 l2)))
     (length l1)))|#

#|(defun correl (l1 l2)
  (assert (equal (length l1) (length l2)))
  (assert (> (length l1) 1))
  (/
   (eval
    (cons '+
	  (mapcar (lambda (x y) (* x y))
		  (funcall #'(lambda (x)
			       (mapcar (lambda (y) (- y x)) l1))
			   (/ (eval (cons '+ l1)) (length l1)))
		  (funcall #'(lambda (x)
			       (mapcar (lambda (y) (- y x)) l2))
			   (/ (eval (cons '+ l2)) (length l2))))))
   (* (- (length l1) 1) (std l1) (std l2))))|#

#|(defun RMSE (l1 l2)
  (assert (equal (length l1) (length l2)))
  (sqrt
   (/
    (eval 
     (cons '+
	   (mapcar (lambda (x y) (* (- x y) (- x y))) l1 l2)))
    (length l1))))|#

#|(defun median (&rest list)
  (let ((len (length list))
	(sortedList (sort list #'<)))
    (if (evenp len)
	(/ (+ (nth (- (/ len 2) 1) sortedList)
	      (nth (/ len 2) sortedList))
	   2)
	(nth (/ (- len 1) 2) sortedList))))|#


#|(defun sum (&rest list)
  (loop for x in list sum x))|#

#|(defun mean (&rest list) 
  (/ (apply 'sum list) (length list)))|#

(defmethod set-next-segment ((obj seq2) &key (ui 0) (uo 0))
  (when (equal *num-trials* (length (ui obj)))
    nil)
  (unless (equal *num-trials* (length (ui obj)))
    (let ((val (if (evenp (length (ui obj))) 1 5)))
      (push ui (ui obj))
      (push uo (uo obj))
      (push val (ei obj))
      (push 0 (eo obj))
      t)))

(defmethod set-next-segment ((obj seq4) &key (ui 0) (uo 0))
  (when (equal *num-trials* (length (ui obj)))
    nil)
  (unless (equal *num-trials* (length (ui obj)))
    (let ((val (nth (mod (length (ui obj)) 4) (list 0 4 2 6))))
      (push ui (ui obj))
      (push uo (uo obj))
      (push val (ei obj))
      (push 0 (eo obj))
      t)))

(defmethod set-next-segment ((obj seq2nos) &key (ui 0) (uo 0))
  (when (equal *num-trials* (length (ui obj)))
    nil)
  (unless (equal *num-trials* (length (ui obj)))
    (let ((val (if (evenp (length (ui obj))) 1 5)))
      (setf val (+ val 
		   (funcall #'(lambda (x) (if (equal x 0) -1 1))
			    (act-r-random 2))))
      (push ui (ui obj))
      (push uo (uo obj))
      (push val (ei obj))
      (push 0 (eo obj))
      t)))
      
(defmethod set-next-segment ((obj del2) &key (ui 0) (uo 0))
  (when (equal *num-trials* (length (ui obj)))
    nil)
  (unless (equal *num-trials* (length (ui obj)))
    (push ui (ui obj))
    (push uo (uo obj))
    (push (+ 2 (* .08080808
		  (+ (length (ei obj)) 0)))
	  (ei obj))
    (push 0 (eo obj))
    t))

(defmethod set-next-segment ((obj del3) &key (ui 0) (uo 0))
  (when (equal *num-trials* (length (ui obj)))
      nil)
  (unless (equal *num-trials* (length (ui obj)))
    (push ui (ui obj))
    (push uo (uo obj))
    (push (+ 2 (* .08080808
		  (+ (length (ei obj)) 0)))
	  (ei obj))
    (push 0 (eo obj))
    t))
     
(defmethod set-next-segment ((obj lininc) &key (ui 0) (uo 0))
  (when (equal *num-trials* (length (ui obj)))
      nil)
  (unless (equal *num-trials* (length (ui obj)))
    (push ui (ui obj))
    (push uo (uo obj))
    (push (+ 2 (* .08
		  (+ (length (ei obj)) 1)))
	  (ei obj))
    (push 0 (eo obj))
    t))
  
(defmethod set-next-segment ((obj lindec) &key (ui 0) (uo 0))
  (when (equal *num-trials* (length (ui obj)))
      nil)
  (unless (equal *num-trials* (length (ui obj)))
    (push ui (ui obj))
    (push uo (uo obj))
    (push (+ 10 (* -.08
		   (+ (length (ei obj)) 0)))
	  (ei obj))
    (push 0 (eo obj))
    t))

(defmethod set-next-segment ((obj nonlininc) &key (ui 0) (uo 0))
  (when (equal *num-trials* (length (ui obj)))
    nil)
  (unless (equal *num-trials* (length (ui obj)))
    (push ui (ui obj))
    (push uo (uo obj))
    (push (* 5 (/ 
		(log (+ (length (ei obj)) 1))
		(log 10))) 
	  (ei obj))
    (push 0 (eo obj))
    t))

(defmethod set-next-segment ((obj nonlindec) &key (ui 0) (uo 0))
  (when (equal *num-trials* (length (ui obj)))
    nil)
  (unless (equal *num-trials* (length (ui obj)))
    (push ui (ui obj))
    (push uo (uo obj))
    (push (* 5 (/
		(log (- 101 (+ (length (ei obj)) 1)))
		(log 10))) 
	  (ei obj))
    (push 0 (eo obj))
    t))

(defmethod get-next-segment :before ((obj experiment-generator))
  (assert ;every slot has to have at least one value 
   (>= (eval (cons 
	      'min 
	      (funcall #'(lambda (obj)
			   (mapcar (lambda (acc) (length (slot-value obj acc))) 
				   (class-slot-names (class-name (class-of obj)))))
		       obj)))
       1)))

(defmethod get-next-segment ((obj seq2))
  (list (first (ui obj)) (first (uo obj)) (first (ei obj)) (first (eo obj))))

(defmethod get-next-segment ((obj seq4))
  (list (first (ui obj)) (first (uo obj)) (first (ei obj)) (first (eo obj))))

(defmethod get-next-segment ((obj seq2nos))
  (list (first (ui obj)) (first (uo obj)) (first (ei obj)) (first (eo obj))))

(defmethod get-next-segment ((obj del2))
  (let ((acc (if (>= (length (ui obj)) 2) 'second nil)))
    (list (if acc (funcall acc (ui obj)) 0) (if acc (funcall acc (uo obj)) 0) (first (ei obj)) (first (eo obj)))))

(defmethod get-next-segment ((obj del3))
  (let ((acc (if (>= (length (ui obj)) 3) 'third nil)))
    (list (if acc (funcall acc (ui obj)) 0) (if acc (funcall acc (uo obj)) 0) (first (ei obj)) (first (eo obj)))))

(defmethod get-next-segment ((obj lininc))
  (list (first (ui obj)) (first (uo obj)) (first (ei obj)) (first (eo obj))))

(defmethod get-next-segment ((obj lindec))
  (list (first (ui obj)) (first (uo obj)) (first (ei obj)) (first (eo obj))))

(defmethod get-next-segment ((obj nonlininc))
  (list (first (ui obj)) (first (uo obj)) (first (ei obj)) (first (eo obj))))

(defmethod get-next-segment ((obj nonlindec))
  (list (first (ui obj)) (first (uo obj)) (first (ei obj)) (first (eo obj))))

#|(defun run-model (experiment-object runFun IVs)
  (setf *hpc-object* experiment-object) ;global used to wrap hpc code with both models
  (eval (push runFun IVs))
  (post-process-object experiment-object))|#
  
#|(defun hpc-run-model (&key (top-object nil))
  (let ((conditions (list 'del2 'del3 'Seq2 'Seq2Nos 'Seq4))
	(num-runs 2)
	(dv-acc (list 'correl 'MAD 'RMSE)) 
	;(dv-acc (list 'deviation))
	(path-to-data-inner nil)
	;(path-to-data-outer nil)
	(path-to-data-outer (list (format nil "~a~a" *pname* "obs data/all-Del2.lisp")
				  (format nil "~a~a" *pname* "obs data/all-Del3.lisp")
				  (format nil "~a~a" *pname* "obs data/all-Seq2.lisp")
				  (format nil "~a~a" *pname* "obs data/all-Seq2Nos.lisp")
				  (format nil "~a~a" *pname* "obs data/all-Seq4.lisp")))
	(runFun 'run-socket)
	(mergeFun 'median)
	(IVs (list :port (+ 9548 2)))
	(transducer-class 'experiment-transducer) 
        ;all inputs up to here
	;variables referencing current various objects all nested under the top object
	(run-object)
	(experiment-object))
    (unless top-object
      (setf top-object 
	    (make-instance 'top-class
			   :dv-acc dv-acc
			   :path-to-data-inner path-to-data-inner
			   :path-to-data-outer path-to-data-outer
			   :runFun runFun
			   :IVs IVs
			   :mergeFun mergeFun
			   :conditions conditions
			   :num-runs num-runs
			   :transducer-class transducer-class)))
    (setf *top-object* top-object) ;keep global pointer around for various shorthanding
    (dotimes (i (num-runs top-object)
	      (setf (run-objects top-object) (reverse (run-objects top-object)))) 
      (push (make-instance 'run-class)
	    (run-objects top-object))
      (setf run-object (first (run-objects top-object)))
      (dolist (ccondition (conditions top-object)
	       (setf (experiment-objects run-object) (reverse (experiment-objects run-object))))
	(push (make-instance 'experiment-class
			     :generator (make-instance ccondition)
			     :transducer (make-instance (transducer-class top-object))
			     :ccondition ccondition) 
	      (experiment-objects run-object))
	(setf experiment-object (first (experiment-objects run-object)))
	(run-model experiment-object (slot-value top-object 'runFun) (slot-value top-object 'IVs)))
      (assert (equal (length (experiment-objects run-object)) (length (conditions top-object))))
      (setf (appended-transducer run-object)
	    (append-objects
	     (mapcar (lambda (x) (transducer x)) (experiment-objects run-object))))
      (dolist (dv-acc (dv-acc top-object))
	(if (and (path-to-data-inner top-object) (slot-of dv-acc 'stat-transducer))
	    (append-statistic (appended-transducer run-object) :path-to-data (path-to-data-inner top-object) :statistic dv-acc))))
    (assert (equal (length (run-objects top-object)) (num-runs top-object)))
    (setf (merged-transducer top-object)
	  (merge-objects (slot-value top-object 'mergeFun) (mapcar (lambda (x) (appended-transducer x)) (run-objects top-object))))
    (dolist (dv-acc (dv-acc top-object))
      (if (and (path-to-data-outer top-object) (slot-of dv-acc 'stat-transducer)) 
	  (append-statistic (merged-transducer top-object) :path-to-data (path-to-data-outer top-object) :statistic dv-acc)))
    (flatten
     (funcall
      #'(lambda (obj) (mapcar (lambda (y) (slot-value obj y))
			      (dv-acc top-object)))
      (merged-transducer top-object)))))|#
	
 



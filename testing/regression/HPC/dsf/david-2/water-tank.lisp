;; chcekcmisretrievals in estimation strategy
;; (misretrievals previous trend observation)
;; export: number of misretrievals 



;; DSF Model
;; (C) David Reitter / CMU
;; 10/12/2009

;; Problems?  Please e-mail reitter@cmu.edu


;; USAGE

;; (load "water-tank.lisp")

;; (run-socket &optional host port)
;; e.g.
;; (run-socket "localhost")

;; if it can't connect, try again 
;; set IP and port in the `run-socket' function.


;; to use the internal function generator / test environment
;; see function `run'

;; Parameters  (set below, lines 166-)
;; (setq *blend-temperature* 0.5)
;; (defparameter *goal-level* 4.0)
;; (defparameter *calc-time-factor* 1.0)
;; (defparameter *environ-wait* 8)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; benchmark:
;; about 45 iterations / sec
;; on MBP with openmcl

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;; emacs stuff
;; (when nil
;; ;; emacs side
;;   (progn 
;;     (server-start)
;;     (slime)
;;     (find-file "water.R")
;;     (R)
;;     (process-send-string (get-ess-process ess-current-process-name) "source('/users/dr/MURI/ACT-UP/water.R')\n"))
;; )


;; Libraries
(declaim (optimize (speed 0) (space 0) (debug 3)))
;placed all the requires and loads at the top of this file



(require "act-up" (pathname (format nil "~aact-up.lisp" (directory-namestring *load-truename*))))
(use-package :act-up)



(require "act-up-experiments" (pathname (format nil "~aact-up-experiments.lisp" (directory-namestring *load-truename*))))
(use-package :act-up-experiments)
(load (pathname (format nil "~asplit-sequence.lisp" (directory-namestring *load-truename*))))
(require 'uni-files (pathname (format nil "~auni-files.lisp" (directory-namestring *load-truename*))))
;#+:sbcl (require 'sb-bsd-sockets) ;don't need this one b/c it's defined in uni-files.lisp (I'm using marcs version of uni-files.lisp)
(load (format nil "~a/actr6-compatibility.lisp" (directory-namestring *load-truename*)))
;have to load actr6-compatibility at this level b/c it was loaded inside of the act-up package, so it's currently not accessible by any functions called within water-tank.lisp
(defparameter *hpc-object* nil) ;global variable used to wrap your model with 'experiment-frame.lisp' stuff

(load (format nil "~aexperiment-frame.lisp" (directory-namestring *load-truename*)))

(defclass david-transducer (experiment-transducer)
  ((count-calc-strategy-used :accessor count-calc-strategy-used :initarg :count-calc-strategy-used :initform nil)
   (count-est-strategy-used :accessor count-est-strategy-used :initarg :count-est-strategy-used :initform nil)
   (count-incorrect-retrievals :accessor count-incorrect-retrievals :initarg :count-incorrect-retrievals :initform nil)
   (last-assumed-trend :accessor last-assumed-trend :initarg :last-assumed-trend :initform nil)
   (current-trend-chunk :accessor current-trend-chunk :initarg :current-trend-chunk :initform nil)))

(defmethod update-transducer ((obj david-transducer))
  (push
   (+ 
    (aif (count-calc-strategy-used obj) (car it) 0)
    (if *last-strategy* (if (equal (strategy-strategy *last-strategy*) 'calc) 1 0) 0))
   (count-calc-strategy-used obj))
  (push
   (+
    (aif (count-est-strategy-used obj) (car it) 0)
    (if *last-strategy* (if (equal (strategy-strategy *last-strategy*) 'guess) 1 0) 0))
   (count-est-strategy-used obj))
  (push
   (+
    (aif (count-incorrect-retrievals obj) (car it) 0)
    (if *last-strategy* (if (not *last-trend-retrieval-correct*) 1 0) 0))
   (count-incorrect-retrievals obj))
  (push *last-assumed-trend* (last-assumed-trend obj))
  (push *current-trend-chunk* (current-trend-chunk obj)))
    

;; MODEL

(defparameter *blend-temperature* 0.25)

(defstruct (trend (:include chunk)) ;; (concept (:print-object print-concept))
  (type  'trend)
  trend
) 
(defstruct (strategy (:include chunk))
  (type  'strategy)
  strategy
  dampen
  success
) 




(defparameter *predictedWaterLevel* nil)
(defparameter *guesstimate* nil)
(defparameter *strategies* nil)

(defparameter *calc-strategies-enabled* t)
(defparameter *guess-strategy-enabled* t)


(defun calc-1 (goal current trend0 trend1)
  `(+ ,goal (+ ,current ,trend0 ,trend1)))
(defun calc-2  (goal current trend0 trend1)
  `(- ,goal (- ,current ,trend0 ,trend1)))
(defun calc-3  (goal current trend0 trend1)
  `(+ ,goal (- ,current ,trend0 ,trend1)))
(defun calc (goal current trend0 trend1)
  `(- ,goal (+ ,current ,trend0 ,trend1)))
(defun model-init ()
  (reset-mp)
  (reset-model)
  (setq  *guesstimate* t *l-trend0* nil *l-trend1* nil *last-strategy* nil)
  (dotimes (n 1)

    ;; a set of possible guessing and calculation strategies
    (setq *strategies* nil)

    (when *guess-strategy-enabled*
      (setq *strategies* (cons 'guess *strategies*))
      (learn-chunk  (make-strategy :strategy 'guess :success 0.2)))

    (when *calc-strategies-enabled*
      (setq *strategies* (append *strategies* '(calc calc-1 calc-2 calc-3) ))

      (learn-chunk  (make-strategy :strategy 'calc :success 0.2
				   ))
      (actup-pass-time 0.3)
    
      (learn-chunk  (make-strategy :strategy 'calc-1 :success 0.2
				   ))
      (learn-chunk  (make-strategy :strategy 'calc-2 :success 0.2 
				   ))
      (learn-chunk  (make-strategy :strategy 'calc-3 :success 0.2 
				   ))))
  ;; source of subject-specific variance
  (learn-chunk  (make-trend :trend (- 10 (random 20)) :type 'l-trend0))
  (learn-chunk  (make-trend :trend (- 10 (random 20)) :type 'l-trend0))
  ;; (learn-chunk  (make-trend :trend 0.0 :type 'trend1))
  (learn-chunk  (make-trend :trend -8.0 :type 'trend2))
  ;; maybe the initial (bordering) chunks need
  ;; much more activation so that they don't decay
)

 

(defun retrieve-chunk (spec &optional cues)
  (best-chunk (filter-chunks (model-chunks act-up::*current-actUP-model*)
			     spec)
	      cues spec))

(defun retrieve-pm-chunk (hard-spec soft-spec &optional cues)
  (best-chunk (filter-chunks (model-chunks act-up::*current-actUP-model*)
			     hard-spec)
	      cues (append hard-spec soft-spec)))
(defun blend-retrieve-chunk (spec &optional cues)
  (let ((cs (filter-chunks (model-chunks act-up::*current-actUP-model*)
			     spec)))
    (if cs
	(blend cs cues nil spec))))
    
(defun trend-trend-safe (trend-chunk)
  (if trend-chunk
      (trend-trend trend-chunk)
      nil))


(defun safe-strategy-strategy (strategy-chunk)
  (if strategy-chunk
      (strategy-strategy strategy-chunk)
      (if *calc-strategies-enabled*
	  'calc 'guess)))

(defun best-by-blend-activation (chunks)
  (let ((ba 0) (bc nil))
    (loop for c in chunks 
       finally (return bc)
       when c 
       when (> (chunk-last-activation c) ba)
       do
	 (setq bc c ba (chunk-last-activation c)))))



(defmacro tround (value)
 `(* (round ,value 0.1) 0.1))

(defvar *l-trend0* nil)

(defvar *l-trend1* nil)

(defvar *last-strategy* nil)

(defparameter *current-trend-chunk* nil)


;;  PARAMETERS

(defparameter *goal-level* 4)

(setf *blend-temperature* 0.5)
(defparameter *calc-time-factor* 1.0)
(defparameter *guess-time-factor* 5)
(defparameter *exact-time-factor* 10)
(defparameter *environ-wait* 8)
;(actUP-pass-time (* *calc-time-factor* (if *guesstimate* 5 10))) ;; seconds

(defvar *last-trend-retrieval-correct* nil "non-nil if the latest direct retrieval of a trend (previous observed environmental inflow) was successful, i.e. actually correct.")
(defvar *last-assumed-trend* nil "The last trend that the model estimated (estimation strategy) or remembered (calculation strategy).")

(defun model-get-inflow (waterLevel envirInflow)
  (declaim (optimize (speed 0) (space 0) (debug 3)))

  ;;  (format t "wl:~a i:~a ~%" waterLevel envirInflow)

  (let* (
	 (current-trend 0) (current-d1 0) (current-d2 0)
	 (last-observed-trend   (if *guesstimate* (trend-trend-safe (retrieve-chunk '(:type l-trend0))) *l-trend0*))
	 (last-observed-d1 (if *guesstimate* (trend-trend-safe (retrieve-chunk '(:type l-trend1))) *l-trend1*)))


    (setq *last-trend-retrieval-correct* (equal last-observed-trend *l-trend0*))
  ;; learn previous action

    ;; observe predicted vs. actual water level
    ;; predicted does not contain any trend  (maybe change?)

    (when *predictedWaterLevel*
      (setq current-trend envirInflow) ;; (- waterLevel *predictedWaterLevel*   ))
      (learn-chunk (make-trend :trend (tround current-trend))))

    ;; learn first derivative
    (when last-observed-trend
      
      (setq current-d1  (- current-trend last-observed-trend ))
      ;; trend1 type chunks is what the estimation strategy blends together


      (setf *current-trend-chunk* (tround current-d1))

      (learn-chunk (make-trend :type 'trend1 :trend (tround current-d1))))

    ;; learn second derivative
    (when last-observed-d1
      (setq current-d2 (- current-d1 last-observed-d1 ))
      (learn-chunk (make-trend :type 'trend2 :trend (tround current-d2))))

   

;; how can we learn the first derivative?

;; 0.: change in water level (beyond expectation from our own correction)
;; 1.: change in 0 compared to last measurement

        ;; learn strategy

    (when *predictedWaterLevel* 
      (if *guesstimate*
	  (learn-chunk (make-strategy :strategy 'guess
				      :success (tround (abs (- *predictedWaterLevel* waterLevel)))))
	  
	  (when  *last-strategy*
	    (learn-chunk (make-strategy :strategy (strategy-strategy *last-strategy*)
					:success (tround (abs (- *predictedWaterLevel* waterLevel))))))))
    

    (actUP-pass-time 2.5)
    
    ;; (retrieve-pm-chunk '(:type strategy) '(:success 0) 
    ;;					       nil)
    (let ((current-strategy (best-chunk  ;; retrieve blended 
			     (loop for s in *strategies* collect  
				  (blend (filter-chunks (model-chunks act-up::*current-actUP-model*)
							`(:type strategy :strategy ,s)) 
					 nil ;; no cues
					 'strategy ;; return type
					 `(:strategy ,s) ;; retrieval spec
					 ))
			     nil ;; no cues
			     `(:success 0.0) ;; retrieval spec
			     ))
	  (dampen nil))


      (unless current-strategy
	(setq current-strategy (make-strategy :strategy (if *guess-strategy-enabled* 'guess ; as it was in competition model
							    (choice *strategies*)))))

      (setq *guesstimate* (eq 'guess (safe-strategy-strategy current-strategy)))
      (setq dampen (strategy-dampen current-strategy))
      
      ;; it's currently unclear how we would
      ;; decide whether damping is needed.
	  

  ;; retrieve chunk  
  (let* ((assumed-trend  envirInflow)

	 (expected-trend-1-chunks  (filter-chunks (model-chunks act-up::*current-actUP-model*)
							       '(:type trend1)))
	 (expected-trend-1-best (if *guesstimate*
				    (best-chunk expected-trend-1-chunks nil)
				    (make-trend :trend current-d1)
				    ))
	 (expected-trend-1-est (blend expected-trend-1-chunks nil 'trend))
	 (assumed-trend-1 (trend-trend 
			   (or 
			    (if (or *guesstimate* )
				(or expected-trend-1-est expected-trend-1-best)
				expected-trend-1-best) (make-trend :trend 0))))
	 (needed-effect (if (or *guesstimate*  (not current-strategy))
			    (- *goal-level* (+ waterLevel assumed-trend  assumed-trend-1 ))
			    (progn #|(print current-strategy)|#
				   (cogn-calculate (funcall (strategy-strategy current-strategy) *goal-level* waterlevel assumed-trend assumed-trend-1)))))
	 (needed-effect-1 0.0) ;; used for damping
	 (chosen-valve-setting needed-effect))

    (setq *last-assumed-trend* assumed-trend-1)

   ;; DAMPING
    (when dampen ;; damping
      (let ((surprisal (- waterLevel *predictedWaterLevel*)))
    	(if (< surprisal -1)
    	    (setq needed-effect-1 (- needed-effect-1 (* 0.3 surprisal))))
    	(if (> surprisal 1)
    	    (setq needed-effect-1 (- needed-effect-1 (* 0.3 surprisal)))))
      (setq chosen-valve-setting (- *goal-level* (+ waterLevel needed-effect-1))))

   #| (format t "strat: ~a level: ~a   needed: ~a    assumed-trend: ~,2F assumed-trend':  ~,2F  assumed-trend'':  ~,2F chosen valve:~a  ~%" 
	    (or (strategy-strategy current-strategy) (strategy-strategy current-strategy))
	    waterLevel needed-effect  assumed-trend  assumed-trend-1 'na chosen-valve-setting)|#
     
    (setq *predictedWaterLevel* *goal-level*) ;; (+ waterLevel chosen-valve-setting assumed-trend-1 assumed-trend-2))

    ;(actUP-pass-time (* *calc-time-factor* (if *guesstimate* 5 10))) ;; seconds
    (actUP-pass-time (* *calc-time-factor* (if *guesstimate* *guess-time-factor* *exact-time-factor*)))

    ;(format t "~d ~d ~d ~d ~d ~d ~d ~d~%" *blend-temperature* *calc-time-factor* *environ-wait* *bll* *ans* *guess-time-factor* *exact-time-factor* *goal-level*)

    (learn-chunk (make-trend :type 'l-trend0 :trend (tround current-trend)))
    (learn-chunk (make-trend :type 'l-trend1 :trend (tround current-d1)))


    (when (not *guesstimate*)
      (setq *l-trend0* (tround current-trend))
      (setq *l-trend1* (tround current-d1)))
    (setq *last-strategy* current-strategy)
    

    (actUP-pass-time *environ-wait*) ;; seconds  ;; this time seems to be crucial - longer, more noise in memembering
    chosen-valve-setting ;; also guarantees *last-strategy*
    ))))

(defun cogn-calculate (expression)
  ;; error probabilities are guesses
  ;; individual differences should be realized here

  ;; to do: pass-time

  (let* ((err-prob 0.0)
	 (res
	  (cond
	    ((numberp expression) expression)
	    ((eq (car expression) '+)
	     ;; addition
	     (setq err-prob 0.03)
	     (apply #'+ (mapcar 'cogn-calculate (cdr expression))))
	    ((eq (car expression) '-)
	     ;; subtraction
	     (setq err-prob 0.06)
	     (apply #'- (mapcar 'cogn-calculate (cdr expression))))
	    ((eq (car expression) '*)
	     ;; multiplication
	     (setq err-prob 0.13)
	     (apply #'* (mapcar 'cogn-calculate (cdr expression))))
	    ((eq (car expression) '/)
	     ;; division
	     (setq err-prob 0.20)
	     (apply #'/ (mapcar 'cogn-calculate (cdr expression)))))))
    (if (< (/ (random 100 act-up-experiments::my-random-state) 100) err-prob)
	(* res (+ 0.5 (/ (random 10 act-up-experiments::my-random-state) 10.0)))
	(+ 0.0 res))))

; unit test
; (cogn-calculate '(+ 4 (/ 5 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SIMULATION ENVIRONMENT

;; environment
(defparameter *stats-output* nil)
(defvar *step* nil) 
(defun run (&optional num-subjects)
  
  (let ((stats-output (open 
		       (merge-pathnames
			(translate-logical-pathname "ACT-R6:")
			"water.txt")
		       :direction :output :if-exists :supersede :if-does-not-exist :create)))
    (setq *stats-output* stats-output)
    (format *stats-output* "envWait calcTimeFac blendTemp subject step Version trend waterLevel chosenValveSetting~%")

;; (loop for *blend-temperature* in '(0.5 1.0) do
;; (loop for *environ-wait* from 3 to 10 do
;; (loop for *calc-time-factor* from 0.4 to 2 by 0.15 do

    (loop for subject from 1 to (or num-subjects 1) do

	 (loop for (fun-name . fun) in (list 
					;; (cons "constant" (lambda (step) 2.5))
					(cons "Linear increase" (lambda (step) (+ 2 (* step 0.08))))
					(cons "Linear decrease" (lambda (step) (+ 10 (* step -0.08))))
					(cons "Non Linear increase" (lambda (step) (* 5 (log step 10))))
					(cons "Non Linear decrease" (lambda (step) (* 5 (log (- 101 step) 10))))
					;; (cons "Sine" (lambda (step) (+ 5 (* (sin (* pi 2 (/ step 100))) 4))))
					;; (cons "Sine2" (lambda (step) (+ 5 (* (sin (* pi 5 (/ step 100))) 4))))
					)
	    do
	      (format t "~a~%" fun-name)
	      (model-init)
	      (let ((modelInflow-queue '( )) ;; configure delay here
		    (waterLevel 2.0))

		(setq modelInflow-queue (append modelInflow-queue 
						(list (model-get-inflow waterLevel 
									0 ;; inflow not shown to the user
									))))

		(loop for step from 1 to 100 do
		     (setq *step* step)
		     (let ((chosenModelInFlow (car (last modelInflow-queue)))
			   (envirInflow (funcall fun step)))
		       
		       (setq waterLevel (+ waterLevel envirInflow))
		       (setq waterLevel  (+ waterLevel (car modelInflow-queue)))
		       (setq waterLevel (max 0 waterLevel))
		       (setq modelInflow-queue (cdr modelInflow-queue))

		       (format *stats-output* "~a ~a ~a ~a ~a \"~a\" ~,5F ~,5F ~,5F ~%"
			       *environ-wait* *calc-time-factor* *blend-temperature* subject step fun-name envirInflow waterLevel chosenModelInFlow)

		       (setq modelInflow-queue (append modelInflow-queue 
						       (list (model-get-inflow waterLevel envirInflow))))
		       )) ;)))
     )))
(close stats-output))
;; (update-emacs-R))
)

(defun run-trace (&optional num-subjects)
  
  (let ((data-input (open "/Users/dr/MURI/ACT-UP/Challenge/human_data.csv"
			    :direction :input)) 
	(stats-output (open "/Users/dr/MURI/ACT-UP/water-trace.txt" 
			    :direction :output :if-exists :supersede :if-does-not-exist :create))
	(current-subject))
    (setq *stats-output* stats-output)
    (format *stats-output* "subject step Version trend waterLevel chosenValveSetting humanValveSetting~%")
     
    (read-line data-input nil 'eof)
    (loop for data-line = (read-line data-input nil 'eof)
	    until (eq data-line 'eof)
	    do
	      (let ((data (split-sequence:SPLIT-SEQUENCE #\, data-line )))
		;(print data-line)
		;; Version,Subject,Time Step,EnvirInFlow,EnvirOutFlow,UserInFlow,UserOutFlow,AmountInTank,Goal

		(let ((fun-name (nth 0 data))
		      (subject (nth 1 data))
		      (step  (read-from-string (nth 2 data)))
		      (envirInflow  (read-from-string (nth 3 data)))
		      (waterLevel  (read-from-string (nth 7 data)))
		      (humanFlow (- (read-from-string (nth 5 data))  (read-from-string (nth 6 data)))))

		  (when (not (equal subject current-subject))
		    (setq current-subject subject)
		    (model-init))
		  (let ((chosenModelInFlow  (model-get-inflow waterLevel envirInflow)))
		    (format *stats-output* "~a ~a \"~a\" ~,5F ~,5F ~,5F ~,5F ~%" subject step fun-name envirInflow waterLevel chosenModelInFlow humanFlow)))))
    (close data-input)
    (close stats-output))
  (update-emacs-R))


;; Debugging

(defun pc ()
  "Print trend chunks"
  (loop for c in (model-chunks act-up::*current-actup-model*) when (trend-p c) do
       (format t "~a trend=~,2F  (~,2F, ~a)~%" (trend-type c) (trend-trend c) (chunk-get-activation c) (chunk-first-presentation c))))

;; (defun update-emacs-R ()
;;   (ccl::run-program "~/sv.aquamacs-emacs.git/nextstep/Aquamacs.app/Contents/MacOS/bin/emacsclient"
;; 		    (list "-eval" "(process-send-string (get-ess-process ess-current-process-name) \"pl()\\n\")")
;; 		    :output t ))
 
       
;; 
(defun test-blending ()
  (model-init)
  (format t "c(")
  (loop for i from 1 to 500 do
       (learn-chunk  (make-trend :trend (+ 10.0 (act-r-noise 0.3))))
       (actUP-pass-time 15)
       (format t "~a, " (trend-trend (blend (filter-chunks (model-chunks act-up::*current-actUP-model*)
							   '(:type trend)
							   ) nil 'trend))))
  (format t " NA)~%~%"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SOCKET API

(defstruct water-state
DSFState
time
goal
amount
ui
uo
ei
eo)

(defvar *last-conn-data* nil)
(defvar *last-water-state* nil)

(defun conn-read ()  
  (let ((data (uni-socket-read-line *v*))) 
    (setf data (format nil "(~a)" data))
    (setf data (read-from-string data))
    (when (listp data)
      ;(print data)
      ;; (STATE TIME 2 GOAL 4.0 AMOUNT 4.0 EI -1.0 EO -1.0 UI 0.0 UO 0.0) 
      (setq *last-conn-data* data)
      (setq *last-water-state*
	  (make-water-state
	   :DSFState (nth 0 data)
	   :time     (nth 2 data)
	   :goal     (nth 4 data)
	   :amount   (nth 6 data)  
	   :ui       (nth 12 data)
	   :uo       (nth 14 data)
	   :ei       (nth 8 data)
	   :eo       (nth 10 data)))
      (push (- (water-state-amount *last-water-state*) (water-state-goal *last-water-state*))
	    (deviation (transducer *hpc-object*)))
      (push (abs (- (water-state-amount *last-water-state*) (water-state-goal *last-water-state*)))
	    (abs-deviation (transducer *hpc-object*)))
      (push (water-state-time *last-water-state*)
	    (ctime (transducer *hpc-object*)))
      (push (water-state-goal *last-water-state*)
	    (goal (transducer *hpc-object*)))
      (push (water-state-amount *last-water-state*)
	    (amount (transducer *hpc-object*)))
      (push (water-state-dsfstate *last-water-state*)
	    (dsfstate (transducer *hpc-object*)))
      (update-transducer (transducer *hpc-object*))	    	    
      )))

(defparameter *dsf-host* nil)
(defparameter *dsf-port* nil)
(defparameter *v* nil)

(defun connsoc (entry-id name version)
  (when *v*
    (close *v*)
    (setq *v* nil))
  (setq *v* (uni-make-socket *dsf-host* *dsf-port*))
  (setq *last-water-state* (make-water-state))
  (uni-send-string
   *v*
   (format nil "~a ~a ~a ~a ~a ~a ~d ~a ~d" entry-id name "version" version "NONACTR" "goal" (goal *hpc-object*) 
	   "amount" (initial-value *hpc-object*))) ;when initialized, the server is now getting initial goal and amount levels from the code here
  (conn-read)) 

(defun conn-decide (ui uo)
  (if (set-next-segment (generator *hpc-object*) :ui ui :uo uo)
    (let ((csegment (get-next-segment (generator *hpc-object*))))
      (uni-send-string *v* (format nil "DECISION UI ~d UO ~d EI ~d EO ~d" 
				   (first csegment) (second csegment) (third csegment) (fourth csegment)))
      (conn-read)
      t)
    nil))
  
(defun conn-quit ()
 (uni-send-string *v* (format nil "QUIT")))

(defun run-model (params)
  (let ((keyList "")
        (conditions (eval (read-from-string (get-matching-line (file-string (get-arg 1)) "conditions=")))))
    (loop for value being the hash-values of params
       using (hash-key key)
       do (setf keyList (concatenate 'string keyList (format nil ":~a ~a " key value))))
    (setf keylist (concatenate 'string "(run-socket " 
                               keyList 
                               (format nil ":port ~a " (+ 9548 (eval (read-from-string (get-word (get-arg 3)))))) 
                               ")"))
    (dolist (condition conditions)
      (setf *hpc-object* (make-instance 'experiment-class
                                        :generator (make-instance condition)
                                        :transducer (make-instance 'david-transducer)
                                        :ccondition condition))
      (eval (read-from-string keyList))
      (post-process-object *hpc-object*)
      (dotimes (i (length (abs-deviation (transducer *hpc-object*))))
        (format t "mod~aAbsDev~a=~a~%" condition (+ i 1) (nth i (abs-deviation (transducer *hpc-object*))))))))
   
(defun run-socket (&key (host nil) (port nil) (blend-temperature 0.5) (calc-time-factor 1.0) (environ-wait 8) (bll .5) (ans .2 )
		   (guess-time-factor 5) (exact-time-factor 10) (calculation-enabled 1.0) (estimation-enabled 1.0))
  (setf *blend-temperature* blend-temperature)
  (setf *calc-time-factor* calc-time-factor)
  (setf *environ-wait* environ-wait)
  (setf *bll* bll)
  (setf *ans* ans)
  (setf *guess-time-factor* guess-time-factor)
  (setf *exact-time-factor* exact-time-factor)  
  (setf *calc-strategies-enabled* (> calculation-enabled .5))
  (setf *guess-strategy-enabled* (> estimation-enabled .5))
  ;not even sure if we need this, since the 'goal' is echoed back by the server, and stored in *last-water-state*
  ;this is why I'm OK with accessing the *hpc-object* directly here, and not passing 'goal-level' in as a parameter
  (setf *goal-level* (goal *hpc-object*)) 
  (model-init)
  (setf *dsf-host* (aif host it "127.0.0.1"))
  (setf *dsf-port* (aif port it 9548))
  (connsoc "ENTRY-ID" "reitter-01" "BATCH")
  (setf flag t)
  (while flag
    (setq *goal-level* (water-state-goal *last-water-state*))
    (let ((new-model-inflow
	   (model-get-inflow 
	    (or (water-state-amount *last-water-state*) 0) ;; current water level
	    (- (max 0 (or (water-state-ei *last-water-state*) 0)) 
	       (max 0 (or (water-state-eo *last-water-state*) 0))) ;; inflow
	    )))
      ;(print new-model-inflow)
      (setf flag
	    (conn-decide ;when no more trials are to be presented, change flag to nil
	     (if (> new-model-inflow 0) new-model-inflow 0)
	     (if (> new-model-inflow 0) 0 (- new-model-inflow))))))
  (conn-quit))

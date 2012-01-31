;; ACT-UP

;; DR, 04/2009

;; NOT FULLY FUNCTIONAL       

;; to load:
;; (require "act-up" "act-up.lisp")
;; (use-package :act-up)



;; production rules?

;; (load "lispdoc.lisp")
;; (lispdoc:lispdoc-html "doc/" :act-up)

(declaim (optimize (speed 1) (space 0) (debug 3)))
 

(defpackage :act-up
  (:use :common-lisp))

(in-package :act-up)



(load (format nil "~a/actr6-compatibility.lisp" (directory-namestring *load-truename*)))
(load (format nil "~a/actr-aux.lisp" (directory-namestring *load-truename*)))

(defstruct meta-process
  "An ACT-UP meta process."
  (actUP-time 0.0 :type number)
  name
)
(export '(meta-process))

(defparameter *current-actUP-meta-process* (make-meta-process))


;; CHUNKS


;; all chunks inherit from this structure:


(export '(chunk define-chunk-type))

(defstruct chunk
  "Type defining an ACT-UP chunk.
Derive your own chunks using this as a base structure
by specifying (:include chunk)."
  ;; helpful for debugging
  name
  (comment nil)
  ;; internal ACT-UP structures
  (total-presentations 0 :type integer)
  (first-presentation nil)
  (recent-presentations nil :type list) ; with the most recent one in car!
  (presentations nil :type list)
  (last-bl-activation 0)
  (activation-time nil)

  ;; we guarantee that the noise is constant
  ;; if time is constant
  (last-noise nil)
  (last-noise-time nil)
 
  (id (gensym "chunk") :type atom)
  (related-chunks nil :type list)

  ;; working on this:
  (references nil :type list)  ; list of chunks that this chunk references
  (referenced-by nil :type list) ; list of chunks that reference this chunk

  (fan nil) ; internal)
)
;; (defparameter chunk-standard-slots '(total-presentations first-presentation recent-presentations presentations last-bl-activation activation-time last-noise last-noise-time id related-chunks references referenced-by fan))

(defmacro define-chunk-type (name &rest members)
  "Define a chunk type of name NAME.
MEMBERS should contain all possible elements of the chunk type.
NAME may be a symbol or a list of form (name2 :include parent-type),
whereas PARENT-TYPE refers to another defined chunk type whose
elements will be inherited.
MEMBERS may be a list of symbols, or also a list of member
specifiers as used with the lisp `defstruct' macro, which see."
  
  (let* ((name-and-options name)
	 (incl
	  (if (consp name-and-options)
	      (list (car name-and-options)
		    (if (eq (cadr name-and-options) :include)
			`(:include ,(caddr name-and-options))
			(error "define-chunk-type: faulty options in NAME.")))
	      (list name-and-options '(:include chunk)))))
    `(defstruct ,incl
       ,@members)
    ))
 

;; (macroexpand '(define-chunk-type test one two))

(defun update-chunk-references (chunk)
  ;;(loop for (_slot . _value) in (structure-alist chunk) do
       ;; to do
       (print "update-chunk-references: not implemented")
       )


;; parameters

(defparameter *bll* 0.5 "Base-level learning decay parameter")
(defparameter *blc* 1.7 "Base-level constant parameter") ; 1.7  ;; seems to result in a ceiling
(defparameter *rt* 1.0 "Reaction time parameter")  ; can be (cons 'pres 4)

(defparameter *ans* 0.2 "Transient noise parameter") ;; transient noise  0.2

(export '(*bll* *blc* *rt* *ans*))

;; a model

;; (defvar *actUP-model-parameters* '(bll blc ol dm-noise))
;; (defstruct model-parameters
;;   (bll 0.5 :type number)
;;   (blc 0.0 :type number)
;;   (ol 2 :type integer)
;;   (dm-noise 0.1 :type number)
;; )

(defstruct declarative-memory
  (chunks nil :type list))

(defstruct model 
  (parms nil :type list) 
  ;; overriding model-specific parameters. association list
  ;; of form (PARM . VALUE).
  ;; if an entry for PARM is present, it will be used rather
  ;; than the global binding.
  ;; NOT IMPLEMENTED YET.

  (dm (make-declarative-memory) :type declarative-memory))


(defparameter *current-actUP-model* (make-model))
(defparameter *actUP-time* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLIENT FUNCTIONS

(export '(current-actUP-model set-current-actUP-model actUP-time actUP-time actUP-pass-time model-chunks 
	  defrule assign-reward
	  define-chunk-type
	  show-chunks chunk-name explain-activation
	  retrieve-chunk blend-retrieve-chunk
	  filter-chunks learn-chunk best-chunk blend reset-mp reset-model
	  reset-sji-fct add-sji-fct ))


(defun current-actUP-model ()
  "Evaluates to the currently active ACT-UP model."
  (or *current-actUP-model*
      (error "No model active.")))

(defun set-current-actUP-model (new-model)
  "Switches the currently active ACT-UP model.
This may set a range of model parameters."
  (setq *current-actUP-model* new-model))

(defun actUP-time (&optional meta-process)
  "Returns the current runtime
An optional parameter META-PROCESS specifies the meta-process to use.
It defaults to the current meta-process."
  (meta-process-actUP-time (or meta-process *current-actUP-meta-process*)))

(defun actUP-pass-time (seconds &optional meta-process)
  "Simulates the passing of time.
An optional parameter META-PROCESS specifies the meta-process to use.
It defaults to the current meta-process."
  (setf (meta-process-actUP-time (or meta-process *current-actUP-meta-process*))
	(+ (meta-process-actUP-time (or meta-process *current-actUP-meta-process*)) seconds)))


(defmacro model-chunks (model)
  "Evaluates to the list of chunks in the given model MODEL."
  `(declarative-memory-chunks (model-dm ,model)))


(defun show-chunks (model &optional constraints)
  "Prints all chunks in model MODEL subject to CONSTRAINTS.
See the function `filter-chunks' for a description of possible constraints."
  (print (mapcar 'chunk-name (if constraints
				 (search-for-chunks model constraints)
				 (declarative-memory-chunks (model-dm model))))))

(defun explain-activation (chunk &optional cues retr-spec)
  "Returns a string with an explanation of the evaluation of CHUNK.
CUES contains retrieval cues spreading activation.
RETR-SPEC describes the retrieval specification for partial matching retrievals."
  (when chunk
    (format nil "  ~a  ~a base-level: ~a  (~a pres) pm: ~a ~a"
	    (actUP-time)
	    (chunk-name chunk)
	    (chunk-get-base-level-activation chunk)
	    (chunk-total-presentations chunk) ;; (chunk-recent-presentations chunk)
	    (if cues
		(format nil "  spreading: ~a" (chunk-get-spreading-activation chunk cues))
		"")
	    (if retr-spec 
		(format nil "partial match: ~a " (chunk-get-partial-match-score chunk retr-spec))
		"-")
	    )))
  

(defmacro normalize-slotname (slot)
  `(intern (string-upcase (symbol-name ,slot))))
(defmacro normalize-slotname-with-package (slot)
  `(intern (string-upcase (symbol-name ,slot)) 'act-up))

(defun search-for-chunks (model args)
;  (say "searching for chunks ~a" args)
  (filter-chunks (model-chunks model) args))




(defun retrieve-chunk (spec &optional cues pm-soft-spec)
  "Retrieve a chunk from declarative memory.
The retrieved chunk is the most highly active chunk among those in
declarative memory that are retrievable and that conform to
specification SPEC.

CUES is, if given, a list of chunks that spread activation
to facilitate the retrieval of a target chunk.

PM-SOFT-SPEC is, if given, a retrieval specification whose 
constraints are soft; partial matching is used for this portion
of the retrieval specification. 

SPEC and PM-SOFT-SPEC are lists of the form (:slot1 value1 :slot2
value2 ...), or (slot1 value1 slot2 value2)."

  (best-chunk (filter-chunks (model-chunks *current-actUP-model*)
			     spec)
	      cues (append spec pm-soft-spec)))


(defun blend-retrieve-chunk (spec &optional cues pm-soft-spec)
  "Retrieve a blended chunk from declarative memory.
The blended chunk is a new chunk represeting the chunks
retrievable from declarative memory under specification SPEC.
The contents of the blended chunk consist of a weighted average
of the retrievable chunks, whereas each chunk is weighted
according to its activation.

CUES is, if given, a list of chunks that spread activation
to facilitate the retrieval of target chunks.

PM-SOFT-SPEC is, if given, a retrieval specification whose 
constraints are soft; partial matching is used for this portion
of the retrieval specification. 

SPEC and PM-SOFT-SPEC are lists of the form (:slot1 value1 :slot2
value2 ...), or (slot1 value1 slot2 value2)."
  (let ((cs (filter-chunks (model-chunks *current-actUP-model*)
			     spec)))
    (if cs
	(blend cs cues nil (append spec pm-soft-spec)))))
    


(defun filter-chunks (chunk-set args)
  "Filter chunks according to ARGS.
ARGS is a list of the form (:slot1 value1 :slot2 value2 ...),
or (slot1 value1 slot2 value2).
CHUNK-SET is the list of chunks to be filtered (1), or an associative array (2)
of the form ((X . chunk1) (Y . chunk2) ...).
returns a list of chunks in case (1) and a list of conses in case (2)."

  (loop for chunk-or-cons in chunk-set append
       (let ((c (if (consp chunk-or-cons) (cdr chunk-or-cons) chunk-or-cons)))
	 (if (loop for argval on args by #'cddr finally (return t) do
		  (let* ((slot (first argval))
			 (value (second argval))
			 (slot-value (slot-value c (normalize-slotname slot))))
		    (unless (or (equal slot-value value)
				(and (eq value 'non-nil) slot-value))
		      (return nil))))
	     (list chunk-or-cons)
	     nil))))



(defparameter *actup--chunk-slots* (mapcar #'car (structure-alist (make-chunk)))
  "Internal to ACT-UP.")


(defun learn-chunk (chunk)
"Learn chunk CHUNK.

This will note a presentation of an existing chunk in the model's DM, if
the existing chunk is unifiable with CHUNK.  If no such chunk exists in DM,
the CHUNK will be added.  If more than one such chunk exists, one of the existing
chunks is noted as 'presented'.

CHUNK may be altered by side-effect.

Returns the added chunk."
  (let* ((model *current-actUP-model*)
	 (chunk-descr
	  (loop for (slot . val) in (structure-alist chunk) append
	       (unless (member slot *actup--chunk-slots*)
		 (list slot val))))

	 ;; we're either taking an existing chunk, or we're adding a new one.
	 (chunk (or (car (search-for-chunks model chunk-descr))
		    (progn
		      (setf (chunk-total-presentations chunk) 0)
		      (setf (chunk-first-presentation chunk) (actUP-time))
		      (push chunk (model-chunks model))
		      chunk))))

    (incf (chunk-total-presentations chunk))
    (push (actUP-time) (chunk-presentations chunk))
    
    (push (actUP-time) (chunk-recent-presentations chunk))
    (if (> (length (chunk-recent-presentations chunk)) 3)
	(setf (cdr (nthcdr 2 (chunk-recent-presentations chunk))) nil)) ;; only OL 3
    (actup-pass-time 0.05) ;; 50ms
    chunk))


(defparameter *lf* 1.0)
(defparameter *le* 1.0)

(defun best-chunk (confusion-set cues &optional request-spec &rest options)
"Retrieves the best chunk in confusion set.
CONFUSION-SET is a list of chunks, out of which the chunk is returned.
CUES is a list of cues that spread activation.
OPTIONS: do not use (yet).

Simulates timing behavior.

See also the higher-level function `retrieve-chunk'."

 ;; retrieve using spreading activation from cues to confusion-set
  (if confusion-set
      (let* ((last-retrieved-activation nil)
	    (best  (loop for c in confusion-set with bc = nil with bs = nil 
			when (if (eq options 'inhibit-cues) (not (member c cues)) t)
			when c ;; allow nil chunks
		  do
		    (let ((s (chunk-get-activation c cues request-spec)))
		     
		      (if (or (not *rt*) 
			      (if (consp *rt*)
				  (> (length (chunk-presentations c)) (cdr *rt*))
				  (> s *rt*)))
			  (if (or (not bc) (> s bs)) (setq bc c bs s))

		;	  (say "chunk ~a falls below RT" (concept-name c))
			  ))
		  finally
			(progn (setq last-retrieved-activation 
				     bs)
			       (return bc))
		    )))

	;; timing
	(if last-retrieved-activation
	    (actUP-pass-time (* *lf* (exp (- (* *le* last-retrieved-activation))))))
	
	;; (say "found best chunk: ~a " (if best (concept-name best) nil))
	best)))

(defun best-n-chunks (n confusion-set cues)
  "Retrieves the best chunks in confusion set.
CONFUSION-SET is a list of chunks, out of which the best N chunks will
be returned. CUES is a list of cues that spread activation.

See also the higher-level functions `retrieve-chunk' and
`blend-retrieve-chunk'."

  ;; retrieve using spreading activation from cues to confusion-set
  (if confusion-set
      (let ((all  (loop for c in confusion-set 
		  append
		    (let ((s (+ (chunk-get-base-level-activation c)
				(if *ans* (act-r-noise *ans*) 0)
				(chunk-get-spreading-activation c cues))))
		     
		      (if (or (not *rt*) 
			      (if (consp *rt*)
				  (> (length (chunk-presentations c)) (cdr *rt*))
				  (> s *rt*)))
			  (list (cons s c)))))
			  ))
	(mapcar 'cdr (subseq (stable-sort all #'> :key #'car) 0 (min n (length all) ))))))

 
(defparameter *blend-temperature* 1.0)

(defun blend (chunks &optional cues chunk-type retrieval-spec)
  "Return a blended variant of chunks.
Activation is calculated using spreading activation from CUES.
The returned chunk is of type CHUNK-TYPE; all CHUNKS must be
of type CHUNK-TYPE or of a supertype thereof.  If CHUNK-TYPE is not
given, all CHUNKS must be of the same class and the returned type
will be this class.
RETRIEVAL-SPEC should contain the retrieval filter used to
obtain CHUNKS; attribute-value pairs in it will be included in
the returned chunk as-is and not be blended from the CHUNKS.

See also the higher-level function `blend-retrieve-chunk'."
 

 
  (if (and chunk-type (symbolp chunk-type))
      (setq chunk-type (find-class chunk-type)))

  ;;convert flat list into assoc list
  (let ((auto-chunk-type nil) 
	(empty-chunk (make-chunk))
	(blend-activation 0)
	(retrieval-spec-alist
	 (loop for (a b) on retrieval-spec by #'cddr collect
	      (cons a b)))
	(slot-values-by-name))
    ;; collect all slots in all chunks along with their values
    (loop for c in chunks do 

	 (if auto-chunk-type
	     (unless (eq chunk-type (class-of c))
	       (error (format "blend: Found chunk of different types ~a and ~a, and no CHUNK-TYPE given."
			      nil (class-name (class-of c)) (class-name chunk-type))))
	     (if chunk-type 
		 (unless (subtypep chunk-type (class-of c))
		   (error (format "blend: Chunk of type ~a is not supertype of given type ~a."
				  nil (class-name (class-of c)) (class-name chunk-type))))
		 (setq auto-chunk-type t
		       chunk-type (class-of c))))

	 (let ((act (chunk-get-activation c cues retrieval-spec)))
	   (setq blend-activation (+ blend-activation
				     (exp act)))
	   (loop for s in (structure-alist c) do
		(when (and (not (assoc (car s) retrieval-spec-alist))
			   ;; not an internal slot from "chunk"
			   (not (slot-exists-p empty-chunk (normalize-slotname-with-package (car s))))
			   ;;   (not (member (normalize-slotname-with-package (car s)) chunk-standard-slots))
			   )
		  (if (not (assoc (car s) slot-values-by-name))
		      (push (cons (car s) nil) slot-values-by-name))
		  (push
		   (cons act (cdr s)) ;; value
		   (cdr (assoc (car s) slot-values-by-name)))))))
    
    (setq blend-activation (if (> blend-activation 0) (log blend-activation) 0)) 
   
    ;; let's blend
    ;; currently, only numerical values are supported    
    
    
    ;; to do: return real object
    ;; (class-of c) ??
    ;; (make-instance class ...)

   (if (and (> blend-activation *rt*) (or retrieval-spec slot-values-by-name))
       ;; (apply #'make-instance    does not work for structures (openmcl)
       ;; 	      chunk-type 
       (apply (find-symbol (format nil "MAKE-~a" (class-name chunk-type)))
	      :last-bl-activation blend-activation
	      :activation-time (actup-time)
	      :comment 'blended
	      (append
	       retrieval-spec
	       (loop for sv in slot-values-by-name append
		  ;; calculate weighted mean
		  ;; weights are determined by boltzmann activation of each chunk
		  ;; i.e. retrieval probability
		  ;; Pi = exp(Ai/t) / sum(j, exp(Aj/t))
		    
		    (let* ((boltzmann-total 0)
			   (sum
			    (loop for (a . n) in (cdr sv) sum
				 (if (numberp n)
				     (let ((boltzmann-act  (exp (/ a *blend-temperature*))))
				       (setq boltzmann-total (+ boltzmann-total boltzmann-act)) ; denominator in boltzmann eq
				       (* boltzmann-act n))
				     (return nil)))))
		      (when sum
			(list (car sv) (/ sum boltzmann-total)))))))
       nil)))

(defun reset-mp ()
  (setq *current-actUP-meta-process* (make-meta-process)))

(defun reset-model ()
  (setq *current-actUP-model* (make-model)))

(defun reset-sji-fct (chunk)
  (setf (chunk-related-chunks chunk) nil))

(defun add-sji-fct (list)
  (loop for (c1 c2 s) in list do
       (unless (eq c1 c2)
	 (setf (chunk-related-chunks c1) (delete (rassoc c2 (chunk-related-chunks c1)) (chunk-related-chunks c1)))
	 (setf (chunk-related-chunks c1)
	       (insert-by-weight (list (cons s c2)) (chunk-related-chunks c1))))
       ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INTERNAL

; private 
;; (defun chunk-slot (chunk slot-name)
;;   (let ((sym (find-symbol (format nil "~A-~A" (type-of chunk) slot-name)
;;                              ;; #.(package-name *package*
;; 			     )))
;;     (if sym
;; 	(funcall (symbol-function
;; 		  sym)
;; 		 chunk)
;; 	(error (format nil "Slot ~a not found." slot-name)))))

(defun chunk-slot (chunk slot-name)
 (slot-value chunk slot-name))
 

(defun chunk-get-fan  (j _i chunk-set)

  (print "chunk-get-fan: not implemented.")

  ;; how to iterate over all slots?

  (let* ((j-al (structure-alist j))
	 (j-len (length j-al)))
    
    (loop for c in chunk-set sum
	 (structure-value-count c j (chunk-id j))
	 ;; to do
)))


(defun chunk-get-noise (chunk)
  (if *ans* 
      (or (and (eq (actUP-time) (chunk-last-noise-time chunk))
	       (chunk-last-noise chunk))
	  (progn
	    (setf (chunk-last-noise chunk) (act-r-noise *ans*)
		  (chunk-last-noise-time chunk) (actUP-time))
	    (chunk-last-noise chunk)))
      0))

(defun chunk-get-activation (chunk &optional cue-chunks retrieval-spec)
  "Calculate current activation of chunk"

  (let ((base-level (chunk-get-base-level-activation chunk))
	(spreading (chunk-get-spreading-activation chunk cue-chunks))
	(partial-matching (chunk-get-partial-match-score chunk retrieval-spec))
	(noise (chunk-get-noise chunk)))

	(+ base-level spreading partial-matching noise)))


;; export

    

; to do
; relax "member" - can't work like this
; update activation
; update references
; initialize name if necessary
; 


   ;; set time


;   (setf (declarative-memory-chunks (model-dm (current-actUP-model))) chunks)






(defun chunk-get-base-level-activation (chunk)

  ;; we're using the Optimized Learning function

  (let ((d *bll*)) ;; (model-parameters-bll (model-parms (current-actUP-model)))
    (+
     ;; standard procedure
     (loop for pres in (chunk-recent-presentations chunk) sum
	  (expt (max 1 (- (actUP-time) pres)) (- d)))
     
     ;; initial BL activation  (e.g., from blending)
     (or (chunk-last-bl-activation chunk) 0)

     ;; optimized learning
     (let ((k (length (chunk-recent-presentations chunk))))
       (if (and (> (chunk-total-presentations chunk) k) (chunk-first-presentation chunk))
	   (let ((last-pres-time (max 1 (- (actUP-time) (or (car (last (chunk-recent-presentations chunk))) 
							       (chunk-first-presentation chunk))))) ;; 0? ;; tn
		 (first-pres-time (max 1 (- (actUP-time) (chunk-first-presentation chunk)))))
	     (if (and first-pres-time
		      (not (= first-pres-time last-pres-time)))
		 (progn
		   (/ (* (- (chunk-total-presentations chunk) k) 
			 (max 0.1 (- (expt first-pres-time (- 1 d)) (expt last-pres-time (- 1 d)))))
		      (* (- 1 d) (max 0.1 (- first-pres-time last-pres-time)))))
		 0))
	   0))
     *blc*)))

(defun chunk-get-spreading-activation (chunk cues)
  (if cues
      (* 1
      (/ (loop for cue in cues sum
	      (or (car (rassoc chunk (chunk-related-chunks cue))) 0.0))
	 (length cues)))
      0))



(defparameter *pm* 1.0)
(defun chunk-get-partial-match-score (chunk retrieval-spec)
  (if *pm*
      (progn
	(* *pm*
	   (loop for (s v) on retrieval-spec  by #'cddr sum
		(value-get-similarity (slot-value chunk (normalize-slotname s)) v))
	   ))

      ;; else
      0))

(defun value-get-similarity (v1 v2)
  (if (and (numberp v1) (numberp v2))
      (if (= v1 v2)
	  0
	  (- (abs (- v1 v2))))  ;; could be done better!
      0 ;; not implemented yet
    ))



;; PROCEDURAL

;; tests

;; (setq *actup-rulegroups* nil)
;; (defrule rule1 (arg1 arg2) :group g1 (print arg1))
;; (defrule rule1b (arg1 arg2) :group (g1 g5) (print arg1))
;; (defrule rule2 (arg2 arg3) :group (g1 g2) (print arg1))
;; (defrule rule3 (arg3 arg4) :group g2 (print arg1))
;; (equal *actup-rulegroups* '((G2 (RULE3 ARG3 ARG4) (RULE2 ARG2 ARG3)) (G5 (RULE1B ARG1 ARG2)) (G1 (RULE2 ARG2 ARG3) (RULE1B ARG1 ARG2) (RULE1 ARG1 ARG2))))
;; (g1 'working 'huh)

(defun set-alist (key val alist-name)
  (let ((kv (assoc key (eval alist-name))))
    (if kv
	(progn (setf (cdr kv) val) 
	       (eval alist-name))
	(set alist-name (acons key val (eval alist-name))))))


(defmacro defrule (name args &rest body)
  "Define an ACT-UP rule.
The syntax follows the Lisp `defun' macro, except
that after arguments to the function to be greated, a 
:group GROUP parameter may follow, defining one or more
rule groups that the rule will belong to.  All rules
defined as part of GROUP must have the same argument
footprint.

This macro will define a Lisp function of name NAME with
arguments ARGS.

If GROUP is given, a function of name GROUP will also be
defined that invokes one of the rules assigned to GROUP."
  ;; remove keyword args from body
  (let ((groups
	 (let (keyw group)
	   (loop while (keywordp (setq keyw (car body))) do
	     (setq body (cdr body))
	     (case keyw
	       (:group (setq group (pop body)))
	       ;; (t (push keyw extra-keywords) (push (pop body) extra-keywords))
	       ))
	   (if (consp group) group (list group)))))

    `(progn
    (defun ,name ,args
       (actup-rule-start ',name ',args)
;; to do: handle signals
       (let ((actup---rule-result
	      (progn
		,@body)))
	 (actup-rule-end ',name actup---rule-result)
	 actup---rule-result))
    (declare-rule ',groups
		   ',name ',args)
)))

;; FIXME this needs to go into the current model!!!!
(defparameter *actup-rule-queue* nil)
(defun actup-rule-start (name args)
  ;;(format t "start: ~s ~s" name args)
  (setq *actup-rule-queue* (cons (cons (actup-time) (sxhash (cons name args))) *actup-rule-queue*)))
(defun actup-rule-end (_name _result)
  ;; (format t "end: ~s ~s" name result)
  (actUP-pass-time 0.050) ;; to do: randomization (:vpft parameter)
)

;; this is, as of now, independent of the model
(defparameter *actup-rulegroups* nil)

;; this needs to go into the model structure
(defparameter *actup-rule-utilities* nil)

(defparameter *egs* nil) ;; transient rule noise

(defun declare-rule (groups name args)
  ;; to do: check number of arguments 
  (when groups
    (loop for g in groups do
	 ;; (re)define lisp function with group name
	 (eval `(defun ,g ,args 
		  ,(format nil "Choose a rule out of group %s" g)
		  (actup-eval-rule ',g ,@args)))

	 (let ((group-cons (assoc g *actup-rulegroups*)))
	   (if group-cons
	       (setf (cdr group-cons)  
		     (acons name args (cdr group-cons)))
	       
	       (setq *actup-rulegroups*
		     (acons 
		      g
		      ( acons name args nil)
		      *actup-rulegroups*)))))))

  
;; test case:
;; (defun rule2 (a1 a2) (print a1))
;; (actup-eval-rule 'g2 1 2)
(defun actup-eval-rule (group &rest args)
  "Evaluates an ACT-UP rule from rule group GROUP.
Passes arguments ARG to the lisp function representing 
the chose rule."

  ;; chose rule with highest utility
  (let* ((rule-util
	 (loop for r in (cdr (assoc group *actup-rulegroups*)) 
	    with utility = 0.0
	    with rule = nil
	    for r-utility = (+ (or (cdr (assoc (sxhash r) *actup-rule-utilities*)) *iu*) (if *egs* (act-r-noise *egs*) 0.0))
	    when (> r-utility utility)
	    do
	      (setq utility r-utility
		    rule r)
	    finally
	      (return (cons utility r ))))
	(rule (second rule-util))
	;; (util (car rule-util))
	 )
    (when rule
      (apply rule args))))

(export '(*au-rpps* *iu* *alpha* *au-rfr* assign-reward))
(defparameter *au-rpps* 0.05
  "Reward proportion per second elapsed.
e.g., after 10 seconds we want to assign 50% of the remaining reward: 0.5/10 = 0.05
time is in between rules.
See also the parameter `*au-rfr*' and the function `assign-reward'.")

(defparameter *au-rfr* 0.10
  "base reward proportion for each rule
e.g., the each rule before the reward trigger gets 10% of the reward.
See also the parameter `*au-rpps*' and the function `assign-reward'.")

(defparameter *alpha* 0.2  "utility learning rate
See also the function `assign-reward'.")

(defparameter *iu* 0.0 "initial utility
See also the function `assign-reward'.")

;; just a linear backpropagatino over time
; quue elements: (time . hash)
; (cons (cons (actup-time) (sxhash (cons name args))) *actup-rule-queue*)
(defun assign-reward (reward)
  "Assign reward to recently invoked rules.
Distributes reward value REWARD across the recently invoked rules.
See parameters `*au-rpps*', `*au-rfr*', `*alpha*', and `*iu*'."
  (let ((time (actup-time))
	(last-time (actup-time)))
    (loop for rc in *actup-rule-queue* do

	 (let* ((r-time (car rc))
		(r-rule-signature (sxhash (cdr rc)))
		(reward-portion (* reward
				   (+ *au-rfr*
				      (* *au-rpps* (- last-time r-time))))))

	   (setq reward (- reward reward-portion)
		 last-time r-time)
	   
	   ;; assign reward
	   
	   (let ((current (or (cdr (assoc r-rule-signature *actup-rule-utilities*)) *iu*)))
	     (set-alist r-rule-signature (+ current
		      (* *alpha* (- reward-portion current)))
			'*actup-rule-utilities*)
	     )))))


;; test case

(defun test-reward-assignment ()
  (setq *actup-rulegroups* nil)
  (defrule rule1 (arg1 _arg2) :group g1 (print arg1))
  (defrule rule1b (arg1 _arg2) :group (g1 g5) (print arg1))
  (defrule rule2 (arg2 _arg3) :group (g1 g2) (print arg2))
  (defrule rule3 (arg3 arg4) :group g2 (print arg3))
  
  (rule1 1 2)
  (rule2 1 2)
  (assign-reward 10.0)

  (g1 5 6)
)





;; experimental

;; maybe we'll leave all parameters global for now
;; (defun switch-to-model (model)

;;   ;; handle dynamic, global variables as model-parameters
;;   (loop for p in *actUP-model-parameters* do
;;        (let ((sym (format nil "*~a*" p)))


;;        (if (boundp sym) ;; dynamically bound
;; 	   (setf (slot-value 
;; 		  (model-parameters *current-actUP-model*)
;; 		  p)
;; 		 (symbol-value sym))
;; 	  ;; else
;; 	   (defvar (symbol-value sym) nil)) ;; make sure it's dynamic and defined
	 
;;        (setf sym (slot-value (model-parameters model) p))))
;;   (setq *current-actUP-model* model))



;; MODULES

;; locations used by visual and manual model
(define-chunk-type location 
    screen-x
  screen-y)

;(load (format nil "~a/au-visual.lisp" (directory-namestring *load-truename*)))
;(load (format nil "~a/au-manual.lisp" (directory-namestring *load-truename*)))




(provide "act-up")
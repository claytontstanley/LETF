;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Niels Taatgen
;;; Copyright   : (c) 2005 Niels Taatgen
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : taatgen@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : temporal.lisp
;;; Version     : 1.0b2
;;; 
;;; Description : Implementation of the temporal module.
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;; 
;;; ----- History -----
;;;
;;; 2005.04.26 Niels
;;;             : Initial creation.
;;; 2010.02.05 Dan [1.0b2]
;;;             : * Moved the noise added to t0 into the reset function and
;;;             :   made that a 3rd pass reset so that if the seed is set in
;;;             :   the model code the temporal module will perform the same
;;;             :   every time.  However, if the model time isn't 0 (other than
;;;             :   during a reset) then it will still add the noise when the 
;;;             :   :time-master-start-increment parameter is changed.
;;;             : * Along with that I changed it from a fixed .075 multiplier
;;;             :   (which is 5 * the default b) to actually 5 times the current
;;;             :   b value (:time-noise).
;;;             : * Also changed the return value for setting :time-master-start-increment
;;;             :   to always be the value set instead of the randomized value
;;;             :   since the non-setting sgp returns the non-randomized value.
;;;             : * Needs a primary reset to create the chunk type.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; User Functions:
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;;  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(require-compiled "GOAL-STYLE-MODULE" "ACT-R6:support;goal-style-module")

(defstruct temporal-module 
  time-noise 
  time-mult 
  time-start-increment 
  time-master-start-increment 
  tick 
  ticks
  next-increment)

(defun create-temporal-module (model-name)
  (declare (ignore model-name))
  (make-temporal-module)
  )


(defun temporal-reset-1 (instance)
  ; Do NOT strict harvest the temporal buffer by default
  (chunk-type time ticks)
  (setf (temporal-module-next-increment instance) nil))

(defun temporal-reset-3 (instance)
  ; Do NOT strict harvest the temporal buffer by default
  (sgp :do-not-harvest temporal)
  (setf (temporal-module-time-start-increment instance)
    (+ (temporal-module-time-master-start-increment instance)
       (act-r-noise (* (temporal-module-time-master-start-increment instance)
                       5 (temporal-module-time-noise instance))))))


(defun temporal-query (instance buffer-name slot value)
  (declare (ignore instance buffer-name))
  (case slot
    (state
     (case value
       (busy nil)
       (free t)
       (error nil)
       (t (print-warning "Unknown state query ~S to temporal module" value)
          nil)))
    (t (print-warning "Unknown query of the temporal module"))))


(defun temporal-request (instance buffer-name chunk-spec)
  
  ;; Don't want to perform the next increment if a new
  ;; request comes in or a bad value could get
  ;; written in.
  
  (when (temporal-module-next-increment instance)
    (delete-event (temporal-module-next-increment instance)))
  
  ;; Assuming that the only valid request is
  ;; +temporal> isa time
  ;;
  (when (not (eq 'clear (chunk-spec-chunk-type chunk-spec)))
    (if (and (eq 'time (chunk-spec-chunk-type chunk-spec))
             (null (chunk-spec-slots chunk-spec)))
      
      
      (progn
        
        (schedule-event-relative 0 'create-new-buffer-chunk 
                                 :module 'temporal
                                 :priority -100 
                                 :details 
                                 "create-new-buffer-chunk isa time"
                                 :params (list 'temporal '(isa time ticks 0)))
        
        (setf (temporal-module-tick instance) 
          (+ (temporal-module-time-start-increment instance) 
             (act-r-noise 
              (* (temporal-module-time-noise instance) 
                 (temporal-module-time-start-increment instance)))))
        
        
        (setf (temporal-module-ticks instance) 0)
        
        (setf (temporal-module-next-increment instance)
          (schedule-event-relative (temporal-module-tick instance)
                                   #'next-time-tick
                                   :module 'temporal
                                   :priority :min
                                   :details "Incrementing time ticks to 1"
                                   :destination 'temporal)))
                                              
      (print-warning "Invalid request made of the ~A module." buffer-name))))

(defun next-time-tick (instance)
  ;; if the chunk in the temporal buffer is of type time
  (when (and (buffer-read 'temporal)
             (eq 'time (chunk-chunk-type-fct (buffer-read 'temporal))))
    
    (incf (temporal-module-ticks instance))
     
    (setf (temporal-module-tick instance) 
      (* (temporal-module-tick instance) (temporal-module-time-mult instance)))
     
    
    (setf (temporal-module-tick instance) 
      (+ (temporal-module-tick instance) 
         (act-r-noise (* (temporal-module-time-noise instance)
                         (temporal-module-tick instance)))))
    
    ;; all buffer actions should done through a scheduled event
    
    (schedule-mod-buffer-chunk 'temporal (list 'ticks (temporal-module-ticks instance)) 0
                               :module 'temporal)
    
    (setf (temporal-module-next-increment instance)
      (schedule-event-relative (temporal-module-tick instance)
                               'next-time-tick
                               :module 'temporal
                               :priority :min
                               :destination 'temporal
                               :details (format nil "Incrementing time ticks to ~D"
                                          (1+ (temporal-module-ticks instance)))))))

(defun temporal-params (tmp param)
  (cond ((consp param)
         (case (car param)
           (:time-noise (setf (temporal-module-time-noise tmp) (cdr param)))
           (:time-mult (setf (temporal-module-time-mult tmp) (cdr param)))
           (:time-master-start-increment (setf (temporal-module-time-master-start-increment tmp) (cdr param))
                                         (unless (zerop (mp-time))
                                           (setf (temporal-module-time-start-increment tmp)
                                             (+ (cdr param) 
                                                (act-r-noise (* (cdr param)
                                                                5 (temporal-module-time-noise tmp))))))
                                         (temporal-module-time-master-start-increment tmp))))
        (t
         (case param
           (:time-noise (temporal-module-time-noise tmp))
           (:time-mult (temporal-module-time-mult tmp))
           (:time-master-start-increment (temporal-module-time-master-start-increment tmp) )))))         


;;; Actually define the module now

(define-module-fct 'temporal '(temporal)
  (list (define-parameter :time-noise :valid-test #'numberp :default-value .015
          :warning "a number" :documentation "Temporal noise")
        (define-parameter :time-master-start-increment :valid-test #'posnum :default-value .011
          :warning "a number" :documentation "Temporal start interval")
        (define-parameter :time-mult :valid-test #'posnum :default-value 1.1
          :warning "a number" :documentation "Temporal multiplier")

        )
  :version "1.0b2"
  :documentation "The temporal module is used to estimate short time intervals"
  :creation #'create-temporal-module
  :query #'temporal-query
  :request #'temporal-request
  :params #'temporal-params
  :reset (list #'temporal-reset-1 nil #'temporal-reset-3))



#|
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
|#

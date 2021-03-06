;;;  -*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell 
;;; Address     : Carnegie Mellon University
;;;             : Psychology Department
;;;             : Pittsburgh,PA 15213-3890
;;;             : db30+@andrew.cmu.edu
;;; 
;;; Copyright   : (c)2002-2005 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : uni-files.lisp
;;; Version     : 2.0
;;; 
;;; Description : Contains the system dependent code for things needed by
;;;             : the environment, but now part of support for use by other
;;;             : things as needed.
;;;             : Defines functions that can open an active socket, start
;;;             : a process, kill a process, check a socket and read a line
;;;             : from a stream.  Also contains any "setup" code necessary
;;;             : for a system.
;;; Bugs        : 
;;; 
;;; Todo        : Make sure the packaging stuff plays well here...
;;; 
;;; ----- History -----
;;;
;;; 05/21/2002  Dan
;;;             : Moved this from server.lisp to better organize things.
;;; 05/22/2002  Dan
;;;             : Some MCL fixes to always print in top listener, and 
;;;             : report errors in a way that doesn't error itself.
;;; 05/24/2002  Dan
;;;             : Had to add the uni-process-system-events because of the
;;;             : stepper - the only way to "stop" ACT-R is to NOT return
;;;             : from a hook, but that's problematic with multiple processes.
;;; 08/15/2002  Dan
;;;             : Started the work to add LispWorks support.
;;; 10/01/2002  Dan
;;;             : Updated version to 1.1 and fixed the packaging
;;;             : for building a standalone in ACL.
;;;             : Added the ACL standalone version of uni-run-process.
;;;             : Added the uni-wait-for function because the process-wait
;;;             : in ACL is much better than looping, and over time I can
;;;             : fill in the equivalent for the other systems.
;;; 11/17/2002  Dan
;;;             : Added a new version of uni-run-process for openmcl to
;;;             : support the standalone version.
;;; 12/05/2002  Dan
;;;             : Added create-valid-pathname to hack around an MCL 5 issue
;;;             : (the MCL 5 version is in mcl-fix because it's got a special
;;;             : macro character which I don't want to define for non-MCL
;;;             : Lisps).
;;;             : Removed the special uni-wait-for-char for MCL because
;;;             : I've figured out how to make the MCL sockets play
;;;             : friendly.
;;; 05/15/2003  : Dan
;;;             : Realized that I broke the uni-make-socket for ACL < v6
;;;             : because the version< test doesn't exist there...
;;; 08/15/2003  Dan
;;;             : Updated the version to 1.3.
;;;             : Added the support functions necessary for CMUCL from
;;;             : Ethan Glasser-Camp at RPI.  Made some minor edits to them
;;;             : and they seem to work now, though it may not be the best
;;;             : situation for the stepper.
;;;             : If anybody has a more elegant solution for the 
;;;             : uni-process-system-events funtion for CMUCL please let me 
;;;             : know.
;;;             : The issue is that while the stepper is active, the Lisp
;;;             : running the model is sitting in a loop waiting for the
;;;             : stepper.  That busy loop needs to do something to:
;;;
;;;             : a) allow a "signal" to get in to say it's time to advance
;;;             : b) not crush the processor if possible.
;;;             :
;;;             : Originally, I tried a null function (no code inside) but
;;;             : that didn't work well because the Lisp sat busy grinding
;;;             : away on that tight loop and never exited or yielded the 
;;;             : processor.  So, as a quick and dirty hack, since I don't
;;;             : know enough about CMUCL multiprocessing I've just made it
;;;             : a call to sleep for 1 second.  That seems to work, but 
;;;             : does make the stepper window a little less responsive.
;;;
;;;             : Also, there are a lot of warnings about unused variables
;;;             : in lambda functions printed as it runs, which I don't
;;;             : know how to suppress.  So, again, if anybody knows the
;;;             : switch to turn that off I'd appreciate it.  Of course,
;;;             : what I really need to do is declare those parameters 
;;;             : as ignored, but that's going to take a while to do...
;;;             : 
;;; 12/11/2003  Dan
;;;             : Updated version to 1.4
;;;             : Just realized that there's a problem with the
;;;             : feature test because CMUCL can use the mp::process-wait
;;;             : version of the uni-wait-for which seems to address
;;;             : some of the stability issues with the stepper, but
;;;             : maybe not all of them...
;;; 4/22/2004   Dan [1.5]
;;;             : Added the license info.
;;; -----------------------------------------------------------------------
;;; 2005.04.12  Dan [2.0]
;;;             : * Moving into ACT-R 6.  
;;;             : * Placed into the support folder and provided as 
;;;             :   "UNI-FILES".  
;;; 2005.04.20  Dan
;;;             : * Having problems with Lispworks 4.3.7 on Windows with the
;;;             :   sending of socket info.  Trying some stuff to fix that.
;;;             : * Added the require Opentransport for MCL.
;;; 2005.08.10 Dan
;;;             : * Addd the packaged-actr check to the uni-run-process
;;;             :   functions so that they can find the other ACT-R stuff...
;;; 2006.06.08 Dan
;;;             : * Realized that uni-wait-for-char has still got environment
;;;             :   specific code in it so removing that now.
;;;             : * Changed uni-process-system-events for CMUCL to use the
;;;             :   function process-yield instead of sleep - should improve
;;;             :   performance/responsiveness of the environment now.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


;;; See if this works ...

#+:lispworks (eval-when (:compile-toplevel :load-toplevel :execute)
               (require "comm"))

#+(and :mcl (not :openmcl)) (eval-when (:compile-toplevel :load-toplevel :execute)
              (require "OPENTRANSPORT"))

;(in-package :cl-user)

;#+:sbcl (eval-when (:compile-toplevel :load-toplevel) (require 'sb-bsd-sockets))
;using the above line of code for marcs model; using the below line of code for david's model
#+:sbcl (require 'sb-bsd-sockets)


#+(and :allegro-ide (not :packaged-actr) (not :ACTR-ENV-ALONE)) (in-package :cg-user)

;;; The following functions need to be defined for every Lisp with which
;;; The environment is going to be used.  Note the ordering of the allegro/
;;; allegro-ide and mcl/openmcl tests - that's important (I could do tests
;;; for the specific systems with ands, nots, and ors, but simple ordering
;;; seems easier).

;;; uni-make-socket
;;; This function takes 2 parameters which are the host and port address of
;;; a passive socket.  It should open an active socket to that address and
;;; return a stream for communicating over that connection.


#+:allegro 
(defun uni-make-socket (host port)
  #+(or :allegro-v5.0.1 :allegro-v5)
  (socket:make-socket :remote-host host :remote-port port)
  #+(and :ALLEGRO-VERSION>= (version>= 6))
  (socket:make-socket :remote-host host :remote-port port :nodelay t)
  )

#+(and :mcl (not :openmcl))
(defun uni-make-socket (host port)
  (ccl::open-tcp-stream host port))

#+:openmcl
(defun uni-make-socket (host port)
   (make-socket :remote-host host :remote-port port))

#+:lispworks
(defun uni-make-socket (host port)
  (comm:open-tcp-stream host port))

#+:cmu
(defun uni-make-socket (host port)
  (system:make-fd-stream
   (extensions:connect-to-inet-socket host port :stream)
   :input t :output t))

; MH 061222 copied cmu to sbcl
; MH 070109 hope this works
;#+:sbcl (require 'sb-bsd-sockets)
;#+:sbcl (use-package 'sb-bsd-sockets)
#+:sbcl
(defun uni-make-socket (host port)
;  (sb-sys:make-fd-stream
;   (extensions:connect-to-inet-socket host port :stream)
;   :input t :output t))
;  (print "uni-make-socket")
;  (print host)
;  (print (sb-bsd-sockets::make-inet-address host))
;  (print port)

  (let* ((sock (make-instance 'sb-bsd-sockets::inet-socket :type :stream :protocol :tcp)))
    ; (print sock)
    (sb-bsd-sockets::socket-connect sock (sb-bsd-sockets::make-inet-address host) port)
    ; (print sock)

    (let ((stream (sb-bsd-sockets::socket-make-stream sock :input t :output t :buffering :none)))

    stream)))


;;; uni-run-process
;;; This function takes 2 parameters.  The first is a string which will be
;;; used to name the process and the second is a function that is to be run
;;; in a new process.  It creates a new process that runs the specified 
;;; function (in the appropriate package) and returns that process.

#+:allegro 
(defun uni-run-process (name function)
  (mp::process-run-function name
                            #'(lambda ()
                                #-:packaged-actr (in-package :cl-user)
                                #+:packaged-actr (in-package :act-r)
                                (funcall function))))


#+(and :allegro-ide (not :ACTR-ENV-ALONE))
(defun uni-run-process (name function)
  (let ((debug-pane *standard-output*))
    (mp::process-run-function name 
                              #'(lambda ()
                                  (let ((*standard-output* debug-pane)
                                        (*error-output* debug-pane))
                                    #-:packaged-actr (in-package :cg-user)
                                    #+:packaged-actr (in-package :act-r)
                                    (funcall function)))))) 

#+(and :allegro :ACTR-ENV-ALONE)
(defun uni-run-process (name function)
    (mp::process-run-function name 
                              #'(lambda ()
                                  (let ((*standard-output* *global-output-stream*)
                                        (*error-output* *global-output-stream*))
                                    (in-package :cl-user)
                                    (funcall function))))) 



#+(and :mcl (not :openmcl))
(defun uni-run-process (name function)
  (let ((front *standard-output*)) 
    (process-run-function (list :name name) 
                          #'(lambda ()
                              (let ((CCL::*SUPPRESS-COMPILER-WARNINGS* t)
                                    (*standard-output* front)
                                    (*error-output* front))
                                #+:packaged-actr (in-package :act-r)
                                (funcall function))))))


#+(and :openmcl (not :ACTR-ENV-ALONE))
(defun uni-run-process (name function)
  (let ((front *standard-output*)) 
    (process-run-function (list :name name) 
                          #'(lambda ()
                              (let ((CCL::*SUPPRESS-COMPILER-WARNINGS* t)
                                    (*standard-output* front)
                                    (*error-output* front))
                                #+:packaged-actr (in-package :act-r)
                                (funcall function))))))

#+(and :openmcl :ACTR-ENV-ALONE)
(defun uni-run-process (name function)
    (process-run-function (list :name name) 
                              #'(lambda ()
                                  (let ((CCL::*SUPPRESS-COMPILER-WARNINGS* t)
                                        (*standard-output* *global-output-stream*)
                                        (*error-output* *global-output-stream*))
                                    (funcall function)))))


#+:lispworks
(defun uni-run-process (name function)
  (let ((front *standard-output*))
    (mp::process-run-function name nil
                              #'(lambda ()
                                  (let ((*standard-output* front)
                                        (*error-output* front)) 
                                    #+:packaged-actr (in-package :act-r)
                                    (funcall function))))))

#+:cmu
(defun uni-run-process (name function)
  (mp:make-process #'(lambda ()
                       #+:packaged-actr (in-package :act-r)
                       (funcall function))
                   :name name))

; MH 061222 copy action
#+:sbcl
(defun uni-run-process (name function)
  (sb-thread:make-thread #'(lambda ()
                       #+:packaged-actr (in-package :act-r)
                       (funcall function))
                   :name name))



;;; uni-process-kill
;;; This function takes one parameter which is a process and kills that
;;; process.

#+(or :lispworks :allegro)
(defun uni-process-kill (process)
  (mp::process-kill process))

#+:mcl
(defun uni-process-kill (process)
  (process-kill process))

#+:cmu
(defun uni-process-kill (process)
  (mp:destroy-process process))


; MH 061222 copy action
#+:sbcl
(defun uni-process-kill (process)
  (sb-thread:terminate-thread process))

;;; uni-wait-for
;;; This function takes one parameter which is a function.  It waits until that
;;; function returns true before returning.

#+(or :allegro :cmu)
(defun uni-wait-for (function)
  (mp::process-wait "Waiting" function))

#-(or :allegro :cmu)
(defun uni-wait-for (function)
  (loop (uni-process-system-events)
        (when (funcall function)
          (return))))

;;; uni-send-string 
;;; This function takes two parameters the first is a socket stream
;;; and the second is a string of a message to send.  That message is
;;; printed down that stream with a "newline" (CR/LF or whatever is used
;;; by the system) after it and the stream is flushed so that the 
;;; line is actually sent.


#+(and :mcl (not :openmcl))
(defun uni-send-string (socket string)
  (ccl::telnet-write-line socket string))

#+(or :openmcl :allegro)
(defun uni-send-string (socket string)
   (format socket "~a~%" string)
   (force-output socket))

#+:lispworks
(defun uni-send-string (socket string)
  (format socket "~a~%" string)
  #+:win32 (sleep .05)
  (finish-output socket))

#+:cmu
(defun uni-send-string (socket string)
  (write-line string socket)
  (finish-output socket))

;MH 061222 copy action
#+:sbcl
(defun uni-send-string (socket string)
  (write-line string socket)
  (finish-output socket))

;;; uni-stream-closed
;;; This function takes one parameter which is a socket stream.  It
;;; should return t if that stream has been closed.  It's only really
;;; here for MCL right now because the way sockets get handled there doesn't 
;;; result in an error on a closed stream, thus the process waiting for
;;; input doesn't ever end. 
;;;


#+:allegro
(defun uni-stream-closed (stream)
  (null stream))

#+(and :mcl (not :openmcl))
(defun uni-stream-closed (stream)
  (ccl::stream-closed-p stream))

#+:openmcl
(defun uni-stream-closed (stream)
  (stream-eofp stream))

#+:cmu
(defun uni-stream-closed (stream)
  (not (open-stream-p stream)))

;MH 061222 copy action
#+:sbcl
(defun uni-stream-closed (stream)
  (not (open-stream-p stream)))

#+:lispworks

;; I don't have this working quite right, but it doesn't
;; matter because it errors out on a read if the other end
;; has closed things down - so it's not really necessary
;; anyway - as with all of them except MCL...

(defun uni-stream-closed (stream)
  ;; problems with (null (comm::socket-stream-socket stream))
  nil)

;;; uni-socket-read-line
;;; This function takes one parameter which is a stream and reads a 
;;; line from it (terminated by some sort of CR/LF depending on the system)
;;; and returns the string containing that line.
;;; I got this function from Scott because in MCL the line endings caused
;;; problems for him and I noticed similar problems.  The allegro version
;;; should work for other Lisps.
;;; I've moved to using the ccl provided function in MCL, and perhaps no
;;; longer need the complex function for the other systems, but for now
;;; I'll leave it.

#+(and :mcl (not :openmcl))
(defun uni-socket-read-line (stream)
  "Read a CRLF-terminated line"
  (ccl::telnet-read-line stream))

#|
#+(or :allegro :lispworks :openmcl)
(defun uni-socket-read-line (stream)
  "Read a CRLF-terminated line"
  (unless (uni-stream-closed stream)
    (do ((line (make-array 10
                           :element-type 'character
                           :adjustable T
                           :fill-pointer 0))
         (char (read-char stream nil nil) 
               (read-char stream nil nil)))
        ((or (null char)
             (and (char= char #\cr)
                  (char= (peek-char NIL stream) #\lf)))
         (when char 
           (read-char stream nil nil))
         line)
      (vector-push-extend char line))))|#

#+(or :allegro :lispworks :openmcl)
(defun uni-socket-read-line (stream)
  "Read a line terminated by \\n"
  (read-line stream nil nil))

#+:cmu
(defun uni-socket-read-line (stream)
  "Read a line terminated by \\n"
  (read-line stream nil nil))

;MH 061222 copy action
#+:sbcl
(defun uni-socket-read-line (stream)
  "Read a line terminated by \\n"
  (read-line stream nil nil))



;;; uni-report-error
;;; This function takes 2 parameters. The first is a condition and the second
;;; is a string.  It prints the string message followed by "Error:" and the
;;; information about the error in the condition to *error-output*.  
;;; If there isn't any error info in the condition then "unspecified error" 
;;; is displayed.

#+:allegro 
(defun uni-report-error (err message)
  (format *error-output* "~a~%Error:~a" message 
    (if (slot-boundp  err 'EXCL::FORMAT-CONTROL)
        (apply #'format nil 
               (simple-condition-format-control err)
               (simple-condition-format-arguments err))
      "unspecified error")))

#+:mcl
(defun uni-report-error (err message)
  (format *error-output* "~a~%Error:~a" message err))

#+:cmu
(defun uni-report-error (err message)
  (format system:*stderr* "~a~%Error:~a" message err))

;MH 061222 copy
#+:sbcl
(defun uni-report-error (err message)
  (format sb-sys:*stderr* "~a~%Error:~a" message err))


#+:lispworks 
(defun uni-report-error (err message)
  (format *error-output* "~a~%Error:~a" message 
    (if  (slot-boundp err 'conditions::format-string)
        (apply #'format nil 
               (simple-condition-format-control err)
               (simple-condition-format-arguments err))
      "unspecified error")))

;;; uni-wait-for-chars
;;; This function takes one parameter which is a socket.  It should wait until
;;; there is a character available on that stream before returning, or in
;;; the event of an error just close down the process associated with handling
;;; that socket stream.  This assumes that read-char blocks "well".
;;;
;;; In MCL the socket read locks the socket for output which isn't good -
;;; so I've got to resort to a polling loop until I figure out how to 'fix'
;;; that (or just wait until MCL and OS 9 are dead and ignore it...)
;;;
;;; I've resolved that problem with MCL, so the same function should
;;; work for all Lisps (at least all supported at this time).

(defun uni-wait-for-char (stream)
  ;; first make sure there's a connection
  (when (uni-stream-closed stream)
    (uni-report-error "Connection closed while waitiing for a character.~%")
    (close stream)
    nil)
      
  ;; then check it for available data terminating on an error
  (multiple-value-bind (value condition)
      (ignore-errors (peek-char nil stream t)) 
    
    (declare (ignore value))
    
    (if (subtypep (type-of condition) 'condition)
        (progn
          (uni-report-error condition "Failed while waiting for a character")
          (close stream)
          nil)
      t)))





;;; uni-process-system-events
;;; This function takes no parameters and calls the necessary function
;;; to process events - process-pending-events, event-dispatch, etc.  It
;;; gets called in a loop that's just waiting for a signal to exit so that
;;; ACT-R stops when the stepper is open. (do I need to do this in a non-ide
;;; Lisp???

#+:allegro
(defun uni-process-system-events ()
  )

#+:cmu

; MH 061222 copy action
; MH 070109 don't want to create my own wait-queue
#+:sbcl
(defun uni-process-system-events ()
  )


;;; This is a pretty bad solution, but I don't know of a better one.
;;; If anybody knows a better function that will release the processor
;;; while sitting in a tight loop please let me know.

;;; Done, found process-yield but unchecked so far.

#-:sbcl
(defun uni-process-system-events ()
  (mp:process-yield))


#+:allegro-ide
(defun uni-process-system-events ()
  (process-pending-events))

#+(and :mcl (not :openmcl))
(defun uni-process-system-events ()
  (event-dispatch))
  
#+:openmcl
(defun uni-process-system-events ()
  )


;;; Not too sure if this is the right function 
#+:lispworks
(defun uni-process-system-events ()
  (mp:process-allow-scheduling))



;;; functionify
;;; This function is needed by some Lisp implementations to coerce a lambda
;;; list to a function that can be funcalled (actually, upon further 
;;; investigation this is a case where ACL is "overly helpful" because the
;;; spec doesn't say a cons (lambda ...) should be coerced to a function, but
;;; it's doing it for me, though I probably shouldn't take advantage of it).

#+:allegro
(defun functionify (x)
  x)

#-:allegro
(defun functionify (x)
  (if (consp x) (coerce x 'function) x))


;;; create-valid-pathname
;;; This function is a hack for running with MCL 5.0 in OSX.
;;; It takes a pathname as a string and returns a valid
;;; pathname as a string for Lisp.
;;; The problem is that MCL 5 under OSX still uses Mac pathnames,
;;; but Tcl/Tk under OSX uses Unix pathnames.  So, when one opens,
;;; saves, loads, etc from the environment the pathname that 
;;; comes over to Lisp is a Unix style name which MCL can't handle.
;;; So, when that's the case I replace the /'s with :'s and tack
;;; the booted drive name onto the front.
;;; Yes, it's a hack, but it seems to do the trick.  If any body
;;; has a better solution please let me know.

#-:ccl-5.0 

(defun create-valid-pathname (path) path)



(provide "UNI-FILES")

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
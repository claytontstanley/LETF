modelProgram= 'run-model

file2load= $1/actr6/load-act-r-6.lisp
file2load=modelShortCircuited.lisp
file2load=statsExtras.lisp

collapseQuota=2
iterations=1

entryFnType=hash
		

Constant=num-runs 15.0
Constant=environ-wait 8.0
IV=blend-temperature .01 .99
IV=bll .01 .99
IV=ans .01 .99

Obsrt1=1.1
#obsrt2=2.2
obsrt2temp=(collapse (list (list 2.2) (list 2.2)) 'mean)
obsrt2temp2=(nth 0 [obsrt2temp]) (nth 0 [obsrt2temp])
obsrt2=(+ [obsrt2temp2])
obsrt3=3.3
obsrt4=4.4
obsrt5=5.5
#ModRT=(list [rt1:2 rt3:5])
CorrelRT=(correl [ModRT] (list [obsrt1:5]))
CorrelRT2=(correl2 [ModRT] (list [obsrt1:5]))
MaxAbsDiffCorrel= (apply 'max (mapcar #'(lambda (x y) (abs (- x y))) [CorrelRT]  [CorrelRT2]))
collapseFn= nil & DV= MaxAbsDiffCorrel

DV= MaxAbsDiffCorrel 0 200

statusPrinter= 'print-session


ObsL1=1.1
ObsL2L3=(list 2.2     3.3)
obsL4=4.4
obsL5=5.5
ModLatency=(list [lat1 lat2 lat3 lat4 lat5])
CorrelLatency=(correl (append (list [obsl1]) [obsl2l3] (list     [obsl4]      [obsl5]))     [ModLatency] :throwoutyernils t)
DV=CorrelLatency 0 1

ObsPos=(list 1.1 3.3 2.2 4.4 5.5)
ModPos=(list [pos1 pos2 pos3 pos4 pos5])
RMSEPos=(rmse [ObsPos] [ModPos] :throwoutyernils t)
NormRMSEPos=(funcall #'(lambda (x) (/ 1 (+ 1 x))) [RMSEPos])
DV=NormRMSEPos

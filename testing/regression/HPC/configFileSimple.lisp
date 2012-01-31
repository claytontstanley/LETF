file2load= $1/actr6/load-act-r-6.lisp
file2load=modelShortCircuited.lisp
file2load=statsExtras.lisp

modelProgram= "$1/sbcl" --core $1/sbcl.core --noinform --noprint --disable-debugger --load model.lisp

runsPerProcess=inf

Constant=num-runs 15.0
Constant=environ-wait 8.0
IV=blend-temperature .01 .99
IV=bll .01 .99
IV=ans .01 .99

obsrt1=1.1
obsrt2=2.2
obsrt3= 3.3
obsrt4= 5.2
obsrt5= 1.1

#obsRT= (list [obsrt1 obsrt2 obsrt3 obsrt4 obsrt5])
obsRT= (fooPOW)

modRT= (list [RT1 RT2 RT3 RT4 RT5])

correlRT= (correl [obsRT] [modRT])
RMSERT= (RMSE [obsRT modRT])

normRMSERT= (- 1 (funcall #'(lambda (x) (/ 1 (+ 1 x))) [RMSERT]))




collapseQuota= 10
collapseFn= #'(lambda (x) (mean x))

DV=correlRT
DV=RMSERT
DV=normRMSERT





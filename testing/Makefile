
all: unitTest

unitTest:
	sbcl --noinform --noprint --disable-debugger --load letf.lisp nil testMM.txt nil

MM:
	cd ../../2009-12-15-present\ wrapping\ the\ wrapper/MindModeling/working\ directory; make

HPC:
	cd ../../2009-12-15-present\ wrapping\ the\ wrapper/HPC/working\ directory; make

C:
	cd ../../2009-12-15-present\ wrapping\ the\ wrapper/c\ example; make

java:
	cd ../../2009-12-15-present\ wrapping\ the\ wrapper/java\ example; make

string:
	cd ../../2009-12-15-present\ wrapping\ the\ wrapper/string\ example; make

deep: MM HPC C java string unitTest

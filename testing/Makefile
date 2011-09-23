DISTRO = SBCL
ifeq (${DISTRO},SBCL)
	CFLAGS = sbcl --noinform --noprint --disable-debugger --load letf.lisp
else
	CFLAGS = ccl -b -l letf.lisp --
endif

unit:
	${CFLAGS} nil testLetf.txt nil

cover:
	cd ..; make compile
	${CFLAGS} nil cover.txt nil

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

u3:
	#should output mm_bold_out.txt and wc to terminal
	cd unit3; make; ls | grep "mm_bold_out.txt"; wc mm_bold_out.txt

deep: MM HPC C java string u3

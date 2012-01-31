DISTRO = SBCL
LETF = ../letf.lisp

ifeq (${DISTRO},SBCL)
	CFLAGS = sbcl --noinform --noprint --disable-debugger --load ${LETF}
else
	CFLAGS = ccl -b -l ../../../letf.lisp --
endif

unit:
	${CFLAGS} nil testLetf.txt nil

cover:
	cd ..; make compile
	${CFLAGS} nil cover.txt nil


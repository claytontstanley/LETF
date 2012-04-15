DISTRO = SBCL
LETF = ../letf.lisp

ifeq (${DISTRO},SBCL)
	CFLAGS = sbcl --noinform --noprint --load ${LETF}
else
	CFLAGS = ccl -b -l ${LETF} --
endif

unit:
	${CFLAGS} nil testLetf.txt nil

cover:
	cd ..; make compile
	${CFLAGS} nil cover.txt nil


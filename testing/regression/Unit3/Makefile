DISTRO = SBCL
ifeq (${DISTRO},SBCL)
	CFLAGS = sbcl --noinform --noprint --disable-debugger --load letf.lisp
else
	CFLAGS = ccl -b -l letf.lisp --
endif

all: unitTest

unitTest:
	rm -f mm_bold_out.txt
	${CFLAGS} darwin configFile-sperling.txt workFile-sperling.txt


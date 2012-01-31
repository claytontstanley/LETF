DISTRO = SBCL
ifeq (${DISTRO},SBCL)
	CFLAGS = sbcl --noinform --noprint --disable-debugger --load letf.lisp
else
	CFLAGS = ccl -b -l letf.lisp --
endif

all: test

test:
	$(CFLAGS) 0 nil  "letf-conf-parabola2d-search-2.txt" "workFile01.txt"



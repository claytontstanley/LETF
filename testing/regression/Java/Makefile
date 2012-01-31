DISTRO = SBCL
ifeq (${DISTRO},SBCL)
	CFLAGS = sbcl --noinform --noprint --disable-debugger --load letf.lisp
else
	CFLAGS = ccl -b -l letf.lisp --
endif

all: test

test:
	$(CFLAGS) darwin configFile.txt workFile.txt





shareDir = /public/GridPortalShared/

all: htmlDocs

htmlDocs:
	$(shareDir)SBCL/linux/sbcl --core $(shareDir)SBCL/linux/sbcl.core --noinform --noprint --disable-debugger --load letf.lisp linux htmlConfig.txt nil

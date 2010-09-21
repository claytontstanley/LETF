

shareDir = /public/GridPortalShared/
appDir= ../../../../../

all: htmlDocs

htmlDocs:
	$(shareDir)SBCL/linux/sbcl --core $(shareDir)SBCL/linux/sbcl.core --noinform --noprint --disable-debugger --load letf.lisp linux htmlConfig.txt nil
	cp -R html $(appDir)docs/cldoc

htmlDocsDarwin:
	sbcl --noinform --noprint --disable-debugger --load letf.lisp darwin htmlConfig.txt nil	

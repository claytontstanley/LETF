include ../Makefile

PLATFORM = $(shell uname | tr '[:upper:]' '[:lower:]')

#Overwriting default path to letf.lisp
LETF = ../../../letf.lisp

all: MM HPC C Java String Unit3

regression:
	make all > regression.txt 2>&1

MM: MM.1 MM.2 MM.3 MM.4 MM.5

HPC: HPC.1 HPC.2 HPC.3 HPC.4 HPC.5 HPC.6 HPC.7

C: C.1

Java: Java.1

String: String.1

Unit3: Unit3.1


MM.1:
	cd MM; $(CFLAGS) nil mm_config_file.txt in.txt

MM.2:
	cd MM; $(CFLAGS) nil mm_config_file2.txt in.txt

MM.3:
	cd MM; $(CFLAGS) nil mm_config_file.txt nil

MM.4:
	cd MM; $(CFLAGS) nil mm_valTest.txt nil

MM.5:
	cd MM; $(CFLAGS) nil mm_partialWorkTest.txt nil

HPC.1:
	cd HPC; $(CFLAGS) darwin "configFile01.txt" "workFile01.txt"

HPC.2:	
	cd HPC; $(CFLAGS) darwin "configFile02.txt" "workFile01.txt"

HPC.3:	
	cd HPC; $(CFLAGS) darwin "configFile03.txt" "workFile01.txt"

HPC.4:
	cd HPC; $(CFLAGS) darwin "configFile03b.txt" "workFile01.txt"

HPC.5:
	cd HPC; $(CFLAGS) darwin "configFile04.lisp" "workFile01.txt"

HPC.6:
	cd HPC; $(CFLAGS) darwin "configFile05.txt" "workFile05.txt"

HPC.7:
	cd HPC; $(CFLAGS) 0 darwin testObservedCollapse.lisp "dsf/david-2/work files/workDavidQuick.txt"

C.1:
	cd C; $(CFLAGS) 0 ${PLATFORM} "letf-conf-parabola2d-search-2.txt" "workFile01.txt"

Java.1:
	cd Java; $(CFLAGS) darwin configFile.txt workFile.txt

String.1:
	cd String; $(CFLAGS) darwin configFile.txt workFile.txt

Unit3.1:
	cd Unit3; \
	rm -f mm_bold_out.txt; \
        ${CFLAGS} darwin configFile-sperling.txt workFile-sperling.txt; \
	wc mm_bold_out.txt


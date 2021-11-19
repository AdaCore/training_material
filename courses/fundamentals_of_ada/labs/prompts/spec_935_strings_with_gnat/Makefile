
include ../Makefile.inc

build_exec: questions_answers
	gprbuild -XMode=Problem
	-obj/main
	gprclean
	gprbuild -XMode=Solution
	obj/main

questions_answers:
	$(ADACUT) -m question template/inputs.adb >    src/inputs.adb
	$(ADACUT) -m answer   template/inputs.adb > answer/inputs.adb
	$(ADACUT) -m question template/inputs.ads >    src/inputs.ads
	$(ADACUT) -m answer   template/inputs.ads > answer/inputs.ads
	$(ADACUT) -m question template/corpuses.adb >    src/corpuses.adb
	$(ADACUT) -m answer   template/corpuses.adb > answer/corpuses.adb
	$(ADACUT) -m question template/corpuses.ads >    src/corpuses.ads
	$(ADACUT) -m answer   template/corpuses.ads > answer/corpuses.ads

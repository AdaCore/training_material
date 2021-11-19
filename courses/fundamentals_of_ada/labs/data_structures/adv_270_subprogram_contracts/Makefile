
include ../Makefile.inc

build_exec: questions_answers
	gprbuild -XMode=Problem
	-obj/main
	gprclean
	gprbuild -XMode=Solution
	obj/main

questions_answers:
	$(ADACUT) -m question template/priority_queue.adb > src/priority_queue.adb
	$(ADACUT) -m answer template/priority_queue.adb > answer/priority_queue.adb
	$(ADACUT) -m question template/main.adb > src/main.adb
	$(ADACUT) -m answer template/main.adb > answer/main.adb
	$(ADACUT) -m question template/priority_queue.ads > src/priority_queue.ads
	$(ADACUT) -m answer template/priority_queue.ads > answer/priority_queue.ads

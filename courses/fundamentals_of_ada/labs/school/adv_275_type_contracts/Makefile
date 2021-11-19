
include ../Makefile.inc

build_exec: questions_answers
	gprbuild -XMode=Problem
	obj/main
	gprbuild -XMode=Solution
	obj/main

questions_answers:
	$(ADACUT) -m question template/main.adb > src/main.adb
	$(ADACUT) -m answer template/main.adb > answer/main.adb
	$(ADACUT) -m question template/schedule.adb > src/schedule.adb
	$(ADACUT) -m answer template/schedule.adb > answer/schedule.adb
	$(ADACUT) -m question template/schedule.ads > src/schedule.ads
	$(ADACUT) -m answer template/schedule.ads > answer/schedule.ads

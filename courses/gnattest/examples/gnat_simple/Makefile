include ../Makefile.inc

simple:
	$(PREFIX)gnattest -Psimple.gpr $(QUIET)
	$(PREFIX)gprbuild -Pobj/gnattest/harness/test_driver.gpr $(QUIET)
ifeq ($(RUN),true)
	obj/gnattest/harness/test_runner$(EXE)
endif

clean:
	$(PREFIX)gprclean -Psimple.gpr
	$(RM) -r obj/gnattest

extract:
	$(ADACUT) -c 1 2 -- src/simple.ads > extracts/simple.ads
	$(ADACUT) -c 1 2 -- src/simple.adb > extracts/simple.adb

.PHONY: clean simple 

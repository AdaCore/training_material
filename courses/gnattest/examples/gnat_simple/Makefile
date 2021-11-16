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
	$(ADACUT) -c 1 2 3 4 -- obj/gnattest/tests/simple-test_data-tests.adb > extracts/simple-test_data-tests.before.adb
	$(ADACUT) -c 1 2 3 4 -- obj/gnattest/tests/simple-test_data-tests.after.adb > extracts/simple-test_data-tests.after.adb

.PHONY: clean simple 

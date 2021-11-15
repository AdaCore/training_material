include ../Makefile.inc

simple_stubbing:
	$(PREFIX)gnattest -Pops --stub $(QUIET)
	make -C obj/gnattest_stub/harness
ifeq ($(RUN),true)
	gnattest obj/gnattest_stub/harness/test_drivers.list
endif

clean:
	$(PREFIX)gprclean -r -Pops
	$(RM) -r obj/gnattest*

.PHONY: clean simple_stubbing

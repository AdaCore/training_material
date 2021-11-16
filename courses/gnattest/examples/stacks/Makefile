include ../Makefile.inc

simple_stubbing:
	$(PREFIX)gnattest -Pprj --stub $(QUIET)
	make -C obj/gnattest_stub/harness
ifeq ($(RUN),true)
	gnattest obj/gnattest_stub/harness/test_drivers.list
endif

extract:
	$(ADACUT) -c 1 2 -- src/integer_stacks.ads > extracts/integer_stacks.type_decl.ads

clean:
	$(PREFIX)gprclean -r -Pprj
	$(RM) -r obj/gnattest*

.PHONY: clean simple_stubbing

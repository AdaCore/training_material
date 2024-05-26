check:
	for f in **/*.gpr; do \
	   echo $$(dirname $$f); \
	   (cd $$(dirname $$f) && gprbuild -P $$(basename $$f) > obj/check.log || echo "<<< FAILURE >>>"); \
	done

clean:
	for f in **/*.gpr; do \
	   echo $$(dirname $$f); \
	   (cd $$(dirname $$f) && gprclean -P $$(basename $$f)); \
	done

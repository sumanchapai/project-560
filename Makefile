.PHONY = demo

# Directory containing example files
EXAMPLES_DIR = examples

# List of demo files
DEMO_FILES = fail_int.ml \
			 fail2_int.ml \
             success_int.ml \
			 success2_int.ml \
			 success3_int.ml 

# Demo target
demo:
	@for file in $(DEMO_FILES); do \
		f=$(EXAMPLES_DIR)/$$file ; \
		echo "-----------------------" ; \
		echo "$$f" ; \
		echo "-----------------------" ; \
		cat "$$f" ; \
		echo "-----------------------" ; \
		dune exec type_checker/main.exe "$$f" ; \
		echo "-----------------------" ; \
		echo "\n\n\n " ;\
	done


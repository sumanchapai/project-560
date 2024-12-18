.PHONY = demo

# Directory containing example files
EXAMPLES_DIR = examples

# List of demo files
DEMO_FILES =  success0_int.ml\
			fail2_int.ml \
			 fail_int.ml \
             success_int.ml \
			 success2_int.ml \
			 success3_int.ml 

# Custom target
custom:
	@read -p "Enter program file:" filepath;\
		f="$$filepath" ; \
		echo "-----------------------" ; \
		echo "$$f" ; \
		echo "-----------------------" ; \
		cat "$$f" ; \
		echo "\n-----------------------" ; \
		dune exec type_checker/main.exe "$$f" ; \
		echo "\n\n\n " ;\
		read line;\
		clear ;\

# Demo target
demo:
	@clear ;\
	for file in $(DEMO_FILES); do \
		f=$(EXAMPLES_DIR)/$$file ; \
		echo "-----------------------" ; \
		echo "$$f" ; \
		echo "-----------------------" ; \
		cat "$$f" ; \
		echo "\n-----------------------" ; \
		dune exec type_checker/main.exe "$$f" ; \
		echo "\n\n\n " ;\
		read line ;\
		clear ;\
	done
	@echo "Thank you!\nMerry Christmas"
	@read  line;\


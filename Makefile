SBCL_CMD := sbcl --noinform --disable-debugger --load
OBJECTS := lisp-inference


all: $(OBJECTS)


$(OBJECTS): src/*.lisp
	$(SBCL_CMD) build.lisp


check:
	@$(SBCL_CMD) run-test.lisp


.PHONY: check

SBCL_CMD := sbcl --noinform --disable-debugger --load
OBJECTS := lisp-inference
DOCKER_IMG = lisp-inference

all: $(OBJECTS)


$(OBJECTS): src/*.lisp
	$(SBCL_CMD) build.lisp


check:
	@$(SBCL_CMD) run-test.lisp


server:
	@$(SBCL_CMD) run-server.lisp

docker-build:
	docker build -t $(DOCKER_IMG) .

docker-run:
	docker run --rm -it --network=host $(DOCKER_IMG)

docker-check:
	docker run --rm -t --entrypoint=ros $(DOCKER_IMG) run -s lisp-inference/test -l run-test.lisp

.PHONY: check

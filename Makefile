SBCL_CMD := sbcl --noinform --disable-debugger --load
OBJECTS := lisp-inference
DOCKER_IMG = lisp-inference
VERSION := latest
PUBLIC_IMG = ryukinix/$(DOCKER_IMG):$(VERSION)

all: $(OBJECTS)


$(OBJECTS): src/*.lisp
	$(SBCL_CMD) build.lisp


check:
	@$(SBCL_CMD) run-test.lisp


server:
	@$(SBCL_CMD) run-server.lisp

docker-build:
	docker build -t $(DOCKER_IMG) .

docker-shell: docker-build
	docker run --rm -it --entrypoint=/bin/bash $(DOCKER_IMG)


docker-run: docker-build
	docker run --rm -it --network=host $(DOCKER_IMG)

docker-check: docker-build
	docker run --rm -t --entrypoint=ros $(DOCKER_IMG) run -s lisp-inference/test -l run-test.lisp

docker-publish: docker-build
	docker tag $(DOCKER_IMG) $(PUBLIC_IMG)
	docker push $(PUBLIC_IMG)

deploy: docker-publish
	ssh starfox bash /home/lerax/Deploy/logic.sh

.PHONY: check docker-build

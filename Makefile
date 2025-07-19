SBCL_CMD := sbcl --noinform --disable-debugger --load scripts/fix-quicklisp.lisp --load
DOCKER_IMG = lisp-inference
VERSION := latest
PUBLIC_IMG = ryukinix/$(DOCKER_IMG):$(VERSION)

lisp-inference: src/*.lisp
	$(SBCL_CMD) scripts/build.lisp


check:
	@$(SBCL_CMD) scripts/run-test.lisp


server:
	@$(SBCL_CMD) scripts/run-server.lisp

docs-worktree:
	@if [ ! -d docs ]; then \
		git worktree add docs gh-pages -f; \
    fi

docs: docs-worktree
	@$(SBCL_CMD) scripts/run-docs.lisp

docs-publish:
	cd docs/ && git add . && git commit -m "Auto-generated commit from make docs-publish" && git push || true

docker-build:
	docker build -t $(DOCKER_IMG) .

docker-shell: docker-build
	docker run --rm -it --entrypoint=/bin/bash $(DOCKER_IMG)

docker-run: docker-build
	docker run --rm -it --network=host $(DOCKER_IMG)

docker-docs: docker-build docs-worktree
	docker run --rm -t \
		   -v $(PWD)/docs:/root/.roswell/local-projects/local/lisp-inference/docs \
           --entrypoint=ros $(DOCKER_IMG) \
           run -l scripts/fix-quicklisp.lisp -l run-docs.lisp

docker-check: docker-build
	docker run --rm -t --entrypoint=ros $(DOCKER_IMG) run -l scripts/fix-quicklisp.lisp -l scripts/run-test.lisp

docker-publish: docker-build
	docker tag $(DOCKER_IMG) $(PUBLIC_IMG)
	docker push $(PUBLIC_IMG)

deploy: docker-publish
	ssh starfox bash /home/lerax/Deploy/logic.sh

appimage: lisp-inference
	bash scripts/appimage.sh

.PHONY: check docker-build docs appimage

UID := $(shell id -u)

.PHONY: docker
docker:
	docker build -t factor-repl .
	docker run \
		-it \
		--rm \
		-v $(shell pwd):/home/node/factor \
		-p 1234:1234 \
		-p 12345:12345 \
		factor-repl

.PHONY: dev
dev:
	parcel serve --hmr-port 12345 index.html

.PHONY: build
build:
	parcel build index.html

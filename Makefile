
.PHONY: build clean default

default: build

build:
	dune build

clean:
	dune clean
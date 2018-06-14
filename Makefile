prog	= bmin

all: build

build:
	stack build

run:
	@stack exec $(prog)

.PHONY: all build run

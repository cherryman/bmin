prog	= bmin

all: build

build:
	@stack build

run: build
	@stack exec $(prog)

repl: build
	@stack ghci

.PHONY: all build run repl

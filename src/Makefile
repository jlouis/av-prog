UMFILE=data/codex.umz
SANDMARK=data/sandmark.umz

.phony: all build run clean
all: build

build:
	ghc -O -fglasgow-exts -XFlexibleInstances -prof -auto-all --make *.hs -o uvm

run: build
	./uvm ${UMFILE}

prof-sandmark: build
	./uvm +RTS -p --RTS ${SANDMARK}

sandmark: build
	./uvm ${SANDMARK}

clean:
	rm *.o *.hi uvm

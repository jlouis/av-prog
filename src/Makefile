UMFILE=data/codex.umz
SANDMARK=data/sandmark.umz

.phony: all build run clean
all: build

build:
	ghc -O -XFlexibleInstances --make *.hs -o uvm

run: build
	./uvm ${UMFILE}

sandmark: build
	./uvm ${SANDMARK}

clean:
	rm *.o *.hi uvm

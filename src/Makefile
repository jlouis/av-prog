UMFILE=data/codex.umz

.phony: all build run clean
all: build

build:
	ghc -XFlexibleInstances --make *.hs -o uvm

run: build
	./uvm ${UMFILE}

clean:
	rm *.o *.hi uvm

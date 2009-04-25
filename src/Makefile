UMFILE=data/codex.umz
SANDMARK=data/sandmark.umz

GHCFLAGS+=-O2 -fglasgow-exts -XFlexibleInstances -auto-all

.phony: all build run clean
all: build

build:
	ghc ${GHCFLAGS} --make *.hs -o uvm

build-prof:
	ghc ${GHCFLAGS} --prof --make *.hs -o uvm

run: build
	./uvm ${UMFILE}

prof-sandmark: build
	./uvm +RTS -p --RTS ${SANDMARK}

sandmark: build
	./uvm ${SANDMARK}

clean:
	rm *.o *.hi uvm

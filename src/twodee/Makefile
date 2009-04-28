GHCFLAGS+=-O2 -fglasgow-exts -XFlexibleInstances -auto-all

.phony: all build run clean
all: build

build:
	ghc ${GHCFLAGS} --make *.hs -o twodee

build-prof:
	ghc ${GHCFLAGS} -prof --make *.hs -o uvm

clean:
	rm -f twodee *.o *.hi

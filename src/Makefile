UMFILE=data/codex.um
UMZFILE=data/codex.umz
SANDMARK=data/sandmark.umz
DECRYPTOR=data/decrypt.command
HAX0R=data/hack-attempt.input
PUBCHECK=data/pubcheck.input

GHCFLAGS+=-O2 -fglasgow-exts -XFlexibleInstances -auto-all

.phony: all build run clean
all: build

build:
	ghc ${GHCFLAGS} --make *.hs -o uvm

build-prof:
	ghc ${GHCFLAGS} -prof --make *.hs -o uvm

run-umz: build
	./uvm ${UMZFILE}

run: build
	./uvm ${UMFILE}

decrypt: build
	./uvm ${UMZFILE} < ${DECRYPTOR} > codex.um

hack: build
	./uvm ${UMFILE} < ${HAX0R}

pubcheck: build
	./uvm ${UMFILE} < ${PUBCHECK}

prof-sandmark: build
	./uvm +RTS -p --RTS ${SANDMARK}

sandmark: build
	time ./uvm ${SANDMARK}

clean:
	rm *.o *.hi uvm

UMFILE=data/codex.um
UMZFILE=data/codex.umz
SANDMARK=data/sandmark.umz
DECRYPTOR=data/decrypt.command
HAX0R=data/hack-attempt.input
PUBCHECK=data/pubcheck.input
ADVISE_ARITH=data/advise.arith.input

GHCFLAGS+=-O2 -fglasgow-exts -XFlexibleInstances -auto-all

.phony: all build run clean two
all: build build-two

build:
	ghc ${GHCFLAGS} --make *.hs UM/Main.hs -main-is UM.Main.main -o uvm

build-two:
	ghc ${GHCFLAGS} --make *.hs Twodee/Main.hs -main-is Twodee.Main.main -o twodee

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

advise-arith:
	./uvm ${UMFILE} < ${ADVISE_ARITH}

prof-sandmark: build
	./uvm +RTS -p --RTS ${SANDMARK}

sandmark: build
	time ./uvm ${SANDMARK}

clean:
	rm -f *.o *.hi UM/*.o Twodee/*.o Ocult/*.o uvm twodee



build:
	stack clean
	stack build

install: build
	stack install

run: build
	clear
	stack exec -- nota

configure:
	cd source
	cabal install
	cd ..
	make install
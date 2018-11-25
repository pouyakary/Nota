
build:
	stack clean
	stack build

install: build
	stack install

run: build
	clear
	stack exec -- nota

checkout:
	git clone https://github.com/karl-chan/gdax-haskell.git lib/gdax-haskell

install:
	stack setup
	stack build --fast --copy-bins
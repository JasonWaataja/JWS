all:
	cabal build

format:
	fourmolu -i $(shell find src -name '*.hs')

hlint:
	hlint src

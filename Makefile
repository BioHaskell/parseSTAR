
test: Tokens
	./Tokens < shifts.str

Tokens: Tokens.hs
	ghc Tokens.hs

%.hs: %.x
	alex Tokens.x
%.o : %.hs
	ghc Tokens.hs

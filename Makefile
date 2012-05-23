GHCFLAGS=-O0
ALEXFLAGS=
#ALEXFLAGS=--info=Tokens.info --debug

test: Tokens
	./Tokens < test.str

Tokens: Tokens.hs
	ghc $(GHCFLAGS) Tokens.hs

%.hs: %.x
	alex $(ALEXFLAGS) Tokens.x
%.o : %.hs
	ghc $(GHCFLAGS) Tokens.hs

clean:
	rm -f `cat .gitignore`

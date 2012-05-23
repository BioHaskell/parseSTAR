GHCFLAGS=
ALEXFLAGS=
#ALEXFLAGS=--info=Tokens.info --debug

test: TestTokens
	./TestTokens < test.str

TestTokens: TestTokens.hs Tokens.hi
	ghc $(GHCFLAGS) TestTokens.hs

%.hs: %.x
	alex $(ALEXFLAGS) Tokens.x
%.o %.hi: %.hs
	ghc $(GHCFLAGS) Tokens.hs

clean:
	rm -f `cat .gitignore`

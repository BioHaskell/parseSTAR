GHCFLAGS=
ALEXFLAGS=
#ALEXFLAGS=--info=Tokens.info --debug
HAPPYFLAGS=--ghc --decode

test: test_parser
#test: test_tokens test_parser

test_tokens: TestTokens
	./TestTokens < test.str

test_parser: Parser
	./Parser < test.str

Parser: Parser.hs Tokens.hs Tokens.hi ParserMonad.hs ParserMonad.hi
	ghc $(GHCFLAGS) $<

TestTokens: TestTokens.hs Tokens.hi
	ghc $(GHCFLAGS) $<

%.hs: %.x
	alex $(ALEXFLAGS) $<

%.o %.hi: %.hs
	ghc $(GHCFLAGS) $<

%.hs: %.y
	happy $(HAPPYFLAGS) $<

clean:
	rm -f `cat .gitignore`

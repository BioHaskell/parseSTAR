#GHCFLAGS=-debug
GHCFLAGS=-rtsopts -prof -auto-all +RTS -H1G -RTS
#GHCFLAGS=-O3 -rtsopts
#RTSFLAGS=-xc -k512M
RTSFLAGS=+RTS -k512M -RTS
ALEXFLAGS=--ghc
HAPPYFLAGS=--ghc --decode

all_parser_tests: Parser
	time ./Parser $(RTSFLAGS) > /dev/null < test.str
	time ./Parser $(RTSFLAGS) > /dev/null < shifts.str
	time ./Parser $(RTSFLAGS) > /dev/null < input.str

test2: test_parser

test: TestParserMonad
	./TestParserMonad

#test: test_tokens test_parser

test_tokens: TestTokens
	./TestTokens ${RTSFLAGS} < test.str

test_parser: Parser
	./Parser +RTS ${RTSFLAGS} < test.str

Parser: Parser.hs Tokens.hs Tokens.hi ParserMonad.hs ParserMonad.hi
	ghc $(GHCFLAGS) $<

TestTokens: TestTokens.hs Tokens.hi
	ghc $(GHCFLAGS) $<

TestParserMonad: TestParserMonad.hs ParserMonad.hi ParserMonad.o Tokens.hi Tokens.o
	ghc $(GHCFLAGS) $<

Tokens.hs: Tokens.x

Parser.hs: Parser.y

ParserMonad.o ParserMonad.hi: ParserMonad.hs Type.hi


%.hs: %.x
	alex $(ALEXFLAGS) $<

%.o %.hi: %.hs
	ghc $(GHCFLAGS) $<

%.hs: %.y
	happy $(HAPPYFLAGS) $<

clean:
	rm -f `cat .gitignore`

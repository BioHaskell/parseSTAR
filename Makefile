#GHCFLAGS=-debug
GHCFLAGS=-rtsopts -prof -auto-all +RTS -H2G -RTS
#GHCFLAGS=-O3 -rtsopts +RTS -H2G -A1M -RTS
#RTSFLAGS=-xc -k512M
RTSFLAGS=+RTS -k256M -H3G -A1M -s -RTS
ALEXFLAGS=--ghc --template=.
HAPPYFLAGS=--ghc --strict #--decode
#HAPPYFLAGS=--glr

test2: Parser
	./Parser +RTS -hy -Pa -xc -RTS test3.str >/dev/null

test_tokens: TestTokens
	./TestTokens ${RTSFLAGS}  test.str

all_parser_tests: Parser
	time ./Parser $(RTSFLAGS) test.str   > /dev/null 
	time ./Parser $(RTSFLAGS) shifts.str > /dev/null 
	time ./Parser $(RTSFLAGS) input.str  > /dev/null 

test: TestParserMonad
	./TestParserMonad

#test: test_tokens test_parser

test_parser: Parser
	./Parser +RTS ${RTSFLAGS} -RTS test.str

Parser: Parser.hs Tokens.hs Tokens.hi
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

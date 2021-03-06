#GHCFLAGS=-debug
#GHCFLAGS=-rtsopts -prof -auto-all +RTS -H2G -RTS
GHCFLAGS=-O3 -rtsopts +RTS -H2G -A1M -RTS -DDEFINE_NFDATA_BYTESTRING
#GHCFLAGS=-prof -auto-all -rtsopts +RTS -H2G -A1M -RTS -DDEFINE_NFDATA_BYTESTRING
#GHCFLAGS=-rtsopts +RTS -H2G -A1M -RTS # GIVES CRASH?
#RTSFLAGS=-xc -k512M
#RTSFLAGS=+RTS -k256M -H3G -A1M -s -RTS
ALEXFLAGS=--ghc --template=alex/
HAPPYFLAGS=--ghc --strict #--decode
#HAPPYFLAGS=--glr

all: executables test_cs

cabal: prepare
	cabal configure && cabal build

prepare: Data/STAR/Tokens.hs

test_cs: test/TestChemShifts
	test/TestChemShifts smallest.str smallest.cs +RTS -H2G -A6M

test/TestChemShifts: test/TestChemShifts.hs Data/STAR/Parser.hs Data/STAR/Tokens.hs Data/STAR/ChemShifts.hs
	ghc $(GHCFLAGS) --make -rtsopts $<

test: test/TestConverter
	test/TestConverter +RTS -h -Pa -xc -RTS largest.str largest.test

test2: test/TestParser
	test/TestParser +RTS -hy -Pa -xc -RTS test3.str >/dev/null

test_tokens: TestTokens
	./TestTokens ${RTSFLAGS}  test.str

all_parser_tests: test/TestParser
	time test/TestParser $(RTSFLAGS) test.str   > /dev/null 
	time test/TestParser $(RTSFLAGS) shifts.str > /dev/null 
	time test/TestParser $(RTSFLAGS) input.str  > /dev/null 

test_parser: test/TestParser
	time test/TestParser +RTS ${RTSFLAGS} -RTS test.str

executables: test/TestParser test/TestCoords test/TestTokens test/TestChemShifts test/MergeCoord test/MergeCS

test/TestParser: test/TestParser.hs Data/STAR/Parser.hs Data/STAR/Tokens.hs Data/STAR/Tokens.hi
	ghc $(GHCFLAGS) $<

test/MergeCS: test/MergeCS.hs Data/STAR/Parser.hs Data/STAR/Tokens.hs Data/STAR/Tokens.hi Data/STAR/ChemShifts.hs
	ghc $(GHCFLAGS) $<

test/TestCoords: test/TestCoords.hs Data/STAR/Parser.hs Data/STAR/Tokens.hs Data/STAR/Tokens.hi
	ghc $(GHCFLAGS) $<

test/TestConverter: test/TestConverter.hs Data/STAR/Parser.hs Data/STAR/Tokens.hs Data/STAR/Tokens.hi Data/STAR/Type.hi
	ghc $(GHCFLAGS) $<

test/TestTokens: test/TestTokens.hs Data/STAR/Tokens.hi
	ghc $(GHCFLAGS) $<

Data/STAR/Parser.hs: Data/STAR/Parser.y

Data/STAR/Tokens.hs: preSrc/Tokens.x
	alex $(ALEXFLAGS) $<
	cp preSrc/Tokens.hs Data/STAR/Tokens.hs
	# We do it because cabal is lame and doesn't allow custom Alex arguments in .cabal.

%.o %.hi: %.hs
	ghc $(GHCFLAGS) $<

%.hs: %.y
	happy $(HAPPYFLAGS) $<

clean:
	-rm -f `cat .gitignore`
	-(cd Data/STAR; rm -f `cat .gitignore`)
	-(cd test; rm -f `cat .gitignore`)
	-rm `find . -iname '*.hi' -or -iname '*.o'`

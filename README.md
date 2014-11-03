parseSTAR
=========

Parsing Biological Magnetic Resonance Databank files in `STAR*` format.

While not optimized for speed, using it on a 48-core AMD Opteron machine
allowed me to parse entire chemical shifts merged with coordinates (676MB)
from BMRB within 8m18.355s. All files that I tried parse within a second.
Thus it may be sufficient for daily use.

To learn the API start with Data.STAR.

[![Build Status](https://api.travis-ci.org/mgajda/parseSTAR.png?branch=master)](https://travis-ci.org/mgajda/parseSTAR)

Details on official releases are on [Hackage](http://hackage.haskell.org/package/parseSTAR).

Alex template modifications
---------------------------
This version of the package is adapted to alex 3.1.3.
Directory alex/ contains Alex templates which were modified
to provide appropriate strictness during lexical analysis.

It seems that default Alex templates provide excessive laziness even in
the bytestring version, that hurts performance.

Please report a bug, if you have troubles compiling it with Alex 3.1.3 or newer.

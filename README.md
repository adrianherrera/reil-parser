# REIL Parser

Parser for the Reverse Engineering Intermediate Language (REIL), as used by
Google's [BinNavi](https://github.com/google/binnavi).

## Generating REIL code
REIL code can either be written by hand (in a text file) or generated in
BinNavi. The stand-alone Python script `scripts\to_reil.py` can be used to
generated REIL code for a module already stored in a BinNavi database. Note
that the stand-alone script must be run in [Jython](http://www.jython.org) and
requires a copy of the BinNavi jar (`binnavi.jar`).

Run `jython scripts/to_reil.py --help` for instructions on how to use this
script to generate REIL code.
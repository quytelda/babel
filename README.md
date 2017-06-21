# Babel [![Build Status](https://travis-ci.org/quytelda/babel.svg?branch=master)](https://travis-ci.org/quytelda/babel)
Babel is a small utility written in Haskell that generates random sequences based on a context free grammar (CFG).  It can be used as a word generator or password generator.

For example:
```
$ babel -n 3 grammar.cfg
abcab
cba
bcabcba
```
In this example, grammar.cfg contains the following:
```
S -> A | B | C
A -> a B | a C | a
B -> b A | b C | b
C -> c A | c B | c
```
# What's Needed
In order to build babel, you must at least have the haskell compiler GHC.  If you have Cabal installed, just run `cabal build` from the top level directory (where babel.cabal is located).  Otherwise, you can compile an executable by running `ghc -o babel --make -isrc src/Main.hs`.

Babel depends on the following haskell packages:
- containers (>=0.5 && <0.6)
- random (>=1.1 && <1.2)
- parsec (>=3.1 && <3.2)
- split (>= 0.2 && <0.3)

All these packages are available in the repositories for most Linux distributions.
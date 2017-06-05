# Babel
Babel is a small utility written in Haskell that generates random sequences based on a context free grammar (CFG).  It can be used as a word generator or password generator.

For example:
```
$ ./babel -n 3 grammar.cfg
abcab
cba
bcabcba
```
If grammar.cfg contains the following:
```
S -> A | B | C;
A -> a B | a C | a;
B -> b A | b C | b;
C -> c A | c B | c;
```
# What's Needed
To compile babel, you should have the Glasgow Haskell Compiler (GHC) and the Haskell random libraries (provides System.Random).

To compile, run `make` from the top level directory (containing `makefile`) or alternatively:
```
$ ghc -o babel --make -isrc src/Main.hs
```
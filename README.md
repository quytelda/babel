# Babel
Babel is a small utility written in Haskell that generates randomized sequences based on configurable parameters.  It can be used for password generation, word generation, and similar applications.

For example:
```
$ ./babel -n 3 UC:LV:LC:UV:D:S:UC:UV
XalU5*QE
SocO2!QE
LilE6_MI

```

# What's Needed
To compile babel, you should have the Glasgow Haskell Compiler (GHC) and the Haskell random libraries (provides System.Random).

To compile, run `make` from the top level directory (containing `makefile`) or alternatively:
```
$ ghc -o babel -isrc src/Babel.hs
```
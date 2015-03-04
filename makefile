EXENAME=babel
SRCDIR=src
ARGS=--make -Wall -i${SRCDIR} -o ${EXENAME}

babel: ${SRCDIR}/Babel.hs ${SRCDIR}/Sequence.hs
	ghc ${ARGS} ${SRCDIR}/Babel.hs

debug: ${SRCDIR}/Babel.hs ${SRCDIR}/Sequence.hs
	ghc ${ARGS} -debug ${SRCDIR}/Babel.hs

clean:
	rm babel
	find . -name "*.hi" -delete
	find . -name "*.o" -delete

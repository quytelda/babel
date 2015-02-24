EXENAME=babel
SRCDIR=src

babel: ${SRCDIR}/Babel.hs
	ghc --make -Wall -i${SRCDIR} -o ${EXENAME} ${SRCDIR}/Babel.hs

clean:
	rm babel
	find . -name "*.hi" -delete
	find . -name "*.o" -delete

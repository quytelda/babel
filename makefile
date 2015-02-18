EXENAME=babel
SRCDIR=src

babel: ${SRCDIR}/Babel.hs
	ghc --make -o ${EXENAME} ${SRCDIR}/Babel.hs

clean:
	rm babel
	find . -name "*.hi" -delete
	find . -name "*.o" -delete

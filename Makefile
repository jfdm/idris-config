##  Makefile

IDRIS := idris
LIB   := config
OPTS  :=

.PHONY: clean lib install clobber check test doc

install: lib
	${IDRIS} ${OPTS} --install ${LIB}.ipkg

lib:
	${IDRIS} ${OPTS} --build ${LIB}.ipkg

clean:
	${IDRIS} --clean ${LIB}.ipkg
	find . -name "*~" -delete

clobber : clean
	find . -name "*.ibc" -delete

check: clobber
	${IDRIS} --checkpkg ${LIB}.ipkg

test:
	${IDRIS} --testpkg ${LIB}.ipkg

doc:
	${IDRIS} --mkdoc ${LIB}.ipkg

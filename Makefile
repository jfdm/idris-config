##  Makefile

IDRIS := idris
LIB   := config
BIN   := configbin
OPTS  :=

.PHONY: clean lib

exe: install
	${IDRIS} ${OPTS} --build ${BIN}.ipkg

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

test :
	echo "Not yet, tests are old and broken."
	#(cd tests; bash runtests.sh)

doc:
	${IDRIS} --mkdoc ${LIB}.ipkg

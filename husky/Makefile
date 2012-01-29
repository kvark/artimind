OPT	=-Wall -XMultiParamTypeClasses -XFunctionalDependencies -XFlexibleInstances
EXE	=test

all:
	cd code && ghc --make Main ${OPT} -o ../${EXE}

debug:
	cd code && ghci Main ${OPT}

clean:
	rm ${EXE}
	find . -name "*.o"	-delete
	find . -name "*.hi"	-delete


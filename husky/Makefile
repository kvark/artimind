OPT	=-XMultiParamTypeClasses -XFunctionalDependencies -XFlexibleInstances
EXE	=test

all:
	cd code && ghc --make Main ${OPT} -o ../${EXE}

clean:
	rm ${EXE}
	find . -name "*.o"	-delete
	find . -name "*.hi"	-delete


OPT	=-XMultiParamTypeClasses -XFunctionalDependencies
EXE	=test

all:
	ghc --make Main ${OPT} -o ${EXE}

clean:
	rm ${EXE}
	find . -name "*.o"	-delete
	find . -name "*.hi"	-delete


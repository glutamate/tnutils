all:
	bnfc NewBug.cf
	happy -gca ParNewBug.y
	alex -g LexNewBug.x
	ghc --make TestNewBug.hs -o TestNewBug
clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f DocNewBug.ps
distclean: clean
	-rm -f DocNewBug.* LexNewBug.* ParNewBug.* LayoutNewBug.* SkelNewBug.* PrintNewBug.* TestNewBug.* AbsNewBug.* TestNewBug ErrM.* SharedString.* NewBug.dtd XMLNewBug.* Makefile*


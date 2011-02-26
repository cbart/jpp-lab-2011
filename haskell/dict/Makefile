HC=ghc
MAIN=Main.hs
TEST=TestSplay.hs
LIBS=Splay.hs

all: $(MAIN) $(LIBS)
	$(HC) -o words --make $(MAIN)
test: $(TEST) $(LIBS)
	$(HC) -o test --make $(TEST)
	./test
clean:
	rm -rf *.hi *.o words test

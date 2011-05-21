PL = swipl
PLD = swipl-ld

all: tours
tours: tours.sav
	$(PLD) --output tours tours.sav
tours.sav: tours.pl
	$(PL) < compile
clean:
	rm -f tours tours.sav
.PHONY: clean, all

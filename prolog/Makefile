PL = sicstus
PLD = spld
PLD_OPTS = --static --exechome=/opt/sicstus4.1.3/bin

all: tours
tours: tours.sav
	$(PLD) $(PLD_OPTS) --output tours tours.sav
tours.sav: tours.pl
	$(PL) < compile
clean:
	rm -f tours tours.sav
.PHONY: clean, all

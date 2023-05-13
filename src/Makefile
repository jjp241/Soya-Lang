BNFC_PATH := /home/students/inf/PUBLIC/MRJP/bin/bnfc
SOYA_DIR := Soya
INTERPRETER := interpreter

.PHONY: all interpreter clean

all: interpreter

interpreter: 
	$(BNFC_PATH) -m --functor -o $(SOYA_DIR) soya.cf
	make -C $(SOYA_DIR)
	ghc -i.:$(SOYA_DIR) -o $@ $^

clean:
	rm -f *.o *.hi $(INTERPRETER)
	make -C $(SOYA_DIR) distclean
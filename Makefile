FPC ?= fpc
SRC := dustwood.pas
BIN := dustwood

.PHONY: all build run clean clean-obj

all: build

build: $(BIN)

$(BIN): $(SRC)
	$(FPC) $(SRC)

run: $(BIN)
	./$(BIN)

clean: clean-obj
	$(RM) $(BIN)

clean-obj:
	$(RM) *.o *.ppu

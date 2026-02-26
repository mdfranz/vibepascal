FPC ?= fpc
SRC_DIR := src/pascal
SRC := $(SRC_DIR)/dustwood.pas
BIN_DIR := bin
BIN := $(BIN_DIR)/dustwood
BUILD_DIR := build

.PHONY: all build run clean clean-obj

all: build

build: $(BIN)

$(BIN): $(SRC)
	@mkdir -p $(BIN_DIR) $(BUILD_DIR)
	$(FPC) -Fu$(SRC_DIR) -FE$(BIN_DIR) -FU$(BUILD_DIR) $(SRC)

run: $(BIN)
	./$(BIN)

clean: clean-obj
	$(RM) $(BIN)

clean-obj:
	$(RM) $(BUILD_DIR)/*.o $(BUILD_DIR)/*.ppu

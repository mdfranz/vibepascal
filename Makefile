FPC ?= fpc
PYTHON ?= python
SRC_DIR := src/pascal
SRC := $(SRC_DIR)/dustwood.pas
BIN_DIR := bin
BIN := $(BIN_DIR)/dustwood
BUILD_DIR := build
TEST_DIR := tests

.PHONY: all build run test clean clean-obj

all: build

build: $(BIN)

$(BIN): $(SRC)
	@mkdir -p $(BIN_DIR) $(BUILD_DIR)
	$(FPC) -Fu$(SRC_DIR) -FE$(BIN_DIR) -FU$(BUILD_DIR) $(SRC)

run: $(BIN)
	./$(BIN)

test:
	$(PYTHON) -m pytest -q $(TEST_DIR)

clean: clean-obj
	$(RM) $(BIN)
	$(RM) -r .pytest_cache
	find . -type d -name "__pycache__" -prune -exec $(RM) -r {} +

clean-obj:
	$(RM) $(BUILD_DIR)/*.o $(BUILD_DIR)/*.ppu

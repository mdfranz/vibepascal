FPC ?= fpc
PYTHON ?= python
GO ?= go
SRC_DIR := src/pascal
SRC := $(SRC_DIR)/dustwood.pas
GO_SRC := src/golang
BIN_DIR := bin
BIN := $(BIN_DIR)/dustwood
GO_BIN := $(BIN_DIR)/dustwood-go
BUILD_DIR := build
TEST_DIR := tests

.PHONY: all build build-go run test clean clean-obj clean-go

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

build-go: $(GO_BIN)

$(GO_BIN): $(shell find $(GO_SRC) -name '*.go')
	@mkdir -p $(BIN_DIR)
	cd $(GO_SRC) && $(GO) build -o ../../$(GO_BIN) .

clean-go:
	$(RM) $(GO_BIN)

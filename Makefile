FPC ?= fpc
PYTHON ?= python
GO ?= go
DOTNET ?= dotnet
SRC_DIR := src/pascal
SRC := $(SRC_DIR)/dustwood.pas
GO_SRC := src/golang
DOTNET_SRC := src/dotnet
BIN_DIR := bin
BIN := $(BIN_DIR)/dustwood
GO_BIN := $(BIN_DIR)/dustwood-go
DOTNET_BIN := $(BIN_DIR)/dustwood-dotnet
BUILD_DIR := build
TEST_DIR := tests

.PHONY: all build build-go build-dotnet build-pascal run run-dotnet test clean clean-obj clean-go clean-dotnet clean-pascal

all: build

build: build-go build-dotnet

build-go: $(GO_BIN)

$(GO_BIN): $(shell find $(GO_SRC) -name '*.go')
	@mkdir -p $(BIN_DIR)
	cd $(GO_SRC) && $(GO) build -o ../../$(GO_BIN) .

build-dotnet:
	@mkdir -p $(BIN_DIR)
	$(DOTNET) build $(DOTNET_SRC) -o $(BIN_DIR)

build-pascal: $(BIN)

$(BIN): $(SRC)
	@mkdir -p $(BIN_DIR) $(BUILD_DIR)
	$(FPC) -Fu$(SRC_DIR) -FE$(BIN_DIR) -FU$(BUILD_DIR) $(SRC)

run: build-go
	./$(GO_BIN)

run-dotnet: build-dotnet
	./$(DOTNET_BIN)

test:
	$(PYTHON) -m pytest -q $(TEST_DIR)

clean: clean-obj clean-go clean-dotnet
	$(RM) $(BIN)
	$(RM) -r .pytest_cache
	find . -type d -name "__pycache__" -prune -exec $(RM) -r {} +

clean-obj:
	$(RM) $(BUILD_DIR)/*.o $(BUILD_DIR)/*.ppu

clean-go:
	$(RM) $(GO_BIN)

clean-dotnet:
	$(DOTNET) clean $(DOTNET_SRC)

clean-pascal: clean-obj
	$(RM) $(BIN)

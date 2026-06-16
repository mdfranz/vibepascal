FPC ?= fpc
PYTHON ?= python
GO ?= go
SRC_DIR := src/pascal
SRC := $(SRC_DIR)/dustwood.pas
GO_SRC := src/golang
RUST_SRC := src/rust
BIN_DIR := bin
BIN := $(BIN_DIR)/dustwood
GO_BIN := $(BIN_DIR)/dustwood-go
RUST_BIN := $(BIN_DIR)/dustwood-rs
BUILD_DIR := build
TEST_DIR := tests

.PHONY: all build build-pascal build-rust run test clean clean-obj clean-pascal clean-go clean-rust

all: build

build: build-go build-rust

build-go: $(GO_BIN)

$(GO_BIN): $(shell find $(GO_SRC) -name '*.go')
	@mkdir -p $(BIN_DIR)
	cd $(GO_SRC) && $(GO) build -o ../../$(GO_BIN) .

build-rust: $(RUST_BIN)

$(RUST_BIN): $(shell find $(RUST_SRC)/src -name '*.rs') $(RUST_SRC)/Cargo.toml
	@mkdir -p $(BIN_DIR)
	cd $(RUST_SRC) && cargo build --release
	cp $(RUST_SRC)/target/release/dustwood-rs $(RUST_BIN)

build-pascal: $(BIN)

$(BIN): $(SRC)
	@mkdir -p $(BIN_DIR) $(BUILD_DIR)
	$(FPC) -Fu$(SRC_DIR) -FE$(BIN_DIR) -FU$(BUILD_DIR) $(SRC)

run: build-go
	./$(GO_BIN)

test:
	$(PYTHON) -m pytest -q $(TEST_DIR)

clean: clean-obj clean-go clean-rust
	$(RM) $(BIN)
	$(RM) -r .pytest_cache
	find . -type d -name "__pycache__" -prune -exec $(RM) -r {} +

clean-obj:
	$(RM) $(BUILD_DIR)/*.o $(BUILD_DIR)/*.ppu

clean-go:
	$(RM) $(GO_BIN)

clean-rust:
	cd $(RUST_SRC) && cargo clean
	$(RM) $(RUST_BIN)

clean-pascal: clean-obj
	$(RM) $(BIN)

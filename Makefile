RUSTOPTS ?=
RUSTC ?= rustc

BIN = ./bin/ixl
TEST = ./bin/test-ixl
CRATE = ixl.rc
SRCS = $(shell find ./ixl -name '*.rs')
RUST = $(RUSTC) $(RUSTOPTS)

CLEAN += $(BIN) $(TEST)

.PHONY: all test
all: $(BIN)
test: $(TEST)

$(BIN): $(CRATE) $(SRCS)
	@mkdir -p $(dir $@)
	$(RUST) $< -o $@

$(TEST): $(CRATE) $(SRCS)
	@mkdir -p $(dir $@)
	$(RUST) --test $< -o $@ || ( touch $@ && exit 1 )
	./$@

.PHONY: clean
clean:
	rm -rf $(CLEAN)

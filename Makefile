MAKEFILE:=$(lastword $(MAKEFILE_LIST))
MAKEFILE_DIR:=$(patsubst %/,%,$(dir $(MAKEFILE)))

SRCS:=$(shell find $(MAKEFILE_DIR)/haskell -name *.hs)

.PHONY: build
build: alfred/alfred-slack

alfred/alfred-slack: $(SRCS)
	cd $(MAKEFILE_DIR)/haskell && stack build
	cp $(MAKEFILE_DIR)/haskell/.stack-work/dist/aarch64-osx/Cabal-*/build/alfred-slack/alfred-slack alfred

.PHONY: clean
clean:
	cd $(MAKEFILE_DIR)/haskell && stack clean
	rm -f alfred/alfred-slack

.PHONY: help
help:
	@cat $(MAKEFILE) | grep -E '^[^ :]+ *:([^=]|$$)' | grep -v '^.PHONY *:' | awk -F':' '{print $$1}'

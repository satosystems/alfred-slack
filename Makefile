MAKEFILE:=$(lastword $(MAKEFILE_LIST))
MAKEFILE_DIR:=$(patsubst %/,%,$(dir $(MAKEFILE)))

SRCS:=$(shell find $(MAKEFILE_DIR)/haskell -name *.hs)

.PHONY: build
build: /Applications/alfred-slack.app/Contents/MacOS/alfred-slack

$(MAKEFILE_DIR)/alfred/alfred-slack: $(SRCS)
	cd $(MAKEFILE_DIR)/haskell && stack build
	cp $(MAKEFILE_DIR)/haskell/.stack-work/dist/aarch64-osx/*-*/build/alfred-slack/alfred-slack $@

$(MAKEFILE_DIR)/alfred/alfred-slack.app/Contents/MacOS/alfred-slack: $(MAKEFILE_DIR)/alfred/alfred-slack
	mkdir -p $(MAKEFILE_DIR)/alfred/alfred-slack.app/Contents/MacOS
	cp $^ $@

/Applications/alfred-slack.app/Contents/MacOS/alfred-slack: $(MAKEFILE_DIR)/alfred/alfred-slack.app/Contents/MacOS/alfred-slack
	rm -rf /Applications/alfred-slack.app
	cp -r $(MAKEFILE_DIR)/alfred/alfred-slack.app /Applications

.PHONY: format
format:
	fourmolu -i $(SRCS)

.PHONY: clean
clean:
	cd $(MAKEFILE_DIR)/haskell && stack clean
	rm -f $(MAKEFILE_DIR)/alfred/alfred-slack
	rm -rf $(MAKEFILE_DIR)/alfred/alfred-slack.app/Contents/MacOS/alfred-slack
	rm -rf /Applications/alfred-slack.app

.PHONY: help
help:
	@cat $(MAKEFILE) | grep -E '^[^ :]+ *:([^=]|$$)' | grep -v '^.PHONY *:' | awk -F':' '{print $$1}'

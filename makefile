CASK ?= cask
EMACS ?= emacs

.PHONY: deps test test-watch compile clean install-and-test release-patch release-minor release-major help

deps:
	$(CASK) install

install-and-test: deps test

test:
	$(CASK) exec buttercup -L . -L lib -L test test/

test-watch:
	@command -v entr >/dev/null 2>&1 || { echo "entr not found; install with 'brew install entr'"; exit 1; }
	@find . -name '*.el' -not -path './.cask/*' -not -path './.git/*' | entr -c $(MAKE) test

compile: deps
	$(CASK) build

clean:
	$(CASK) clean-elc

release-patch:
	$(EMACS) --batch --load scripts/release.el --eval "(release-version \"patch\")"

release-minor:
	$(EMACS) --batch --load scripts/release.el --eval "(release-version \"minor\")"

release-major:
	$(EMACS) --batch --load scripts/release.el --eval "(release-version \"major\")"

help:
	@echo "Available targets:"
	@echo "  deps                       - Install dependencies"
	@echo "  install deps and run tests - Run tests"
	@echo "  test                       - Run tests"
	@echo "  test-watch                 - Re-run tests on .el file changes (requires entr)"
	@echo "  compile                    - Compile source files"
	@echo "  clean                      - Clean compiled files"
	@echo "  release-patch              - Release patch version"
	@echo "  release-minor              - Release minor version"
	@echo "  release-major              - Release major version"
	@echo "  help                       - Show this help"

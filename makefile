CASK ?= cask
EMACS ?= emacs

.PHONY: deps test compile clean release-patch release-minor release-major help

deps:
	$(CASK) install

test: deps
	$(CASK) exec buttercup -L . -L lib -L test test/

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
	@echo "  deps           - Install dependencies"
	@echo "  test           - Run tests"
	@echo "  compile        - Compile source files"
	@echo "  clean          - Clean compiled files"
	@echo "  release-patch  - Release patch version"
	@echo "  release-minor  - Release minor version"
	@echo "  release-major  - Release major version"
	@echo "  help           - Show this help"

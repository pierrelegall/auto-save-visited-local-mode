.POSIX:
EMACS = emacs

# Package details
PACKAGE = auto-save-visited-local-mode
VERSION = 0.1.2

# Files
EL_FILES = $(PACKAGE).el
ELC_FILES = $(EL_FILES:.el=.elc)
TEST_FILES = $(PACKAGE)-tests.el

.PHONY: help all compile test clean checkdoc package-lint

help:
	@echo "Available targets:"
	@echo "  all          - Compile .el files (default)"
	@echo "  compile      - Byte-compile package files"
	@echo "  test         - Run test suite"
	@echo "  checkdoc     - Check documentation style"
	@echo "  package-lint - Run package-lint checks"
	@echo "  clean        - Remove compiled files"

all: compile

compile: $(ELC_FILES)

%.elc: %.el
	$(EMACS) -Q --batch -L . -f batch-byte-compile $<

test: compile
	./run-tests.sh

checkdoc:
	$(EMACS) -Q --batch \
		--eval "(checkdoc-file \"$(PACKAGE).el\")"

package-lint:
	$(EMACS) -Q --batch \
		--eval "(progn \
			(require 'package) \
			(push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) \
			(package-initialize) \
			(unless package-archive-contents (package-refresh-contents)) \
			(package-install 'package-lint))" \
		-l package-lint \
		-f package-lint-batch-and-exit $(PACKAGE).el

clean:
	rm -f $(ELC_FILES)
	rm -f *~

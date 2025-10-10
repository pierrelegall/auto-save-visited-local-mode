#!/bin/bash
# Run test suite for auto-save-visited-local-mode

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "Running auto-save-visited-local-mode test suite..."
echo

# Find Emacs
EMACS=${EMACS:-emacs}

# Check if Emacs is available
if ! command -v "$EMACS" &> /dev/null; then
  echo -e "${RED}Error: Emacs not found. Please install Emacs or set EMACS environment variable.${NC}"
  exit 1
fi

# Run tests
"$EMACS" -Q --batch \
  -L . \
  -l auto-save-visited-local-mode-tests.el \
  -f ert-run-tests-batch-and-exit

TEST_EXIT_CODE=$?

echo
if [ $TEST_EXIT_CODE -eq 0 ]; then
  echo -e "${GREEN}All tests passed!${NC}"
else
  echo -e "${RED}Tests failed with exit code $TEST_EXIT_CODE${NC}"
fi

exit $TEST_EXIT_CODE

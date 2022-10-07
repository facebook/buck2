#!/bin/sh
set -e

# Just diffs the two resources. These should be the same, if they aren't `diff` will return non-zero exit code
diff "$BUCK_DEFAULT_RUNTIME_RESOURCES/DATA" "$BUCK_DEFAULT_RUNTIME_RESOURCES/buck2/tests/targets/rules/shell/copy_data"

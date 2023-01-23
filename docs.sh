#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

print_help() {
  echo "Generates documentation in fbsource/fbcode/buck2/docs/generated"
  echo ""
  echo "By default, uses \`buck2.sh\` for built in documentation, and"
  echo "generates documentation for a predefined list of common .bzl files"
  echo "Args:"
  echo "  --help Show this help"
  echo "  --prod Generate documentation from \`buck2\`, rather than \`buck2.sh\`"
  exit 1
}

BUCK2_COMMAND="./buck2.sh"
for arg in "$@"; do
  case "$arg" in
    --help) print_help ;;
    --prod)
      BUCK2_COMMAND="buck2"
      shift
      ;;
  esac
done

set -e

# Generate the documentation for native starlark objects, and the
# prelude by default. Allows other .bzl files to be passed in, but
# note that they will end up in the fbcode//buck2/docs/generated dir
#
# NOTE: If you change the .bzl files that have docs generated for them,
#       https://www.internalfb.com/intern/configerator/edit/?path=static_docs%2Fwebsite_config.cconf
#       needs to be updated in order for the docs sandcastle job to
#       know that it needs to regenerate documentation.
cd "$( dirname "${BASH_SOURCE[0]}" )"
# Clear the docs folder first so that if we change the names of any
# objects, we'll remove old docs
rm -rf docs/generated

# Copy the starlark docs over. docusaurus does not handle upward path traversal very well.
mkdir -p docs/generated/
cp -prvf starlark-rust/docs docs/generated/starlark_rust_docs
"$BUCK2_COMMAND" docs starlark --format=markdown_files --markdown-files-destination-dir=docs/generated --builtins prelude//:prelude.bzl "$@"
cp -prvf docs/generated/native/bxl docs/generated/bxl
rm -rf docs/generated/native/bxl

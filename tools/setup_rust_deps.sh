#!/usr/bin/env bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# Bootstrap script to set up Rust third-party dependencies for Buck2.
#
# This handles the two required steps:
#   1. Vendor all Cargo crate sources into shim/third-party/rust/vendor/
#   2. Generate Buck2 build rules into shim/third-party/rust/BUCK.reindeer
#
# Usage:
#   ./tools/setup_rust_deps.sh
#
# Prerequisites:
#   - dotslash (https://dotslash-cli.com) must be installed and on PATH

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
REINDEER="${REPO_ROOT}/bootstrap/reindeer"
THIRD_PARTY_DIR="${REPO_ROOT}/shim/third-party/rust"

echo "=== Buck2 Rust Third-Party Setup ==="
echo ""
echo "Repo root:       ${REPO_ROOT}"
echo "Third-party dir: ${THIRD_PARTY_DIR}"
echo "Reindeer:        ${REINDEER}"
echo ""

# Check that reindeer binary/dotslash stub exists
if [ ! -f "${REINDEER}" ]; then
    echo "ERROR: reindeer not found at ${REINDEER}"
    echo "Make sure you have the complete Buck2 repository checked out."
    exit 1
fi

# Step 1: Vendor crate sources
echo "Step 1/2: Vendoring Rust crate sources..."
echo "  This downloads all dependencies listed in Cargo.toml."
echo "  (This may take several minutes on first run)"
echo ""
"${REINDEER}" --third-party-dir "${THIRD_PARTY_DIR}" vendor
echo ""
echo "  Vendor complete."

# Step 2: Generate Buck2 build rules
echo ""
echo "Step 2/2: Generating Buck2 build rules (BUCK.reindeer)..."
"${REINDEER}" --third-party-dir "${THIRD_PARTY_DIR}" buckify
echo ""
echo "  Build rules generated."

# Verify
echo ""
echo "=== Verification ==="
if [ -d "${THIRD_PARTY_DIR}/vendor" ]; then
    CRATE_COUNT=$(ls -d "${THIRD_PARTY_DIR}/vendor"/*/ 2>/dev/null | wc -l | tr -d ' ')
    echo "  vendor/:       OK (${CRATE_COUNT} crates)"
else
    echo "  vendor/:       MISSING"
fi

if [ -f "${THIRD_PARTY_DIR}/BUCK.reindeer" ]; then
    LINE_COUNT=$(wc -l < "${THIRD_PARTY_DIR}/BUCK.reindeer" | tr -d ' ')
    echo "  BUCK.reindeer: OK (${LINE_COUNT} lines)"
else
    echo "  BUCK.reindeer: MISSING"
fi

echo ""
echo "Setup complete! You can now build Buck2 with:"
echo "  buck2 build //..."

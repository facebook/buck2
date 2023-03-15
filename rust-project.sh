#!/usr/bin/env bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# This script is used to generate rust-project.json when using internal rust-analyzer
# on internal VSCode on devserver/OD.
# If this is not working, make sure your VSCode's root workspace is open at fbsource.
# Check out the full wiki guide at https://www.internalfb.com/intern/wiki/Rust-at-meta/rust-project/

# Fail if we have any errors
set -e

# Change to this directory
cd -- "$(dirname -- "$0")"

arc rust-project develop fbcode//buck2:buck2 --out ../../rust-project.json

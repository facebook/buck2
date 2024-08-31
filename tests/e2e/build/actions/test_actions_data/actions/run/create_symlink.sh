#!/bin/sh
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# create relative symlink to `create_symlink.sh`
script_path_relative_to_symlink="$(realpath --relative-to="$(dirname "$1")" "$0")"
ln -s "$script_path_relative_to_symlink" "$1"

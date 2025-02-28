# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

"""Provides file_alias macro."""

load("@fbsource//tools/build_defs:fb_native_wrapper.bzl", "fb_native")

_DEFAULT_VISIBILITY = ["PUBLIC"]

def file_alias(name, source, visibility = None):
    """Exports a file from source in current package.

    This is useful for renaming files that are passed as resources.

    Args:
      name: output file name.
      source: path or label identifying the original file.
      visibility: targets this file should be made visible to.
    """
    visibility = visibility if visibility != None else _DEFAULT_VISIBILITY
    fb_native.export_file(name = name, src = source, visibility = visibility)

# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# @oss-disable[end= ]: load("@prelude//apple/meta_only:xcode_argsfiles.bzl", "get_meta_specific_xcode_arg_substitutions")

def _get_meta_specific_xcode_arg_substitutions():
    # @oss-disable[end= ]: return get_meta_specific_xcode_arg_substitutions()
    return [] # @oss-enable

XCODE_ARGSFILES_SUB_TARGET = "xcode-argsfiles"

XCODE_ARG_SUBSTITUTIONS = [
    (regex("-filter-error=.+"), "-fcolor-diagnostics"),
    (regex("-filter-ignore=.+"), "-fcolor-diagnostics"),
    (regex("-filter-warning=.+"), "-fcolor-diagnostics"),
] + _get_meta_specific_xcode_arg_substitutions()

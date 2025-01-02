# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

XCODE_ARGSFILES_SUB_TARGET = "xcode-argsfiles"

XCODE_ARG_SUBSTITUTIONS = [
    (regex("-filter-error=.+"), "-fcolor-diagnostics"),
    (regex("-filter-ignore=.+"), "-fcolor-diagnostics"),
    (regex("-filter-warning=.+"), "-fcolor-diagnostics"),
    # @oss-disable: (regex("-fobjc-export-direct-methods"), "-fcolor-diagnostics"), 
    # @oss-disable: (regex("-fpika-runtime-checks"), "-fcolor-diagnostics"), 
]

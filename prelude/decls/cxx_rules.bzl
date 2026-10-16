# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# Deprecated: the C++ rules are declared in `@prelude//cxx`; load them from there.

load("@prelude//cxx:attrs.bzl", _BUILD_INFO_ATTR = "BUILD_INFO_ATTR")
load("@prelude//cxx:rules.bzl", _rules = "rules")

BUILD_INFO_ATTR = _BUILD_INFO_ATTR

cxx_rules = struct(**_rules)

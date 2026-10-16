# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# Deprecated: the Android rules are declared in `@prelude//android`; load them from there.

load("@prelude//android:attrs.bzl", _META_INTERNAL_EXTRA_PARAMS = "META_INTERNAL_EXTRA_PARAMS", _RE_CAPS = "RE_CAPS", _RE_USE_CASE = "RE_USE_CASE", _RType = "RType")
load("@prelude//android:rules.bzl", _rules = "rules")

META_INTERNAL_EXTRA_PARAMS = _META_INTERNAL_EXTRA_PARAMS
RE_CAPS = _RE_CAPS
RE_USE_CASE = _RE_USE_CASE
RType = _RType

android_rules = struct(**_rules)

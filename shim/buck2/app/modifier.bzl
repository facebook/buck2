# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# OSS stubs for the Meta-internal `ovr_config//`-driven modifier set used by
# `tests/buck_e2e.bzl`. The OSS build doesn't have those config targets, so
# return empty lists.

def buck2_modifiers():
    return []

def disable_buck2_modifiers():
    return []

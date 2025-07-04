# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//apple:apple_bundle.bzl", "apple_bundle_impl")

def apple_watchos_bundle_impl(ctx: AnalysisContext) -> list[Provider]:
    # This rule is _equivalent_ to `apple_bundle` except it applies
    # an incoming watchOS transition.
    return apple_bundle_impl(ctx)

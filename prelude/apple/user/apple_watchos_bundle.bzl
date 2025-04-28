# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//apple:apple_bundle.bzl", "apple_bundle_impl")
load("@prelude//apple:apple_bundle_attrs.bzl", "apple_watchos_bundle_attrs")
load("@prelude//user:rule_spec.bzl", "RuleRegistrationSpec")
load(":watch_transition.bzl", "watch_transition")

def apple_watchos_bundle_impl(ctx: AnalysisContext) -> list[Provider]:
    # This rule is _equivalent_ to `apple_bundle` except it applies
    # an incoming watchOS transition.
    return apple_bundle_impl(ctx)

registration_spec = RuleRegistrationSpec(
    name = "apple_watchos_bundle",
    impl = apple_watchos_bundle_impl,
    attrs = apple_watchos_bundle_attrs(),
    cfg = watch_transition,
)

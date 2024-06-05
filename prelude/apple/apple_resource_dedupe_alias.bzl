# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//user:rule_spec.bzl", "RuleRegistrationSpec")
load("@prelude//apple/user/apple_resource_transition.bzl", "apple_resource_transition")

def _impl(ctx: AnalysisContext) -> list[Provider]:
    return ctx.attrs.actual.providers

registration_spec = RuleRegistrationSpec(
    name = "apple_resource_dedupe_alias",
    impl = _impl,
    attrs = {
        "actual": attrs.transition_dep(cfg = apple_resource_transition),
        "skip_universal_resource_dedupe": attrs.bool(default = False),
    },
)

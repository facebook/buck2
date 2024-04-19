# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//user:rule_spec.bzl", "RuleRegistrationSpec")

def _impl(_: AnalysisContext) -> list[Provider]:
    return [
        DefaultInfo(),
    ]

registration_spec = RuleRegistrationSpec(
    name = "apple_xcframework",
    impl = _impl,
    attrs = {
    },
)

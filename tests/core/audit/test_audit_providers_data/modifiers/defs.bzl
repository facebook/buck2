# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

Provider1 = provider(fields = ["os"])
Provider2 = provider(fields = ["cpu"])

def _impl(ctx: AnalysisContext) -> list[Provider]:
    return [
        DefaultInfo(),
        Provider1(os = ctx.attrs.os),
        Provider2(cpu = ctx.attrs.cpu),
    ]

provider_test_rule = rule(
    attrs = {
        "cpu": attrs.string(),
        "os": attrs.string(),
    },
    impl = _impl,
)

def _impl_with_subtargets(ctx: AnalysisContext) -> list[Provider]:
    return [
        DefaultInfo(
            sub_targets = {
                "sub": [DefaultInfo(), Provider1(os = ctx.attrs.os)],
            },
        ),
    ]

provider_test_rule_with_subtargets = rule(
    attrs = {
        "os": attrs.string(),
    },
    impl = _impl_with_subtargets,
)

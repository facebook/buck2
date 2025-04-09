# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _impl_with_exclusions(ctx: AnalysisContext) -> list[Provider]:
    custom_soft_errors = {
        category: DepOnlyIncompatibleRollout(
            target_patterns = v["target_patterns"],
            exclusions = v.get("exclusions", []),
        )
        for category, v in ctx.attrs.custom_soft_errors.items()
    }
    return [
        DefaultInfo(),
        DepOnlyIncompatibleInfo(
            custom_soft_errors = custom_soft_errors,
        ),
    ]

dep_only_incompatible_info = rule(
    impl = _impl_with_exclusions,
    attrs = {
        "custom_soft_errors": attrs.dict(attrs.string(), attrs.dict(attrs.string(), attrs.list(attrs.string()))),
    },
    is_configuration_rule = True,
)

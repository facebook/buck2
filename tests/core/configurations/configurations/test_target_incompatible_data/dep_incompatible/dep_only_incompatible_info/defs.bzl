# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _impl(ctx: AnalysisContext) -> list[Provider]:
    return [
        DefaultInfo(),
        DepOnlyIncompatibleInfo(custom_soft_errors = ctx.attrs.custom_soft_errors),
    ]

dep_only_incompatible_info = rule(
    impl = _impl,
    attrs = {
        "custom_soft_errors": attrs.dict(key = attrs.string(), value = attrs.list(attrs.string())),
    },
    is_configuration_rule = True,
)

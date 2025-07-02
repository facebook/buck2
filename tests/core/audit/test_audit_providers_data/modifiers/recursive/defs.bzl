# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

RecursiveProvider = provider(fields = ["os"])

def _impl(ctx: AnalysisContext) -> list[Provider]:
    return [
        DefaultInfo(),
        RecursiveProvider(os = ctx.attrs.os),
    ]

os_info_rule = rule(
    attrs = {
        "os": attrs.string(),
    },
    impl = _impl,
)

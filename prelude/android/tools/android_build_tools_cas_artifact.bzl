# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _android_build_tools_cas_artifact_impl(ctx: AnalysisContext) -> list[Provider]:
    out = ctx.actions.cas_artifact(
        ctx.label.name,
        ctx.attrs.digest,
        "android_build_infra_tools",
        expires_after_timestamp = 1900700695,  # Mon Mar 25 2030 20:24:55 GMT
    )

    return [DefaultInfo(default_output = out)]

android_build_tools_cas_artifact = rule(
    impl = _android_build_tools_cas_artifact_impl,
    attrs = {
        "digest": attrs.string(),
    },
)

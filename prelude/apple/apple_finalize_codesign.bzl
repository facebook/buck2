# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load(":apple_bundle_types.bzl", "AppleBundleInfo")

def _apple_finalize_bundle_impl(ctx):
    bundle_artifact = ctx.attrs.bundle[DefaultInfo].default_outputs[0]
    finalized_bundle = ctx.actions.declare_output(bundle_artifact.basename)

    cmd = cmd_args([
        ctx.attrs.finalizer[RunInfo],
        "--input-bundle-path",
        bundle_artifact,
        "--output-bundle-path",
        finalized_bundle.as_output(),
        "--sign-key",
        ctx.attrs.sign_key,
    ])
    ctx.actions.run(
        cmd,
        category = "apple_finalize_bundle",
        identifier = bundle_artifact.basename,
    )

    original_bundle_info = ctx.attrs.bundle[AppleBundleInfo]
    finalized_bundle_info = AppleBundleInfo(
        bundle = finalized_bundle,
        bundle_type = original_bundle_info.bundle_type,
        binary_name = original_bundle_info.binary_name,
        contains_watchapp = original_bundle_info.contains_watchapp,
        skip_copying_swift_stdlib = original_bundle_info.skip_copying_swift_stdlib,
    )

    return [
        DefaultInfo(default_output = finalized_bundle),
        finalized_bundle_info,
    ]

apple_finalize_bundle = rule(
    attrs = {
        "bundle": attrs.dep(),
        "finalizer": attrs.exec_dep(providers = [RunInfo]),
        "sign_key": attrs.string(default = "fbios-debug"),
    },
    impl = _apple_finalize_bundle_impl,
)

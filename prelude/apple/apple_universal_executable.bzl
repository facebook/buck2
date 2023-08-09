# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(
    "@prelude//:artifact_tset.bzl",
    "project_artifacts",
)
load(":apple_dsym.bzl", "DSYM_SUBTARGET", "get_apple_dsym_ext")
load(":apple_universal_binaries.bzl", "create_universal_binary")

def _get_universal_binary_name(binary_deps: dict[str, Dependency]):
    # Because `binary_deps` is a split transition of the same target,
    # the filenames would be identical, so we just pick the first one.
    first_binary_dep = binary_deps.values()[0]
    first_binary_artifact = first_binary_dep[DefaultInfo].default_outputs[0]

    # The universal executable should have the same name as the base/thin ones
    return first_binary_artifact.short_path

def apple_universal_executable_impl(ctx: AnalysisContext) -> list[Provider]:
    dsym_name = ctx.attrs.name + ".dSYM"
    binary_outputs = create_universal_binary(
        ctx = ctx,
        binary_deps = ctx.attrs.executable,
        binary_name = _get_universal_binary_name(ctx.attrs.executable),
        dsym_bundle_name = dsym_name,
        split_arch_dsym = ctx.attrs.split_arch_dsym,
    )
    sub_targets = {}
    if ctx.attrs.split_arch_dsym:
        sub_targets[DSYM_SUBTARGET] = [DefaultInfo(default_outputs = binary_outputs.debuggable_info.dsyms)]
    else:
        bundle_binary_dsym_artifact = get_apple_dsym_ext(
            ctx = ctx,
            executable = binary_outputs.binary,
            debug_info = project_artifacts(
                actions = ctx.actions,
                tsets = [],
            ),
            action_identifier = ctx.attrs.name + "_dsym",
            output_path = dsym_name,
        )
        sub_targets[DSYM_SUBTARGET] = [DefaultInfo(default_outputs = [bundle_binary_dsym_artifact])]

    return [
        DefaultInfo(default_output = binary_outputs.binary, sub_targets = sub_targets),
    ]

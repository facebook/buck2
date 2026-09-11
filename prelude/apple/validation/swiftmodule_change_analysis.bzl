# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load(
    "@prelude//:artifact_tset.bzl",
    "make_artifact_tset",
    "stringify_artifact_label",
)
load("@prelude//apple:apple_toolchain_types.bzl", "AppleToolsInfo")
load("@prelude//apple/swift:swift_types.bzl", "SwiftDependencyInfo")

# Backs the apple_bundle `[swiftmodule-change-analysis]` subtarget
def get_swiftmodule_change_analysis_output(ctx: AnalysisContext, deps: list[Dependency]) -> Artifact | None:
    apple_tools = ctx.attrs._apple_tools[AppleToolsInfo]

    if apple_tools.swiftmodule_change_analysis == None:
        return None

    swiftmodule_change_analysis_tsets = [dep[SwiftDependencyInfo].swiftmodule_change_analysis_tset for dep in deps if SwiftDependencyInfo in dep]
    if not swiftmodule_change_analysis_tsets:
        return None

    tset = make_artifact_tset(
        actions = ctx.actions,
        label = ctx.label,
        children = swiftmodule_change_analysis_tsets,
    )
    if tset._tset == None:
        return None

    label_to_artifacts = {}
    for infos in tset._tset.traverse():
        for info in infos:
            label_to_artifacts.setdefault(stringify_artifact_label(info.label), []).extend(info.artifacts)

    if not label_to_artifacts:
        return None

    manifest = ctx.actions.write_json(
        "swiftmodule_change_analysis/manifest.json",
        label_to_artifacts,
        # with_inputs = True here means any change to a dependency's
        # swiftmodule will re-trigger this action.
        with_inputs = True,
        has_content_based_path = False,
    )

    # Not content-addressed, and `no_outputs_cleanup` below keeps this file
    # around across runs, so the checker can compare the current state
    # against what it saw the last time this action ran.
    state = ctx.actions.declare_output("swiftmodule_change_analysis/state.json", has_content_based_path = False)
    output = ctx.actions.declare_output("swiftmodule_change_analysis/differences.json", has_content_based_path = False)

    swiftmodule_change_analysis_tool = apple_tools.swiftmodule_change_analysis

    command = cmd_args([
        swiftmodule_change_analysis_tool,
        "--target",
        stringify_artifact_label(ctx.label),
        "--manifest",
        manifest,
        "--state",
        state.as_output(),
        "--output",
        output.as_output(),
    ])

    ctx.actions.run(
        command,
        category = "swiftmodule_change_analysis",
        identifier = ctx.attrs.name,
        # prefer_local as only the local action will do anything useful
        prefer_local = True,
        allow_cache_upload = False,
        no_outputs_cleanup = True,
    )

    return output

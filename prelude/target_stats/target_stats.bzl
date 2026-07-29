# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# Shared implementation for the target_stats integration, reused by
# apple_library, cxx_library and android_library. A supporting rule calls
# target_stats_providers_and_subtargets(...) when the target_stats config is
# enabled and its toolchain carries a TargetStatsToolsInfo; the result is a
# TargetStatsInfo provider (carrying a per-target manifest + a transitive set
# over deps) and the [target_stats] / [all_target_stats] sub-targets.

load(":target_stats_tools.bzl", "TargetStatsToolsInfo")
load(
    ":target_stats_types.bzl",
    "TargetStatsInfo",
    "TargetStatsInfoTSet",
    "TargetStatsRecord",
)

# Which cycle tool to run for a target (Apple file cycles, Android package
# cycles, or none).
CycleMode = enum("file", "package", "none")

_OUT_DIR = "__target_stats__"

def _file_stats_action(ctx: AnalysisContext, tools: TargetStatsToolsInfo, src: Artifact) -> Artifact:
    out = ctx.actions.declare_output(_OUT_DIR, src.short_path + ".file_stats.json", has_content_based_path = False)
    ctx.actions.run(
        cmd_args([tools.file_stats, "--input", src, "--output", out.as_output()]),
        category = "target_stats_file_stats",
        identifier = src.short_path,
        allow_cache_upload = True,
    )
    return out

def _extract_target_data_action(ctx: AnalysisContext, tools: TargetStatsToolsInfo, file_stats_by_path: dict[str, Artifact]) -> Artifact:
    # Manifest maps each source's Buck path -> its file_stats JSON; with_inputs
    # carries those JSON artifacts as inputs so the tool can read them.
    manifest = ctx.actions.write_json(
        _OUT_DIR + "/extract_manifest.json",
        file_stats_by_path,
        with_inputs = True,
        has_content_based_path = False,
    )
    out = ctx.actions.declare_output(_OUT_DIR, "target_data.json", has_content_based_path = False)
    ctx.actions.run(
        cmd_args([tools.extract_target_data, "--manifest", manifest, "--output", out.as_output()]),
        category = "target_stats_extract_target_data",
        allow_cache_upload = True,
    )
    return out

def _cycles_action(
    ctx: AnalysisContext, tools: TargetStatsToolsInfo, cycle_mode: CycleMode, target_data: Artifact, module_name: str, swift_dot: Artifact | None
) -> Artifact | None:
    if cycle_mode == CycleMode("none"):
        return None
    out = ctx.actions.declare_output(_OUT_DIR, "cycles.json", has_content_based_path = False)
    if cycle_mode == CycleMode("file"):
        args = cmd_args([
            tools.file_cycles,
            "--target-data",
            target_data,
            "--module-name",
            module_name,
            "--target-path",
            str(ctx.label.raw_target()),
            "--output",
            out.as_output(),
        ])
        if swift_dot != None:
            args.add("--swift-dot", swift_dot)
    else:
        args = cmd_args([
            tools.package_cycles,
            "--target-data",
            target_data,
            "--output",
            out.as_output(),
        ])
    ctx.actions.run(
        args,
        category = "target_stats_cycles",
        allow_cache_upload = True,
    )
    return out

def target_stats_providers_and_subtargets(
    ctx: AnalysisContext,
    *,
    tools: TargetStatsToolsInfo,
    srcs: list[Artifact],
    deps: list[Dependency],
    cycle_mode: CycleMode,
    module_name: str,
    swift_dot: Artifact | None = None,
) -> (list[Provider], dict[str, list[Provider]]):
    """Build the target_stats actions, provider and sub-targets for one target.

    Returns (providers, subtargets): a single TargetStatsInfo (with a tset over
    deps' TargetStatsInfo), and the [target_stats] (this target's manifest) and
    [all_target_stats] (whole-graph manifests) sub-targets.
    """
    label = str(ctx.label.raw_target())

    # Per-file metrics, keyed by each source's package-relative path.
    file_stats_by_path = {}
    for src in srcs:
        file_stats_by_path[src.short_path] = _file_stats_action(ctx, tools, src)

    target_data = _extract_target_data_action(ctx, tools, file_stats_by_path)
    cycles = _cycles_action(ctx, tools, cycle_mode, target_data, module_name, swift_dot)

    # Per-target manifest: aggregate info + cycles + file -> file_stats mapping.
    # write_json(with_inputs) returns a cmd_args bundle (not an Artifact), so
    # declare the output explicitly and keep the bundle to materialize the
    # referenced per-file JSONs / cycles / target_data alongside the manifest.
    manifest = ctx.actions.declare_output(_OUT_DIR, "target_stats.json", has_content_based_path = False)
    manifest_inputs = ctx.actions.write_json(
        manifest,
        {
            "cycles": cycles,
            "file_stats": file_stats_by_path,
            "target": label,
            "target_data": target_data,
        },
        with_inputs = True,
    )

    children = [dep[TargetStatsInfo].tset for dep in deps if dep.get(TargetStatsInfo) != None]
    tset = ctx.actions.tset(
        TargetStatsInfoTSet,
        value = TargetStatsRecord(label = label, manifest = manifest),
        children = children,
    )
    info = TargetStatsInfo(label = label, tset = tset)

    # [all_target_stats]: a file listing every transitive target's manifest.
    all_manifests = ctx.actions.write(
        _OUT_DIR + "/all_target_stats.txt",
        tset.project_as_args("manifests"),
        with_inputs = True,
        has_content_based_path = False,
    )

    subtargets = {
        "all_target_stats": [DefaultInfo(default_output = all_manifests)],
        "target_stats": [DefaultInfo(default_output = manifest, other_outputs = [manifest_inputs]), info],
    }
    return [info], subtargets

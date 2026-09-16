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

def _file_stats_action(ctx: AnalysisContext, tools: TargetStatsToolsInfo, src: Artifact, name: str) -> Artifact:
    out = ctx.actions.declare_output(_OUT_DIR, name + ".file_stats.json", has_content_based_path = False)
    ctx.actions.run(
        cmd_args([tools.file_stats, "--input", src, "--output", out.as_output()]),
        category = "target_stats_file_stats",
        identifier = name,
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

def _all_target_stats_subtarget(ctx: AnalysisContext, tset: TargetStatsInfoTSet) -> list[Provider]:
    """A JSON object mapping every transitive target's label to its manifest."""
    manifest_by_target = {record.label: record.manifest for record in tset.traverse()}
    out = ctx.actions.declare_output(_OUT_DIR, "all_target_stats.json", has_content_based_path = False)
    out_with_inputs = ctx.actions.write_json(out, manifest_by_target, with_inputs = True)
    return [
        DefaultInfo(
            default_output = out,
            other_outputs = [out_with_inputs, tset.project_as_args("manifests")],
        )
    ]

def target_stats_aggregate_providers_and_subtargets(
    ctx: AnalysisContext,
    *,
    deps: list[Dependency],
) -> (list[Provider], dict[str, list[Provider]]):
    """target_stats for a rule that contributes no sources of its own.

    Bundles and app binaries exist to aggregate what is beneath them, so they
    get no per-target manifest and therefore no [target_stats]; they expose
    [all_target_stats] over the whole graph and re-export a TargetStatsInfo so
    an enclosing bundle keeps propagating (e.g. an app bundle over its
    extensions).
    """
    children = [dep[TargetStatsInfo].tset for dep in deps if dep.get(TargetStatsInfo) != None]
    tset = ctx.actions.tset(TargetStatsInfoTSet, children = children)
    info = TargetStatsInfo(label = str(ctx.label.raw_target()), tset = tset)
    subtargets = {
        "all_target_stats": _all_target_stats_subtarget(ctx, tset),
    }
    return [info], subtargets

def target_stats_providers_and_subtargets(
    ctx: AnalysisContext,
    *,
    tools: TargetStatsToolsInfo,
    srcs: dict[str, Artifact],
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
    for name, src in srcs.items():
        file_stats_by_path[name] = _file_stats_action(ctx, tools, src, name)

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
        value = TargetStatsRecord(
            label = label,
            manifest = manifest,
            manifest_with_inputs = manifest_inputs,
        ),
        children = children,
    )
    info = TargetStatsInfo(label = label, tset = tset)

    subtargets = {
        "all_target_stats": _all_target_stats_subtarget(ctx, tset),
        "target_stats": [DefaultInfo(default_output = manifest, other_outputs = [manifest_inputs]), info],
    }
    return [info], subtargets

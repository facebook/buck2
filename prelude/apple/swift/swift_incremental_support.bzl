# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//apple:apple_toolchain_types.bzl", "AppleToolchainInfo")
load("@prelude//apple:apple_utility.bzl", "get_module_name")
load("@prelude//apple/swift:swift_toolchain_types.bzl", "SwiftObjectFormat")
load("@prelude//apple/swift:swift_types.bzl", "SwiftCompilationModes")
load(
    "@prelude//cxx:compile.bzl",
    "CxxSrcWithFlags",
)

_WriteOutputFileMapOutput = record(
    artifacts = field(list[Artifact]),
    swiftdeps = field(list[Artifact]),
    main_swiftdeps = field(Artifact),
    output_map_artifact = field(Artifact),
)

IncrementalCompilationOutput = record(
    incremental_flags_cmd = field(cmd_args),
    artifacts = field(list[Artifact]),
    output_map_artifact = field(Artifact),
)

SwiftCompilationMode = enum(*SwiftCompilationModes)

SwiftIncrementalBuildFilesTreshold = 20

def should_build_swift_incrementally(ctx: AnalysisContext, srcs_count: int) -> bool:
    toolchain = ctx.attrs._apple_toolchain[AppleToolchainInfo].swift_toolchain_info

    # Incremental builds are only supported when object files are generated.
    if toolchain.object_format != SwiftObjectFormat("object"):
        return False

    mode = SwiftCompilationMode(ctx.attrs.swift_compilation_mode)
    if mode == SwiftCompilationMode("wmo"):
        return False
    elif mode == SwiftCompilationMode("incremental"):
        return True
    return srcs_count >= SwiftIncrementalBuildFilesTreshold

def get_incremental_object_compilation_flags(ctx: AnalysisContext, srcs: list[CxxSrcWithFlags]) -> IncrementalCompilationOutput:
    output_file_map = _write_output_file_map(ctx, get_module_name(ctx), srcs, "object", ".o")
    return _get_incremental_compilation_flags_and_objects(output_file_map, cmd_args(["-emit-object"]))

def get_incremental_swiftmodule_compilation_flags(ctx: AnalysisContext, srcs: list[CxxSrcWithFlags]) -> IncrementalCompilationOutput:
    output_file_map = _write_output_file_map(ctx, get_module_name(ctx), srcs, "swiftmodule", ".swiftmodule")
    return _get_incremental_compilation_flags_and_objects(output_file_map, cmd_args())

def _get_incremental_compilation_flags_and_objects(
        output_file_map: _WriteOutputFileMapOutput,
        additional_flags: cmd_args) -> IncrementalCompilationOutput:
    cmd = cmd_args([
        "-incremental",
        "-enable-incremental-imports",
        "-enable-batch-mode",
        "-driver-batch-count",
        "1",
        "-output-file-map",
        output_file_map.output_map_artifact,
    ])
    cmd.add(additional_flags)

    cmd = cmd.hidden([swiftdep.as_output() for swiftdep in output_file_map.swiftdeps])
    cmd = cmd.hidden([artifact.as_output() for artifact in output_file_map.artifacts])
    cmd = cmd.hidden(output_file_map.main_swiftdeps.as_output())

    return IncrementalCompilationOutput(
        incremental_flags_cmd = cmd,
        artifacts = output_file_map.artifacts,
        output_map_artifact = output_file_map.output_map_artifact,
    )

def _write_output_file_map(
        ctx: AnalysisContext,
        module_name: str,
        srcs: list[CxxSrcWithFlags],
        compilation_mode: str,  # Either "object" or "swiftmodule"
        extension: str) -> _WriteOutputFileMapOutput:  # Either ".o" or ".swiftmodule"
    module_swiftdeps = ctx.actions.declare_output("module-build-record." + compilation_mode + ".swiftdeps")

    output_file_map = {
        "": {
            "swift-dependencies": module_swiftdeps,
        },
    }

    artifacts = []
    swiftdeps = []
    for src in srcs:
        file_name = src.file.basename
        output_artifact = ctx.actions.declare_output(file_name + extension)
        swiftdeps_artifact = ctx.actions.declare_output(file_name + "." + compilation_mode + ".swiftdeps")

        part_map = {
            compilation_mode: output_artifact,
            "swift-dependencies": swiftdeps_artifact,
        }
        output_file_map[src.file] = part_map
        artifacts.append(output_artifact)
        swiftdeps.append(swiftdeps_artifact)

    output_map_artifact = ctx.actions.write_json(module_name + "-OutputFileMap." + compilation_mode + ".json", output_file_map)

    return _WriteOutputFileMapOutput(
        artifacts = artifacts,
        swiftdeps = swiftdeps,
        main_swiftdeps = module_swiftdeps,
        output_map_artifact = output_map_artifact,
    )

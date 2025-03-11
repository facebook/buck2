# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//apple/swift:swift_toolchain_types.bzl", "SwiftObjectFormat")
load("@prelude//apple/swift:swift_types.bzl", "SwiftCompilationModes")
load(
    "@prelude//cxx:cxx_sources.bzl",
    "CxxSrcWithFlags",  # @unused Used as a type
)
load("@prelude//utils:buckconfig.bzl", "read_bool")
load(":swift_toolchain.bzl", "get_swift_toolchain_info")

_SKIP_INCREMENTAL_OUTPUTS = read_bool("apple", "skip_swift_incremental_outputs", False, False, True)

_WriteOutputFileMapOutput = record(
    artifacts = field(list[Artifact]),
    outputs = field(list[Artifact]),
    output_map_artifact = field(Artifact),
    swiftdeps = field(list[Artifact]),
)

IncrementalCompilationOutput = record(
    incremental_flags_cmd = field(cmd_args),
    artifacts = field(list[Artifact]),
    output_map_artifact = field(Artifact),
    num_threads = field(int),
    swiftdeps = field(list[Artifact]),
)

SwiftCompilationMode = enum(*SwiftCompilationModes)

_INCREMENTAL_SRC_THRESHOLD = 20

# The maxmium number of threads, we don't use
# host_info to prevent cache misses across
# different hardware models.
_MAX_NUM_THREADS = 4

# The maximum number of srcs per parallel action
_SRCS_PER_THREAD = 50

def should_build_swift_incrementally(ctx: AnalysisContext, srcs_count: int) -> bool:
    toolchain = get_swift_toolchain_info(ctx)

    # Incremental builds are only supported when object files are generated.
    if toolchain.object_format != SwiftObjectFormat("object"):
        return False

    mode = SwiftCompilationMode(ctx.attrs.swift_compilation_mode)
    if mode == SwiftCompilationMode("wmo"):
        return False
    elif mode == SwiftCompilationMode("incremental"):
        return True
    return srcs_count >= _INCREMENTAL_SRC_THRESHOLD

def get_incremental_object_compilation_flags(ctx: AnalysisContext, srcs: list[CxxSrcWithFlags]) -> IncrementalCompilationOutput:
    output_file_map = _write_output_file_map(ctx, srcs)
    return _get_incremental_compilation_flags_and_objects(output_file_map, len(srcs))

def _get_incremental_num_threads(num_srcs: int) -> int:
    if num_srcs == 0:
        return 1

    src_threads = (num_srcs + _SRCS_PER_THREAD - 1) // _SRCS_PER_THREAD
    return min(_MAX_NUM_THREADS, src_threads)

def _get_incremental_compilation_flags_and_objects(
        output_file_map: _WriteOutputFileMapOutput,
        num_srcs: int) -> IncrementalCompilationOutput:
    num_threads = _get_incremental_num_threads(num_srcs)
    cmd = cmd_args(
        [
            "-disable-cmo",
            "-emit-object",
            "-enable-batch-mode",
            "-enable-incremental-imports",
            "-incremental",
            "-j",
            str(num_threads),
            "-output-file-map",
            output_file_map.output_map_artifact.as_output() if _SKIP_INCREMENTAL_OUTPUTS else output_file_map.output_map_artifact,
            "-skip-incremental-outputs" if _SKIP_INCREMENTAL_OUTPUTS else "",
        ],
        hidden = [output.as_output() for output in output_file_map.outputs],
    )

    return IncrementalCompilationOutput(
        incremental_flags_cmd = cmd,
        artifacts = output_file_map.artifacts,
        output_map_artifact = output_file_map.output_map_artifact,
        num_threads = num_threads,
        swiftdeps = output_file_map.swiftdeps,
    )

def _write_output_file_map(
        ctx: AnalysisContext,
        srcs: list[CxxSrcWithFlags]) -> _WriteOutputFileMapOutput:
    artifacts = []
    all_outputs = []
    swiftdeps = []

    output_file_map = {}

    if not _SKIP_INCREMENTAL_OUTPUTS:
        # swift-driver doesn't respect extension for root swiftdeps file and it always has to be `.priors`.
        module_swiftdeps = ctx.actions.declare_output("__swift_incremental__/swiftdeps/module-build-record.priors")
        output_file_map = {
            "": {
                "swift-dependencies": module_swiftdeps,
            },
        }
        all_outputs = [module_swiftdeps]
        swiftdeps = [module_swiftdeps]

    for src in srcs:
        file_name = src.file.basename
        output_artifact = ctx.actions.declare_output("__swift_incremental__/objects/" + file_name + ".o")
        artifacts.append(output_artifact)
        all_outputs.append(output_artifact)

        if not _SKIP_INCREMENTAL_OUTPUTS:
            swiftdeps_artifact = ctx.actions.declare_output("__swift_incremental__/swiftdeps/" + file_name + ".swiftdeps")
            output_file_map[src.file] = {
                "object": output_artifact,
                "swift-dependencies": swiftdeps_artifact,
            }
            swiftdeps.append(swiftdeps_artifact)
            all_outputs.append(swiftdeps_artifact)

    if _SKIP_INCREMENTAL_OUTPUTS:
        output_map_artifact = ctx.actions.declare_output("__swift_incremental__/output_file_map.json")
    else:
        output_map_artifact = ctx.actions.write_json("__swift_incremental__/output_file_map.json", output_file_map, pretty = True)

    return _WriteOutputFileMapOutput(
        artifacts = artifacts,
        outputs = all_outputs,
        output_map_artifact = output_map_artifact,
        swiftdeps = swiftdeps,
    )

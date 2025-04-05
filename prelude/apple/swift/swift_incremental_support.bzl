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

# The maxmium number of threads, we don't use
# host_info to prevent cache misses across
# different hardware models.
_MAX_NUM_THREADS = 6

# This is the default, but specifying it explicitly
# is clearer.
_SWIFT_BATCH_SIZE = 25

def should_build_swift_incrementally(ctx: AnalysisContext) -> bool:
    toolchain = get_swift_toolchain_info(ctx)

    # Incremental builds are only supported when object files are generated.
    if toolchain.object_format != SwiftObjectFormat("object"):
        return False

    return SwiftCompilationMode(ctx.attrs.swift_compilation_mode) == SwiftCompilationMode("incremental")

def get_incremental_object_compilation_flags(
        ctx: AnalysisContext,
        srcs: list[CxxSrcWithFlags],
        output_swiftmodule: Artifact,
        output_header: Artifact) -> IncrementalCompilationOutput:
    output_file_map = _write_output_file_map(ctx, srcs)
    return _get_incremental_compilation_flags_and_objects(
        output_file_map,
        output_swiftmodule,
        output_header,
        len(srcs),
    )

def _get_incremental_num_threads(num_srcs: int) -> int:
    if num_srcs == 0:
        return 1

    src_threads = (num_srcs + _SWIFT_BATCH_SIZE - 1) // _SWIFT_BATCH_SIZE
    return min(_MAX_NUM_THREADS, src_threads)

def _get_incremental_compilation_flags_and_objects(
        output_file_map: _WriteOutputFileMapOutput,
        output_swiftmodule: Artifact,
        output_header: Artifact,
        num_srcs: int) -> IncrementalCompilationOutput:
    cmd = cmd_args(
        [
            "-disable-cmo",
            "-emit-object",
            "-enable-batch-mode",
            "-enable-incremental-imports",
            "-experimental-emit-module-separately",
            "-incremental",
            "-j",
            str(_MAX_NUM_THREADS),
            "-driver-batch-size-limit",
            str(_SWIFT_BATCH_SIZE),
            "-output-file-map",
            # When skipping incremental outputs, we write the contents of the output_file_map in the swift wrapper
            # and need to ensure this is an output file (vs being an input in normal cases)
            output_file_map.output_map_artifact.as_output() if _SKIP_INCREMENTAL_OUTPUTS else output_file_map.output_map_artifact,
            "-emit-objc-header",
            "-emit-objc-header-path",
            output_header.as_output(),
            "-emit-module",
            "-emit-module-path",
            output_swiftmodule.as_output(),
        ],
        hidden = [output.as_output() for output in output_file_map.outputs],
    )

    if _SKIP_INCREMENTAL_OUTPUTS:
        cmd.add("-skip-incremental-outputs")

    return IncrementalCompilationOutput(
        incremental_flags_cmd = cmd,
        artifacts = output_file_map.artifacts,
        output_map_artifact = output_file_map.output_map_artifact,
        num_threads = _get_incremental_num_threads(num_srcs),
        swiftdeps = output_file_map.swiftdeps,
    )

def _write_output_file_map(
        ctx: AnalysisContext,
        srcs: list[CxxSrcWithFlags]) -> _WriteOutputFileMapOutput:
    if _SKIP_INCREMENTAL_OUTPUTS:
        all_outputs = []
        swiftdeps = []
        artifacts = []

        for src in srcs:
            file_name = src.file.basename
            output_artifact = ctx.actions.declare_output("__swift_incremental__/objects/" + file_name + ".o")
            artifacts.append(output_artifact)
            all_outputs.append(output_artifact)

        # When skipping incremental outputs, we write the contents of the output_file_map in the swift wrapper
        # and need to ensure this is an output file (vs being an input in normal cases)
        output_map_artifact = ctx.actions.declare_output("__swift_incremental__/output_file_map.json")

    else:
        # swift-driver doesn't respect extension for root swiftdeps file and it always has to be `.priors`.
        module_swiftdeps = ctx.actions.declare_output("__swift_incremental__/swiftdeps/module-build-record.priors")
        output_file_map = {
            "": {
                "swift-dependencies": module_swiftdeps,
            },
        }
        all_outputs = [module_swiftdeps]
        swiftdeps = [module_swiftdeps]
        artifacts = []

        for src in srcs:
            file_name = src.file.basename
            output_artifact = ctx.actions.declare_output("__swift_incremental__/objects/" + file_name + ".o")
            artifacts.append(output_artifact)
            all_outputs.append(output_artifact)
            swiftdeps_artifact = ctx.actions.declare_output("__swift_incremental__/swiftdeps/" + file_name + ".swiftdeps")
            output_file_map[src.file] = {
                "object": output_artifact,
                "swift-dependencies": swiftdeps_artifact,
            }
            swiftdeps.append(swiftdeps_artifact)
            all_outputs.append(swiftdeps_artifact)

        output_map_artifact = ctx.actions.write_json("__swift_incremental__/output_file_map.json", output_file_map, pretty = True)

    return _WriteOutputFileMapOutput(
        artifacts = artifacts,
        outputs = all_outputs,
        output_map_artifact = output_map_artifact,
        swiftdeps = swiftdeps,
    )

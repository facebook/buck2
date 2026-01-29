# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//apple/swift:swift_toolchain_types.bzl", "SwiftObjectFormat")
load("@prelude//apple/swift:swift_types.bzl", "SwiftCompilationModes")
load(
    "@prelude//cxx:cxx_sources.bzl",
    "CxxSrcWithFlags",  # @unused Used as a type
)
load(":swift_toolchain.bzl", "get_swift_toolchain_info")

_OutputFileMapData = record(
    artifacts = field(list[Artifact]),
    outputs = field(list[Artifact]),
    output_file_map = field(dict),
    swiftdeps = field(list[Artifact]),
    depfiles = field(list[Artifact]),
)

IncrementalCompilationOutput = record(
    artifacts = field(list[Artifact]),
    incremental_flags_cmd = field(cmd_args),
    num_threads = field(int),
    output_file_map = field(dict),
    skip_incremental_outputs = field(bool),
    incremental_remote_outputs = field(bool),
    swiftdeps = field(list[Artifact]),
    depfiles = field(list[Artifact]),
    swiftdoc = field(Artifact),
)

IncrementalCompilationInput = record(
    swiftdeps = field(list[Artifact]),
    depfiles = field(list[Artifact]),
    swiftdoc = field(Artifact),
)

SwiftCompilationMode = enum(*SwiftCompilationModes)

# The maxmium number of threads, we don't use
# host_info to prevent cache misses across
# different hardware models.
INCREMENTAL_SWIFT_COMPILE_MAX_NUM_THREADS = 6

# This is the default, but specifying it explicitly
# is clearer.
INCREMENTAL_SWIFT_COMPILE_BATCH_SIZE = 25

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
        output_swiftdoc: Artifact,
        output_header: Artifact) -> IncrementalCompilationOutput:
    output_file_map_data = _get_output_file_map(ctx, srcs)
    return _get_incremental_compilation_flags_and_objects(
        ctx,
        output_file_map_data,
        output_swiftmodule,
        output_swiftdoc,
        output_header,
        len(srcs),
    )

def _get_incremental_num_threads(num_srcs: int) -> int:
    if num_srcs == 0:
        return 1

    src_threads = (num_srcs + INCREMENTAL_SWIFT_COMPILE_BATCH_SIZE - 1) // INCREMENTAL_SWIFT_COMPILE_BATCH_SIZE
    return min(INCREMENTAL_SWIFT_COMPILE_MAX_NUM_THREADS, src_threads)

def _get_skip_swift_incremental_outputs(ctx: AnalysisContext):
    return getattr(ctx.attrs, "_skip_swift_incremental_outputs", False)

def get_incremental_file_hashing_enabled(ctx: AnalysisContext):
    toolchain = get_swift_toolchain_info(ctx)
    return toolchain.supports_incremental_file_hashing and getattr(ctx.attrs, "swift_incremental_file_hashing", False) and should_build_swift_incrementally(ctx)

def get_swift_incremental_logging_enabled(ctx: AnalysisContext):
    return getattr(ctx.attrs, "swift_incremental_logging", False)

def get_incremental_remote_outputs_enabled(ctx: AnalysisContext):
    return getattr(ctx.attrs, "incremental_remote_outputs", False)

def get_uses_content_based_paths(ctx):
    toolchain = get_swift_toolchain_info(ctx)
    return toolchain.uses_experimental_content_based_path_hashing or getattr(ctx.attrs, "has_content_based_path", False)

def _get_incremental_compilation_flags_and_objects(
        ctx: AnalysisContext,
        output_file_map_data: _OutputFileMapData,
        output_swiftmodule: Artifact,
        output_swiftdoc: Artifact | None,
        output_header: Artifact,
        num_srcs: int) -> IncrementalCompilationOutput:
    extra_hidden = [output_swiftdoc.as_output()] if output_swiftdoc else []
    cmd = cmd_args(
        [
            "-disable-cmo",
            "-emit-object",
            "-enable-batch-mode",
            "-enable-incremental-imports",
            "-experimental-emit-module-separately",
            "-incremental",
            "-j",
            str(INCREMENTAL_SWIFT_COMPILE_MAX_NUM_THREADS),
            "-driver-batch-size-limit",
            str(INCREMENTAL_SWIFT_COMPILE_BATCH_SIZE),
            "-emit-objc-header",
            "-emit-objc-header-path",
            output_header.as_output(),
            "-emit-module",
            "-emit-module-path",
            output_swiftmodule.as_output(),
        ],
        hidden = [output.as_output() for output in output_file_map_data.outputs] + extra_hidden,
    )

    skip_incremental_outputs = _get_skip_swift_incremental_outputs(ctx)
    incremental_remote_outputs = False
    uses_experimental_content_based_path_hashing = get_uses_content_based_paths(ctx)
    if get_swift_incremental_logging_enabled(ctx):
        cmd.add([
            "-driver-show-incremental",
            "-driver-show-job-lifecycle",
        ])

    if skip_incremental_outputs:
        # When skipping incremental outputs, we write the contents of the
        # output_file_map in the swift wrapper and need to ensure this is
        # an output file (vs being an input in normal cases)
        output_map_artifact = ctx.actions.declare_output("__swift_incremental__/output_file_map.json", uses_experimental_content_based_path_hashing = uses_experimental_content_based_path_hashing)
        cmd.add(
            "-Xwrapper",
            "-skip-incremental-outputs",
            "-output-file-map",
            output_map_artifact.as_output(),
        )
    elif get_incremental_file_hashing_enabled(ctx):
        cmd.add(
            "-enable-incremental-file-hashing",
            "-avoid-emit-module-source-info",
        )
    if get_incremental_remote_outputs_enabled(ctx):
        incremental_remote_outputs = True
        cmd.add(
            "-Xwrapper",
            "--no-file-prefix-map",
            "-dwarf-version=5",
            "-Xcc",
            "-working-directory",
            "-Xcc",
            ".",
        )

    return IncrementalCompilationOutput(
        artifacts = output_file_map_data.artifacts,
        incremental_flags_cmd = cmd,
        num_threads = _get_incremental_num_threads(num_srcs),
        output_file_map = output_file_map_data.output_file_map,
        skip_incremental_outputs = skip_incremental_outputs,
        swiftdeps = output_file_map_data.swiftdeps,
        depfiles = output_file_map_data.depfiles,
        incremental_remote_outputs = incremental_remote_outputs,
        swiftdoc = output_swiftdoc,
    )

def _get_output_file_map(
        ctx: AnalysisContext,
        srcs: list[CxxSrcWithFlags]) -> _OutputFileMapData:
    uses_experimental_content_based_path_hashing = get_uses_content_based_paths(ctx)
    if _get_skip_swift_incremental_outputs(ctx):
        all_outputs = []
        swiftdeps = []
        artifacts = []
        depfiles = []
        output_file_map = {}

        for src in srcs:
            file_name = src.file.basename
            output_artifact = ctx.actions.declare_output("__swift_incremental__/objects/" + file_name + ".o", uses_experimental_content_based_path_hashing = uses_experimental_content_based_path_hashing)
            artifacts.append(output_artifact)
            all_outputs.append(output_artifact)
    else:
        # swift-driver doesn't respect extension for root swiftdeps file and it always has to be `.priors`.
        module_swiftdeps = ctx.actions.declare_output("__swift_incremental__/swiftdeps/module-build-record.priors", uses_experimental_content_based_path_hashing = uses_experimental_content_based_path_hashing)
        output_file_map = {
            "": {
                "swift-dependencies": module_swiftdeps,
            },
        }
        all_outputs = [module_swiftdeps]
        swiftdeps = [module_swiftdeps]
        artifacts = []
        depfiles = []
        toolchain = get_swift_toolchain_info(ctx)

        for src in srcs:
            file_name = src.file.basename
            output_artifact = ctx.actions.declare_output("__swift_incremental__/objects/" + file_name + ".o", uses_experimental_content_based_path_hashing = uses_experimental_content_based_path_hashing)
            artifacts.append(output_artifact)
            all_outputs.append(output_artifact)
            swiftdeps_artifact = ctx.actions.declare_output("__swift_incremental__/swiftdeps/" + file_name + ".swiftdeps", uses_experimental_content_based_path_hashing = uses_experimental_content_based_path_hashing)
            output_file_map[src.file] = {
                "object": output_artifact,
                "swift-dependencies": swiftdeps_artifact,
            }
            swiftdeps.append(swiftdeps_artifact)
            all_outputs.append(swiftdeps_artifact)
            if toolchain.use_depsfiles and not get_incremental_file_hashing_enabled(ctx):
                deps_artifact = ctx.actions.declare_output("__swift_incremental__/objects/" + file_name + ".d", uses_experimental_content_based_path_hashing = uses_experimental_content_based_path_hashing)
                depfiles.append(deps_artifact)
                all_outputs.append(deps_artifact)

    return _OutputFileMapData(
        artifacts = artifacts,
        outputs = all_outputs,
        output_file_map = output_file_map,
        swiftdeps = swiftdeps,
        depfiles = depfiles,
    )

# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//apple:apple_utility.bzl", "get_module_name")
load(
    "@prelude//cxx:compile.bzl",
    "CxxSrcWithFlags",
)

_WriteOutputFileMapOutput = record(
    objects = field(list[Artifact]),
    swiftdeps = field(list[Artifact]),
    main_swiftdeps = field(Artifact),
    output_map_artifact = field(Artifact),
)

IncrementalCompilationOutput = record(
    incremental_flags_cmd = field(cmd_args),
    artifacts = field(list[Artifact]),
)

def get_incremental_compilation_flags_and_objects(ctx: AnalysisContext, srcs: list[CxxSrcWithFlags]) -> IncrementalCompilationOutput:
    output_file_map = _write_output_file_map(ctx, get_module_name(ctx), srcs)

    cmd = cmd_args([
        "-incremental",
        "-enable-incremental-imports",
        "-enable-batch-mode",
        "-driver-batch-count",
        "1",
        "-emit-object",
        "-output-file-map",
        output_file_map.output_map_artifact,
    ])

    cmd = cmd.hidden([swiftdep.as_output() for swiftdep in output_file_map.swiftdeps])
    cmd = cmd.hidden([object_file.as_output() for object_file in output_file_map.objects])
    cmd = cmd.hidden(output_file_map.main_swiftdeps.as_output())

    return IncrementalCompilationOutput(
        incremental_flags_cmd = cmd,
        artifacts = output_file_map.objects,
    )

def _write_output_file_map(
        ctx: AnalysisContext,
        module_name: str,
        srcs: list[CxxSrcWithFlags]) -> _WriteOutputFileMapOutput:
    module_swiftdeps = ctx.actions.declare_output("module-build-record.swiftdeps")

    output_file_map = {
        "": {
            "swift-dependencies": module_swiftdeps,
        },
    }

    objects = []
    swiftdeps = []
    for src in srcs:
        file_name = src.file.basename
        object_artifact = ctx.actions.declare_output(file_name + ".o")
        swiftdeps_artifact = ctx.actions.declare_output(file_name + ".swiftdeps")

        part_map = {
            "object": object_artifact,
            "swift-dependencies": swiftdeps_artifact,
        }
        output_file_map[src.file] = part_map
        objects.append(object_artifact)
        swiftdeps.append(swiftdeps_artifact)

    output_map_artifact = ctx.actions.write_json(module_name + "-OutputFileMap.json", output_file_map)

    return _WriteOutputFileMapOutput(
        objects = objects,
        swiftdeps = swiftdeps,
        main_swiftdeps = module_swiftdeps,
        output_map_artifact = output_map_artifact,
    )

# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//cxx:compile_types.bzl", "CxxSrcCompileCommand")

CudaCompileStyle = enum(
    # Use NVCC as the compiler driver and compile a CUDA file in a single Buck
    # action.
    "mono",
    # NVCC provides the compilation plan, but use one Buck action per compilation
    # sub-command.
    "dist",
)

def cuda_compile(
        ctx: AnalysisContext,
        cmd: cmd_args,
        src_compile_cmd: CxxSrcCompileCommand,
        identifier: str,
        action_dep_files: dict[str, ArtifactTag],
        allow_dep_file_cache_upload: bool,
        error_handler_args: dict[str, [typing.Callable, None]]):
    if ctx.attrs.cuda_compile_style == CudaCompileStyle("mono").value:
        ctx.actions.run(
            cmd,
            category = src_compile_cmd.cxx_compile_cmd.category,
            identifier = identifier,
            dep_files = action_dep_files,
            allow_cache_upload = src_compile_cmd.cxx_compile_cmd.allow_cache_upload,
            allow_dep_file_cache_upload = allow_dep_file_cache_upload,
            **error_handler_args
        )
    elif ctx.attrs.cuda_compile_style == CudaCompileStyle("dist").value:
        cmd.add("-_NVCC_DRYRUN_")
        ctx.actions.run(
            cmd,
            category = "cuda_compile_prepare",
            identifier = identifier,
            dep_files = action_dep_files,
            allow_cache_upload = True,
            allow_dep_file_cache_upload = allow_dep_file_cache_upload,
            **error_handler_args
        )
    else:
        fail("Unsupported CUDA compile style: {}".format(ctx.attrs.cuda_compile_style))

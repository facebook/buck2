# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//cxx:compile_types.bzl", "CxxSrcCompileCommand")
load("@prelude//cxx:headers.bzl", "add_headers_dep_files")

CudaCompileInfo = record(
    # Output base filename without extension
    filename = field(str),
    # Buck action identifier
    identifier = field(str),
    # Output sub-directory where all CUDA compilation artifacts will go to
    output_prefix = field(str),
)

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
        object: Artifact,
        src_compile_cmd: CxxSrcCompileCommand,
        cuda_compile_info: CudaCompileInfo,
        action_dep_files: dict[str, ArtifactTag],
        allow_dep_file_cache_upload: bool,
        error_handler_args: dict[str, [typing.Callable, None]]) -> Artifact | None:
    cmd.add("-o", object.as_output())
    headers_dep_files = src_compile_cmd.cxx_compile_cmd.headers_dep_files
    if headers_dep_files:
        cmd = add_headers_dep_files(
            ctx,
            cmd,
            headers_dep_files,
            src_compile_cmd.src,
            cuda_compile_info.filename,
            action_dep_files,
        )
    if ctx.attrs.cuda_compile_style == CudaCompileStyle("mono").value:
        ctx.actions.run(
            cmd,
            category = src_compile_cmd.cxx_compile_cmd.category,
            identifier = cuda_compile_info.identifier,
            dep_files = action_dep_files,
            allow_cache_upload = src_compile_cmd.cxx_compile_cmd.allow_cache_upload,
            allow_dep_file_cache_upload = allow_dep_file_cache_upload,
            **error_handler_args
        )
        return None
    elif ctx.attrs.cuda_compile_style == CudaCompileStyle("dist").value:
        # Pass a flag to wrap_nvcc.py to enable nvcc -dryrun
        cmd.add("-_NVCC_DRYRUN_")

        # Create the following files for each CUDA file:
        # - Envvars to run the NVCC sub-commands with.
        # - A dependency graph of the NVCC sub-commands.
        # TODO: Use these in a dynamic_output action
        nvcc_dryrun_dag = ctx.actions.declare_output(
            cuda_compile_info.output_prefix,
            "{}.json".format(cuda_compile_info.filename),
        )
        cmd.add(["-_NVCC_DRYRUN_DAG_OUT_", nvcc_dryrun_dag.as_output()])
        nvcc_dryrun_env = ctx.actions.declare_output(
            cuda_compile_info.output_prefix,
            "{}.env".format(cuda_compile_info.filename),
        )
        cmd.add(["-_NVCC_DRYRUN_ENV_OUT_", nvcc_dryrun_env.as_output()])
        ctx.actions.run(
            cmd,
            category = "cuda_compile_prepare",
            identifier = cuda_compile_info.identifier,
            dep_files = action_dep_files,
            allow_cache_upload = True,
            allow_dep_file_cache_upload = allow_dep_file_cache_upload,
            **error_handler_args
        )
        return nvcc_dryrun_dag
    else:
        fail("Unsupported CUDA compile style: {}".format(ctx.attrs.cuda_compile_style))

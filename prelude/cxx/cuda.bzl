# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//cxx:compile_types.bzl", "CxxSrcCompileCommand")
load("@prelude//cxx:compiler.bzl", "get_output_flags")
load("@prelude//cxx:cxx_context.bzl", "get_cxx_toolchain_info")
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
        error_handler: [typing.Callable, None]) -> list[Artifact] | None:
    if ctx.attrs.cuda_compile_style == CudaCompileStyle("mono").value:
        # Bind the object output for monolithic NVCC compilation.
        cmd.add(get_output_flags(src_compile_cmd.cxx_compile_cmd.compiler_type, object))
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
        ctx.actions.run(
            cmd,
            category = src_compile_cmd.cxx_compile_cmd.category,
            identifier = cuda_compile_info.identifier,
            dep_files = action_dep_files,
            allow_cache_upload = src_compile_cmd.cxx_compile_cmd.allow_cache_upload,
            allow_dep_file_cache_upload = allow_dep_file_cache_upload,
            error_handler = error_handler,
        )
        return None
    elif ctx.attrs.cuda_compile_style == CudaCompileStyle("dist").value:
        return dist_nvcc(ctx, cmd, object, src_compile_cmd, cuda_compile_info)
    else:
        fail("Unsupported CUDA compile style: {}".format(ctx.attrs.cuda_compile_style))

def dist_nvcc(
        ctx: AnalysisContext,
        cmd: cmd_args,
        object: Artifact,
        src_compile_cmd: CxxSrcCompileCommand,
        cuda_compile_info: CudaCompileInfo) -> list[Artifact] | None:
    hostcc_argsfile = ctx.actions.declare_output(
        cuda_compile_info.output_prefix,
        "{}.hostcc_argsfile".format(cuda_compile_info.filename),
    )

    # Create the following files for each CUDA file:
    # - Envvars to run the NVCC sub-commands with.
    # - A dependency graph of the NVCC sub-commands.
    env = ctx.actions.declare_output(
        cuda_compile_info.output_prefix,
        "{}.env".format(cuda_compile_info.filename),
    )
    subcmds = ctx.actions.declare_output(
        cuda_compile_info.output_prefix,
        "{}.json".format(cuda_compile_info.filename),
    )

    # We'll first run nvcc with -dryrun. So do not bind the object file yet.
    cmd.add(["-o", object.short_path])
    original_cmd = cmd.copy()
    cmd.add([
        "-_NVCC_DRYRUN_",
        "-_NVCC_HOSTCC_ARGSFILE_",
        hostcc_argsfile.as_output(),
        "-_NVCC_DRYRUN_ENV_OUT_",
        env.as_output(),
        "-_NVCC_DRYRUN_DAG_OUT_",
        subcmds.as_output(),
    ])

    # Run nvcc with -dryrun to create the inputs needed for dist nvcc.
    ctx.actions.run(cmd, category = "cuda_compile_prepare", identifier = cuda_compile_info.identifier)

    def nvcc_dynamic_compile(ctx: AnalysisContext, artifacts: dict, outputs: dict[Artifact, Artifact]):
        file2artifact = {}
        plan = artifacts[subcmds].read_json()

        # Create artifacts for all intermetidate input and output files.
        for cmd_node in plan:
            node_inputs = cmd_node["inputs"]
            node_outputs = cmd_node["outputs"]
            for input in node_inputs:
                if input not in file2artifact:
                    if input.endswith(".cu"):
                        file2artifact[input] = src_compile_cmd.src
                    else:
                        input_artifact = ctx.actions.declare_output(input)
                        file2artifact[input] = input_artifact
            for output in node_outputs:
                if output not in file2artifact:
                    if output.endswith(".o"):
                        file2artifact[output] = outputs[object]
                    else:
                        output_artifact = ctx.actions.declare_output(output)
                        file2artifact[output] = output_artifact

        # Create the nvcc envvars for the sub-commands.
        subcmd_env = {}
        for line in artifacts[env].read_string().splitlines():
            key, value = line.split("=", 1)
            subcmd_env[key] = value

        toolchain = get_cxx_toolchain_info(ctx)
        for cmd_node in plan:
            subcmd = cmd_args()
            exe = cmd_node["cmd"].pop(0)
            if "g++" in exe or "clang++" in exe:
                # Add the original command as a hidden dependency, so that
                # we have access to the host compiler and header files.
                subcmd.add(cmd_args(hidden = original_cmd))
            elif "ptxas" in exe:
                # Ptxas occasionally produces an empty output. The root cause
                # is unknown as we're unable to reproduce it locally. Check the
                # output is not empty
                subcmd.add(toolchain.internal_tools.check_nonempty_output)
            subcmd.add(exe)
            for token in cmd_node["cmd"]:
                # Replace the {input} and {output} placeholders with the actual
                # artifacts. node["inputs"] and node["outputs"] are used as a
                # queue here where the files will always be correctly replaced
                # in a FIFO order.
                if "{input}" in token:
                    input = cmd_node["inputs"].pop(0)
                    left, right = token.split("{input}", 1)
                    subcmd.add(cmd_args([left, file2artifact[input], right], delimiter = ""))
                elif "{output}" in token:
                    output = cmd_node["outputs"].pop(0)
                    left, right = token.split("{output}", 1)
                    subcmd.add(
                        cmd_args([left, file2artifact[output].as_output(), right], delimiter = ""),
                    )
                elif token.startswith("-Wp,@"):
                    subcmd.add(cmd_args(hostcc_argsfile, format = "-Wp,@{}"))
                else:
                    subcmd.add(token)

            # Some nodes have hidden dependencies (deps that don't appear in
            # the cmd). Add them to the hidden field of cmd_args.
            if cmd_node["hidden"]:
                subcmd.add(cmd_args(hidden = [file2artifact[f] for f in cmd_node["hidden"]]))

            # Add the cuda toolchain deps so that we can find the Nvidia tools
            # and CUDA header files.
            subcmd.add(cmd_args(hidden = [toolchain.cuda_compiler_info.compiler]))
            ctx.actions.run(
                subcmd,
                category = cmd_node["category"],
                env = subcmd_env,
                identifier = cuda_compile_info.identifier,
                allow_cache_upload = src_compile_cmd.cxx_compile_cmd.allow_cache_upload,
                prefer_remote = True if "preproc" in cmd_node["category"] else False,
            )

    ctx.actions.dynamic_output(
        dynamic = [env, subcmds],
        inputs = [],
        outputs = [object.as_output()],
        f = nvcc_dynamic_compile,
    )
    return [subcmds, env]

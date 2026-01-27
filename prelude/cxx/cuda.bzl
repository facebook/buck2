# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//cxx:compile_types.bzl", "CudaDistributedCompileOutput", "CxxSrcCompileCommand")
load("@prelude//cxx:compiler.bzl", "get_output_flags")
load("@prelude//cxx:cxx_toolchain_types.bzl", "CxxToolchainInfo")
load("@prelude//cxx:headers.bzl", "add_headers_dep_files")
load(
    "@prelude//utils:utils.bzl",
    "as_output",
)

CudaCompileInfo = record(
    # Output base filename without extension
    filename = field(str),
    # Buck action identifier
    identifier = field(str),
    # Output sub-directory where all CUDA compilation artifacts will go to
    output_prefix = field(str),
    uses_experimental_content_based_path_hashing = field(bool),
)

CudaCompileStyle = enum(
    # Use NVCC as the compiler driver and compile a CUDA file in a single Buck
    # action.
    "mono",
    # NVCC provides the compilation plan, but use one Buck action per compilation
    # sub-command.
    "dist",
)

def declare_cuda_dist_compile_output(actions: AnalysisActions, cuda_compile_info: CudaCompileInfo) -> CudaDistributedCompileOutput:
    """
    Declare output artifacts for CUDA distributed compilation upfront.
    This should be called during analysis before the dynamic action.
    """
    content_based = cuda_compile_info.uses_experimental_content_based_path_hashing

    # Create the following files for each CUDA file:
    # - Envvars to run the NVCC sub-commands with.
    # - A dependency graph of the NVCC sub-commands.
    env = actions.declare_output(
        cuda_compile_info.output_prefix,
        "{}.env".format(cuda_compile_info.filename),
        uses_experimental_content_based_path_hashing = content_based,
    )
    subcmds = actions.declare_output(
        cuda_compile_info.output_prefix,
        "{}.json".format(cuda_compile_info.filename),
        uses_experimental_content_based_path_hashing = content_based,
    )
    return CudaDistributedCompileOutput(
        nvcc_dag = subcmds,
        nvcc_env = env,
    )

def cuda_mono_compile(
        actions: AnalysisActions,
        cmd: cmd_args,
        object: OutputArtifact,
        src_compile_cmd: CxxSrcCompileCommand,
        cuda_compile_info: CudaCompileInfo,
        action_dep_files: dict[str, ArtifactTag],
        allow_dep_file_cache_upload: bool,
        error_handler: [typing.Callable, None]) -> None:
    """
    Compile a CUDA file monolithically using NVCC as the compiler driver.
    All compilation happens in a single Buck action.
    """

    # Bind the object output for monolithic NVCC compilation.
    cmd.add(get_output_flags(src_compile_cmd.cxx_compile_cmd.compiler_type, object))
    headers_dep_files = src_compile_cmd.cxx_compile_cmd.headers_dep_files
    if headers_dep_files:
        cmd = add_headers_dep_files(
            actions,
            cmd,
            headers_dep_files,
            src_compile_cmd.src,
            cuda_compile_info.filename,
            action_dep_files,
        )
    actions.run(
        cmd,
        category = src_compile_cmd.cxx_compile_cmd.category,
        identifier = cuda_compile_info.identifier,
        dep_files = action_dep_files,
        allow_cache_upload = src_compile_cmd.cxx_compile_cmd.allow_cache_upload,
        allow_dep_file_cache_upload = allow_dep_file_cache_upload,
        error_handler = error_handler,
    )

def cuda_distributed_compile(
        actions: AnalysisActions,
        toolchain: CxxToolchainInfo,
        cmd: cmd_args,
        object: OutputArtifact,
        cuda_dist_output: CudaDistributedCompileOutput,
        src_compile_cmd: CxxSrcCompileCommand,
        cuda_compile_info: CudaCompileInfo) -> None:
    """
    Compile a CUDA file using distributed compilation.
    NVCC provides the compilation plan, but compilation is split into
    one Buck action per sub-command.
    """
    content_based = cuda_compile_info.uses_experimental_content_based_path_hashing
    hostcc_argsfile = actions.declare_output(
        cuda_compile_info.output_prefix,
        "{}.hostcc_argsfile".format(cuda_compile_info.filename),
        uses_experimental_content_based_path_hashing = content_based,
    )

    # We'll first run nvcc with -dryrun. So do not bind the object file yet.
    cmd.add(["-o", object.short_path])
    original_cmd = cmd.copy()
    cmd.add([
        "-_NVCC_DRYRUN_",
        "-_NVCC_HOSTCC_ARGSFILE_",
        hostcc_argsfile.as_output(),
        "-_NVCC_DRYRUN_ENV_OUT_",
        as_output(cuda_dist_output.nvcc_env),
        "-_NVCC_DRYRUN_DAG_OUT_",
        as_output(cuda_dist_output.nvcc_dag),
    ])

    # Run nvcc with -dryrun to create the inputs needed for dist nvcc.
    actions.run(cmd, category = "cuda_compile_prepare", identifier = cuda_compile_info.identifier)

    actions.dynamic_output_new(_nvcc_dynamic_compile_rule(
        toolchain = toolchain,
        cuda_compile_info = cuda_compile_info,
        src_compile_cmd = src_compile_cmd,
        original_cmd = original_cmd,
        hostcc_argsfile = hostcc_argsfile,
        plan_artifact = cuda_dist_output.nvcc_dag,
        env_artifact = cuda_dist_output.nvcc_env,
        output_declared_artifact = object,
    ))

# Keep the old cuda_compile function for backward compatibility
def cuda_compile(
        actions: AnalysisActions,
        toolchain: CxxToolchainInfo,
        cmd: cmd_args,
        object: Artifact,
        src_compile_cmd: CxxSrcCompileCommand,
        cuda_compile_info: CudaCompileInfo,
        action_dep_files: dict[str, ArtifactTag],
        allow_dep_file_cache_upload: bool,
        error_handler: [typing.Callable, None],
        cuda_compile_style: CudaCompileStyle | None) -> list[Artifact] | None:
    """
    Compile a CUDA file using either monolithic or distributed compilation.
    This is a convenience function that dispatches to the appropriate implementation.
    """
    if cuda_compile_style == CudaCompileStyle("mono"):
        cuda_mono_compile(
            actions,
            cmd,
            as_output(object),
            src_compile_cmd,
            cuda_compile_info,
            action_dep_files,
            allow_dep_file_cache_upload,
            error_handler,
        )
        return None
    elif cuda_compile_style == CudaCompileStyle("dist"):
        # For dist style, outputs must be declared externally
        cuda_dist_output = declare_cuda_dist_compile_output(actions, cuda_compile_info)
        cuda_distributed_compile(
            actions,
            toolchain,
            cmd,
            as_output(object),
            cuda_dist_output,
            src_compile_cmd,
            cuda_compile_info,
        )
        return [cuda_dist_output.nvcc_dag, cuda_dist_output.nvcc_env]
    else:
        fail("Unsupported CUDA compile style: {}".format(cuda_compile_style))

def _create_file_to_artifact_map(
        actions: AnalysisActions,
        plan_json: list[dict[str, typing.Any]],
        src_compile_cmd: CxxSrcCompileCommand,
        output_declared_artifact: OutputArtifact,
        uses_experimental_content_based_path_hashing: bool) -> dict[str, Artifact | OutputArtifact]:
    # Create artifacts for all intermediate input and output files.
    file2artifact = {}
    for cmd_node in plan_json:
        node_inputs = cmd_node["inputs"]
        node_outputs = cmd_node["outputs"]
        for input in node_inputs:
            if input not in file2artifact:
                if input.endswith(".cu"):
                    file2artifact[input] = src_compile_cmd.src
                else:
                    input_artifact = actions.declare_output(
                        input,
                        uses_experimental_content_based_path_hashing = uses_experimental_content_based_path_hashing,
                    )
                    file2artifact[input] = input_artifact
        for output in node_outputs:
            if output not in file2artifact:
                if output.endswith(".o"):
                    file2artifact[output] = output_declared_artifact
                else:
                    output_artifact = actions.declare_output(
                        output,
                        uses_experimental_content_based_path_hashing = uses_experimental_content_based_path_hashing,
                    )
                    file2artifact[output] = output_artifact
    return file2artifact

def _create_nvcc_subcmd_env(env_artifact: ArtifactValue) -> dict[str, str]:
    # Create the nvcc envvars for the sub-commands.
    subcmd_env = {}
    for line in env_artifact.read_string().splitlines():
        key, value = line.split("=", 1)
        subcmd_env[key] = value
    return subcmd_env

def _include_symlinked_stubs_dir(
        actions: AnalysisActions,
        file2artifact: dict[str, typing.Any],
        subcmd: cmd_args) -> None:
    """
    .cudafe1.stub.c and .fatbin.c files are hardcoded into the cudafe1.cpp file
    and its includes like below:

    #include "add.cu.o.compute_90a.cudafe1.stub.c"

    Before content-based hashing, this worked fine because all outputs were under
    the same directory. However, with content-based hashing this no longer works
    because the output files are put under the output_artifacts sub-directory.

    Fix this by creating a directory containing symlinks to the actual stubs, and
    adding it to the cuda_cxx_compile include search path.
    """
    stubs_dir = actions.declare_output(
        "__stubs__",
        dir = True,
        uses_experimental_content_based_path_hashing = True,
    )
    stubs = {}
    for file, artifact in file2artifact.items():
        if file.endswith(".cudafe1.stub.c") or file.endswith(".fatbin.c"):
            # Remove the parent paths because the includes are the filenames only.
            # We can do this because each dynamic compile deals with only one CUDA
            # source file.
            stubs[artifact.basename] = artifact
    symlinked_dir = actions.symlinked_dir(
        stubs_dir,
        stubs,
        uses_experimental_content_based_path_hashing = True,
    )
    subcmd.add(cmd_args(symlinked_dir, format = "-I{}"))

def _nvcc_dynamic_compile(
        actions: AnalysisActions,
        toolchain: CxxToolchainInfo,
        cuda_compile_info: CudaCompileInfo,
        src_compile_cmd: CxxSrcCompileCommand,
        original_cmd: cmd_args,
        hostcc_argsfile: Artifact,
        plan_artifact: ArtifactValue,
        env_artifact: ArtifactValue,
        output_declared_artifact: OutputArtifact) -> list[Provider]:
    plan = plan_artifact.read_json()
    content_based = cuda_compile_info.uses_experimental_content_based_path_hashing
    file2artifact = _create_file_to_artifact_map(
        actions,
        plan,
        src_compile_cmd,
        output_declared_artifact,
        content_based,
    )
    subcmd_env = _create_nvcc_subcmd_env(env_artifact)

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

        if content_based and cmd_node["category"] == "cuda_cxx_compile":
            _include_symlinked_stubs_dir(actions, file2artifact, subcmd)

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
                artifact = file2artifact[output]
                if isinstance(artifact, Artifact):
                    bindable = artifact.as_output()
                else:
                    bindable = artifact
                subcmd.add(cmd_args([left, bindable, right], delimiter = ""))
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
        actions.run(
            subcmd,
            category = cmd_node["category"],
            env = subcmd_env,
            identifier = cuda_compile_info.identifier,
            allow_cache_upload = src_compile_cmd.cxx_compile_cmd.allow_cache_upload,
            prefer_remote = True if "preproc" in cmd_node["category"] else False,
        )

    return [DefaultInfo()]

_nvcc_dynamic_compile_rule = dynamic_actions(
    impl = _nvcc_dynamic_compile,
    attrs = {
        "cuda_compile_info": dynattrs.value(CudaCompileInfo),
        "env_artifact": dynattrs.artifact_value(),
        "hostcc_argsfile": dynattrs.value(Artifact),
        "original_cmd": dynattrs.value(cmd_args),
        "output_declared_artifact": dynattrs.output(),
        "plan_artifact": dynattrs.artifact_value(),
        "src_compile_cmd": dynattrs.value(CxxSrcCompileCommand),
        "toolchain": dynattrs.value(CxxToolchainInfo),
    },
)

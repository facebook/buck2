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
    uses_content_based_paths = field(bool),
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
    content_based = cuda_compile_info.uses_content_based_paths

    # Create the following files for each CUDA file:
    # - Envvars to run the NVCC sub-commands with.
    # - A dependency graph of the NVCC sub-commands.
    # - Argsfile for the host compiler.
    env = actions.declare_output(
        cuda_compile_info.output_prefix,
        "{}.env".format(cuda_compile_info.filename),
        has_content_based_path = content_based,
    )
    subcmds = actions.declare_output(
        cuda_compile_info.output_prefix,
        "{}.json".format(cuda_compile_info.filename),
        has_content_based_path = content_based,
    )
    hostcc_argsfile = actions.declare_output(
        cuda_compile_info.output_prefix,
        "{}.hostcc_argsfile".format(cuda_compile_info.filename),
        has_content_based_path = content_based,
    )
    return CudaDistributedCompileOutput(
        nvcc_dag = subcmds,
        nvcc_env = env,
        hostcc_argsfile = hostcc_argsfile,
    )

def cuda_mono_compile(
    actions: AnalysisActions,
    cmd: cmd_args,
    object: OutputArtifact,
    src_compile_cmd: CxxSrcCompileCommand,
    cuda_compile_info: CudaCompileInfo,
    action_dep_files: dict[str, ArtifactTag],
    allow_dep_file_cache_upload: bool,
    error_handler: [typing.Callable, None],
) -> None:
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
    cuda_compile_info: CudaCompileInfo,
) -> None:
    """
    Compile a CUDA file using distributed compilation.
    NVCC provides the compilation plan, but compilation is split into
    one Buck action per sub-command.

    Compilation modes that require a whole-program device link step are NOT
    supported here: relocatable device code (-rdc=true / --device-c), device
    LTO (-dlto), and PTX-only codegen all make nvcc -dryrun emit an nvlink
    sub-command that dist NVCC cannot distribute. Such targets must set
    cuda_compile_style = "mono". If one of these modes reaches this path, the
    dryrun parser (tools/build/cuda/nvcc_dryrun.py) fails the
    cuda_compile_prepare action with an actionable error that names the source
    .cu and points to the mono escape hatch.
    """
    hostcc_argsfile = cuda_dist_output.hostcc_argsfile

    # We'll first run nvcc with -dryrun. So do not bind the object file yet.
    cmd.add(["-o", object.short_path])
    original_cmd = cmd.copy()

    # Dep-file filtering drops the tagged host argsfile from each sub-action key,
    # so a change to the flags themselves would otherwise go unnoticed. This
    # fingerprint carries the same flags with content-based paths rendered as a
    # placeholder: it moves when a flag really changes, not when a path does.
    headers_dep_files = src_compile_cmd.cxx_compile_cmd.headers_dep_files
    hostcc_argsfile_fingerprint = None
    if headers_dep_files != None:
        hostcc_argsfile_fingerprint, _ = actions.write(
            "__redacted__/{}.hostcc_argsfile_fingerprint".format(cuda_compile_info.filename),
            cmd_args(original_cmd, src_compile_cmd.cxx_compile_cmd.argsfile.args, quote = "shell"),
            allow_args = True,
            has_content_based_path = cuda_compile_info.uses_content_based_paths,
            use_dep_files_placeholder_for_content_based_paths = True,
        )
    cmd.add([
        "-_NVCC_DRYRUN_",
        "-_NVCC_HOSTCC_ARGSFILE_",
        as_output(hostcc_argsfile),
        "-_NVCC_DRYRUN_ENV_OUT_",
        as_output(cuda_dist_output.nvcc_env),
        "-_NVCC_DRYRUN_DAG_OUT_",
        as_output(cuda_dist_output.nvcc_dag),
    ])

    # The dry run never opens this file. It matches the flag, echoed as one
    # `-specs=<path>` token, so the plan can carry a placeholder in place of a
    # path that moves with the header closure.
    file_prefix_specs = src_compile_cmd.cxx_compile_cmd.argsfile.file_prefix_specs
    if file_prefix_specs != None:
        cmd.add(
            "-_NVCC_FILE_PREFIX_SPECS_",
            cmd_args(file_prefix_specs, format = "-specs={}"),
        )

    # Run nvcc with -dryrun to create the inputs needed for dist nvcc.
    actions.run(cmd, category = "cuda_compile_prepare", identifier = cuda_compile_info.identifier)

    actions.dynamic_output_new(
        _nvcc_dynamic_compile_rule(
            toolchain = toolchain,
            cuda_compile_info = cuda_compile_info,
            src_compile_cmd = src_compile_cmd,
            original_cmd = original_cmd,
            hostcc_argsfile = hostcc_argsfile,
            hostcc_argsfile_fingerprint = hostcc_argsfile_fingerprint,
            file_prefix_specs = file_prefix_specs,
            plan_artifact = cuda_dist_output.nvcc_dag,
            env_artifact = cuda_dist_output.nvcc_env,
            output_declared_artifact = object,
        )
    )

# Keep the old cuda_compile function for backward compatibility
def cuda_compile(
    actions: AnalysisActions,
    toolchain: CxxToolchainInfo,
    cmd: cmd_args,
    object: OutputArtifact,
    src_compile_cmd: CxxSrcCompileCommand,
    cuda_compile_info: CudaCompileInfo,
    action_dep_files: dict[str, ArtifactTag],
    allow_dep_file_cache_upload: bool,
    error_handler: [typing.Callable, None],
    cuda_compile_style: CudaCompileStyle | None,
    cuda_dist_output: CudaDistributedCompileOutput | None = None,
) -> None:
    """
    Compile a CUDA file using either monolithic or distributed compilation.
    This is a convenience function that dispatches to the appropriate implementation.
    """
    if cuda_compile_style == CudaCompileStyle("mono"):
        cuda_mono_compile(
            actions,
            cmd,
            object,
            src_compile_cmd,
            cuda_compile_info,
            action_dep_files,
            allow_dep_file_cache_upload,
            error_handler,
        )
        return None
    elif cuda_compile_style == CudaCompileStyle("dist"):
        if cuda_dist_output == None:
            fail("cuda_dist_output is required for distributed CUDA compilation")
        cuda_distributed_compile(
            actions,
            toolchain,
            cmd,
            object,
            cuda_dist_output,
            src_compile_cmd,
            cuda_compile_info,
        )
        return None
    else:
        fail("Unsupported CUDA compile style: {}".format(cuda_compile_style))

def _create_file_to_artifact_map(
    actions: AnalysisActions,
    plan_json: list[dict[str, typing.Any]],
    src_compile_cmd: CxxSrcCompileCommand,
    output_declared_artifact: OutputArtifact,
    uses_content_based_paths: bool,
) -> dict[str, Artifact | OutputArtifact]:
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
                        has_content_based_path = uses_content_based_paths,
                    )
                    file2artifact[input] = input_artifact
        for output in node_outputs:
            if output not in file2artifact:
                if output.endswith(".o"):
                    file2artifact[output] = output_declared_artifact
                else:
                    output_artifact = actions.declare_output(
                        output,
                        has_content_based_path = uses_content_based_paths,
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

def _include_symlinked_stubs_dir(actions: AnalysisActions, file2artifact: dict[str, typing.Any], subcmd: cmd_args) -> None:
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
        has_content_based_path = True,
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
        has_content_based_path = True,
    )
    subcmd.add(cmd_args(symlinked_dir, format = "-I{}"))

def _nvcc_dynamic_compile(
    actions: AnalysisActions,
    toolchain: CxxToolchainInfo,
    cuda_compile_info: CudaCompileInfo,
    src_compile_cmd: CxxSrcCompileCommand,
    original_cmd: cmd_args,
    hostcc_argsfile: Artifact,
    hostcc_argsfile_fingerprint: Artifact | None,
    file_prefix_specs: Artifact | None,
    plan_artifact: ArtifactValue,
    env_artifact: ArtifactValue,
    output_declared_artifact: OutputArtifact,
) -> list[Provider]:
    plan = plan_artifact.read_json()
    content_based = cuda_compile_info.uses_content_based_paths
    file2artifact = _create_file_to_artifact_map(
        actions,
        plan,
        src_compile_cmd,
        output_declared_artifact,
        content_based,
    )
    subcmd_env = _create_nvcc_subcmd_env(env_artifact)

    headers_dep_files = src_compile_cmd.cxx_compile_cmd.headers_dep_files

    # Tagging lets dep-file filtering drop the argsfile's own path from the
    # sub-action key; the untagged fingerprint is what still reruns them when a
    # flag changes.
    if headers_dep_files != None and hostcc_argsfile_fingerprint != None:
        hostcc_wp_form = cmd_args(
            headers_dep_files.tag.tag_artifacts(hostcc_argsfile),
            format = "-Wp,@{}",
            hidden = [hostcc_argsfile_fingerprint],
        )
    else:
        hostcc_wp_form = cmd_args(hostcc_argsfile, format = "-Wp,@{}")

    # Rendering the specs as an artifact rather than the plan's literal path is
    # what lets buck normalize it: its path moves with the header closure, and
    # the flags it carries are covered by the argsfile fingerprint.
    if file_prefix_specs != None and headers_dep_files != None:
        specs_form = cmd_args(
            headers_dep_files.tag.tag_artifacts(file_prefix_specs),
            format = "-specs={}",
        )
    else:
        specs_form = cmd_args(file_prefix_specs, format = "-specs={}")

    category_counts = {}
    for cmd_node in plan:
        subcmd = cmd_args()
        exe = cmd_node["cmd"].pop(0)
        is_host_compiler = "g++" in exe or "clang++" in exe
        if is_host_compiler:
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
                subcmd.add(hostcc_wp_form)
            elif token == "{file_prefix_specs}":
                if file_prefix_specs == None:
                    fail("CUDA plan references file-prefix specs without a specs artifact")
                subcmd.add(specs_form)
            else:
                subcmd.add(token)

        # Some nodes have hidden dependencies (deps that don't appear in
        # the cmd). Add them to the hidden field of cmd_args.
        if cmd_node["hidden"]:
            subcmd.add(cmd_args(hidden = [file2artifact[f] for f in cmd_node["hidden"]]))

        # Add the cuda toolchain deps so that we can find the Nvidia tools
        # and CUDA header files.
        subcmd.add(cmd_args(hidden = [toolchain.cuda_compiler_info.compiler]))

        # `original_cmd` pins the target's whole declared header closure into every
        # host compiler sub-action. Without dep files none of it is prunable, so any
        # header change anywhere in the closure re-runs all of these sub-actions even
        # when their output is byte-identical. Let the host compiler report the
        # headers it actually read so buck can prune the rest from the action key.
        action_dep_files = {}
        if is_host_compiler and headers_dep_files:
            # Categories can repeat within a plan, so disambiguate with a
            # per-category ordinal.
            ordinal = category_counts.get(cmd_node["category"], 0)
            category_counts[cmd_node["category"]] = ordinal + 1
            subcmd = add_headers_dep_files(
                actions,
                subcmd,
                headers_dep_files,
                src_compile_cmd.src,
                "{}/{}/{}".format(cuda_compile_info.filename, cmd_node["category"], ordinal),
                action_dep_files,
            )

        actions.run(
            subcmd,
            category = cmd_node["category"],
            env = subcmd_env,
            identifier = cuda_compile_info.identifier,
            dep_files = action_dep_files,
            allow_cache_upload = src_compile_cmd.cxx_compile_cmd.allow_cache_upload,
            allow_dep_file_cache_upload = False,
            prefer_remote = True if "preproc" in cmd_node["category"] else False,
        )

    return [DefaultInfo()]

_nvcc_dynamic_compile_rule = dynamic_actions(
    impl = _nvcc_dynamic_compile,
    attrs = {
        "cuda_compile_info": dynattrs.value(CudaCompileInfo),
        "env_artifact": dynattrs.artifact_value(),
        "file_prefix_specs": dynattrs.option(dynattrs.value(Artifact)),
        "hostcc_argsfile": dynattrs.value(Artifact),
        "hostcc_argsfile_fingerprint": dynattrs.option(dynattrs.value(Artifact)),
        "original_cmd": dynattrs.value(cmd_args),
        "output_declared_artifact": dynattrs.output(),
        "plan_artifact": dynattrs.artifact_value(),
        "src_compile_cmd": dynattrs.value(CxxSrcCompileCommand),
        "toolchain": dynattrs.value(CxxToolchainInfo),
    },
)

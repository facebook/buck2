# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//:paths.bzl", "paths")
load("@prelude//apple:apple_error_handler.bzl", "apple_build_error_handler", "swift_error_deserializer", "swift_error_handler")
load("@prelude//apple/swift:apple_sdk_modules_utility.bzl", "is_sdk_modules_provided")
load("@prelude//cxx:argsfiles.bzl", "CompileArgsfile")
load("@prelude//cxx:cxx_sources.bzl", "CxxSrcWithFlags")
load(
    ":swift_incremental_support.bzl",
    "IncrementalCompilationInput",  # @unused Used as a type
    "get_incremental_split_actions",
    "get_uses_content_based_paths",
    "should_build_swift_incrementally",
)
load(":swift_output_file_map.bzl", "add_output_file_map_flags", "add_serialized_diagnostics_output")
load(":swift_toolchain.bzl", "get_swift_toolchain_info")
load(":swift_toolchain_types.bzl", "SwiftToolchainInfo")

CompileWithArgsFileCmdOutput = record(
    cmd = field(cmd_args),
    argsfile = field(Artifact),
    argsfile_cmd_form = field(cmd_args),
    shell_quoted_args = field(cmd_args),
    error_outputs = field(list[OutputArtifact]),
    output_file_map_artifact = field(Artifact | None),
    error_deserializer = field(RunInfo | None),
)

def compile_with_argsfile_cmd(
        ctx: AnalysisContext,
        category: str,
        shared_flags: cmd_args,
        srcs: list[CxxSrcWithFlags],
        additional_flags: cmd_args,
        toolchain: SwiftToolchainInfo,
        output_file_map: dict,
        supports_output_file_map: bool,
        supports_serialized_errors: bool,
        skip_incremental_outputs: bool,
        incremental_remote_outputs: bool,
        objects: list[Artifact],
        incremental_artifacts: IncrementalCompilationInput | None,
        artifact_tag: ArtifactTag | None) -> CompileWithArgsFileCmdOutput:
    object_outputs = [obj.as_output() for obj in objects]

    uses_content_based_paths = get_uses_content_based_paths(ctx)

    writable_incremental_args = []
    if incremental_artifacts:
        writable_incremental_args.extend(object_outputs)
        writable_incremental_args.extend([swiftdep.as_output() for swiftdep in incremental_artifacts.swiftdeps])
        writable_incremental_args.extend([depfile.as_output() for depfile in incremental_artifacts.depfiles])
        if incremental_artifacts.swiftdoc:
            writable_incremental_args.append(incremental_artifacts.swiftdoc.as_output())

    cmd = cmd_args(toolchain.compiler)
    cmd.add(additional_flags)

    # Hack to make output_file_map mutable when the default is used.
    output_file_map = output_file_map or {}

    # Assemble argsfile with compiler flags. We don't use `with_inputs` in the
    # write action as this strips tagged values and breaks dependency file
    # input tracking.
    shell_quoted_args = cmd_args(shared_flags, quote = "shell")

    if artifact_tag != None and uses_content_based_paths:
        # This is a little bit convoluted due to the way that content-based paths affect argfiles.
        # If an unused tagged input changes, we don't want to re-run the action, but if it is a
        # content-based input that is written to the argfile, then the argfile will also change
        # and that would cause a re-run.
        #
        # We therefore write the argfile twice: the "real" argfile, which is used in the action
        # and tagged as unused so that it is not used for dep-file comparison, and an argfile
        # that uses placeholders instead of content-based paths, which is not tagged for dep-files
        # and therefore causes a dep-file miss if it changes.
        argsfile, _ = ctx.actions.write(".{}_argsfile".format(category), shell_quoted_args, allow_args = True, has_content_based_path = uses_content_based_paths)
        placeholder_argsfile, _ = ctx.actions.write(".{}_argsfile_placeholder".format(category), shell_quoted_args, allow_args = True, use_dep_files_placeholder_for_content_based_paths = True, has_content_based_path = uses_content_based_paths)
        cmd.add(cmd_args(hidden = placeholder_argsfile))
        argsfile_cmd_form = cmd_args(artifact_tag.tag_artifacts(argsfile), format = "@{}", delimiter = "", hidden = shared_flags)
    else:
        argsfile, _ = ctx.actions.write(".{}_argsfile".format(category), shell_quoted_args, allow_args = True, has_content_based_path = uses_content_based_paths)
        argsfile_cmd_form = cmd_args(argsfile, format = "@{}", delimiter = "", hidden = shared_flags)
    cmd.add(argsfile_cmd_form)

    # Assemble argsfile with Swift source files.
    if srcs:
        swift_quoted_files = cmd_args([s.file for s in srcs], quote = "shell")

        # This path needs to be kept in sync with the _SWIFT_FILES_ARGSFILE
        # variable in swift_exec.py.
        swift_files, _ = ctx.actions.write(".{}_swift_srcs".format(category), swift_quoted_files, allow_args = True, has_content_based_path = uses_content_based_paths)
        swift_files_cmd_form = cmd_args(swift_files, format = "@{}", delimiter = "", hidden = swift_quoted_files)
        cmd.add(swift_files_cmd_form)

    # If the toolchain supports serialized error output, add the output files
    # to deserialize the errors. This uses the output file map if supported,
    # otherwise will pass frontend flags.
    error_deserializer = swift_error_deserializer(ctx)
    error_outputs = []

    if supports_serialized_errors and error_deserializer:
        json_error_output = ctx.actions.declare_output("__diagnostics__/{}.json".format(category), has_content_based_path = uses_content_based_paths).as_output()
        error_outputs.append(json_error_output)
        is_incremental = incremental_artifacts != None
        add_serialized_diagnostics_output(
            ctx = ctx,
            output_file_map = output_file_map if supports_output_file_map else None,
            cmd = cmd,
            diagnostics_output = json_error_output,
            is_incremental = is_incremental,
            skip_incremental_outputs = skip_incremental_outputs,
            split_actions = get_incremental_split_actions(ctx),
        )
        cmd.add(
            "-Xwrapper",
            cmd_args(error_deserializer, format = "-serialized-diagnostics-to-json={}"),
            "-Xwrapper",
            cmd_args(json_error_output, format = "-json-error-output-path={}"),
        )
        if incremental_remote_outputs and should_build_swift_incrementally(ctx) and not skip_incremental_outputs:
            # Serialized diagnostics in Swift incremental mode will produce a
            # .dia output for each .o output. These need to be made writable.
            for obj in objects:
                name_without_extension, _ = paths.split_extension(obj.short_path)
                diag_output = ctx.actions.declare_output("{}.dia".format(name_without_extension), has_content_based_path = uses_content_based_paths)
                cmd.add(cmd_args(hidden = diag_output.as_output()))
                writable_incremental_args.append(diag_output.as_output())

    # If an output file map is provided, serialize it and add to the command.
    if output_file_map:
        if not supports_output_file_map:
            fail("Output file maps are not supported for {}".format(category))
        output_file_map_artifact = add_output_file_map_flags(ctx, output_file_map, cmd, category)
    else:
        output_file_map_artifact = None

    if writable_incremental_args and incremental_remote_outputs:
        cmd.add(["-Xwrapper", "--writable-incremental-paths"])
        for flag in writable_incremental_args:
            cmd.add(["-Xwrapper", flag])

    return CompileWithArgsFileCmdOutput(
        cmd = cmd,
        argsfile = argsfile,
        argsfile_cmd_form = argsfile_cmd_form,
        shell_quoted_args = shell_quoted_args,
        error_outputs = error_outputs,
        output_file_map_artifact = output_file_map_artifact,
        error_deserializer = error_deserializer,
    )

def compile_with_argsfile(
        ctx: AnalysisContext,
        category: str,
        shared_flags: cmd_args,
        srcs: list[CxxSrcWithFlags],
        additional_flags: cmd_args,
        toolchain: SwiftToolchainInfo,
        num_threads: int = 1,
        dep_files: dict[str, ArtifactTag] = {},
        output_file_map: dict = {},
        allow_cache_upload = False,
        local_only = False,
        prefer_local = False,
        no_outputs_cleanup = False,
        supports_output_file_map = True,
        supports_serialized_errors = True,
        skip_incremental_outputs = False,
        incremental_remote_outputs = False,
        objects = [],
        incremental_artifacts: IncrementalCompilationInput | None = None,
        artifact_tag: ArtifactTag | None = None) -> (CompileArgsfile, Artifact | None):
    cmd_output = compile_with_argsfile_cmd(
        ctx = ctx,
        category = category,
        shared_flags = shared_flags,
        srcs = srcs,
        additional_flags = additional_flags,
        toolchain = toolchain,
        output_file_map = output_file_map,
        supports_output_file_map = supports_output_file_map,
        supports_serialized_errors = supports_serialized_errors,
        skip_incremental_outputs = skip_incremental_outputs,
        incremental_remote_outputs = incremental_remote_outputs,
        objects = objects,
        incremental_artifacts = incremental_artifacts,
        artifact_tag = artifact_tag,
    )

    ctx.actions.run(
        cmd_output.cmd,
        allow_cache_upload = allow_cache_upload,
        allow_dep_file_cache_upload = (len(dep_files) > 0),
        category = category,
        dep_files = dep_files,
        error_handler = swift_error_handler if cmd_output.error_deserializer else apple_build_error_handler,
        local_only = local_only,
        no_outputs_cleanup = no_outputs_cleanup,
        incremental_remote_outputs = incremental_remote_outputs,
        outputs_for_error_handler = cmd_output.error_outputs,
        prefer_local = prefer_local,
        unique_input_inodes = True,
        weight = num_threads,
    )

    argsfile = CompileArgsfile(
        file = cmd_output.argsfile,
        cmd_form = cmd_output.argsfile_cmd_form,
        args = cmd_output.shell_quoted_args,
        args_without_file_prefix_args = shared_flags,
    )

    return argsfile, cmd_output.output_file_map_artifact

def uses_explicit_modules(ctx: AnalysisContext) -> bool:
    swift_toolchain = get_swift_toolchain_info(ctx)
    return ctx.attrs.uses_explicit_modules and is_sdk_modules_provided(swift_toolchain)

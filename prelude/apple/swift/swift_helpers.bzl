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
    "should_build_swift_incrementally",
)
load(":swift_output_file_map.bzl", "add_output_file_map_flags", "add_serialized_diagnostics_output")
load(":swift_toolchain.bzl", "get_swift_toolchain_info")
load(":swift_toolchain_types.bzl", "SwiftToolchainInfo")

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
        incremental_artifacts: IncrementalCompilationInput | None = None) -> (CompileArgsfile, Artifact | None):
    writable_incremental_args = [obj.as_output() for obj in objects]
    if incremental_artifacts:
        writable_incremental_args = [swiftdep.as_output() for swiftdep in incremental_artifacts.swiftdeps] + [depfile.as_output() for depfile in incremental_artifacts.depfiles]

    cmd = cmd_args(toolchain.compiler)
    cmd.add(additional_flags)

    # Hack to make output_file_map mutable when the default is used.
    output_file_map = output_file_map or {}

    # Assemble argsfile with compiler flags. We don't use `with_inputs` in the
    # write action as this strips tagged values and breaks dependency file
    # input tracking.
    shell_quoted_args = cmd_args(shared_flags, quote = "shell")

    # Many tests rely on the path of the argsfile, so you probably don't
    # want to change this.
    argsfile, _ = ctx.actions.write(".{}_argsfile".format(category), shell_quoted_args, allow_args = True)
    argsfile_cmd_form = cmd_args(argsfile, format = "@{}", delimiter = "", hidden = shared_flags)
    cmd.add(argsfile_cmd_form)

    # Assemble argsfile with Swift source files.
    if srcs:
        swift_quoted_files = cmd_args([s.file for s in srcs], quote = "shell")

        # This path needs to be kept in sync with the _SWIFT_FILES_ARGSFILE
        # variable in swift_exec.py.
        swift_files, _ = ctx.actions.write(".{}_swift_srcs".format(category), swift_quoted_files, allow_args = True)
        swift_files_cmd_form = cmd_args(swift_files, format = "@{}", delimiter = "", hidden = swift_quoted_files)
        cmd.add(swift_files_cmd_form)

    # If the toolchain supports serialized error output, add the output files
    # to deserialize the errors. This uses the output file map if supported,
    # otherwise will pass frontend flags.
    error_deserializer = swift_error_deserializer(ctx)
    error_outputs = []

    if supports_serialized_errors and error_deserializer:
        json_error_output = ctx.actions.declare_output("__diagnostics__/{}.json".format(category)).as_output()
        error_outputs.append(json_error_output)
        add_serialized_diagnostics_output(
            output_file_map = output_file_map if supports_output_file_map else None,
            cmd = cmd,
            diagnostics_output = json_error_output,
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
                name_without_extension, _ = paths.split_extension(obj.short_path())
                diag_output = ctx.actions.declare_output("{}.dia".format(name_without_extension))
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

    ctx.actions.run(
        cmd,
        allow_cache_upload = allow_cache_upload,
        allow_dep_file_cache_upload = (len(dep_files) > 0),
        category = category,
        dep_files = dep_files,
        error_handler = swift_error_handler if error_deserializer else apple_build_error_handler,
        local_only = local_only,
        no_outputs_cleanup = no_outputs_cleanup,
        incremental_remote_outputs = incremental_remote_outputs,
        outputs_for_error_handler = error_outputs,
        prefer_local = prefer_local,
        unique_input_inodes = True,
        weight = num_threads,
    )

    argsfile = CompileArgsfile(
        file = argsfile,
        cmd_form = argsfile_cmd_form,
        args = shell_quoted_args,
        args_without_file_prefix_args = shared_flags,
    )

    return argsfile, output_file_map_artifact

def uses_explicit_modules(ctx: AnalysisContext) -> bool:
    swift_toolchain = get_swift_toolchain_info(ctx)
    return ctx.attrs.uses_explicit_modules and is_sdk_modules_provided(swift_toolchain)

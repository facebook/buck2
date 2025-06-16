# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//apple:apple_error_handler.bzl", "apple_build_error_handler", "apple_error_deserializer", "swift_error_handler")
load("@prelude//apple/swift:apple_sdk_modules_utility.bzl", "is_sdk_modules_provided")
load("@prelude//cxx:argsfiles.bzl", "CompileArgsfile", "CompileArgsfiles")
load("@prelude//cxx:cxx_sources.bzl", "CxxSrcWithFlags")
load(":swift_incremental_support.bzl", "should_build_swift_incrementally")
load(":swift_output_file_map.bzl", "add_output_file_map_flags", "add_serialized_diagnostics_output")
load(":swift_toolchain.bzl", "get_swift_toolchain_info")
load(":swift_toolchain_types.bzl", "SwiftToolchainInfo")

def compile_with_argsfile(
        ctx: AnalysisContext,
        category: str,
        extension: str,
        shared_flags: cmd_args,
        srcs: list[CxxSrcWithFlags],
        additional_flags: cmd_args,
        toolchain: SwiftToolchainInfo,
        identifier: str | None = None,
        num_threads: int = 1,
        cacheable: bool = True,
        dep_files: dict[str, ArtifactTag] = {},
        output_file_map: dict = {}) -> (CompileArgsfiles, Artifact | None):
    cmd = cmd_args(toolchain.compiler)
    cmd.add(additional_flags)

    # Hack to make output_file_map mutable when the default is used.
    output_file_map = output_file_map or {}

    # Assemble argsfile with compiler flags. We don't use `with_inputs` in the
    # write action as this strips tagged values and breaks dependency file
    # input tracking.
    shell_quoted_args = cmd_args(shared_flags, quote = "shell")
    argsfile, _ = ctx.actions.write(extension + "_compile_argsfile", shell_quoted_args, allow_args = True)
    argsfile_cmd_form = cmd_args(argsfile, format = "@{}", delimiter = "", hidden = shared_flags)
    cmd.add(argsfile_cmd_form)

    # Assemble argsfile with Swift source files.
    if srcs:
        swift_quoted_files = cmd_args([s.file for s in srcs], quote = "shell")
        swift_files, _ = ctx.actions.write(extension + "_files", swift_quoted_files, allow_args = True)
        swift_files_cmd_form = cmd_args(swift_files, format = "@{}", delimiter = "", hidden = swift_quoted_files)
        cmd.add(swift_files_cmd_form)

    # If the toolchain supports serialized error output, add the output files
    # to the output file map so we can deserialize the errors.
    error_deserializer = apple_error_deserializer(ctx)
    error_outputs = []
    if error_deserializer:
        json_error_output = ctx.actions.declare_output("__diagnostics__/{}_{}.json".format(ctx.attrs.name, category)).as_output()
        error_outputs.append(json_error_output)
        add_serialized_diagnostics_output(output_file_map, cmd, json_error_output)
        cmd.add(
            "-Xwrapper",
            cmd_args(error_deserializer, format = "-serialized-diagnostics-to-json={}"),
            "-Xwrapper",
            cmd_args(json_error_output, format = "-json-error-output-path={}"),
        )

    # If an output file map is provided, serialize it and add to the command.
    if output_file_map:
        output_file_map_artifact = add_output_file_map_flags(ctx, output_file_map, cmd, category)
    else:
        output_file_map_artifact = None

    build_swift_incrementally = should_build_swift_incrementally(ctx)
    explicit_modules_enabled = uses_explicit_modules(ctx)

    # If we prefer to execute locally (e.g., for perf reasons), ensure we upload to the cache,
    # so that CI builds populate caches used by developer machines.
    allow_cache_upload = True
    local_only = False

    # Swift compilation on RE without explicit modules is impractically expensive
    # because there's no shared module cache across different libraries.
    prefer_local = not explicit_modules_enabled

    if (not cacheable) or (build_swift_incrementally and not toolchain.supports_relative_resource_dir):
        # When Swift code is built incrementally, the swift-driver embeds absolute paths into
        # the artifacts without relative resource dir support. In this case we can only build locally.
        allow_cache_upload = False
        local_only = True
        prefer_local = False
    elif build_swift_incrementally:
        # Swift incremental compilation requires the swiftdep files which are only present when
        # compiling locally. Prefer local unless otherwise overridden.
        prefer_local = True

    ctx.actions.run(
        cmd,
        category = category,
        identifier = identifier,
        # When building incrementally, we need to preserve local state between invocations.
        no_outputs_cleanup = build_swift_incrementally,
        error_handler = swift_error_handler if error_deserializer else apple_build_error_handler,
        outputs_for_error_handler = error_outputs,
        weight = num_threads,
        allow_cache_upload = allow_cache_upload,
        local_only = local_only,
        prefer_local = prefer_local,
        dep_files = dep_files,
        allow_dep_file_cache_upload = True,
        # Swift compiler requires unique inodes for all input files.
        unique_input_inodes = True,
    )

    argsfile = CompileArgsfile(
        file = argsfile,
        cmd_form = argsfile_cmd_form,
        args = shell_quoted_args,
        args_without_file_prefix_args = shared_flags,
    )

    # Swift correctly handles relative paths and we can utilize the relative argsfile for Xcode.
    return CompileArgsfiles(relative = {extension: argsfile}, xcode = {extension: argsfile}), output_file_map_artifact

def uses_explicit_modules(ctx: AnalysisContext) -> bool:
    swift_toolchain = get_swift_toolchain_info(ctx)
    return ctx.attrs.uses_explicit_modules and is_sdk_modules_provided(swift_toolchain)

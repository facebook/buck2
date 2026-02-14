# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load(
    "@prelude//:artifact_tset.bzl",
    "project_artifacts",
)
load("@prelude//:local_only.bzl", "link_cxx_binary_locally")
load("@prelude//:paths.bzl", "paths")
load("@prelude//:resources.bzl", "create_resource_db", "gather_resources")
load(
    "@prelude//apple:apple_frameworks.bzl",
    "apple_build_link_args_with_deduped_flags",
    "apple_get_link_info_by_deduping_link_infos",
)
load("@prelude//cxx:cxx_library_utility.bzl", "cxx_attr_deps", "cxx_attr_use_content_based_paths")
load(
    "@prelude//cxx:cxx_link_utility.bzl",
    "executable_shared_lib_arguments",
    "make_link_args",
)
load(
    "@prelude//cxx:cxx_toolchain_types.bzl",
    "LinkerType",
)
load("@prelude//cxx:debug.bzl", "SplitDebugMode")
load("@prelude//cxx:dwp.bzl", "dwp", "dwp_available")
load(
    "@prelude//cxx:linker.bzl",
    "get_import_library",
    "get_output_flags",
    "get_shared_library_name_linker_flags",
)
load(
    "@prelude//cxx:transformation_spec.bzl",
    "TransformationSpecContext",  # @unused Used as a type
)
load(
    "@prelude//linking:link_info.bzl",
    "LibOutputStyle",  # @unused Used as a type
    "LinkArgs",
    "LinkInfos",  # @unused Used as a type
    "LinkStrategy",  # @unused Used as a type
    "create_merged_link_info",
    "get_link_args_for_strategy",
)
load(
    "@prelude//linking:shared_libraries.bzl",
    "merge_shared_libraries",
)
load("@prelude//linking:strip.bzl", "strip_debug_info")
load("@prelude//linking:types.bzl", "Linkage")
load("@prelude//os_lookup:defs.bzl", "OsLookup")
load("@prelude//rust/tools:attrs.bzl", "RustInternalToolsInfo")
load("@prelude//utils:argfile.bzl", "at_argfile")
load("@prelude//utils:cmd_script.bzl", "cmd_script")
load(
    "@prelude//utils:utils.bzl",
    "flatten_dict",
)
load(
    ":build_params.bzl",
    "BuildParams",  # @unused Used as a type
    "CrateType",
    "Emit",
    "MetadataKind",
    "ProfileMode",  # @unused Used as a type
    "RelocModel",
    "crate_type_codegen",
    "crate_type_linked",
    "dep_metadata_of_emit",
)
load(":clippy_configuration.bzl", "ClippyConfiguration")
load(
    ":context.bzl",
    "CommonArgsInfo",
    "CompileContext",
    "CrateName",  # @unused Used as a type
    "DepCollectionContext",
    "output_filename",
)
load(
    ":extern.bzl",
    "crate_map_arg",
    "extern_arg",
)
load(
    ":failure_filter.bzl",
    "failure_filter",
)
load(
    ":link_info.bzl",
    "RustArtifact",
    "RustCxxLinkGroupInfo",  #@unused Used as a type
    "RustDependency",
    "RustLinkInfo",
    "TransitiveDeps",
    "attr_crate",
    "attr_simple_crate_for_filenames",
    "executable_shared_lib_arguments_from_shared_library_info",
    "get_available_proc_macros",
    "inherited_dep_external_debug_infos",
    "inherited_external_debug_info_from_dep_infos",
    "inherited_merged_link_infos",
    "inherited_rust_external_debug_info",
    "inherited_shared_libs",
    "normalize_crate",
    "resolve_rust_deps",
    "strategy_info",
)
load(":outputs.bzl", "RustcOutput")
load(":resources.bzl", "rust_attr_resources")
load(":rust_toolchain.bzl", "PanicRuntime", "RustToolchainInfo")

def generate_rustdoc(
        ctx: AnalysisContext,
        compile_ctx: CompileContext,
        # link style doesn't matter, but caller should pass in build params
        # with static-pic (to get best cache hits for deps)
        params: BuildParams,
        default_roots: list[str],
        document_private_items: bool) -> Artifact:
    toolchain_info = compile_ctx.toolchain_info

    common_args = _compute_common_args(
        ctx = ctx,
        compile_ctx = compile_ctx,
        dep_ctx = compile_ctx.dep_ctx,
        # to make sure we get the rmeta's generated for the crate dependencies,
        # rather than full .rlibs
        emit = Emit("metadata-fast"),
        params = params,
        default_roots = default_roots,
        infallible_diagnostics = False,
        incremental_enabled = False,
        is_rustdoc_test = False,
        profile_mode = None,
    )

    subdir = common_args.subdir + "-rustdoc"
    output = ctx.actions.declare_output(subdir)

    plain_env, path_env = process_env(compile_ctx, toolchain_info.rustdoc_env | ctx.attrs.env)
    plain_env["RUSTDOC_BUCK_TARGET"] = cmd_args(str(ctx.label.raw_target()))

    if toolchain_info.rust_target_path != None:
        path_env["RUST_TARGET_PATH"] = toolchain_info.rust_target_path[DefaultInfo].default_outputs[0]

    rustdoc_cmd = cmd_args(
        toolchain_info.rustdoc,
        "--rustc-action-separator",
        toolchain_info.rustdoc_flags,
        ctx.attrs.rustdoc_flags,
        common_args.args,
        cmd_args(output.as_output(), format = "--out-dir={}"),
        hidden = [toolchain_info.rustdoc, compile_ctx.symlinked_srcs],
    )

    if document_private_items:
        rustdoc_cmd.add("--document-private-items")

    rustdoc_cmd_action = cmd_args(
        [cmd_args("--env=", k, "=", v, delimiter = "") for k, v in plain_env.items()],
        [cmd_args("--path-env=", k, "=", v, delimiter = "") for k, v in path_env.items()],
        rustdoc_cmd,
    )

    rustdoc_cmd = _long_command(
        ctx = ctx,
        exe = compile_ctx.internal_tools_info.rustc_action,
        args = rustdoc_cmd_action,
        argfile_name = "{}.args".format(subdir),
    )

    ctx.actions.run(rustdoc_cmd, category = "rustdoc")

    return output

def generate_rustdoc_coverage(
        ctx: AnalysisContext,
        compile_ctx: CompileContext,
        # link strategy doesn't matter, but caller should pass in build params
        # with static-pic (to get best cache hits for deps)
        params: BuildParams,
        default_roots: list[str]) -> Artifact:
    toolchain_info = compile_ctx.toolchain_info

    common_args = _compute_common_args(
        ctx = ctx,
        compile_ctx = compile_ctx,
        dep_ctx = compile_ctx.dep_ctx,
        # to make sure we get the rmeta's generated for the crate dependencies,
        # rather than full .rlibs
        emit = Emit("metadata-fast"),
        params = params,
        default_roots = default_roots,
        infallible_diagnostics = False,
        incremental_enabled = False,
        is_rustdoc_test = False,
        profile_mode = None,
    )

    file = common_args.subdir + "-rustdoc-coverage"
    output = ctx.actions.declare_output(file)

    plain_env, path_env = process_env(compile_ctx, ctx.attrs.env)
    plain_env["RUSTDOC_BUCK_TARGET"] = cmd_args(str(ctx.label.raw_target()))

    if toolchain_info.rust_target_path != None:
        path_env["RUST_TARGET_PATH"] = toolchain_info.rust_target_path[DefaultInfo].default_outputs[0]

    # `--show-coverage` is unstable.
    plain_env["RUSTC_BOOTSTRAP"] = cmd_args("1")
    unstable_options = ["-Zunstable-options"]

    rustdoc_cmd_action = cmd_args(
        [cmd_args("--env=", k, "=", v, delimiter = "") for k, v in plain_env.items()],
        [cmd_args("--path-env=", k, "=", v, delimiter = "") for k, v in path_env.items()],
        toolchain_info.rustdoc,
        "--rustc-action-separator",
        toolchain_info.rustdoc_flags,
        ctx.attrs.rustdoc_flags,
        common_args.args,
        unstable_options,
        "--show-coverage",
    )

    rustdoc_cmd = _long_command(
        ctx = ctx,
        exe = compile_ctx.internal_tools_info.rustc_action,
        args = rustdoc_cmd_action,
        argfile_name = "{}.args".format(file),
    )

    cmd = cmd_args([compile_ctx.internal_tools_info.rustdoc_coverage, output.as_output(), rustdoc_cmd])

    ctx.actions.run(cmd, category = "rustdoc_coverage")

    return output

def generate_rustdoc_test(
        ctx: AnalysisContext,
        compile_ctx: CompileContext,
        rlib: Artifact,
        link_infos: dict[LibOutputStyle, LinkInfos],
        params: BuildParams,
        default_roots: list[str]) -> cmd_args:
    toolchain_info = compile_ctx.toolchain_info
    internal_tools_info = compile_ctx.internal_tools_info
    doc_dep_ctx = DepCollectionContext(
        advanced_unstable_linking = compile_ctx.dep_ctx.advanced_unstable_linking,
        include_doc_deps = True,
        is_proc_macro = False,
        explicit_sysroot_deps = compile_ctx.dep_ctx.explicit_sysroot_deps,
        panic_runtime = compile_ctx.dep_ctx.panic_runtime,
    )

    resources = create_resource_db(
        ctx = ctx,
        name = "doctest/resources.json",
        binary = rlib,
        resources = flatten_dict(gather_resources(
            label = ctx.label,
            resources = rust_attr_resources(ctx),
            deps = cxx_attr_deps(ctx),
        ).values()),
    )

    # Gather and setup symlink tree of transitive shared library deps.
    if params.dep_link_strategy == LinkStrategy("shared"):
        shlib_info = merge_shared_libraries(
            ctx.actions,
            deps = inherited_shared_libs(ctx, doc_dep_ctx),
        )
        executable_args = executable_shared_lib_arguments_from_shared_library_info(
            ctx,
            compile_ctx.cxx_toolchain_info,
            compile_ctx.internal_tools_info,
            resources,
            shlib_info,
        )
    else:
        executable_args = executable_shared_lib_arguments(
            ctx,
            compile_ctx.cxx_toolchain_info,
            resources,
            shared_libs = [],
        )

    common_args = _compute_common_args(
        ctx = ctx,
        compile_ctx = compile_ctx,
        dep_ctx = doc_dep_ctx,
        emit = Emit("link"),
        params = params,
        default_roots = default_roots,
        infallible_diagnostics = False,
        is_rustdoc_test = True,
        incremental_enabled = False,
        profile_mode = None,
    )

    link_args_output = make_link_args(
        ctx,
        ctx.actions,
        compile_ctx.cxx_toolchain_info,
        [
            LinkArgs(flags = executable_args.extra_link_args),
            get_link_args_for_strategy(
                ctx,
                # Since we pass the rlib in and treat it as a dependency to the rustdoc test harness,
                # we need to ensure that the rlib's link info is added to the linker, otherwise we may
                # end up with missing symbols that are defined within the crate.
                [create_merged_link_info(
                    ctx,
                    compile_ctx.cxx_toolchain_info.pic_behavior,
                    link_infos,
                    deps = inherited_merged_link_infos(ctx, doc_dep_ctx),
                    preferred_linkage = Linkage("static"),
                )] + inherited_merged_link_infos(ctx, doc_dep_ctx),
                params.dep_link_strategy,
                prefer_stripped = False,
                transformation_spec_context = None,
            ),
        ],
    )

    link_args_output.link_args.add(ctx.attrs.doc_linker_flags or [])

    linker_argsfile, _ = ctx.actions.write(
        "{}/__{}_linker_args.txt".format(common_args.subdir, common_args.tempfile),
        link_args_output.link_args,
        allow_args = True,
    )

    if compile_ctx.exec_is_windows:
        runtool = ["--test-runtool=cmd.exe", "--test-runtool-arg=/V:OFF", "--test-runtool-arg=/C"]
    else:
        runtool = ["--test-runtool=/usr/bin/env"]

    plain_env, path_env = process_env(compile_ctx, ctx.attrs.env)
    doc_plain_env, doc_path_env = process_env(compile_ctx, ctx.attrs.doc_env)
    for k, v in doc_plain_env.items():
        path_env.pop(k, None)
        plain_env[k] = v
    for k, v in doc_path_env.items():
        plain_env.pop(k, None)
        path_env[k] = v

    # `--runtool` is unstable.
    plain_env["RUSTC_BOOTSTRAP"] = cmd_args("1")
    unstable_options = ["-Zunstable-options"]

    if toolchain_info.rust_target_path != None:
        path_env["RUST_TARGET_PATH"] = toolchain_info.rust_target_path[DefaultInfo].default_outputs[0]

    rustdoc_cmd = cmd_args(
        [cmd_args("--env=", k, "=", v, delimiter = "") for k, v in plain_env.items()],
        [cmd_args("--path-env=", k, "=", v, delimiter = "") for k, v in path_env.items()],
        toolchain_info.rustdoc,
        "--rustc-action-separator",
        "--test",
        unstable_options,
        cmd_args("--test-builder=", toolchain_info.compiler, delimiter = ""),
        toolchain_info.rustdoc_flags,
        ctx.attrs.rustdoc_flags,
        common_args.args,
        extern_arg([], attr_crate(ctx), rlib),
        "--extern=proc_macro" if ctx.attrs.proc_macro else [],
        cmd_args(compile_ctx.linker_args, format = "-Clinker={}"),
        cmd_args(linker_argsfile, format = "-Clink-arg=@{}"),
        runtool,
        cmd_args(internal_tools_info.rustdoc_test_with_resources, format = "--test-runtool-arg={}"),
        cmd_args("--test-runtool-arg=--resources=", resources, delimiter = ""),
        "--color=always",
        "--test-args=--color=always",
        cmd_args("--remap-path-prefix=", compile_ctx.symlinked_srcs, compile_ctx.path_sep, "=", compile_ctx.symlinked_srcs.owner.path, compile_ctx.path_sep, delimiter = ""),
        hidden = [
            compile_ctx.symlinked_srcs,
            link_args_output.hidden,
            executable_args.runtime_files,
        ],
    )

    return _long_command(
        ctx = ctx,
        exe = internal_tools_info.rustc_action,
        args = rustdoc_cmd,
        argfile_name = "{}.args".format(common_args.subdir),
    )

# Generate a compilation action. A single instance of rustc can emit
# numerous output artifacts, so return an artifact object for each of
# them.
def rust_compile(
        ctx: AnalysisContext,
        compile_ctx: CompileContext,
        emit: Emit,
        params: BuildParams,
        default_roots: list[str],
        incremental_enabled: bool,
        extra_link_args: list[typing.Any] = [],
        predeclared_output: Artifact | None = None,
        extra_flags: list[str | ResolvedStringWithMacros | Artifact] = [],
        allow_cache_upload: bool = False,
        # Setting this to true causes the diagnostic outputs that are generated
        # from this action to always be successfully generated, even if
        # compilation fails. This should not generally be used if the "real"
        # output of the action is going to be depended on
        infallible_diagnostics: bool = False,
        rust_cxx_link_group_info: RustCxxLinkGroupInfo | None = None,
        transformation_spec_context: TransformationSpecContext | None = None,
        profile_mode: ProfileMode | None = None) -> RustcOutput:
    toolchain_info = compile_ctx.toolchain_info

    lints = _lint_flags(compile_ctx, infallible_diagnostics, emit == Emit("clippy"))

    # If we are building metadata-full for a dylib target, we want the hollow-rlib version of rmeta, not the shared lib version.
    if compile_ctx.dep_ctx.advanced_unstable_linking and emit == Emit("metadata-full") and params.crate_type == CrateType("dylib"):
        params = BuildParams(
            crate_type = CrateType("rlib"),
            reloc_model = params.reloc_model,
            dep_link_strategy = params.dep_link_strategy,
            prefix = "lib",
            suffix = ".rlib",
        )

    common_args = _compute_common_args(
        ctx = ctx,
        compile_ctx = compile_ctx,
        dep_ctx = compile_ctx.dep_ctx,
        emit = emit,
        params = params,
        default_roots = default_roots,
        infallible_diagnostics = infallible_diagnostics,
        incremental_enabled = incremental_enabled,
        is_rustdoc_test = False,
        profile_mode = profile_mode,
    )

    deferred_link_cmd = None

    # TODO(pickett): We can expand this to support all linked crate types (cdylib + binary)
    # We can also share logic here for producing linked artifacts with cxx_library (instead of using)
    # deferred_link_action
    if _deferred_link_enabled(compile_ctx, params, emit):
        out_argsfile = ctx.actions.declare_output(common_args.subdir + "/extracted-link-args.args")
        out_artifacts_dir = ctx.actions.declare_output(common_args.subdir + "/extracted-link-artifacts", dir = True)
        linker_cmd = cmd_args(
            compile_ctx.internal_tools_info.extract_link_action,
            cmd_args(out_argsfile.as_output(), format = "--out_argsfile={}"),
            cmd_args(out_artifacts_dir.as_output(), format = "--out_artifacts={}"),
            compile_ctx.linker_args,
        )

        linker_args = cmd_script(
            actions = ctx.actions,
            name = common_args.subdir + "/linker_wrapper",
            cmd = linker_cmd,
            language = ctx.attrs._exec_os_type[OsLookup].script,
        )

        deferred_link_cmd = cmd_args(
            compile_ctx.internal_tools_info.deferred_link_action,
            compile_ctx.linker_args,
            cmd_args(out_argsfile, format = "@{}"),
            hidden = out_artifacts_dir,
        )
    else:
        linker_args = compile_ctx.linker_args

    rustc_cmd = cmd_args(
        # Lints go first to allow other args to override them.
        lints,
        # Report unused --extern crates in the notification stream.
        ["--json=unused-externs-silent", "-Wunused-crate-dependencies"] if toolchain_info.report_unused_deps else [],
        common_args.args,
        cmd_args("--remap-path-prefix=", compile_ctx.symlinked_srcs, compile_ctx.path_sep, "=", compile_ctx.symlinked_srcs.owner.path, compile_ctx.path_sep, delimiter = ""),
        ["-Zremap-cwd-prefix=."] if toolchain_info.nightly_features else [],
        cmd_args(linker_args, format = "-Clinker={}"),
        extra_flags,
    )

    rustc_bin = compile_ctx.clippy_wrapper if emit == Emit("clippy") else toolchain_info.compiler

    # If we're using failure filtering then we need to make sure the final
    # artifact location is the predeclared one since its specific path may have
    # already been encoded into the other compile args (eg rpath). So we still
    # let rustc_emit generate its own output artifacts, and then make sure we
    # use the predeclared one as the output after the failure filter action
    # below. Otherwise we'll use the predeclared outputs directly.
    if infallible_diagnostics:
        emit_op = _rustc_emit(
            ctx = ctx,
            compile_ctx = compile_ctx,
            emit = emit,
            subdir = common_args.subdir,
            params = params,
            incremental_enabled = incremental_enabled,
            profile_mode = profile_mode,
        )
    else:
        emit_op = _rustc_emit(
            ctx = ctx,
            compile_ctx = compile_ctx,
            emit = emit,
            subdir = common_args.subdir,
            params = params,
            predeclared_output = predeclared_output,
            incremental_enabled = incremental_enabled,
            deferred_link = deferred_link_cmd != None,
            profile_mode = profile_mode,
        )

    if emit == Emit("clippy"):
        clippy_toml = None
        if ctx.attrs.clippy_configuration:
            clippy_toml = ctx.attrs.clippy_configuration[ClippyConfiguration].clippy_toml
        elif toolchain_info.clippy_toml:
            clippy_toml = toolchain_info.clippy_toml

        if clippy_toml:
            # Clippy wants to be given a path to a directory containing a
            # clippy.toml (or .clippy.toml). Our buckconfig accepts an arbitrary
            # label like //path/to:my-clippy.toml which may not have the
            # filename that clippy looks for. Here we make a directory that
            # symlinks the requested configuration file under the required name.
            clippy_conf_dir = ctx.actions.symlinked_dir(
                common_args.subdir + "-clippy-configuration",
                {"clippy.toml": clippy_toml},
            )
            emit_op.env["CLIPPY_CONF_DIR"] = clippy_conf_dir

    split_debug_mode = compile_ctx.cxx_toolchain_info.split_debug_mode or SplitDebugMode("none")
    link_with_split_debug = emit == Emit("link") and split_debug_mode != SplitDebugMode("none")
    if link_with_split_debug:
        dep_external_debug_infos = inherited_dep_external_debug_infos(
            ctx = ctx,
            dep_ctx = compile_ctx.dep_ctx,
            dep_link_strategy = params.dep_link_strategy,
        )
    else:
        dep_external_debug_infos = []

    import_library = None
    pdb_artifact = None
    dwp_inputs = []
    if crate_type_linked(params.crate_type) and common_args.emit_requires_linking:
        subdir = common_args.subdir
        tempfile = common_args.tempfile

        # If this crate type has an associated native dep link style, include deps
        # of that style.

        if rust_cxx_link_group_info:
            filtered_links = rust_cxx_link_group_info.filtered_links

            # Unfortunately, link_groups does not use MergedLinkInfo to represent the args
            # for the resolved nodes in the graph.
            # Thus, we have no choice but to traverse all the nodes to dedupe the framework linker args.
            additional_links = apple_get_link_info_by_deduping_link_infos(
                ctx,
                infos = filtered_links,
                framework_linkable = None,
                swiftmodule_linkable = None,
            )
            if additional_links:
                filtered_links.append(additional_links)

            inherited_link_args = LinkArgs(
                infos = filtered_links + [rust_cxx_link_group_info.symbol_files_info],
            )
        else:
            inherited_link_args = apple_build_link_args_with_deduped_flags(
                ctx,
                deps_merged_link_infos = inherited_merged_link_infos(
                    ctx,
                    compile_ctx.dep_ctx,
                ),
                frameworks_linkable = None,
                link_strategy = params.dep_link_strategy,
                swiftmodule_linkable = None,
                prefer_stripped = False,
                transformation_spec_context = transformation_spec_context,
            )

        if params.crate_type in (CrateType("cdylib"), CrateType("dylib")):
            (import_library, import_library_args) = get_import_library(
                ctx = ctx,
                linker_type = compile_ctx.cxx_toolchain_info.linker_info.type,
                output_short_path = emit_op.output.short_path,
            )
        else:
            import_library_args = []

        link_args_output = make_link_args(
            ctx,
            ctx.actions,
            compile_ctx.cxx_toolchain_info,
            [
                LinkArgs(flags = extra_link_args),
                inherited_link_args,
                LinkArgs(flags = import_library_args),
            ],
            output_short_path = emit_op.output.short_path,
        )

        # Pass to the link wrapper the paths to the .dwo/.o files to rewrite, if we are
        # using split debug with content-based paths.
        if (
            dep_external_debug_infos and
            compile_ctx.cxx_toolchain_info.cxx_compiler_info.supports_content_based_paths and
            # Darwin does not embed paths in object files themselves, but rather
            # the linker writes those paths based on the location of object files passed
            # to the link.
            compile_ctx.cxx_toolchain_info.linker_info.type != LinkerType("darwin")
        ):
            separate_debug_info_path_file, _ = ctx.actions.write(
                "{}/__{}_dwo_paths.txt".format(subdir, tempfile),
                project_artifacts(ctx.actions, dep_external_debug_infos),
                allow_args = True,
            )
            separate_debug_info_args = cmd_args(
                "--rewrite-content-based-dwo-paths",
                separate_debug_info_path_file,
                "--content-based-dwo-suffix",
                ".dwo" if split_debug_mode == SplitDebugMode("split") else ".o",
            )
        else:
            separate_debug_info_path_file = None
            separate_debug_info_args = []

        linker_argsfile, _ = ctx.actions.write(
            "{}/__{}_linker_args.txt".format(subdir, tempfile),
            cmd_args(link_args_output.link_args, separate_debug_info_args),
            allow_args = True,
        )
        linker_hidden = link_args_output.hidden
        if separate_debug_info_path_file:
            linker_hidden.append(separate_debug_info_path_file)

        pdb_artifact = link_args_output.pdb_artifact
        dwp_inputs = [link_args_output.link_args]

        # If we are deferring the real link to a separate action, we no longer pass the linker
        # argsfile to rustc. This allows the rustc action to complete with only transitive dep rmeta.
        if deferred_link_cmd != None:
            deferred_link_cmd.add(cmd_args(linker_argsfile, format = "@{}"))
            deferred_link_cmd.add(cmd_args(hidden = linker_hidden))

            if toolchain_info.sysroot_path:
                deferred_link_cmd.add(cmd_args(hidden = toolchain_info.sysroot_path))

            # The -o flag passed to the linker by rustc is a temporary file. So we will strip it
            # out in `extract_link_action.py` and provide our own output path here.
            deferred_link_cmd.add(get_output_flags(compile_ctx.cxx_toolchain_info.linker_info.type, emit_op.output))
        else:
            rustc_cmd.add(cmd_args(linker_argsfile, format = "-Clink-arg=@{}"))
            rustc_cmd.add(cmd_args(hidden = linker_hidden))

    if toolchain_info.rust_target_path != None:
        emit_op.env["RUST_TARGET_PATH"] = toolchain_info.rust_target_path[DefaultInfo].default_outputs[0]

    invoke = _rustc_invoke(
        ctx = ctx,
        compile_ctx = compile_ctx,
        common_args = common_args,
        prefix = "{}/{}".format(common_args.subdir, common_args.tempfile),
        rustc_cmd = cmd_args(
            rustc_bin,
            "--rustc-action-separator",
            rustc_cmd,
            emit_op.args,
        ),
        required_outputs = [emit_op.output],
        is_clippy = emit.value == "clippy",
        infallible_diagnostics = infallible_diagnostics,
        allow_cache_upload = allow_cache_upload and emit != Emit("clippy"),
        crate_map = common_args.crate_map,
        env = emit_op.env,
        incremental_enabled = incremental_enabled,
        deferred_link_cmd = deferred_link_cmd,
        profile_mode = profile_mode,
    )

    if infallible_diagnostics and emit != Emit("clippy"):
        # This is only needed when this action's output is being used as an
        # input, so we only need standard diagnostics (clippy is always
        # asked for explicitly).
        filtered_output = failure_filter(
            ctx = ctx,
            compile_ctx = compile_ctx,
            predeclared_output = predeclared_output,
            build_status = invoke.build_status,
            required = emit_op.output,
            stderr = invoke.diag_txt,
            identifier = invoke.identifier,
        )
    else:
        filtered_output = emit_op.output

    singleton_tset = ctx.actions.tset(
        TransitiveDeps,
        value = RustArtifact(
            artifact = filtered_output,
            crate = attr_crate(ctx),
        ),
    )

    if link_with_split_debug:
        dwo_output_directory = emit_op.extra_out

        # staticlibs and cdylibs are "bundled" in the sense that they are used
        # without their dependencies by the rest of the rules. This is normally
        # correct, except that the split debuginfo rustc emits for these crate
        # types is not bundled. This is arguably inconsistent behavior from
        # rustc, but in any case, it means we need to do this bundling manually
        # by collecting all the external debuginfo from dependencies
        if params.crate_type == CrateType("cdylib") or params.crate_type == CrateType("staticlib"):
            extra_external_debug_info = inherited_rust_external_debug_info(
                ctx = ctx,
                dep_ctx = compile_ctx.dep_ctx,
                link_strategy = params.dep_link_strategy,
            )
        else:
            extra_external_debug_info = []
        all_external_debug_info = inherited_external_debug_info_from_dep_infos(
            ctx = ctx,
            dwo_output_directory = dwo_output_directory,
            dep_infos = dep_external_debug_infos,
        )
        dwp_inputs.extend(project_artifacts(ctx.actions, all_external_debug_info))
    else:
        dwo_output_directory = None
        extra_external_debug_info = []

    if emit == Emit("link") and \
       dwp_available(compile_ctx.cxx_toolchain_info):
        dwp_output = dwp(
            ctx,
            compile_ctx.cxx_toolchain_info,
            emit_op.output,
            identifier = "{}/__{}_{}_dwp".format(common_args.subdir, common_args.tempfile, str(emit)),
            category_suffix = "rust",
            # TODO(T110378142): Ideally, referenced objects are a list of
            # artifacts, but currently we don't track them properly.  So, we
            # just pass in the full link line and extract all inputs from that,
            # which is a bit of an overspecification.
            referenced_objects = dwp_inputs,
        )
    else:
        dwp_output = None

    stripped_output = strip_debug_info(
        ctx.actions,
        paths.join(common_args.subdir, "stripped", output_filename(
            compile_ctx,
            attr_simple_crate_for_filenames(ctx),
            Emit("link"),
            params,
        )),
        filtered_output,
        compile_ctx.cxx_toolchain_info,
        has_content_based_path = cxx_attr_use_content_based_paths(ctx),
    )

    # When profile_mode is remarks, the remarks are included in the diagnostic stream
    # (same as diag_txt/diag_json), not a separate artifact
    remarks_txt = invoke.diag_txt if profile_mode == ProfileMode("remarks") else None
    remarks_json = invoke.diag_json if profile_mode == ProfileMode("remarks") else None

    return RustcOutput(
        output = filtered_output,
        singleton_tset = singleton_tset,
        stripped_output = stripped_output,
        diag_txt = invoke.diag_txt,
        diag_json = invoke.diag_json,
        import_library = import_library,
        pdb = pdb_artifact,
        dwp_output = dwp_output,
        dwo_output_directory = dwo_output_directory,
        extra_external_debug_info = extra_external_debug_info,
        profile_output = emit_op.profile_out,
        remarks_txt = remarks_txt,
        remarks_json = remarks_json,
    )

# --extern <crate>=<path> for direct dependencies
# -Ldependency=<dir> for transitive dependencies
# For native dependencies, we use -Clink-arg=@argsfile
#
# Second element of returned tuple is an @argsfile containing the -Ldependency=<dir> for transitive dependencies.
# It is separate from the first element because some commands (e.g., rustc) do NOT support nested @argsfiles.
#
# Third element of returned tuple is a mapping from crate names back to target
# label, needed for applying autofixes for rustc's unused_crate_dependencies
# lint by tracing Rust crate names in the compiler diagnostic back to which
# dependency entry in the BUCK file needs to be removed.
#
# The `compile_ctx` may be omitted if there are no dependencies with dynamic
# crate names.
#
# cwd: Optional directory the @argsfiles contents will be relative to (e.g., relative -Ldependency paths for rustc).
def dependency_args(
        ctx: AnalysisContext,
        internal_tools_info: RustInternalToolsInfo,
        transitive_dependency_dirs: set[Artifact],
        toolchain_info: RustToolchainInfo,
        deps: list[RustDependency],
        subdir: str,
        dep_link_strategy: LinkStrategy,
        dep_metadata_kind: MetadataKind,
        is_rustdoc_test: bool,
        cwd: Artifact | None = None) -> (cmd_args, cmd_args, list[(CrateName, Label)]):
    args = cmd_args()
    transitive_deps = []
    crate_targets = []
    available_proc_macros = get_available_proc_macros(ctx)
    for dep in deps:
        if dep.name:
            crate = CrateName(
                simple = normalize_crate(dep.name),
                dynamic = None,
            )
        else:
            crate = dep.info.crate

        strategy = strategy_info(toolchain_info, dep.info, dep_link_strategy)

        artifact = strategy.outputs[dep_metadata_kind]
        singleton_tset = strategy.singleton_tset[dep_metadata_kind]
        transitive_artifacts = strategy.transitive_deps[dep_metadata_kind]

        for marker in strategy.transitive_proc_macro_deps:
            info = available_proc_macros[marker.label][RustLinkInfo]
            strategy = strategy_info(toolchain_info, info, dep_link_strategy)
            transitive_deps.append(strategy.singleton_tset[MetadataKind("link")])

        args.add(extern_arg(dep.flags, crate, artifact))
        crate_targets.append((crate, dep.label))

        # Because deps of this *target* can also be transitive deps of this
        # compiler invocation, pass the artifact (under its original crate name)
        # through `-L` unconditionally for doc tests.
        if is_rustdoc_test:
            transitive_deps.append(singleton_tset)

        # Unwanted transitive_deps have already been excluded
        transitive_deps.append(transitive_artifacts)

    prefix = "{}-deps{}".format(subdir, dep_metadata_kind.value)
    transitive_deps = ctx.actions.tset(TransitiveDeps, children = transitive_deps)
    argsfile = symlinked_dirs(ctx, internal_tools_info, transitive_dependency_dirs, prefix, transitive_deps, cwd)

    return (args, argsfile, crate_targets)

def symlinked_dirs(
        ctx: AnalysisContext,
        internal_tools_info: RustInternalToolsInfo,
        transitive_dependency_dirs: set,
        prefix: str,
        transitive_deps: TransitiveDeps,
        cwd: Artifact | None) -> cmd_args:
    name = "{}-symlinked_dirs".format(prefix)

    transitive_dependency_dir = ctx.actions.declare_output(name, dir = True)

    artifacts = transitive_deps.project_as_json("artifacts")

    # Pass the list of rlibs to transitive_dependency_symlinks.py through a file
    # because there can be a lot of them. This avoids running out of command
    # line length, particularly on Windows.
    artifacts_json = ctx.actions.write_json(
        ctx.actions.declare_output("{}-symlinked_dirs.json".format(prefix)),
        artifacts,
        pretty = True,
    )

    arguments = [
        internal_tools_info.transitive_dependency_symlinks_tool,
        cmd_args(ctx.label.name, format = "--name={}"),
        cmd_args(transitive_dependency_dir.as_output(), format = "--out-dir={}"),
        cmd_args(
            artifacts_json,
            format = "--artifacts={}",
            # Don't take a dependency on all the artifacts in here, just the dynamic names; the
            # rmetas/rlibs we only want to create symlinks to, so there's no need for them to
            # actually be available
            hidden = transitive_deps.project_as_args("dynamic_name_args"),
        ),
    ]

    if cwd:
        arguments.append(cmd_args(
            transitive_dependency_dir.as_output(),
            format = "--out-dir-relative-to-cwd={}",
            relative_to = cwd,
        ))

    ctx.actions.run(
        arguments,
        category = "deps",
        identifier = str(len(transitive_dependency_dirs)),
    )

    transitive_dependency_dirs.add(transitive_dependency_dir)

    return cmd_args(
        # Reference the directory Artifact (not the dirs file), so all of its childern are included.
        transitive_dependency_dir,
        format = "@{}/dirs",
        hidden = transitive_deps.project_as_args("artifacts_args"),
    )

def _lintify(flag: str, clippy: bool, lints: list[ResolvedStringWithMacros]) -> cmd_args:
    return cmd_args(
        [lint for lint in lints if clippy or not str(lint).startswith("\"clippy::")],
        format = "-{}{{}}".format(flag),
    )

def _lint_flags(compile_ctx: CompileContext, infallible_diagnostics: bool, is_clippy: bool) -> cmd_args:
    toolchain_info = compile_ctx.toolchain_info

    return cmd_args(
        _lintify("A", is_clippy, toolchain_info.allow_lints),
        _lintify("D", is_clippy, toolchain_info.deny_lints),
        _lintify("D" if infallible_diagnostics else "W", is_clippy, toolchain_info.deny_on_check_lints),
        _lintify("W", is_clippy, toolchain_info.warn_lints),
    )

def _rustc_flags(flags: list[str | ResolvedStringWithMacros | Artifact]) -> list[str | ResolvedStringWithMacros | Artifact]:
    # Rustc's "-g" flag is documented as being exactly equivalent to
    # "-Cdebuginfo=2". Rustdoc supports the latter, it just doesn't have the
    # "-g" shorthand for it.
    for i, flag in enumerate(flags):
        if str(flag) == '"-g"':
            flags[i] = "-Cdebuginfo=2"

    return flags

# Differently parameterized build outputs need to be assigned nonoverlapping
# output paths. For example the pic and non-pic rlib cannot both be written to
# libfoo.rlib. We place artifacts into a unique subdirectory for each
# permutation of build parameters.
#
# Keep this short or it exacerbates filepath length limits on Windows.
#
# Common examples:
#     rlib pic static_pic metadata-fast diag => "LPPMD"
#     bin pic shared link => "XPHL"
def _abbreviated_subdir(
        crate_type: CrateType,
        reloc_model: RelocModel,
        dep_link_strategy: LinkStrategy,
        emit: Emit,
        is_rustdoc_test: bool,
        infallible_diagnostics: bool,
        incremental_enabled: bool,
        profile_mode: ProfileMode | None) -> str:
    crate_type = {
        CrateType("bin"): "X",  # mnemonic: "eXecutable"
        CrateType("rlib"): "L",  # "Library"
        CrateType("dylib"): "D",
        CrateType("proc-macro"): "M",  # "Macro"
        CrateType("cdylib"): "C",
        CrateType("staticlib"): "S",
    }[crate_type]

    reloc_model = {
        RelocModel("static"): "S",
        RelocModel("pic"): "P",
        RelocModel("pie"): "I",
        RelocModel("dynamic-no-pic"): "N",
        RelocModel("ropi"): "O",
        RelocModel("rwpi"): "W",
        RelocModel("ropi-rwpi"): "R",
        RelocModel("default"): "D",
    }[reloc_model]

    dep_link_strategy = {
        LinkStrategy("static"): "T",
        LinkStrategy("static_pic"): "P",
        LinkStrategy("shared"): "H",
    }[dep_link_strategy]

    emit = {
        Emit("asm"): "s",
        Emit("llvm-bc"): "b",
        Emit("llvm-ir"): "i",
        Emit("llvm-ir-noopt"): "n",
        Emit("obj"): "o",
        Emit("link"): "L",
        Emit("dep-info"): "d",
        Emit("mir"): "m",
        Emit("expand"): "e",
        Emit("clippy"): "c",
        Emit("metadata-full"): "F",  # "Full metadata"
        Emit("metadata-fast"): "M",  # "Metadata"
    }[emit]

    profile_mode = {
        None: "",
        ProfileMode("llvm-time-trace"): "L",
        ProfileMode("self-profile"): "P",
        ProfileMode("remarks"): "R",
    }[profile_mode]

    return crate_type + reloc_model + dep_link_strategy + emit + \
           ("T" if is_rustdoc_test else "") + \
           ("D" if infallible_diagnostics else "") + \
           ("I" if incremental_enabled else "") + \
           profile_mode

# Compute which are common to both rustc and rustdoc
def _compute_common_args(
        ctx: AnalysisContext,
        compile_ctx: CompileContext,
        dep_ctx: DepCollectionContext,
        emit: Emit,
        params: BuildParams,
        default_roots: list[str],
        infallible_diagnostics: bool,
        incremental_enabled: bool,
        is_rustdoc_test: bool,
        profile_mode: ProfileMode | None) -> CommonArgsInfo:
    crate_type = params.crate_type

    args_key = (crate_type, emit, params.dep_link_strategy, is_rustdoc_test, infallible_diagnostics, incremental_enabled, profile_mode)
    if args_key in compile_ctx.common_args:
        return compile_ctx.common_args[args_key]

    subdir = _abbreviated_subdir(
        crate_type = crate_type,
        reloc_model = params.reloc_model,
        dep_link_strategy = params.dep_link_strategy,
        emit = emit,
        is_rustdoc_test = is_rustdoc_test,
        infallible_diagnostics = infallible_diagnostics,
        incremental_enabled = incremental_enabled,
        profile_mode = profile_mode,
    )

    # Included in tempfiles
    tempfile = "{}-{}".format(attr_simple_crate_for_filenames(ctx), emit.value)

    root = crate_root(ctx, default_roots)
    if compile_ctx.exec_is_windows:
        root = root.replace("/", "\\")

    # With `advanced_unstable_linking`, we unconditionally pass the metadata
    # artifacts. There are two things that work together to make this possible
    # in the case of binaries:
    #
    #  1. The actual rlibs appear in the link providers, so they'll still be
    #     available for the linker to link in
    #  2. The metadata artifacts aren't rmetas, but rather rlibs that just
    #     don't contain any generated code. Rustc can't distinguish these
    #     from real rlibs, and so doesn't throw an error
    #
    # The benefit of doing this is that there's no requirement that the
    # dependency's generated code be provided to the linker via an rlib. It
    # could be provided by other means, say, a link group
    dep_metadata_kind = dep_metadata_of_emit(emit)

    # FIXME(JakobDegen): This computation is an awfully broad over-approximation
    emit_requires_linking = dep_metadata_kind == MetadataKind("link")
    if compile_ctx.dep_ctx.advanced_unstable_linking or not crate_type_codegen(crate_type):
        if dep_metadata_kind == MetadataKind("link"):
            dep_metadata_kind = MetadataKind("full")

    dep_args, dep_argsfiles, crate_map = dependency_args(
        ctx = ctx,
        internal_tools_info = compile_ctx.internal_tools_info,
        transitive_dependency_dirs = compile_ctx.transitive_dependency_dirs,
        toolchain_info = compile_ctx.toolchain_info,
        deps = resolve_rust_deps(ctx, dep_ctx),
        subdir = subdir,
        dep_link_strategy = params.dep_link_strategy,
        dep_metadata_kind = dep_metadata_kind,
        is_rustdoc_test = is_rustdoc_test,
    )

    # Add dep_argsfiles to dep_args becuase rustc_action supports nested @argfiles
    dep_args.add(dep_argsfiles)

    if crate_type == CrateType("proc-macro"):
        dep_args.add("--extern=proc_macro")

    if crate_type in [CrateType("cdylib"), CrateType("dylib")] and emit_requires_linking:
        linker_info = compile_ctx.cxx_toolchain_info.linker_info
        shlib_name = compile_ctx.soname
        dep_args.add(cmd_args(
            get_shared_library_name_linker_flags(linker_info.type, shlib_name),
            format = "-Clink-arg={}",
        ))

    toolchain_info = compile_ctx.toolchain_info
    edition = ctx.attrs.edition or toolchain_info.default_edition or \
              fail("missing 'edition' attribute, and there is no 'default_edition' set by the toolchain")

    crate = attr_crate(ctx)
    if crate.dynamic:
        crate_name_arg = cmd_args("--crate-name", cmd_args("@", crate.dynamic, delimiter = ""))
    else:
        crate_name_arg = cmd_args("--crate-name=", crate.simple, delimiter = "")

    # The `-Cprefer-dynamic` flag controls rustc's choice of artifacts for
    # transitive dependencies, both for loading metadata and linking them.
    # Direct dependencies are given to rustc one-by-one using `--extern` with a
    # path to a specific artifact, so there is never ambiguity what artifact to
    # use for a direct dependency. But transitive dependencies are passed in
    # bulk via zero or more `-Ldependency` flags, which are directories
    # containing artifacts. Within those directories, information about a
    # specific crate might be available from more than one artifact, such as a
    # dylib and rlib for the same crate.
    #
    # With `-Cprefer-dynamic=no` (the default), when a transitive dependency
    # exists as both rlib and dylib, metadata is loaded from the rlib. If some
    # dependencies are available in dylib but not rlib, the dylib is used for
    # those. With `-Cprefer-dynamic=yes`, when a transitive dependency exists as
    # both rlib and dylib, instead the dylib is used.
    #
    # The ambiguity over whether to use rlib or dylib for a particular
    # transitive dependency only occurs if the rlib and dylib both describe the
    # same crate i.e. contain the same crate hash.
    #
    # Buck-built libraries never produce an rlib and dylib containing the same
    # crate hash, since that only occurs when outputting multiple crate types
    # through a single rustc invocation: `--crate-type=rlib --crate-type=dylib`.
    # In Buck, different crate types are built by different rustc invocations.
    # But Cargo does invoke rustc with multiple crate types when you write
    # `[lib] crate-type = ["rlib", "dylib"]` in Cargo.toml, and in fact the
    # standard libraries built by x.py and distributed by Rustup are built this
    # way.
    if toolchain_info.explicit_sysroot_deps:
        # Standard libraries are being passed explicitly, and Buck-built
        # dependencies never collide on crate hash, so `-Cprefer-dynamic` cannot
        # make a difference.
        prefer_dynamic_flags = []
    elif crate_type == CrateType("dylib") and toolchain_info.advanced_unstable_linking:
        # Use standard library dylibs from the implicit sysroot.
        prefer_dynamic_flags = ["-Cprefer-dynamic=yes"]
    else:
        # Use standard library rlibs from the implicit sysroot.
        prefer_dynamic_flags = ["-Cprefer-dynamic=no"]  # (the default)

    split_debuginfo_flags = {
        # Rustc's default behavior: debug info is put into every rlib and
        # staticlib, then copied into the executables and shared libraries by
        # the linker. This corresponds to `-gno-split-dwarf` in Clang.
        SplitDebugMode("none"): [],

        # Split DWARF: debug info is placed into *.dwo files in the directory
        # specified by `--out-dir`. In Buck, this directory is usually called
        # "extras" that is a sibling of the main output artifact (rlib,
        # staticlib, executable, or shared library).
        #
        # Rustc produces one *.dwo per LLVM codegen unit, meaning potentially
        # multiple per crate. The only debug info included into the main output
        # artifact is the list of the associated *.dwo filenames in which the
        # real debug info is provided.
        #
        # For each binary target, we have a separate step which involves
        # `llvm-dwp` to combine all the *.dwo files from the dependency graph
        # into one *.dwp. This is handled as a separate Buck action from the
        # compiler/linker invocation responsible for linking the executable or
        # shared library artifact.
        #
        # Rust's `-Csplit-debuginfo=unpacked` corresponds to `-gsplit-dwarf=split`
        # in Clang, which behaves as just described.
        #
        # There is a second Clang debug mode, `-gsplit-dwarf=single`, that we
        # also implement using `-Csplit-debuginfo=unpacked` in Rust. In Clang,
        # "single" means include debug info into object files conceptually like
        # `-gno-split-dwarf`, but do _not_ copy it at link time into executables
        # and shared libraries. Similar to "split", there is an `llvm-dwp` step,
        # separate from linking, to combine debug info from the dependency graph
        # into one *.dwp output. Rustc has an option `-Csplit-debuginfo=packed`
        # which works this way, putting split debug info into rlibs for
        # libraries but keeping it separate for binaries. We have not been able
        # to use `-Csplit-debuginfo=packed` because it runs into an error "unit
        # referenced by executable was not found" when dealing with chains of
        # dependencies from Rust -> C++ -> Rust (T147665047).
        SplitDebugMode("single"): ["-Csplit-debuginfo=unpacked"],
        SplitDebugMode("split"): ["-Csplit-debuginfo=unpacked"],
    }[compile_ctx.cxx_toolchain_info.split_debug_mode or SplitDebugMode("none")]

    args = cmd_args(
        cmd_args(compile_ctx.symlinked_srcs, compile_ctx.path_sep, root, delimiter = ""),
        crate_name_arg,
        "--crate-type={}".format(crate_type.value),
        "-Crelocation-model={}".format(params.reloc_model.value),
        "--edition={}".format(edition),
        "-Cmetadata={}".format(_metadata(compile_ctx, ctx.label, is_rustdoc_test)[0]),
        # Make diagnostics json with the option to extract rendered text
        ["--error-format=json", "--json=diagnostic-rendered-ansi"] if not is_rustdoc_test else [],
        prefer_dynamic_flags,
        ["--target={}".format(toolchain_info.rustc_target_triple)] if toolchain_info.rustc_target_triple else [],
        split_debuginfo_flags,
        compile_ctx.sysroot_args,
        ["-Cpanic=abort", "-Zpanic-abort-tests=yes"] if toolchain_info.panic_runtime == PanicRuntime("abort") else [],
        _rustc_flags(toolchain_info.rustc_flags),
        # `rustc_check_flags` is specifically interpreted as flags that are used
        # only on the metadata-fast graph.
        _rustc_flags(toolchain_info.rustc_check_flags) if dep_metadata_kind == MetadataKind("fast") else [],
        _rustc_flags(toolchain_info.rustc_coverage_flags) if ctx.attrs.coverage else [],
        _rustc_flags(ctx.attrs.rustc_flags),
        _rustc_flags(toolchain_info.extra_rustc_flags),
        cmd_args(ctx.attrs.features, format = '--cfg=feature="{}"'),
        dep_args,
    )

    common_args = CommonArgsInfo(
        args = args,
        subdir = subdir,
        tempfile = tempfile,
        crate_type = crate_type,
        params = params,
        emit = emit,
        emit_requires_linking = emit_requires_linking,
        crate_map = crate_map,
    )

    compile_ctx.common_args[args_key] = common_args
    return common_args

# Returns the full label and its hash. The full label is used for `-Cmetadata`
# which provided the primary disambiguator for two otherwise identically named
# crates. The hash is added to the filename to give them a lower likelihood of
# duplicate names, but it doesn't matter if they collide.
def _metadata(
        compile_ctx: CompileContext,
        label: Label,
        is_rustdoc_test: bool) -> (str, str):
    raw_target = str(label.raw_target())
    configuration_hash = compile_ctx.toolchain_info.configuration_hash or label.configured_target().config().hash
    metadata = "{}#{}".format(raw_target, configuration_hash)

    if is_rustdoc_test:
        metadata = "doctest/" + metadata

    int_hash = hash(metadata)
    if int_hash < 0:
        int_hash = -int_hash

    hex_hash = "%x" % int_hash
    hex_hash = "0" * (8 - len(hex_hash)) + hex_hash

    return (metadata, hex_hash)

def crate_root(
        ctx: AnalysisContext,
        default_roots: list[str]) -> str:
    if ctx.attrs.crate_root:
        return ctx.attrs.crate_root

    srcs = [s.short_path for s in ctx.attrs.srcs] + ctx.attrs.mapped_srcs.values()

    candidates = set()
    if getattr(ctx.attrs, "crate_dynamic", None):
        crate_with_suffix = None
    else:
        crate_with_suffix = attr_crate(ctx).simple + ".rs"
    for src in srcs:
        filename = src.split("/")[-1]
        if filename in default_roots or filename == crate_with_suffix:
            candidates.add(src)

    if len(candidates) == 1:
        return candidates.pop()

    fail("Could not infer crate_root." +
         "\nMake sure you have one of {} in your `srcs` attribute.".format(default_roots) +
         "\nOr add 'crate_root = \"src/example.rs\"' to your attributes to disambiguate. candidates={}".format(candidates))

def _explain(
        crate_type: CrateType,
        link_strategy: LinkStrategy,
        emit: Emit,
        infallible_diagnostics: bool,
        profile_mode: ProfileMode | None) -> str:
    base = None
    if emit == Emit("metadata-full"):
        link_strategy_suffix = {
            LinkStrategy("static"): " [static]",
            LinkStrategy("static_pic"): " [pic]",
            LinkStrategy("shared"): " [shared]",
        }[link_strategy]
        base = "metadata" + link_strategy_suffix

    if emit == Emit("metadata-fast"):
        base = "diag" if infallible_diagnostics else "check"

    if emit == Emit("link"):
        link_strategy_suffix = {
            LinkStrategy("static"): "",
            LinkStrategy("static_pic"): " [pic]",
            LinkStrategy("shared"): " [shared]",
        }[link_strategy]
        if crate_type == CrateType("bin"):
            base = "link" + link_strategy_suffix
        if crate_type == CrateType("rlib"):
            base = "rlib" + link_strategy_suffix
        if crate_type == CrateType("dylib"):
            base = "dylib" + link_strategy_suffix
        if crate_type == CrateType("proc-macro"):
            base = "proc-macro"  # always static_pic
        if crate_type == CrateType("cdylib"):
            base = "cdylib" + link_strategy_suffix
        if crate_type == CrateType("staticlib"):
            base = "staticlib" + link_strategy_suffix

    if emit == Emit("expand"):
        base = "expand"

    for emit_type in ["asm", "llvm-ir", "mir"]:
        if emit == Emit(emit_type):
            link_strategy_suffix = {
                LinkStrategy("static"): " [static]",
                LinkStrategy("static_pic"): " [pic]",
                LinkStrategy("shared"): " [shared]",
            }[link_strategy]
            base = emit_type + link_strategy_suffix

    if emit == Emit("llvm-ir-noopt"):
        base = "llvm-ir-noopt"

    if base == None:
        fail("unrecognized rustc action:", crate_type, link_strategy, emit)

    if profile_mode:
        return "{} [{}]".format(base, profile_mode.value)
    else:
        return base

EmitOperation = record(
    output = field(Artifact),
    args = field(cmd_args),
    env = field(dict[str, str]),
    extra_out = field(Artifact | None),
    profile_out = field(Artifact | None),
)

# Take a desired output and work out how to convince rustc to generate it
def _rustc_emit(
        ctx: AnalysisContext,
        compile_ctx: CompileContext,
        emit: Emit,
        subdir: str,
        params: BuildParams,
        incremental_enabled: bool,
        profile_mode: ProfileMode | None,
        predeclared_output: Artifact | None = None,
        deferred_link: bool = False) -> EmitOperation:
    simple_crate = attr_simple_crate_for_filenames(ctx)
    crate_type = params.crate_type

    emit_args = cmd_args()
    emit_env = {}
    extra_out = None
    profile_out = None

    if predeclared_output:
        emit_output = predeclared_output

        # Don't support profiles with predeclared outputs
        crate_name_and_extra_for_profile = None
    else:
        extra_hash = "-" + _metadata(compile_ctx, ctx.label, False)[1]
        emit_args.add("-Cextra-filename={}".format(extra_hash))
        filename = subdir + "/" + output_filename(compile_ctx, simple_crate, emit, params, extra_hash)
        crate_name_and_extra_for_profile = simple_crate + extra_hash

        emit_output = ctx.actions.declare_output(filename)

    if emit == Emit("expand"):
        emit_env["RUSTC_BOOTSTRAP"] = "1"
        emit_args.add(
            "-Zunpretty=expanded",
            cmd_args(emit_output.as_output(), format = "-o{}"),
        )
    else:
        # Even though the unstable flag only appears on one of the branches, we need
        # an identical environment between the `-Zno-codegen` and non-`-Zno-codegen`
        # command or else there are "found possibly newer version of crate" errors.
        emit_env["RUSTC_BOOTSTRAP"] = "1"

        if emit == Emit("metadata-full"):
            if crate_type_codegen(crate_type):
                # We don't ever have metadata-only deps on codegen crates, so we can
                # fall back to the `metadata-fast` behavior. Normally though, this
                # artifact should be unused and so this shouldn't matter.
                effective_emit = "metadata"
            else:
                # As we're doing a pipelined build, instead of emitting an actual rmeta
                # we emit a "hollow" .rlib - ie, it only contains lib.rmeta and no object
                # code. It should contain full information needed by any dependent
                # crate which is generating code (MIR, etc).
                #
                # IMPORTANT: this flag is the only way that the Emit("metadata") and
                # Emit("link") operations are allowed to diverge without causing them to
                # get different crate hashes.
                emit_args.add("-Zno-codegen")
                effective_emit = "link"
        elif emit == Emit("metadata-fast") or emit == Emit("clippy"):
            effective_emit = "metadata"
        elif emit == Emit("llvm-ir-noopt"):
            effective_emit = "llvm-ir"
            emit_args.add("-Cno-prepopulate-passes")
        else:
            effective_emit = emit.value

        # When using deferred link, we still want to pass `--emit` to rustc to trigger
        # the correct compilation behavior, but we do not want to pass emit_output here.
        # Instead, we will bind the emit output to the actual deferred link action.
        if deferred_link and effective_emit == "link":
            emit_args.add(cmd_args("--emit=", effective_emit, delimiter = ""))
        else:
            emit_args.add(cmd_args("--emit=", effective_emit, "=", emit_output.as_output(), delimiter = ""))

        # Strip file extension from directory name.
        base, _ext = paths.split_extension(output_filename(compile_ctx, simple_crate, emit, params))
        extra_dir = subdir + "/extras/" + base
        extra_out = ctx.actions.declare_output(extra_dir, dir = True)
        emit_args.add(cmd_args(extra_out.as_output(), format = "--out-dir={}"))

        if incremental_enabled:
            incremental_out = ctx.actions.declare_output("{}/extras/incremental".format(subdir))
            incremental_cmd = cmd_args(incremental_out.as_output(), format = "-Cincremental={}")
            emit_args.add(incremental_cmd)

        if profile_mode == ProfileMode("llvm-time-trace"):
            emit_args.add("-Zllvm-time-trace=yes")
            profile_out = extra_out.project(crate_name_and_extra_for_profile + ".llvm_timings.json")
        elif profile_mode == ProfileMode("self-profile"):
            self_profile = ctx.actions.declare_output("{}/extra/self-profile".format(subdir), dir = True)
            emit_args.add("-Zself-profile-events=default,args")
            emit_args.add(cmd_args("-Zself-profile=", self_profile.as_output(), delimiter = ""))
            profile_out = self_profile
        elif profile_mode == ProfileMode("remarks"):
            # Enable LLVM remarks - they appear in the diagnostic stream (stderr)
            # Use the configured filter, or default to "all"
            # Allow comma-separated values for convenience (rustc expects space-separated)
            remarks_filter = (compile_ctx.toolchain_info.remarks or "all").replace(",", " ")
            emit_args.add("-Cremark={}".format(remarks_filter))

    return EmitOperation(
        output = emit_output,
        args = emit_args,
        env = emit_env,
        extra_out = extra_out,
        profile_out = profile_out,
    )

Invoke = record(
    diag_txt = field(Artifact),
    diag_json = field(Artifact),
    build_status = field(Artifact | None),
    identifier = field(str | None),
)

# Invoke rustc and capture outputs
def _rustc_invoke(
        ctx: AnalysisContext,
        compile_ctx: CompileContext,
        common_args: CommonArgsInfo,
        prefix: str,
        rustc_cmd: cmd_args,
        required_outputs: list[Artifact],
        is_clippy: bool,
        infallible_diagnostics: bool,
        allow_cache_upload: bool,
        incremental_enabled: bool,
        crate_map: list[(CrateName, Label)],
        env: dict[str, str | ResolvedStringWithMacros | Artifact],
        deferred_link_cmd: cmd_args | None,
        profile_mode: ProfileMode | None) -> Invoke:
    toolchain_info = compile_ctx.toolchain_info

    plain_env, path_env = process_env(compile_ctx, toolchain_info.rustc_env | ctx.attrs.env)

    more_plain_env, more_path_env = process_env(compile_ctx, env)
    plain_env.update(more_plain_env)
    path_env.update(more_path_env)

    # Save diagnostic outputs
    diag = "clippy" if is_clippy else "diag"
    diag_json = ctx.actions.declare_output("{}-{}.json".format(prefix, diag))
    diag_txt = ctx.actions.declare_output("{}-{}.txt".format(prefix, diag))

    compile_cmd = cmd_args(
        cmd_args(diag_json.as_output(), format = "--diag-json={}"),
        cmd_args(diag_txt.as_output(), format = "--diag-txt={}"),
        ["--remap-cwd-prefix=."] if not toolchain_info.nightly_features else [],
        "--buck-target={}".format(ctx.label.raw_target()),
        hidden = [toolchain_info.compiler, compile_ctx.symlinked_srcs],
    )

    for k, v in crate_map:
        compile_cmd.add(crate_map_arg(k, v))
    for k, v in plain_env.items():
        compile_cmd.add(cmd_args("--env=", k, "=", v, delimiter = ""))
    for k, v in path_env.items():
        compile_cmd.add(cmd_args("--path-env=", k, "=", v, delimiter = ""))

    build_status = None
    if infallible_diagnostics:
        # Build status for fail filter
        build_status = ctx.actions.declare_output("{}_build_status-{}.json".format(prefix, diag))
        compile_cmd.add(cmd_args(build_status.as_output(), format = "--failure-filter={}"))
        for out in required_outputs:
            compile_cmd.add("--required-output", out.short_path, out.as_output())

    compile_cmd.add(rustc_cmd)

    compile_cmd = _long_command(
        ctx = ctx,
        exe = compile_ctx.internal_tools_info.rustc_action,
        args = compile_cmd,
        argfile_name = "{}-{}.args".format(prefix, diag),
    )

    local_only = False
    prefer_local = False
    if incremental_enabled:
        local_only = True
    elif common_args.crate_type == CrateType("bin") and \
         common_args.emit == Emit("link") and \
         link_cxx_binary_locally(ctx):
        prefer_local = True

    if is_clippy:
        category = "clippy"
        identifier = ""
    else:
        category = "rustc"
        identifier = _explain(
            crate_type = common_args.crate_type,
            link_strategy = common_args.params.dep_link_strategy,
            emit = common_args.emit,
            infallible_diagnostics = infallible_diagnostics,
            profile_mode = profile_mode,
        )

    if incremental_enabled:
        if not identifier.endswith("]"):
            identifier += " "
        identifier += "[incr]"

    ctx.actions.run(
        compile_cmd,
        local_only = local_only,
        # We only want to prefer_local here if rustc is performing the link
        prefer_local = prefer_local and deferred_link_cmd == None,
        category = category,
        identifier = identifier,
        no_outputs_cleanup = incremental_enabled,
        # We want to unconditionally cache object file compilations when rustc is not linking
        allow_cache_upload = allow_cache_upload or deferred_link_cmd != None,
        error_handler = toolchain_info.rust_error_handler,
    )

    if deferred_link_cmd:
        ctx.actions.run(
            deferred_link_cmd,
            local_only = local_only,
            prefer_local = prefer_local,
            category = "deferred_link",
            identifier = identifier,
            allow_cache_upload = allow_cache_upload,
        )

    return Invoke(
        diag_txt = diag_txt,
        diag_json = diag_json,
        build_status = build_status,
        identifier = identifier,
    )

# Our rustc and rustdoc commands can have arbitrarily large number of `--extern`
# flags, so write to file to avoid hitting the platform's limit on command line
# length. This limit is particularly small on Windows.
def _long_command(
        ctx: AnalysisContext,
        exe: RunInfo,
        args: cmd_args,
        argfile_name: str) -> cmd_args:
    return cmd_args(
        exe,
        at_argfile(
            actions = ctx.actions,
            name = argfile_name,
            args = args,
            allow_args = True,
        ),
    )

_DOUBLE_ESCAPED_NEWLINE_RE = regex("\\\\n")
_ESCAPED_NEWLINE_RE = regex("\\n")
_DIRECTORY_ENV = [
    "CARGO_MANIFEST_DIR",
    "OUT_DIR",
]

# Separate env settings into "plain" and "with path". Path env vars are often
# used in Rust `include!()` and similar directives, which always interpret the
# path relative to the source file containing the directive. Since paths in env
# vars are often expanded from macros such as `$(location)`, they will be
# cell-relative which will not work properly. To solve this, we canonicalize
# paths to absolute paths so they'll work in any context. Hence the need to
# distinguish path from non-path. (This will not work if the value contains both
# path and non-path content, but we'll burn that bridge when we get to it.)
def process_env(
        compile_ctx: CompileContext,
        env: dict[str, str | ResolvedStringWithMacros | Artifact],
        escape_for_rustc_action: bool = True) -> (dict[str, cmd_args], dict[str, cmd_args]):
    # Values with inputs (ie artifact references).
    path_env = {}

    # Plain strings.
    plain_env = {}

    for k, v in env.items():
        arg = cmd_args(v)
        if len(arg.inputs) > 0:
            path_env[k] = arg
        elif escape_for_rustc_action:
            # Environment variables may have newlines, escape them for now.
            # Will be unescaped in rustc_action.
            # Variable may have "\\n" as well.
            # Example: \\n\n -> \\\n\n -> \\\\n\\n
            plain_env[k] = cmd_args(
                v,
                replace_regex = [
                    (_DOUBLE_ESCAPED_NEWLINE_RE, "\\\n"),
                    (_ESCAPED_NEWLINE_RE, "\\n"),
                ],
            )
        else:
            plain_env[k] = arg

    # If CARGO_MANIFEST_DIR is not already expressed in terms of $(location ...)
    # of some target, then interpret it as a relative path inside of the crate's
    # sources.
    #
    # For example in the following case:
    #
    #     http_archive(
    #         name = "foo.crate",
    #         ...
    #     )
    #
    #     rust_library(
    #         name = "foo",
    #         srcs = [":foo.crate"],
    #         crate_root = "foo.crate/src/lib.rs",
    #         env = {
    #             "CARGO_MANIFEST_DIR": "foo.crate",
    #         },
    #     )
    #
    # then the manifest directory refers to the directory which is the parent of
    # `src` inside the archive.
    #
    # By putting the environment variable into path_env, rustc_action.py will
    # take care of turning this into an absolute path before rustc sees it. This
    # matches Cargo which also always provides CARGO_MANIFEST_DIR as an absolute
    # path. A relative path would be problematic because it can't simultaneously
    # support both of the following real-world cases: `include!` which resolves
    # relative paths relative to the file containing the include:
    #
    #     include!(concat!(env!("CARGO_MANIFEST_DIR"), "/src/thing.rs"));
    #
    # and proc macros using std::fs to read thing like .pest grammars, which
    # would need paths relative to the directory that rustc got invoked in
    # (which is the repo root in Buck builds).
    for key in _DIRECTORY_ENV:
        value = plain_env.pop(key, None)
        if value:
            path_env[key] = cmd_args(
                compile_ctx.symlinked_srcs,
                compile_ctx.path_sep,
                value,
                delimiter = "",
            )

    return (plain_env, path_env)

def _deferred_link_enabled(compile_ctx: CompileContext, params: BuildParams, emit: Emit) -> bool:
    return compile_ctx.toolchain_info.advanced_unstable_linking and \
           params.crate_type == CrateType("dylib") and \
           emit == Emit("link") and \
           compile_ctx.cxx_toolchain_info.linker_info.type == LinkerType("gnu")

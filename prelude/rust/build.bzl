# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(
    "@prelude//:artifact_tset.bzl",
    "project_artifacts",
)
load("@prelude//:local_only.bzl", "link_cxx_binary_locally")
load("@prelude//:paths.bzl", "paths")
load("@prelude//:resources.bzl", "create_resource_db", "gather_resources")
load("@prelude//cxx:cxx_context.bzl", "get_cxx_toolchain_info")
load("@prelude//cxx:cxx_library_utility.bzl", "cxx_attr_deps")
load(
    "@prelude//cxx:cxx_link_utility.bzl",
    "executable_shared_lib_arguments",
    "make_link_args",
)
load("@prelude//cxx:cxx_toolchain_types.bzl", "LinkerInfo")
load("@prelude//cxx:debug.bzl", "SplitDebugMode")
load("@prelude//cxx:dwp.bzl", "dwp", "dwp_available")
load(
    "@prelude//cxx:linker.bzl",
    "get_shared_library_name_linker_flags",
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
    "traverse_shared_library_info",
)
load("@prelude//linking:strip.bzl", "strip_debug_info")
load("@prelude//linking:types.bzl", "Linkage")
load("@prelude//os_lookup:defs.bzl", "OsLookup")
load("@prelude//rust/tools:attrs.bzl", "RustInternalToolsInfo")
load("@prelude//utils:argfile.bzl", "at_argfile")
load("@prelude//utils:cmd_script.bzl", "ScriptOs", "cmd_script")
load("@prelude//utils:utils.bzl", "flatten_dict")
load(
    ":build_params.bzl",
    "BuildParams",  # @unused Used as a type
    "CrateType",
    "Emit",
    "MetadataKind",
    "ProfileMode",  # @unused Used as a type
    "crate_type_codegen",
    "crate_type_linked",
    "dep_metadata_of_emit",
    "output_filename",
)
load(":clippy_configuration.bzl", "ClippyConfiguration")
load(
    ":context.bzl",
    "CommonArgsInfo",
    "CompileContext",
    "CrateName",  # @unused Used as a type
    "DepCollectionContext",
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
    "RustCxxLinkGroupInfo",  #@unused Used as a type
    "RustDependency",
    "RustLinkInfo",
    "attr_crate",
    "attr_simple_crate_for_filenames",
    "attr_soname",
    "get_available_proc_macros",
    "inherited_external_debug_info",
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

def compile_context(ctx: AnalysisContext, binary: bool = False) -> CompileContext:
    toolchain_info = ctx.attrs._rust_toolchain[RustToolchainInfo]
    internal_tools_info = ctx.attrs._rust_internal_tools_toolchain[RustInternalToolsInfo]
    cxx_toolchain_info = get_cxx_toolchain_info(ctx)

    # Setup source symlink tree.
    srcs = {src.short_path: src for src in ctx.attrs.srcs}
    srcs.update({k: v for v, k in ctx.attrs.mapped_srcs.items()})

    # Decide whether to use symlinked_dir or copied_dir.
    prefixes = {}
    symlinked_srcs = None

    if "generated" in ctx.attrs.labels:
        # For generated code targets, we always want to copy files in the [sources]
        # subtarget, never symlink.
        #
        # This ensures that IDEs that open the generated file always see the correct
        # directory structure.
        #
        # VS Code will expand symlinks when doing go-to-definition. In normal source
        # files this takes us back to the correct path, but for generated files the
        # expanded path may not be a well-formed crate layout.
        symlinked_srcs = ctx.actions.copied_dir("__srcs", srcs)
    else:
        # If a source is a prefix of any other source, use copied_dir. This supports
        # e.g. `srcs = [":foo.crate"]` where :foo.crate is an http_archive, together
        # with a `mapped_srcs` which overlays additional generated files into that
        # directory. Symlinked_dir would error in this situation.
        for src in sorted(srcs.keys(), key = len, reverse = True):
            if src in prefixes:
                symlinked_srcs = ctx.actions.copied_dir("__srcs", srcs)
                break
            components = src.split("/")
            for i in range(1, len(components)):
                prefixes["/".join(components[:i])] = None

    # Otherwise, symlink it.
    if not symlinked_srcs:
        symlinked_srcs = ctx.actions.symlinked_dir("__srcs", srcs)

    linker = _linker_args(ctx, cxx_toolchain_info.linker_info, binary = binary)
    clippy_wrapper = _clippy_wrapper(ctx, toolchain_info)

    dep_ctx = DepCollectionContext(
        advanced_unstable_linking = toolchain_info.advanced_unstable_linking,
        include_doc_deps = False,
        is_proc_macro = getattr(ctx.attrs, "proc_macro", False),
        explicit_sysroot_deps = toolchain_info.explicit_sysroot_deps,
        panic_runtime = toolchain_info.panic_runtime,
    )

    # When we pass explicit sysroot deps, we need to override the default sysroot to avoid accidentally
    # linking against the prebuilt sysroot libs provided by the toolchain. Rustc requires a specific layout
    # for these libs, so we need to carefully recreate the directory structure below.
    if toolchain_info.explicit_sysroot_deps:
        empty_dir = ctx.actions.copied_dir("empty_dir", {})
        empty_sysroot = ctx.actions.copied_dir("empty_sysroot", {"lib/rustlib/" + toolchain_info.rustc_target_triple + "/lib": empty_dir})

        sysroot_args = cmd_args("--sysroot=", empty_sysroot, delimiter = "")
    elif toolchain_info.sysroot_path:
        sysroot_args = cmd_args("--sysroot=", toolchain_info.sysroot_path, delimiter = "")
    else:
        sysroot_args = cmd_args()

    return CompileContext(
        toolchain_info = toolchain_info,
        internal_tools_info = internal_tools_info,
        cxx_toolchain_info = cxx_toolchain_info,
        dep_ctx = dep_ctx,
        symlinked_srcs = symlinked_srcs,
        linker_args = linker,
        clippy_wrapper = clippy_wrapper,
        common_args = {},
        transitive_dependency_dirs = {},
        sysroot_args = sysroot_args,
    )

def generate_rustdoc(
        ctx: AnalysisContext,
        compile_ctx: CompileContext,
        # link style doesn't matter, but caller should pass in build params
        # with static-pic (to get best cache hits for deps)
        params: BuildParams,
        default_roots: list[str],
        document_private_items: bool) -> Artifact:
    exec_is_windows = ctx.attrs._exec_os_type[OsLookup].platform == "windows"

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

    plain_env, path_env = process_env(compile_ctx, toolchain_info.rustdoc_env | ctx.attrs.env, exec_is_windows)
    plain_env["RUSTDOC_BUCK_TARGET"] = cmd_args(str(ctx.label.raw_target()))

    rustdoc_cmd = cmd_args(
        toolchain_info.rustdoc,
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

    rustdoc_cmd = cmd_args(
        toolchain_info.rustdoc,
        toolchain_info.rustdoc_flags,
        ctx.attrs.rustdoc_flags,
        common_args.args,
        "-Zunstable-options",
        "--show-coverage",
    )

    exec_is_windows = ctx.attrs._exec_os_type[OsLookup].platform == "windows"
    plain_env, path_env = process_env(compile_ctx, ctx.attrs.env, exec_is_windows)
    plain_env["RUSTDOC_BUCK_TARGET"] = cmd_args(str(ctx.label.raw_target()))

    rustdoc_cmd_action = cmd_args(
        [cmd_args("--env=", k, "=", v, delimiter = "") for k, v in plain_env.items()],
        [cmd_args("--path-env=", k, "=", v, delimiter = "") for k, v in path_env.items()],
        rustdoc_cmd,
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
    exec_is_windows = ctx.attrs._exec_os_type[OsLookup].platform == "windows"

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
    shared_libs = []
    if params.dep_link_strategy == LinkStrategy("shared"):
        shlib_info = merge_shared_libraries(
            ctx.actions,
            deps = inherited_shared_libs(ctx, doc_dep_ctx),
        )
        shared_libs.extend(traverse_shared_library_info(shlib_info))
    executable_args = executable_shared_lib_arguments(
        ctx,
        compile_ctx.cxx_toolchain_info,
        resources,
        shared_libs,
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
                    deps = inherited_merged_link_infos(ctx, doc_dep_ctx).values(),
                    preferred_linkage = Linkage("static"),
                )] + inherited_merged_link_infos(ctx, doc_dep_ctx).values(),
                params.dep_link_strategy,
            ),
        ],
    )

    link_args_output.link_args.add(ctx.attrs.doc_linker_flags or [])

    linker_argsfile, _ = ctx.actions.write(
        "{}/__{}_linker_args.txt".format(common_args.subdir, common_args.tempfile),
        link_args_output.link_args,
        allow_args = True,
    )

    if exec_is_windows:
        runtool = ["--runtool=cmd.exe", "--runtool-arg=/V:OFF", "--runtool-arg=/C"]
    else:
        runtool = ["--runtool=/usr/bin/env"]

    plain_env, path_env = process_env(compile_ctx, ctx.attrs.env, exec_is_windows)
    doc_plain_env, doc_path_env = process_env(compile_ctx, ctx.attrs.doc_env, exec_is_windows)
    for k, v in doc_plain_env.items():
        path_env.pop(k, None)
        plain_env[k] = v
    for k, v in doc_path_env.items():
        plain_env.pop(k, None)
        path_env[k] = v

    # `--runtool` is unstable.
    plain_env["RUSTC_BOOTSTRAP"] = cmd_args("1")
    unstable_options = ["-Zunstable-options"]

    path_sep = "\\" if exec_is_windows else "/"
    rustdoc_cmd = cmd_args(
        [cmd_args("--env=", k, "=", v, delimiter = "") for k, v in plain_env.items()],
        [cmd_args("--path-env=", k, "=", v, delimiter = "") for k, v in path_env.items()],
        toolchain_info.rustdoc,
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
        cmd_args(internal_tools_info.rustdoc_test_with_resources, format = "--runtool-arg={}"),
        cmd_args("--runtool-arg=--resources=", resources, delimiter = ""),
        "--color=always",
        "--test-args=--color=always",
        cmd_args("--remap-path-prefix=", compile_ctx.symlinked_srcs, path_sep, "=", ctx.label.path, path_sep, delimiter = ""),
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
        extra_flags: list[[str, ResolvedStringWithMacros]] = [],
        allow_cache_upload: bool = False,
        # Setting this to true causes the diagnostic outputs that are generated
        # from this action to always be successfully generated, even if
        # compilation fails. This should not generally be used if the "real"
        # output of the action is going to be depended on
        infallible_diagnostics: bool = False,
        rust_cxx_link_group_info: [RustCxxLinkGroupInfo, None] = None,
        profile_mode: ProfileMode | None = None) -> RustcOutput:
    exec_is_windows = ctx.attrs._exec_os_type[OsLookup].platform == "windows"

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
    if params.crate_type == CrateType("dylib") and emit == Emit("link") and compile_ctx.dep_ctx.advanced_unstable_linking:
        out_argsfile = ctx.actions.declare_output(common_args.subdir + "/extracted-link-args.args")
        out_version_script = ctx.actions.declare_output(common_args.subdir + "/version-script")
        out_objects_dir = ctx.actions.declare_output(common_args.subdir + "/objects", dir = True)
        linker_cmd = cmd_args(
            compile_ctx.internal_tools_info.extract_link_action,
            cmd_args(out_argsfile.as_output(), format = "--out_argsfile={}"),
            cmd_args(out_version_script.as_output(), format = "--out_version-script={}") if out_version_script else cmd_args(),
            cmd_args(out_objects_dir.as_output(), format = "--out_objects={}"),
            compile_ctx.linker_args,
        )

        linker_args = cmd_script(
            ctx = ctx,
            name = common_args.subdir + "/linker_wrapper",
            cmd = linker_cmd,
            os = ScriptOs("windows" if ctx.attrs._exec_os_type[OsLookup].platform == "windows" else "unix"),
        )

        deferred_link_cmd = cmd_args(
            compile_ctx.internal_tools_info.deferred_link_action,
            cmd_args(out_objects_dir, format = "--objects={}"),
            cmd_args(out_version_script, format = "--version-script={}"),
            compile_ctx.linker_args,
            cmd_args(out_argsfile, format = "@{}"),
        )
    else:
        linker_args = compile_ctx.linker_args

    path_sep = "\\" if exec_is_windows else "/"
    rustc_cmd = cmd_args(
        # Lints go first to allow other args to override them.
        lints,
        # Report unused --extern crates in the notification stream.
        ["--json=unused-externs-silent", "-Wunused-crate-dependencies"] if toolchain_info.report_unused_deps else [],
        common_args.args,
        cmd_args("--remap-path-prefix=", compile_ctx.symlinked_srcs, path_sep, "=", ctx.label.path, path_sep, delimiter = ""),
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
            emit = emit,
            subdir = common_args.subdir,
            params = params,
            incremental_enabled = incremental_enabled,
            profile_mode = profile_mode,
        )
    else:
        emit_op = _rustc_emit(
            ctx = ctx,
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

    pdb_artifact = None
    dwp_inputs = []
    if crate_type_linked(params.crate_type) and common_args.emit_requires_linking:
        subdir = common_args.subdir
        tempfile = common_args.tempfile

        # If this crate type has an associated native dep link style, include deps
        # of that style.

        if rust_cxx_link_group_info:
            inherited_link_args = LinkArgs(
                infos = rust_cxx_link_group_info.filtered_links + [rust_cxx_link_group_info.symbol_files_info],
            )

        else:
            inherited_link_args = get_link_args_for_strategy(
                ctx,
                inherited_merged_link_infos(
                    ctx,
                    compile_ctx.dep_ctx,
                ).values(),
                params.dep_link_strategy,
            )

        link_args_output = make_link_args(
            ctx,
            ctx.actions,
            compile_ctx.cxx_toolchain_info,
            [
                LinkArgs(flags = extra_link_args),
                inherited_link_args,
            ],
            output_short_path = emit_op.output.short_path,
        )
        linker_argsfile, _ = ctx.actions.write(
            "{}/__{}_linker_args.txt".format(subdir, tempfile),
            link_args_output.link_args,
            allow_args = True,
        )

        pdb_artifact = link_args_output.pdb_artifact
        dwp_inputs = [link_args_output.link_args]

        # If we are deferring the real link to a separate action, we no longer pass the linker
        # argsfile to rustc. This allows the rustc action to complete with only transitive dep rmeta.
        if deferred_link_cmd != None:
            deferred_link_cmd.add(cmd_args(linker_argsfile, format = "@{}"))
            deferred_link_cmd.add(cmd_args(hidden = link_args_output.hidden))

            # The -o flag passed to the linker by rustc is a temporary file. So we will strip it
            # out in `extract_link_action.py` and provide our own output path here.
            deferred_link_cmd.add("-o", emit_op.output.as_output())
        else:
            rustc_cmd.add(cmd_args(linker_argsfile, format = "-Clink-arg=@{}"))
            rustc_cmd.add(cmd_args(hidden = link_args_output.hidden))

    if toolchain_info.rust_target_path != None:
        emit_op.env["RUST_TARGET_PATH"] = toolchain_info.rust_target_path[DefaultInfo].default_outputs[0]

    invoke = _rustc_invoke(
        ctx = ctx,
        compile_ctx = compile_ctx,
        common_args = common_args,
        prefix = "{}/{}".format(common_args.subdir, common_args.tempfile),
        rustc_cmd = cmd_args(rustc_bin, rustc_cmd, emit_op.args),
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

    split_debug_mode = compile_ctx.cxx_toolchain_info.split_debug_mode or SplitDebugMode("none")
    if emit == Emit("link") and split_debug_mode != SplitDebugMode("none"):
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
        all_external_debug_info = inherited_external_debug_info(
            ctx = ctx,
            dep_ctx = compile_ctx.dep_ctx,
            dwo_output_directory = dwo_output_directory,
            dep_link_strategy = params.dep_link_strategy,
        )
        dwp_inputs.extend(project_artifacts(ctx.actions, [all_external_debug_info]))
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
        ctx,
        paths.join(common_args.subdir, "stripped", output_filename(
            attr_simple_crate_for_filenames(ctx),
            Emit("link"),
            params,
        )),
        filtered_output,
    )

    return RustcOutput(
        output = filtered_output,
        stripped_output = stripped_output,
        diag_txt = invoke.diag_txt,
        diag_json = invoke.diag_json,
        pdb = pdb_artifact,
        dwp_output = dwp_output,
        dwo_output_directory = dwo_output_directory,
        extra_external_debug_info = extra_external_debug_info,
        profile_output = emit_op.profile_out,
    )

# --extern <crate>=<path> for direct dependencies
# -Ldependency=<dir> for transitive dependencies
# For native dependencies, we use -Clink-arg=@argsfile
#
# Second element of returned tuple is a mapping from crate names back to target
# label, needed for applying autofixes for rustc's unused_crate_dependencies
# lint by tracing Rust crate names in the compiler diagnostic back to which
# dependency entry in the BUCK file needs to be removed.
#
# The `compile_ctx` may be omitted if there are no dependencies with dynamic
# crate names.
def dependency_args(
        ctx: AnalysisContext,
        compile_ctx: CompileContext | None,
        toolchain_info: RustToolchainInfo,
        deps: list[RustDependency],
        subdir: str,
        dep_link_strategy: LinkStrategy,
        dep_metadata_kind: MetadataKind,
        is_rustdoc_test: bool) -> (cmd_args, list[(CrateName, Label)]):
    args = cmd_args()
    transitive_deps = {}
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
        transitive_artifacts = strategy.transitive_deps[dep_metadata_kind]

        for marker in strategy.transitive_proc_macro_deps.keys():
            info = available_proc_macros[marker.label][RustLinkInfo]
            strategy = strategy_info(toolchain_info, info, dep_link_strategy)
            transitive_deps[strategy.outputs[MetadataKind("link")]] = info.crate

        args.add(extern_arg(dep.flags, crate, artifact))
        crate_targets.append((crate, dep.label))

        # Because deps of this *target* can also be transitive deps of this
        # compiler invocation, pass the artifact (under its original crate name)
        # through `-L` unconditionally for doc tests.
        if is_rustdoc_test:
            transitive_deps[artifact] = dep.info.crate

        # Unwanted transitive_deps have already been excluded
        transitive_deps.update(transitive_artifacts)

    dynamic_artifacts = {}
    simple_artifacts = {}
    for artifact, crate_name in transitive_deps.items():
        if crate_name.dynamic:
            dynamic_artifacts[artifact] = crate_name
        else:
            simple_artifacts[artifact] = None

    prefix = "{}-deps{}".format(subdir, dep_metadata_kind.value)
    if simple_artifacts:
        args.add(simple_symlinked_dirs(ctx, prefix, simple_artifacts))
    if dynamic_artifacts:
        args.add(dynamic_symlinked_dirs(ctx, compile_ctx, prefix, dynamic_artifacts))

    return (args, crate_targets)

def simple_symlinked_dirs(
        ctx: AnalysisContext,
        prefix: str,
        artifacts: dict[Artifact, None]) -> cmd_args:
    # Add as many -Ldependency dirs as we need to avoid name conflicts
    deps_dirs = [{}]
    for dep in artifacts.keys():
        name = dep.basename
        if name in deps_dirs[-1]:
            deps_dirs.append({})
        deps_dirs[-1][name] = dep

    symlinked_dirs = []
    for idx, srcs in enumerate(deps_dirs):
        name = "{}-{}".format(prefix, idx)
        symlinked_dirs.append(ctx.actions.symlinked_dir(name, srcs))

    return cmd_args(symlinked_dirs, format = "-Ldependency={}")

def dynamic_symlinked_dirs(
        ctx: AnalysisContext,
        compile_ctx: CompileContext,
        prefix: str,
        artifacts: dict[Artifact, CrateName]) -> cmd_args:
    name = "{}-dyn".format(prefix)
    transitive_dependency_dir = ctx.actions.declare_output(name, dir = True)

    # Pass the list of rlibs to transitive_dependency_symlinks.py through a file
    # because there can be a lot of them. This avoids running out of command
    # line length, particularly on Windows.
    relative_path = lambda artifact: cmd_args(
        artifact,
        delimiter = "",
        ignore_artifacts = True,
        relative_to = transitive_dependency_dir.project("i"),
    )
    artifacts_json = ctx.actions.write_json(
        ctx.actions.declare_output("{}-dyn.json".format(prefix)),
        [
            (relative_path(artifact), crate.dynamic)
            for artifact, crate in artifacts.items()
        ],
        with_inputs = True,
        pretty = True,
    )

    ctx.actions.run(
        [
            compile_ctx.internal_tools_info.transitive_dependency_symlinks_tool,
            cmd_args(transitive_dependency_dir.as_output(), format = "--out-dir={}"),
            cmd_args(artifacts_json, format = "--artifacts={}"),
        ],
        category = "deps",
        identifier = str(len(compile_ctx.transitive_dependency_dirs)),
    )

    compile_ctx.transitive_dependency_dirs[transitive_dependency_dir] = None
    return cmd_args(transitive_dependency_dir, format = "@{}/dirs", hidden = artifacts.keys())

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

def _rustc_flags(flags: list[[str, ResolvedStringWithMacros]]) -> list[[str, ResolvedStringWithMacros]]:
    # Rustc's "-g" flag is documented as being exactly equivalent to
    # "-Cdebuginfo=2". Rustdoc supports the latter, it just doesn't have the
    # "-g" shorthand for it.
    for i, flag in enumerate(flags):
        if str(flag) == '"-g"':
            flags[i] = "-Cdebuginfo=2"

    return flags

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
    exec_is_windows = ctx.attrs._exec_os_type[OsLookup].platform == "windows"
    path_sep = "\\" if exec_is_windows else "/"

    crate_type = params.crate_type

    args_key = (crate_type, emit, params.dep_link_strategy, is_rustdoc_test, infallible_diagnostics, incremental_enabled, profile_mode)
    if args_key in compile_ctx.common_args:
        return compile_ctx.common_args[args_key]

    # Keep filenames distinct in per-flavour subdirs
    subdir = "{}-{}-{}-{}".format(crate_type.value, params.reloc_model.value, params.dep_link_strategy.value, emit.value)
    if is_rustdoc_test:
        subdir = "{}-rustdoc-test".format(subdir)
    if infallible_diagnostics:
        subdir = "{}-diag".format(subdir)
    if incremental_enabled:
        subdir = "{}-incr".format(subdir)
    if profile_mode:
        subdir = "{}-prof-{}".format(subdir, profile_mode.value)

    # Included in tempfiles
    tempfile = "{}-{}".format(attr_simple_crate_for_filenames(ctx), emit.value)

    root = crate_root(ctx, default_roots)
    if exec_is_windows:
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

    dep_args, crate_map = dependency_args(
        ctx = ctx,
        compile_ctx = compile_ctx,
        toolchain_info = compile_ctx.toolchain_info,
        deps = resolve_rust_deps(ctx, dep_ctx),
        subdir = subdir,
        dep_link_strategy = params.dep_link_strategy,
        dep_metadata_kind = dep_metadata_kind,
        is_rustdoc_test = is_rustdoc_test,
    )

    if crate_type == CrateType("proc-macro"):
        dep_args.add("--extern=proc_macro")

    if crate_type in [CrateType("cdylib"), CrateType("dylib")] and emit_requires_linking:
        linker_info = compile_ctx.cxx_toolchain_info.linker_info
        shlib_name = attr_soname(ctx)
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
        cmd_args(compile_ctx.symlinked_srcs, path_sep, root, delimiter = ""),
        crate_name_arg,
        "--crate-type={}".format(crate_type.value),
        "-Crelocation-model={}".format(params.reloc_model.value),
        "--edition={}".format(edition),
        "-Cmetadata={}".format(_metadata(ctx.label, is_rustdoc_test)[0]),
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

# Return wrapper script for clippy-driver to make sure sysroot is set right
# We need to make sure clippy is using the same sysroot - compiler, std libraries -
# as rustc itself, so explicitly invoke rustc to get the path. This is a
# (small - ~15ms per invocation) perf hit but only applies when generating
# specifically requested clippy diagnostics.
def _clippy_wrapper(
        ctx: AnalysisContext,
        toolchain_info: RustToolchainInfo) -> cmd_args:
    clippy_driver = cmd_args(toolchain_info.clippy_driver)
    rustc_print_sysroot = cmd_args(toolchain_info.compiler, "--print=sysroot", delimiter = " ")
    if toolchain_info.rustc_target_triple:
        rustc_print_sysroot.add("--target={}".format(toolchain_info.rustc_target_triple))

    skip_setting_sysroot = toolchain_info.explicit_sysroot_deps != None or toolchain_info.sysroot_path != None

    if ctx.attrs._exec_os_type[OsLookup].platform == "windows":
        wrapper_file, _ = ctx.actions.write(
            ctx.actions.declare_output("__clippy_driver_wrapper.bat"),
            [
                "@echo off",
                "set __CLIPPY_INTERNAL_TESTS=true",
            ] + [
                cmd_args(rustc_print_sysroot, format = 'FOR /F "tokens=* USEBACKQ" %%F IN (`{}`) DO (set SYSROOT=%%F)') if not skip_setting_sysroot else "",
                cmd_args(clippy_driver, format = "{} %*"),
            ],
            allow_args = True,
        )
    else:
        wrapper_file, _ = ctx.actions.write(
            ctx.actions.declare_output("__clippy_driver_wrapper.sh"),
            [
                "#!/usr/bin/env bash",
                # Force clippy to be clippy: https://github.com/rust-lang/rust-clippy/blob/e405c68b3c1265daa9a091ed9b4b5c5a38c0c0ba/src/driver.rs#L334
                "export __CLIPPY_INTERNAL_TESTS=true",
            ] + (
                [] if skip_setting_sysroot else [cmd_args(rustc_print_sysroot, format = "export SYSROOT=$({})")]
            ) + [
                cmd_args(clippy_driver, format = "{} \"$@\"\n"),
            ],
            is_executable = True,
            allow_args = True,
        )

    return cmd_args(wrapper_file, hidden = [clippy_driver, rustc_print_sysroot])

# This is a hack because we need to pass the linker to rustc
# using -Clinker=path and there is currently no way of doing this
# without an artifact. We create a wrapper (which is an artifact),
# and add -Clinker=
def _linker_args(
        ctx: AnalysisContext,
        linker_info: LinkerInfo,
        binary: bool = False) -> cmd_args:
    linker = cmd_args(
        linker_info.linker,
        linker_info.linker_flags or [],
        # For "binary" rules, add C++ toolchain binary-specific linker flags.
        # TODO(agallagher): This feels a bit wrong -- it might be better to have
        # the Rust toolchain have it's own `binary_linker_flags` instead of
        # implicltly using the one from the C++ toolchain.
        linker_info.binary_linker_flags if binary else [],
        ctx.attrs.linker_flags,
    )

    return cmd_script(
        ctx = ctx,
        name = "linker_wrapper",
        cmd = linker,
        os = ScriptOs("windows" if ctx.attrs._exec_os_type[OsLookup].platform == "windows" else "unix"),
    )

# Returns the full label and its hash. The full label is used for `-Cmetadata`
# which provided the primary disambiguator for two otherwise identically named
# crates. The hash is added to the filename to give them a lower likelihood of
# duplicate names, but it doesn't matter if they collide.
def _metadata(label: Label, is_rustdoc_test: bool) -> (str, str):
    label = str(label.raw_target())
    if is_rustdoc_test:
        label = "doctest/" + label
    h = hash(label)
    if h < 0:
        h = -h
    h = "%x" % h
    return (label, "0" * (8 - len(h)) + h)

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

    if emit == Emit("llvm-ir"):
        link_strategy_suffix = {
            LinkStrategy("static"): " [static]",
            LinkStrategy("static_pic"): " [pic]",
            LinkStrategy("shared"): " [shared]",
        }[link_strategy]
        base = "llvm-ir" + link_strategy_suffix

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
        extra_hash = "-" + _metadata(ctx.label, False)[1]
        emit_args.add("-Cextra-filename={}".format(extra_hash))
        filename = subdir + "/" + output_filename(simple_crate, emit, params, extra_hash)
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
        base, _ext = paths.split_extension(output_filename(simple_crate, emit, params))
        extra_dir = subdir + "/extras/" + base
        extra_out = ctx.actions.declare_output(extra_dir, dir = True)
        emit_args.add(cmd_args(extra_out.as_output(), format = "--out-dir={}"))

        if incremental_enabled:
            build_mode = ctx.attrs.incremental_build_mode
            incremental_out = ctx.actions.declare_output("{}/extras/incremental/{}".format(subdir, build_mode))
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
    identifier = field([str, None]),
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
    exec_is_windows = ctx.attrs._exec_os_type[OsLookup].platform == "windows"

    toolchain_info = compile_ctx.toolchain_info

    plain_env, path_env = process_env(compile_ctx, ctx.attrs.env, exec_is_windows)

    more_plain_env, more_path_env = process_env(compile_ctx, env, exec_is_windows)
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
    )

    if deferred_link_cmd:
        ctx.actions.run(
            deferred_link_cmd,
            local_only = local_only,
            prefer_local = prefer_local,
            category = "deferred_link",
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
        exec_is_windows: bool,
        escape_for_rustc_action: bool = True) -> (dict[str, cmd_args], dict[str, cmd_args]):
    # Values with inputs (ie artifact references).
    path_env = {}

    # Plain strings.
    plain_env = {}

    for k, v in env.items():
        v = cmd_args(v)
        if len(v.inputs) > 0:
            path_env[k] = v
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
            plain_env[k] = cmd_args(v)

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
                "\\" if exec_is_windows else "/",
                value,
                delimiter = "",
            )

    return (plain_env, path_env)

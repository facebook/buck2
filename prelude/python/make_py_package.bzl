# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

"""
Rule for the inplace pex builder, and some utility methods for generic pex builder
execution
"""

load("@prelude//:artifact_tset.bzl", "project_artifacts")
load("@prelude//:local_only.bzl", "package_python_locally")
load("@prelude//:paths.bzl", "paths")
load(
    "@prelude//cxx:cxx_library_utility.bzl",
    "cxx_is_gnu",
)
load(
    "@prelude//linking:link_info.bzl",
    "DepMetadata",
    "LinkArgs",
    "LinkInfosTSet",
    "dedupe_dep_metadata",
    "get_link_info",
    "truncate_dep_metadata",
)
load(
    "@prelude//linking:shared_libraries.bzl",
    "SharedLibrary",  # @unused Used as a type
    "gen_shared_libs_action",
    "traverse_shared_library_info",
    "zip_shlibs",
)
load("@prelude//os_lookup:defs.bzl", "Os", "OsLookup")
load("@prelude//python:manifest.bzl", "create_manifest_for_entries")
load("@prelude//unix:providers.bzl", "UnixEnv", "create_unix_env_info")
load("@prelude//utils:arglike.bzl", "ArgLike")
load(":compile.bzl", "PycInvalidationMode")
load(":interface.bzl", "EntryPoint", "EntryPointKind", "PythonLibraryManifestsInterface")
load(":internal_tools.bzl", "PythonInternalToolsInfo")
load(":manifest.bzl", "ManifestInfo")  # @unused Used as a type
load(":python.bzl", "manifests_to_interface")
load(":python_library.bzl", "gather_dep_libraries")
load(":toolchain.bzl", "PackageStyle", "PythonToolchainInfo", "get_package_style")

# This represents the input to the creation of a Pex. Manifests provide source
# files, extensions are native extensions, and compile indicates whether we
# should also include bytecode from manifests.
PexModules = record(
    manifests = field(PythonLibraryManifestsInterface),
    extensions = field(ManifestInfo | None, None),
    extra_manifests = field(ManifestInfo | None, None),
    repl_manifests = field(PythonLibraryManifestsInterface | None, None),
    compile = field(bool, False),
)

# The output of pex creation. It's everything needed to make the DefaultInfo and RunInfo
# providers.
PexProviders = record(
    default_output = field(Artifact),
    other_outputs = list[ArgLike],
    other_outputs_prefix = str | None,
    hidden_resources = list[ArgLike],
    sub_targets = dict[str, list[Provider]],
    run_cmd = cmd_args,
    dbg_source_db = field(Artifact | None, None),
)

ManifestModule = record(
    manifest = ArgLike,
    artifacts = list[ArgLike],
)

def make_py_package_providers(
        ctx: AnalysisContext,
        manifest_identifier: str,
        pex: PexProviders) -> list[Provider]:
    providers = [
        make_default_info(pex),
        make_run_info(pex),
    ]

    # UnixEnv is used by other rules like conda_package that
    # copies python libraries/binaries to the output directory.
    # Only standalone works here since inplace uses link tree.
    if manifest_identifier == "standalone":
        providers.append(create_unix_env_info(
            actions = ctx.actions,
            env = UnixEnv(
                label = ctx.label,
                binaries = [
                    create_manifest_for_entries(
                        ctx = ctx,
                        name = "unix_env_for_{}".format(manifest_identifier),
                        entries = [
                            (ctx.label.name, pex.default_output, ""),
                        ],
                    ),
                ],
            ),
        ))

    return providers

def _live_par_generated_files(
        ctx: AnalysisContext,
        output: Artifact,
        python_toolchain: PythonToolchainInfo,
        python_internal_tools: PythonInternalToolsInfo,
        build_args: list[ArgLike],
        main: EntryPoint,
        preload_libraries: ArgLike,
        output_suffix: str) -> list[(Artifact, str)]:
    artifacts = []
    artifacts.append((python_internal_tools.run_lpar_main, "__run_lpar_main__.py"))

    lpar_bootstrap = ctx.actions.declare_output("_bootstrap.sh{}".format(output_suffix))
    gen_bootstrap = cmd_args(python_toolchain.gen_lpar_bootstrap[RunInfo])

    # Add passthrough args
    gen_bootstrap.add(cmd_args([cmd_args(arg, replace_regex = ("--passthrough=", "")) for arg in build_args]))

    # in tools/make_par/buck.py we add "os.curdir" to the ld_library_path
    gen_bootstrap.add("--ld-library=.")
    for lib_path in python_toolchain.native_library_runtime_paths:
        gen_bootstrap.add("--ld-library={}".format(lib_path))

    if main[0] == EntryPointKind("module"):
        gen_bootstrap.add(["--main-module", main[1]])
    else:
        gen_bootstrap.add(["--main-function", main[1]])
    if ctx.attrs.runtime_env:
        for k, v in ctx.attrs.runtime_env.items():
            gen_bootstrap.add(cmd_args(["--runtime_env={}={}".format(k, v)]))

    gen_bootstrap.add([
        "--python",
        python_toolchain.interpreter,
    ])
    gen_bootstrap.add(["--main-runner", python_toolchain.main_runner])

    gen_bootstrap.add(preload_libraries)

    gen_bootstrap.add(["--bootstrap-output", lpar_bootstrap.as_output()])
    gen_bootstrap.add(["--output", output.as_output()])
    ctx.actions.run(gen_bootstrap, category = "par", identifier = "lpar_gen_bootstrap{}".format(output_suffix))
    artifacts.append((lpar_bootstrap, "_bootstrap.sh"))
    return artifacts

def _live_par_windows_bootstrap(
        ctx: AnalysisContext,
        output: Artifact,
        python_toolchain: PythonToolchainInfo,
        python_internal_tools: PythonInternalToolsInfo,
        main: EntryPoint,
        preload_libraries: ArgLike,
        shared_libraries: list[(SharedLibrary, str)],
        symlink_tree_path: Artifact,
        output_suffix: str):
    # Windows can't use _bootstrap.sh, so reuse existing make_py_package_inplace tool
    # to generate a Python bootstrap script.
    bootstrap = cmd_args(python_internal_tools.make_py_package_inplace)
    bootstrap.add(_pex_bootstrap_args(
        python_toolchain,
        main,
        output,
        len(shared_libraries) > 0,
        preload_libraries,
        symlink_tree_path,
        PackageStyle("inplace"),
        True,
    ))
    if ctx.attrs.runtime_env:
        for k, v in ctx.attrs.runtime_env.items():
            bootstrap.add(cmd_args(["--runtime_env", "{}={}".format(k, v)]))
    ctx.actions.run(bootstrap, category = "par", identifier = "bootstrap{}".format(output_suffix))

def make_default_info(pex: PexProviders) -> Provider:
    return DefaultInfo(
        default_output = pex.default_output,
        other_outputs = pex.other_outputs + pex.hidden_resources,
        sub_targets = pex.sub_targets,
    )

def make_run_info(pex: PexProviders, run_with_inplace: bool = False) -> Provider:
    if run_with_inplace and "inplace" in pex.sub_targets:
        # If running with inplace, we want to use the RunInfo of inplace subtarget.
        return pex.sub_targets["inplace"][1]

    return RunInfo(pex.run_cmd)

def _srcs(srcs: list[typing.Any], format = "{}") -> cmd_args:
    args = cmd_args()
    for src in srcs:
        args.add(cmd_args(src, format = format))
    return args

def _fail_at_build_time(
        ctx: AnalysisContext,
        python_internal_tools: PythonInternalToolsInfo,
        msg: str) -> PexProviders:
    error_message = ctx.actions.write("__error_message", msg)
    dummy_output = ctx.actions.declare_output("__dummy_output")
    cmd = cmd_args([
        python_internal_tools.fail_with_message,
        error_message,
        dummy_output.as_output(),
    ])
    ctx.actions.run(cmd, category = "par", identifier = "failure")
    return PexProviders(
        default_output = dummy_output,
        other_outputs = [],
        other_outputs_prefix = None,
        hidden_resources = [],
        sub_targets = {},
        run_cmd = cmd_args(),
    )

def _fail(
        ctx: AnalysisContext,
        python_internal_tools: PythonInternalToolsInfo,
        suffix: str,
        msg: str) -> PexProviders:
    if suffix:
        return _fail_at_build_time(ctx, python_internal_tools, msg)

    # suffix is empty, which means this is the default subtarget. All failures must
    # occur at analysis time
    fail(msg)

# TODO(nmj): Resources
# TODO(nmj): Figure out how to harmonize these flags w/ existing make_xar
#                 invocations. It might be perfectly reasonable to just have a wrapper
#                 script that invokes make_xar in a slightly different way.
def make_py_package(
        ctx: AnalysisContext,
        python_toolchain: PythonToolchainInfo,
        python_internal_tools: PythonInternalToolsInfo,
        # A rule-provided tool to use to build the PEX.
        make_py_package_cmd: RunInfo | None,
        package_style: PackageStyle,
        build_args: list[ArgLike],
        pex_modules: PexModules,
        shared_libraries: list[(SharedLibrary, str)],
        preload_labels: set[Label],
        main: EntryPoint,
        allow_cache_upload: bool,
        link_args: list[LinkArgs] = [],
        debuginfo_files: list[(str | (str, SharedLibrary, str), Artifact)] = []) -> PexProviders:
    """
    Passes a standardized set of flags to a `make_py_package` binary to create a python
    "executable".

    Arguments:
        - python_toolchain: Used to locate the PEX binaries.
        - package_style: How to package this binary. Might be controlled by the
          toolchain, but also by the rule.
        - build_args: Extra arguments to pass to the PEX binary.
        - pex_modules: Manifests for sources to package.
        - shared_libraries: Shared libraries to link in. Mapping of soname to
          artifact and whether they should be preloaded.
        - main: the name of the entry point to execute when running the
          resulting binary.
    """
    srcs = []
    srcs.extend(pex_modules.manifests.src_manifests())

    if pex_modules.extensions:
        srcs.append(pex_modules.extensions.manifest)

    if ctx.attrs._exec_os_type[OsLookup].os == Os("macos"):
        # preload_deps might include additional shared libraries which macOS will
        # not be able to load unless they're inside the PAR.
        _, preload_deps_shared_libraries = gather_dep_libraries(ctx.attrs.preload_deps)
        shared_libraries = shared_libraries + [
            (lib, "")
            for info in preload_deps_shared_libraries
            for lib in traverse_shared_library_info(info, transformation_provider = None)
        ]

    preload_libraries = _preload_libraries_args(
        ctx = ctx,
        shared_libraries = [
            (shlib, libdir)
            for shlib, libdir in shared_libraries
            if shlib.label in preload_labels
        ],
    )

    # Add link metadata to manifest_module_entries if requested.
    manifest_module_entries = _add_dep_metadata_to_manifest_module(ctx, shared_libraries, link_args, python_toolchain)
    generated_files = []

    startup_functions_loader = generate_startup_function_loader(ctx, manifest_module_entries)
    startup_function = ctx.actions.write_json(
        "manifest/startup_function_loader.manifest",
        [
            ["__par__/__startup_function_loader__.py", startup_functions_loader, "prelude//python:make_py_package.bzl"],
        ],
        with_inputs = True,
    )
    generated_files.append((startup_functions_loader, "__par__/__startup_function_loader__.py"))

    manifest_module = _generate_manifest_module(ctx, manifest_module_entries, python_internal_tools, srcs)
    if manifest_module:
        generated_files.append((manifest_module.artifacts[1], "__manifest__.py"))
        generated_files.append((manifest_module.artifacts[0], "__manifest__.json"))

    common_modules_args, dep_artifacts, debug_artifacts = _pex_modules_common_args(
        ctx,
        pex_modules,
        [startup_function] if startup_function else [],
        shared_libraries,
        debuginfo_files = debuginfo_files,
    )

    default = _make_py_package_wrapper(
        ctx,
        python_toolchain,
        python_internal_tools,
        make_py_package_cmd,
        package_style,
        build_args,
        shared_libraries,
        generated_files,
        preload_libraries,
        common_modules_args,
        dep_artifacts,
        debug_artifacts,
        main,
        manifest_module,
        pex_modules,
        output_suffix = "",
        allow_cache_upload = allow_cache_upload,
    )

    # lets make a shell
    if ctx.attrs.repl_main:
        repl_deps, _ = gather_dep_libraries(ctx.attrs.repl_only_deps)
        repl_manifests = manifests_to_interface(repl_deps[0].manifests)

        repl_pex_modules = PexModules(
            manifests = pex_modules.manifests,
            extra_manifests = pex_modules.extra_manifests,
            extensions = pex_modules.extensions,
            repl_manifests = repl_manifests,
            compile = pex_modules.compile,
        )

        repl_common_modules_args, repl_dep_artifacts, repl_debug_artifacts = _pex_modules_common_args(
            ctx,
            repl_pex_modules,
            [startup_function] if startup_function else [],
            shared_libraries,
            debuginfo_files = debuginfo_files,
            suffix = "_repl",
        )

        default.sub_targets["repl"] = make_py_package_providers(
            ctx,
            "repl",
            _make_py_package_wrapper(
                ctx,
                python_toolchain,
                python_internal_tools,
                make_py_package_cmd,
                PackageStyle("inplace"),
                build_args,
                shared_libraries,
                generated_files,
                preload_libraries,
                repl_common_modules_args,
                repl_dep_artifacts,
                repl_debug_artifacts,
                (EntryPointKind("function"), ctx.attrs.repl_main),
                manifest_module,
                repl_pex_modules,
                output_suffix = "-repl",
                allow_cache_upload = allow_cache_upload,
            ),
        )

    for style in PackageStyle.values():
        pex_providers = default if style == package_style.value else _make_py_package_wrapper(
            ctx,
            python_toolchain,
            python_internal_tools,
            make_py_package_cmd,
            PackageStyle(style),
            build_args,
            shared_libraries,
            generated_files,
            preload_libraries,
            common_modules_args,
            dep_artifacts,
            debug_artifacts,
            main,
            manifest_module,
            pex_modules,
            output_suffix = "-{}".format(style),
            allow_cache_upload = allow_cache_upload,
        )
        default.sub_targets[style] = make_py_package_providers(
            ctx,
            style,
            pex_providers,
        )

    # cpp binaries already emit a `debuginfo` subtarget with a different format,
    # so we opt to use a more specific subtarget
    default.sub_targets["par-debuginfo"] = _debuginfo_subtarget(ctx, debug_artifacts)
    return default

def _make_py_package_wrapper(
        ctx: AnalysisContext,
        python_toolchain: PythonToolchainInfo,
        python_internal_tools: PythonInternalToolsInfo,
        make_py_package_cmd: RunInfo | None,
        package_style: PackageStyle,
        build_args: list[ArgLike],
        shared_libraries: list[(SharedLibrary, str)],
        generated_files: list[(Artifact, str)],
        preload_libraries: cmd_args,
        common_modules_args: cmd_args,
        dep_artifacts: list[ArgLike],
        debug_artifacts: list[(str | (str, SharedLibrary, str), ArgLike)],
        main: EntryPoint,
        manifest_module: ManifestModule | None,
        pex_modules: PexModules,
        output_suffix: str,
        allow_cache_upload: bool) -> PexProviders:
    if package_style == PackageStyle("inplace") and ctx.attrs.use_rust_make_par:
        return _make_py_package_live(
            ctx,
            python_toolchain.make_py_package_live[RunInfo],
            pex_modules,
            build_args,
            preload_libraries,
            main,
            shared_libraries,
            generated_files,
            python_toolchain,
            python_internal_tools,
            output_suffix,
        )
    return _make_py_package_impl(
        ctx,
        python_toolchain,
        python_internal_tools,
        make_py_package_cmd,
        package_style,
        build_args,
        len(shared_libraries) > 0,
        preload_libraries,
        common_modules_args,
        dep_artifacts,
        debug_artifacts,
        main,
        manifest_module,
        pex_modules,
        output_suffix = output_suffix,
        allow_cache_upload = allow_cache_upload,
    )

def _make_py_package_impl(
        ctx: AnalysisContext,
        python_toolchain: PythonToolchainInfo,
        python_internal_tools: PythonInternalToolsInfo,
        make_py_package_cmd: RunInfo | None,
        package_style: PackageStyle,
        build_args: list[ArgLike],
        shared_libraries: bool,
        preload_libraries: cmd_args,
        common_modules_args: cmd_args,
        dep_artifacts: list[ArgLike],
        debug_artifacts: list[(str | (str, SharedLibrary, str), ArgLike)],
        main: EntryPoint,
        manifest_module: ManifestModule | None,
        pex_modules: PexModules,
        output_suffix: str,
        allow_cache_upload: bool) -> PexProviders:
    name = "{}{}".format(ctx.attrs.name, output_suffix)
    standalone = package_style == PackageStyle("standalone")
    inplace = package_style in [PackageStyle("inplace"), PackageStyle("inplace_lite")]

    runtime_files = []
    sub_targets = {}
    hidden_resources = []
    if pex_modules.manifests.has_hidden_resources(standalone):
        if standalone:
            # constructing this error message is expensive, only do it when we abort analysis
            error_msg = "standalone builds don't support hidden resources" if output_suffix else _hidden_resources_error_message(ctx.label, pex_modules.manifests.hidden_resources(standalone))

            return _fail(ctx, python_internal_tools, output_suffix, error_msg)
        else:
            hidden_resources = pex_modules.manifests.hidden_resources(standalone)

    pyc_mode = PycInvalidationMode("checked_hash") if inplace else PycInvalidationMode("unchecked_hash")

    # Accumulate all of the artifacts required by the build
    runtime_artifacts = []
    runtime_artifacts.extend(dep_artifacts)
    runtime_artifacts.extend(pex_modules.manifests.resource_artifacts(standalone))
    if pex_modules.compile:
        runtime_artifacts.extend(pex_modules.manifests.bytecode_artifacts(pyc_mode))
    if manifest_module:
        runtime_artifacts.extend(manifest_module.artifacts)

    symlink_tree_path = None
    if standalone:
        if python_toolchain.make_py_package_standalone == None:
            return _fail(
                ctx,
                python_internal_tools,
                output_suffix,
                "Python toolchain does not provide make_py_package_standalone",
            )
    else:
        symlink_tree_path = ctx.actions.declare_output("{}#link-tree".format(name), dir = True)

    modules_args = _pex_modules_args(
        ctx,
        common_modules_args,
        runtime_artifacts,
        debug_artifacts,
        package_style,
        pyc_mode,
        symlink_tree_path,
        manifest_module,
        pex_modules,
        output_suffix,
    )

    output = ctx.actions.declare_output("{}{}".format(name, ctx.attrs.extension or python_toolchain.pex_extension))

    bootstrap_args = _pex_bootstrap_args(
        python_toolchain,
        main,
        output,
        shared_libraries,
        preload_libraries,
        symlink_tree_path,
        package_style,
        True if ctx.attrs.zip_safe == None else ctx.attrs.zip_safe,
    )
    bootstrap_args.add(build_args)
    if standalone:
        bootstrap_args.add(ctx.attrs.standalone_build_args)
    else:
        bootstrap_args.add(ctx.attrs.inplace_build_args)
        runtime_files.extend(runtime_artifacts)
        runtime_files.append(symlink_tree_path)

    # For standalone builds, or builds setting make_py_package we generate args for calling make_par.py
    if standalone or make_py_package_cmd != None:
        # We support building _standalone_ packages locally to e.g. support fbcode's
        # current style of build info stamping (e.g. T10696178).
        prefer_local = (not getattr(ctx.attrs, "optimize_for_action_throughput", False)) and standalone and package_python_locally(ctx, python_toolchain)
        prefer_remote = getattr(ctx.attrs, "optimize_for_action_throughput", False)

        cmd = cmd_args(
            make_py_package_cmd if make_py_package_cmd != None else python_toolchain.make_py_package_standalone,
        )
        cmd.add(modules_args)
        cmd.add(bootstrap_args)
        if ctx.attrs.runtime_env:
            for k, v in ctx.attrs.runtime_env.items():
                cmd.add(cmd_args(["--passthrough", "--runtime_env={}={}".format(k, v)]))
        if package_style == PackageStyle("outplace"):
            cmd.add(cmd_args("--passthrough=--copy-files", hidden = runtime_artifacts))

        identifier_prefix = "standalone{}" if standalone else "inplace{}"
        ctx.actions.run(
            cmd,
            prefer_local = prefer_local,
            prefer_remote = prefer_remote,
            category = "par",
            identifier = identifier_prefix.format(output_suffix),
            allow_cache_upload = allow_cache_upload,
            error_handler = python_toolchain.python_error_handler,
        )

    else:
        modules = cmd_args(python_internal_tools.make_py_package_modules)
        modules.add(modules_args)
        if package_style == PackageStyle("outplace"):
            modules.add(cmd_args("--copy-files", hidden = runtime_artifacts))
        ctx.actions.run(modules, category = "par", identifier = "modules{}".format(output_suffix))

        bootstrap = cmd_args(python_internal_tools.make_py_package_inplace)
        bootstrap.add(bootstrap_args)
        if python_toolchain.native_library_env_var != None:
            bootstrap.add(cmd_args(python_toolchain.native_library_env_var, format = "--native-libs-env-var={}"))

        if ctx.attrs.runtime_env:
            for k, v in ctx.attrs.runtime_env.items():
                bootstrap.add(cmd_args(["--runtime_env", "{}={}".format(k, v)]))

        ctx.actions.run(bootstrap, category = "par", identifier = "bootstrap{}".format(output_suffix))

    run_args = []

    # Windows can't run PAR directly.
    if ctx.attrs._exec_os_type[OsLookup].os == Os("windows"):
        run_args.append(ctx.attrs._python_toolchain[PythonToolchainInfo].interpreter)
    run_args.append(output)

    if symlink_tree_path != None:
        sub_targets["link-tree"] = [DefaultInfo(
            default_output = symlink_tree_path,
            other_outputs = runtime_files + (hidden_resources if package_style == PackageStyle("outplace") else []),
            sub_targets = {},
        )]

    return PexProviders(
        default_output = output,
        other_outputs = runtime_files,
        other_outputs_prefix = symlink_tree_path.short_path if symlink_tree_path != None else None,
        hidden_resources = hidden_resources,
        sub_targets = sub_targets,
        run_cmd = cmd_args(
            run_args,
            hidden = runtime_files + hidden_resources + [python_toolchain.interpreter],
        ),
    )

def _make_py_package_live(
        ctx: AnalysisContext,
        make_py_package_live: RunInfo,
        pex_modules: PexModules,
        build_args: list[ArgLike],
        preload_libraries: cmd_args,
        main: EntryPoint,
        shared_libraries: list[(SharedLibrary, str)],
        common_generated_files: list[(Artifact, str)],
        python_toolchain: PythonToolchainInfo,
        python_internal_tools: PythonInternalToolsInfo,
        output_suffix: str) -> PexProviders:
    """
    Bundle contents of par into symlink dir
    * generated_files
      * _bootstrap.sh
      * __manifest__.py
      * __manifest__.json
      * __run_lpar_main__.py
      * __par__/__startup_function_loader__.py
    * source files
    * resources
    * bytecode
    * extras
      * dbg-db.json
      * sitecustomize.py
      * static_extension_finder.py
      * cxx_python_extension stub files
    * extensions
      * cxx_python_extensions ".so" files - when using native linking these are the "unembedable" extensions
    * native-libraries
      * All of the required shared cxx libraries
      * Link group libraries
      * native-executable
    * TODO: bundled-runtime
    * Debug info:
      * External debug info is included in runtime_files so that it is materialized at build time
      since we are building an inplace par and the .so files symlink to their
      actual build location the debug-info does not need to be included in the
      link tree
      * EXCLUDED: dwp files - we do not package dwp files in inplace pars
      * EXCLUDED: debuginfo - we do not package debug-info for inplace pars
    """
    sub_targets = {}
    name = "{}{}".format(ctx.attrs.name, output_suffix)

    symlink_tree_path = ctx.actions.declare_output("{}#link-tree".format(name), dir = True)
    runtime_files = [symlink_tree_path]

    output = ctx.actions.declare_output("{}{}".format(name, ctx.attrs.extension or python_toolchain.pex_extension))

    generated_files = []
    generated_files.extend(common_generated_files)

    is_windows = ctx.attrs._target_os_type[OsLookup].os == Os("windows")

    if is_windows:
        _live_par_windows_bootstrap(
            ctx,
            output,
            python_toolchain,
            python_internal_tools,
            main,
            preload_libraries,
            shared_libraries,
            symlink_tree_path,
            output_suffix,
        )
    else:
        generated_files.extend(_live_par_generated_files(
            ctx,
            output,
            python_toolchain,
            python_internal_tools,
            build_args,
            main,
            preload_libraries,
            output_suffix,
        ))

    cmd = cmd_args(make_py_package_live)
    cmd.add(cmd_args(symlink_tree_path.as_output(), format = "--output-path={}"))

    sources = pex_modules.manifests.src_manifests()
    if pex_modules.repl_manifests:
        sources.extend(pex_modules.repl_manifests.src_manifests())
        runtime_files.extend(pex_modules.repl_manifests.src_artifacts())

    source_manifests_path = ctx.actions.write(
        "__source_manifests{}.txt".format(output_suffix),
        sources,
    )
    cmd.add(cmd_args(source_manifests_path, format = "--sources={}", hidden = sources))
    runtime_files.extend(pex_modules.manifests.src_artifacts())

    # Gather inplace binary resources
    resources = pex_modules.manifests.resource_manifests(False)
    if resources:
        resource_artifacts = pex_modules.manifests.resource_artifacts(False)
        resource_manifests_path = ctx.actions.write(
            "__resource_manifests{}.txt".format(output_suffix),
            resources,
        )

        # Since we allow including directories for resources we have to enumerate the directory at build time, so we pass the resource artifacts as a hidden arg so that it will be materialized on disk when we build the par.
        # cmd.add(cmd_args(resource_manifests_path, format = "--resources={}", hidden = [resources, resource_artifacts]))
        # This was previously broken and the fix incurs a non-trivial build speed regression
        cmd.add(cmd_args(resource_manifests_path, format = "--resources={}", hidden = [resources]))
        runtime_files.extend(resource_artifacts)

    if pex_modules.compile:
        # bytecode is compiled per library so the actual bytecode artifacts are directories
        # the compile command outputs json manifest in the form
        # [(src, dst, origin),]
        bytecode_manifests = pex_modules.manifests.bytecode_manifests(PycInvalidationMode("checked_hash"))
        bytecode_manifests_path = ctx.actions.write(
            "__bytecode_manifests{}.txt".format(output_suffix),
            bytecode_manifests,
        )
        cmd.add(cmd_args(bytecode_manifests_path, format = "--bytecode={}", hidden = bytecode_manifests))

        bytecode_artifacts = pex_modules.manifests.bytecode_artifacts(PycInvalidationMode("checked_hash"))
        runtime_files.extend(bytecode_artifacts)

    if pex_modules.extra_manifests != None:
        extras_manifest = ctx.actions.write("{}-extra.txt".format(name), [cmd_args(a, p, delimiter = "::") for a, p in pex_modules.extra_manifests.artifacts], with_inputs = True)
        cmd.add(cmd_args(extras_manifest.without_associated_artifacts(), format = "--extras={}"))
        runtime_files.append(extras_manifest)

    if pex_modules.extensions != None:
        cmd.add(cmd_args(pex_modules.extensions.manifest, format = "--extensions={}"))
        runtime_files.extend([a[0] for a in pex_modules.extensions.artifacts])

    native_libraries_manifest = gen_shared_libs_action(
        actions = ctx.actions,
        out = "{}-native-libraries.txt".format(name),
        shared_libs = [shlib for shlib, _ in shared_libraries],
        gen_action = lambda actions, output, shared_libs: actions.write(
            output,
            cmd_args(
                [
                    cmd_args(shlib.lib.output, paths.join(libdir, soname), delimiter = "::")
                    for soname, shlib, libdir in zip_shlibs(shared_libs, shared_libraries)
                ],
            ),
        ),
    )
    cmd.add(cmd_args(native_libraries_manifest.without_associated_artifacts(), format = "--shared-libs={}"))
    runtime_files.append(native_libraries_manifest)

    # Due to dynamic action weirdness we need to explicitly add the shared_libs to the output
    runtime_files.extend([shlib.lib.output for shlib, _ in shared_libraries])

    # Make sure all external debug-info is materialized at runtime
    runtime_files.extend(project_artifacts(
        ctx.actions,
        [
            shlib.lib.external_debug_info
            for shlib, _ in shared_libraries
        ],
    ))

    generated_manifest = ctx.actions.write("{}-generated.txt".format(name), [cmd_args(a, p, delimiter = "::") for a, p in generated_files], with_inputs = True)
    cmd.add(cmd_args(generated_manifest.without_associated_artifacts(), format = "--generated={}"))
    runtime_files.append(generated_manifest)

    state = ctx.actions.declare_output("{}-state.json".format(name))
    cmd.add(cmd_args(state.as_output(), format = "--state={}"))
    runtime_files.append(state)
    sub_targets["state"] = [DefaultInfo(default_output = state)]

    if ctx.attrs.use_rust_make_par_incremental:
        cmd.add(["--incremental"])
        ctx.actions.run(
            cmd,
            metadata_env_var = "ACTION_METADATA",
            metadata_path = "action_metadata-{}.json".format(name),
            category = "par",
            identifier = "make_live_par_incremental{}".format(output_suffix),
            no_outputs_cleanup = True,
        )
    else:
        ctx.actions.run(cmd, category = "par", identifier = "make_live_par{}".format(output_suffix), prefer_local = False)

    hidden_resources = pex_modules.manifests.hidden_resources(False)
    sub_targets["link-tree"] = [DefaultInfo(
        default_output = symlink_tree_path,
        other_outputs = runtime_files + hidden_resources,
        sub_targets = {},
    )]

    run_args = []
    if is_windows:
        run_args.append(python_toolchain.interpreter)
    run_args.append(output)

    return PexProviders(
        default_output = output,
        other_outputs = runtime_files,
        other_outputs_prefix = symlink_tree_path.short_path,
        hidden_resources = hidden_resources,
        sub_targets = sub_targets,
        run_cmd = cmd_args(
            run_args,
            hidden = runtime_files + hidden_resources + [python_toolchain.interpreter],
        ),
    )

def _debuginfo_subtarget(
        ctx: AnalysisContext,
        debug_artifacts: list[(str | (str, SharedLibrary, str), ArgLike)]) -> list[Provider]:
    for_shared_libs = []
    other = []
    for name, artifact in debug_artifacts:
        if type(name) == type(()):
            for_shared_libs.append((name[1], (artifact, name[0], name[2])))
        else:
            if len(name) == 0:
                # This is external debug information, most likely coming from execution
                # platform, skip packaging them because they are not important.
                continue
            other.append((artifact, name))
    out = gen_shared_libs_action(
        actions = ctx.actions,
        out = "debuginfo.manifest.json",
        shared_libs = [shlib for shlib, _ in for_shared_libs],
        gen_action = lambda actions, output, shared_libs: actions.write_json(
            output,
            [
                (debug, paths.join(libdir, soname + ext))
                for soname, _, (debug, libdir, ext) in zip_shlibs(shared_libs, for_shared_libs)
            ] + other,
        ),
    )
    return [DefaultInfo(default_output = out, other_outputs = [d for _, d in debug_artifacts])]

def _preload_libraries_args(ctx: AnalysisContext, shared_libraries: list[(SharedLibrary, str)]) -> cmd_args:
    preload_libraries_path = gen_shared_libs_action(
        actions = ctx.actions,
        out = "__preload_libraries.txt",
        shared_libs = [shlib for shlib, _ in shared_libraries],
        gen_action = lambda actions, output, shared_libs: actions.write(
            output,
            [
                "--preload={}".format(paths.join(libdir, soname))
                for soname, _, libdir in zip_shlibs(shared_libs, shared_libraries)
            ],
        ),
    )
    return cmd_args(preload_libraries_path, format = "@{}")

def _pex_bootstrap_args(
        toolchain: PythonToolchainInfo,
        main: EntryPoint,
        output: Artifact,
        shared_libraries: bool,
        preload_libraries: cmd_args,
        symlink_tree_path: Artifact | None,
        package_style: PackageStyle,
        zip_safe: bool) -> cmd_args:
    cmd = cmd_args()
    cmd.add(preload_libraries)
    cmd.add([
        "--python",
        toolchain.interpreter,
        "--host-python",
        toolchain.host_interpreter,
    ])
    if main[0] == EntryPointKind("module"):
        cmd.add(["--entry-point", main[1]])
    else:
        cmd.add(["--main-function", main[1]])
    if symlink_tree_path != None:
        cmd.add(cmd_args(["--modules-dir", symlink_tree_path], ignore_artifacts = True))

    cmd.add(["--main-runner", toolchain.main_runner])

    # Package style `inplace_lite` cannot be used with shared libraries
    if package_style == PackageStyle("inplace_lite") and not shared_libraries:
        cmd.add("--use-lite")
    cmd.add(output.as_output())

    if package_style == PackageStyle("standalone") and not zip_safe:
        cmd.add("--no-zip-safe")

    for lib_path in toolchain.native_library_runtime_paths:
        cmd.add("--native-library-runtime-path={}".format(lib_path))

    return cmd

def _pex_modules_common_args(
        ctx: AnalysisContext,
        pex_modules: PexModules,
        extra_manifests: list[ArgLike],
        shared_libraries: list[(SharedLibrary, str)],
        debuginfo_files: list[(str | (str, SharedLibrary, str), Artifact)],
        suffix: str = "") -> (cmd_args, list[ArgLike], list[(str | (str, SharedLibrary, str), ArgLike)]):
    srcs = []
    src_artifacts = []
    deps = []
    debug_artifacts = []

    srcs.extend(pex_modules.manifests.src_manifests())
    src_artifacts.extend(pex_modules.manifests.src_artifacts())

    if pex_modules.extensions:
        srcs.append(pex_modules.extensions.manifest)
        src_artifacts.extend([a[0] for a in pex_modules.extensions.artifacts])

    if pex_modules.extra_manifests:
        srcs.append(pex_modules.extra_manifests.manifest)
        src_artifacts.extend([a[0] for a in pex_modules.extra_manifests.artifacts])

    if pex_modules.repl_manifests:
        srcs.extend(pex_modules.repl_manifests.src_manifests())
        src_artifacts.extend(pex_modules.repl_manifests.src_artifacts())

    if extra_manifests:
        srcs.extend(extra_manifests)
        deps.extend(extra_manifests)

    deps.extend(src_artifacts)

    src_manifests_path = ctx.actions.write(
        "__src_manifests{}.txt".format(suffix),
        _srcs(srcs, format = "--module-manifest={}"),
    )
    native_libraries = gen_shared_libs_action(
        actions = ctx.actions,
        out = "__native_libraries{}__.txt".format(suffix),
        shared_libs = [shlib for shlib, _ in shared_libraries],
        gen_action = lambda actions, output, shared_libs: actions.write(
            output,
            cmd_args(
                _srcs(
                    [shlib.lib.output for shlib in shared_libs.values()],
                    format = "--native-library-src={}",
                ),
                [
                    "--native-library-dest={}".format(paths.join(libdir, soname))
                    for soname, _, libdir in zip_shlibs(shared_libs, shared_libraries)
                ],
            ),
        ),
    )

    src_manifest_args = cmd_args(src_manifests_path, hidden = srcs)

    cmd = cmd_args()
    cmd.add(cmd_args(src_manifest_args, format = "@{}"))
    cmd.add(cmd_args(native_libraries, format = "@{}"))

    if debuginfo_files:
        debuginfo_srcs_path = ctx.actions.write(
            "__debuginfo___srcs{}.txt".format(suffix),
            _srcs([src for _, src in debuginfo_files], format = "--debuginfo-src={}"),
        )
        debuginfo_srcs_args = cmd_args(debuginfo_srcs_path)
        cmd.add(cmd_args(debuginfo_srcs_args, format = "@{}"))
        for name, artifact in debuginfo_files:
            if type(name) != type(""):
                libdir, shlib, ext = name
                name = paths.join(libdir, shlib.soname.ensure_str() + ext)
            debug_artifacts.append((name, artifact))

    if ctx.attrs.package_split_dwarf_dwp:
        if ctx.attrs.strip_libpar == "extract" and get_package_style(ctx) == PackageStyle("standalone") and cxx_is_gnu(ctx):
            dwp_ext = ".debuginfo.dwp"
        else:
            dwp_ext = ".dwp"
        dwp_args = gen_shared_libs_action(
            actions = ctx.actions,
            out = "__dwp{}__.txt".format(suffix),
            shared_libs = [shlib for shlib, _ in shared_libraries],
            gen_action = lambda actions, output, shared_libs: actions.write(
                output,
                cmd_args(
                    _srcs(
                        [
                            shlib.lib.dwp
                            for shlib in shared_libs.values()
                            if shlib.lib.dwp != None
                        ],
                        format = "--dwp-src={}",
                    ),
                    _srcs(
                        [
                            paths.join(libdir, soname + dwp_ext)
                            for soname, shlib, libdir in zip_shlibs(shared_libs, shared_libraries)
                            if shlib.lib.dwp != None
                        ],
                        format = "--dwp-dest={}",
                    ),
                ),
            ),
        )
        cmd.add(cmd_args(dwp_args, format = "@{}"))

        for shlib, libdir in shared_libraries:
            if shlib.lib.dwp != None:
                debug_artifacts.append(((libdir, shlib, dwp_ext), shlib.lib.dwp))

    for shlib, _ in shared_libraries:
        deps.append(shlib.lib.output)

    external_debug_info = project_artifacts(
        ctx.actions,
        [
            shlib.lib.external_debug_info
            for shlib, _ in shared_libraries
        ],
    )

    # HACK: external_debug_info has an empty path
    debug_artifacts.extend([("", d) for d in external_debug_info])

    return (cmd, deps, debug_artifacts)

def _pex_modules_args(
        ctx: AnalysisContext,
        common_args: cmd_args,
        dep_artifacts: list[ArgLike],
        debug_artifacts: list[(str | (str, SharedLibrary, str), ArgLike)],
        package_style: PackageStyle,
        pyc_mode: PycInvalidationMode,
        symlink_tree_path: Artifact | None,
        manifest_module: ManifestModule | None,
        pex_modules: PexModules,
        output_suffix: str) -> cmd_args:
    """
    Produces args to deal with a PEX's modules. Returns args to pass to the
    modules builder.
    """

    cmd = []
    hidden = []

    cmd.append(common_args)

    if manifest_module != None:
        cmd.append(cmd_args(manifest_module.manifest, format = "--module-manifest={}"))

    if pex_modules.compile:
        bytecode_manifests = pex_modules.manifests.bytecode_manifests(pyc_mode)

        bytecode_manifests_path = ctx.actions.write(
            "__bytecode_manifests{}.txt".format(output_suffix),
            _srcs(
                bytecode_manifests,
                format = "--module-manifest={}",
            ),
        )
        cmd.append(cmd_args(bytecode_manifests_path, format = "@{}"))
        hidden.append(bytecode_manifests)

        # If content-based path hashing is enabled, we need to pass in the actual
        # bytecode artifacts alongside the manifest in order to replace the
        # placeholder "output_artifacts" portion of the path with the resolved hash.
        if (
            getattr(ctx.attrs, "supports_pyc_content_based_paths", False) and
            ctx.attrs._python_toolchain[PythonToolchainInfo].supports_content_based_paths
        ):
            bytecode_artifacts = pex_modules.manifests.bytecode_artifacts(pyc_mode)

            bytecode_artifacts_path = ctx.actions.write(
                "__bytecode_artifacts{}.txt".format(output_suffix),
                cmd_args(bytecode_artifacts),
            )
            cmd.append(cmd_args(bytecode_artifacts_path, format = "--bytecode-artifacts={}"))

    if symlink_tree_path != None:
        cmd.extend(["--modules-dir", symlink_tree_path.as_output()])
    else:
        # Accumulate all the artifacts we depend on. Only add them to the command
        # if we are not going to create symlinks.
        hidden.append(dep_artifacts)

    hidden.extend([s for _, s in debug_artifacts])

    standalone = package_style == PackageStyle("standalone")
    resources = pex_modules.manifests.resource_manifests(standalone)
    if resources:
        resource_manifests_path = ctx.actions.write(
            "__resource_manifests{}.txt".format(output_suffix),
            _srcs(resources, format = "--resource-manifest={}"),
        )
        resource_manifest_args = cmd_args(resource_manifests_path, hidden = resources)
        cmd.append(cmd_args(resource_manifest_args, format = "@{}"))

    return cmd_args(cmd, hidden = hidden)

def _hidden_resources_error_message(current_target: Label, hidden_resources: list[ArgLike] | None) -> str:
    """
    Friendlier error message about putting non-python resources into standalone bins
    """
    owner_to_artifacts = {}

    for resource_set in hidden_resources:
        for resources in resource_set.traverse():
            for r in resources:
                # TODO: `r` is sometimes a tset projection, but it shouldn't be
                if getattr(r, "is_source", True):
                    # Source files; do a string repr so that we get the
                    # package path in there too
                    owner_to_artifacts.setdefault("", []).append(str(r))
                else:
                    owner_to_artifacts.setdefault(r.owner, []).append(r.short_path)

    msg = (
        "Cannot package hidden srcs/resources in a standalone python_binary. " +
        'Eliminate resources in non-Python dependencies of this python binary, set `package_style = "inplace"` on ' +
        str(current_target.raw_target()) + ", " +
        'use `strip_mode="full"` or turn off Split DWARF `-c fbcode.split-dwarf=false` on C++ binary resources.\n'
    )

    for (rule, resources) in owner_to_artifacts.items():
        if rule != "":
            msg += "Hidden srcs/resources for {}\n".format(rule)
        else:
            msg += "Debug instructions:\n"
            msg += "Find the reason this file was included with `buck2 cquery 'allpaths({}, owner(%s))' <file paths>`\n".format(current_target.raw_target())
        for resource in sorted(resources):
            msg += "  {}\n".format(resource)
    return msg

def _get_shared_library_dep_metadata(
        ctx: AnalysisContext,
        shared_libraries: list[(SharedLibrary, str)],
        link_args: list[LinkArgs]) -> list[DepMetadata]:
    """
    Dedupes the linker metadata for each shared library into a single string.

    Note: this can be expensive because we're traversing a tset rather than
    projecting the value. This is unfortunate but we need to be able to truncate
    how much metadata we put into the manifest module.
    """
    link_infos_tsets = []
    link_infos = []

    def add_inner_infos(args: LinkArgs) -> None:
        if args.tset != None:
            tset = args.tset
            link_infos_tsets.append(tset.infos)
        elif args.infos != None:
            link_infos.extend(args.infos)

    for args in link_args:
        add_inner_infos(args)

    for lib, _ in shared_libraries:
        # Note: refer to SharedLibrary.LinkedObject.link_args here. SharedLibrary.link_args isn't
        # always populated.
        for args in lib.lib.link_args or []:
            if args:
                add_inner_infos(args)

    tset = ctx.actions.tset(LinkInfosTSet, children = link_infos_tsets)
    metadatas = []
    for info in tset.traverse():
        metadatas.extend(get_link_info(info).metadata)
    for info in link_infos:
        metadatas.extend(info.metadata)

    return dedupe_dep_metadata(metadatas)

def _add_dep_metadata_to_manifest_module(
        ctx: AnalysisContext,
        shared_libraries: list[(SharedLibrary, str)],
        link_args: list[LinkArgs],
        python_toolchain: PythonToolchainInfo) -> dict[str, typing.Any] | None:
    """
    Updates manifest_module_entries with link metadata if they exist.
    """

    # FIXME(JakobDegen): Hack: Outside of the fbcode macros, we don't pass `--no-manifest` to the
    # par builder which means that we can't generate any manifest data in the prelude
    if python_toolchain.manifest_module_entries == None or "fbcode_macros" not in ctx.attrs.labels:
        if ctx.attrs.manifest_module_entries == None:
            return None
        manifest_module_entries = dict(ctx.attrs.manifest_module_entries)
    else:
        manifest_module_entries = dict(python_toolchain.manifest_module_entries)
        if ctx.attrs.manifest_module_entries != None:
            for k, v in ctx.attrs.manifest_module_entries.items():
                if k not in manifest_module_entries:
                    manifest_module_entries[k] = v
                    continue
                if isinstance(manifest_module_entries[k], dict) and isinstance(v, dict):
                    d = dict(manifest_module_entries[k])
                    d.update(v)
                    manifest_module_entries[k] = d
                else:
                    fail("Cannot merge manifest_module_entries entry {}: {} and {}".format(k, manifest_module_entries[k], v))

    metadatas = _get_shared_library_dep_metadata(ctx, shared_libraries, link_args)
    manifest_module_entries["library_versions"] = [
        metadata.version
        for metadata in truncate_dep_metadata(metadatas)
    ]

    return manifest_module_entries

def generate_startup_function_loader(
        ctx: AnalysisContext,
        manifest_module_entries: dict[str, typing.Any] | None) -> Artifact:
    """
    Generate `__startup_function_loader__.py` used for early bootstrap of a par.
    Things that go here are also enumerated in `__manifest__['startup_functions']`
    Some examples include:
     * static extension finder init
     * eager import loader init
     * cinderx init
    """

    if manifest_module_entries == None:
        startup_functions_list = ""
    else:
        startup_functions_list = "\n".join(
            [
                "'''" + startup_function + "''',"
                for _, startup_function in sorted(manifest_module_entries.get("startup_functions", {}).items())
            ],
        )

    src_startup_functions_path = ctx.actions.write(
        "manifest/__startup_function_loader__.py",
        """
import importlib
import warnings

VARS = {vars}
STARTUP_FUNCTIONS=[{startup_functions_list}]

VARS["_dearg"] = lambda *args, **kwargs: (args, kwargs)


def load_startup_functions():
    for name in STARTUP_FUNCTIONS:
        mod, sep, func = name.partition(":")
        if sep:
            try:
                func, _, args = func.partition("(")
                args, kwargs = eval("_dearg(" + args, VARS) if args else ((), {{}})
                module = importlib.import_module(mod)
                getattr(module, func)(*args, **kwargs)
            except Exception as e:
                # TODO: Ignoring errors for now.
                warnings.warn(
                    "Startup function '%s' (%s:%s) not executed: %s"
                    % (func, mod, func, e),
                    stacklevel=1,
                )

        """.format(
            startup_functions_list = startup_functions_list,
            vars = {
                "label": repr(ctx.label),
                "name": ctx.attrs.name,
            },
        ),
    )
    return src_startup_functions_path

def _generate_manifest_module(
        ctx: AnalysisContext,
        manifest_module_entries: dict[str, typing.Any] | None,
        python_internal_tools: PythonInternalToolsInfo,
        src_manifests: list[ArgLike]) -> ManifestModule | None:
    """
    Generates a __manifest__.py module, and an extra entry to add to source manifests.

    The contents of the manifest are taken from an attribute if it exists. If the
    attribute is None, this function does nothing.
    """

    if manifest_module_entries == None:
        return None
    module = ctx.actions.declare_output("manifest/__manifest__.py")
    entries_json = ctx.actions.write_json("manifest/entries.json", manifest_module_entries)
    src_manifests_path = ctx.actions.write(
        "__module_manifests.txt",
        src_manifests,
    )
    cmd = cmd_args(
        python_internal_tools.make_py_package_manifest_module,
        ["--manifest-entries", entries_json],
        ["--module-manifests", src_manifests_path],
        ["--output", module.as_output()],
        hidden = src_manifests,
    )
    ctx.actions.run(cmd, category = "par", identifier = "manifest-module")

    json_entries_output = ctx.actions.declare_output("manifest/__manifest__.json")
    ctx.actions.copy_file(json_entries_output.as_output(), entries_json)

    src_manifest = ctx.actions.write_json(
        "manifest/module_manifest.json",
        [
            ["__manifest__.py", module, "prelude//python:make_py_package.bzl"],
            ["__manifest__.json", json_entries_output, "prelude//python:make_py_package.bzl"],
        ],
        with_inputs = True,
    )

    return ManifestModule(manifest = src_manifest, artifacts = [json_entries_output, module])

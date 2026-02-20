# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load(
    "@prelude//:artifacts.bzl",
    "ArtifactGroupInfo",
    "ArtifactOutputs",  # @unused Used as a type
)
load("@prelude//:paths.bzl", "paths")
load("@prelude//:resources.bzl", "gather_resources")
load("@prelude//cxx:cxx_context.bzl", "get_opt_cxx_toolchain_info")
load(
    "@prelude//cxx:cxx_library_utility.bzl",
    "cxx_is_gnu",
)
load("@prelude//cxx:cxx_utility.bzl", "cxx_attrs_get_allow_cache_upload")
load(
    "@prelude//linking:link_info.bzl",
    "LinkArgs",  # @unused Used as a type
    "LinkedObject",
)
load(
    "@prelude//linking:linkable_graph.bzl",
    _linkable_graph = "linkable_graph",
)
load(
    "@prelude//linking:shared_libraries.bzl",
    "SharedLibrary",
    "to_soname",
    "traverse_shared_library_info",
)
load("@prelude//linking:strip.bzl", "strip_debug_with_gnu_debuglink")
load("@prelude//python:compute_providers.bzl", "ExecutableType", "compute_providers")
load(
    "@prelude//python/linking:link_helper.bzl",
    "LinkProviders",
    "cxx_implicit_attrs",
    "process_native_linking_rule",
    "python_implicit_attrs",
)
load("@prelude//python/linking:native.bzl", "process_native_linking")
load("@prelude//python/linking:native_python_util.bzl", "compute_link_strategy", "merge_native_deps")
load("@prelude//python/linking:omnibus.bzl", "process_omnibus_linking")
load("@prelude//utils:utils.bzl", "value_or")
load(":compile.bzl", "compile_manifests")
load(
    ":interface.bzl",
    "EntryPoint",
    "EntryPointKind",
)
load(":internal_tools.bzl", "PythonInternalToolsInfo")
load(":lazy_imports.bzl", "run_lazy_imports_analyzer")
load(":make_py_package.bzl", "PexModules", "PexProviders", "make_py_package")
load(
    ":manifest.bzl",
    "ManifestInfo",  # @unused Used as a type
    "create_manifest_for_extensions",
    "create_manifest_for_source_map",
)
load(":python.bzl", "PythonLibraryInfo", "manifests_to_interface")
load(
    ":python_library.bzl",
    "create_python_library_info",
    "gather_dep_libraries",
    "py_attr_resources",
    "py_resources",
    "qualify_srcs",
)
load(":python_runtime_bundle.bzl", "PythonRuntimeBundleInfo")
load(":source_db.bzl", "create_dbg_source_db", "create_python_source_db_info", "create_source_db_no_deps")
load(":toolchain.bzl", "NativeLinkStrategy", "PackageStyle", "PythonToolchainInfo", "get_package_style")
load(":typing.bzl", "create_per_target_type_check")
load(":versions.bzl", "LibraryName", "LibraryVersion", "gather_versioned_dependencies", "resolve_versions")

# We do a lot of merging extensions, so don't use O(n) type annotations
def _merge_extensions(
        # {str: ("_a", "label")}
        extensions,
        # Label
        incoming_label,
        # {str: "_a"}
        incoming_extensions) -> None:
    """
    Merges a incoming_extensions into `extensions`. Fails if duplicate dests exist.
    """
    for extension_name, incoming_artifact in incoming_extensions.items():
        existing = extensions.get(extension_name)
        if existing != None and existing[0] != incoming_artifact:
            existing_artifact, existing_label = existing
            error = (
                "Duplicate extension: {}! Conflicting mappings:\n" +
                "{} from {}\n" +
                "{} from {}"
            )
            fail(
                error.format(
                    extension_name,
                    existing_artifact,
                    existing_label,
                    incoming_artifact,
                    incoming_label,
                ),
            )
        extensions[extension_name] = (incoming_artifact, incoming_label)

def _qualify_entry_point(main: EntryPoint, base_module: str) -> EntryPoint:
    qualname = main[1]
    fqname = qualname
    if qualname.startswith("."):
        fqname = base_module + qualname
        if fqname.startswith("."):
            fqname = fqname[1:]
    return (main[0], fqname)

def python_executable(
        ctx: AnalysisContext,
        main: EntryPoint,
        srcs: dict[str, Artifact],
        default_resources: dict[str, ArtifactOutputs],
        standalone_resources: dict[str, ArtifactOutputs] | None,
        compile: bool,
        allow_cache_upload: bool,
        executable_type: ExecutableType) -> list[Provider] | Promise:
    # Returns a three tuple: the Python binary, all its potential runtime files,
    # and a provider for its source DB.
    raw_deps = ctx.attrs.deps

    # TODO(nmj): See if people are actually setting cxx_platform here. Really
    #                 feels like it should be a property of the python platform
    # `preload_deps` is used later to configure `LD_PRELOAD` environment variable,
    # here we make the actual libraries to appear in the distribution.
    # TODO: make fully consistent with its usage later
    raw_deps.extend(ctx.attrs.preload_deps)

    selected_deps = resolve_versions(
        gather_versioned_dependencies(raw_deps),
        {
            LibraryName(value = key): LibraryVersion(value = ver)
            for key, ver in ctx.attrs.version_selections.items()
        },
    )
    raw_deps.extend(selected_deps)

    python_deps, shared_deps = gather_dep_libraries(raw_deps, resolve_versioned_deps = False)

    src_manifest = None
    bytecode_manifest = None

    python_internal_tools = ctx.attrs._python_internal_tools[PythonInternalToolsInfo]
    for artifact in python_internal_tools.runtime_library[ArtifactGroupInfo].artifacts:
        srcs[artifact.short_path] = artifact

    if srcs:
        src_manifest = create_manifest_for_source_map(ctx, "srcs", srcs)

        # TODO(T245694881) let the toolchain decide whether pyc's should be precompiled
        py_version = ctx.attrs._python_toolchain[PythonToolchainInfo].version
        if py_version == None or "3.15" not in py_version:
            bytecode_manifest = compile_manifests(ctx, [src_manifest])

    all_default_resources = {}
    all_standalone_resources = {}
    cxx_extra_resources = {}
    for cxx_resources in gather_resources(ctx.label, deps = raw_deps).values():
        for name, resource in cxx_resources.items():
            cxx_extra_resources[paths.join("__cxx_resources__", name)] = resource
    all_default_resources.update(cxx_extra_resources)
    all_standalone_resources.update(cxx_extra_resources)

    if default_resources:
        all_default_resources.update(default_resources)
    if standalone_resources:
        all_standalone_resources.update(standalone_resources)

    library_info = create_python_library_info(
        ctx.actions,
        ctx.label,
        srcs = src_manifest,
        src_types = src_manifest,
        default_resources = py_resources(ctx, all_default_resources) if all_default_resources else None,
        standalone_resources = py_resources(ctx, all_standalone_resources, "_standalone") if all_standalone_resources else None,
        bytecode = bytecode_manifest,
        deps = python_deps,
        shared_libraries = shared_deps,
        native_deps = merge_native_deps(ctx, raw_deps),
        is_native_dep = False,
        par_style = ctx.attrs.par_style,
    )

    source_db_no_deps = create_source_db_no_deps(ctx, srcs)

    return _convert_python_library_to_executable(
        ctx,
        _qualify_entry_point(
            main,
            ctx.attrs.base_module if ctx.attrs.base_module != None else ctx.label.package.replace("/", "."),
        ),
        library_info,
        raw_deps,
        compile,
        allow_cache_upload,
        src_manifest,
        python_deps,
        source_db_no_deps,
        executable_type,
    )

def _add_executable_subtargets(
        ctx,
        exe: PexProviders,
        dbg_source_db: DefaultInfo,
        dbg_source_db_output: Artifact | None,
        library_info: PythonLibraryInfo,
        main: EntryPoint,
        source_db_no_deps: DefaultInfo,
        src_manifest: ManifestInfo | None,
        python_deps: list[PythonLibraryInfo]) -> PexProviders:
    python_toolchain = ctx.attrs._python_toolchain[PythonToolchainInfo]
    exe = PexProviders(
        default_output = exe.default_output,
        other_outputs = exe.other_outputs,
        other_outputs_prefix = exe.other_outputs_prefix,
        hidden_resources = exe.hidden_resources,
        sub_targets = exe.sub_targets,
        run_cmd = cmd_args(python_toolchain.run_prefix, exe.run_cmd),
        dbg_source_db = dbg_source_db_output,
    )

    exe.sub_targets.update({
        "dbg-source-db": [dbg_source_db],
        "library-info": [library_info],
        "main": [DefaultInfo(default_output = ctx.actions.write_json("main.json", main))],
        "source-db-no-deps": [source_db_no_deps, create_python_source_db_info(library_info.manifests)],
    })

    # Type check
    type_checker = python_toolchain.type_checker
    if type_checker != None:
        exe.sub_targets.update({
            "typecheck": [
                create_per_target_type_check(
                    ctx,
                    type_checker,
                    src_manifest,
                    python_deps,
                    typeshed = python_toolchain.typeshed_stubs,
                    py_version = ctx.attrs.py_version_for_type_checking,
                    typing_enabled = ctx.attrs.typing,
                    sharding_enabled = ctx.attrs.shard_typing,
                ),
            ],
        })

    return exe

def _compute_pex_providers(
        ctx,
        src_manifest: ManifestInfo | None,
        python_deps: list[PythonLibraryInfo],
        source_db_no_deps: DefaultInfo,
        main: EntryPoint,
        compile: bool,
        library: PythonLibraryInfo,
        allow_cache_upload: bool,
        shared_libs: list[(SharedLibrary, str)],
        extensions: dict[str, (LinkedObject, Label)],
        link_args: list[LinkArgs],
        extra: dict[str, typing.Any],
        link_extra_artifacts: dict[str, typing.Any],
        executable_type: ExecutableType,
        linker_map_data = None) -> list[Provider] | Promise:
    dbg_source_db_output = ctx.actions.declare_output("dbg-db.json", has_content_based_path = True)
    dbg_source_db = create_dbg_source_db(ctx, dbg_source_db_output, src_manifest, python_deps)

    extra_artifacts = {key: value for key, value in link_extra_artifacts.items()}

    link_strategy = compute_link_strategy(ctx)
    build_args = ctx.attrs.build_args
    python_toolchain = ctx.attrs._python_toolchain[PythonToolchainInfo]
    python_internal_tools = ctx.attrs._python_internal_tools[PythonInternalToolsInfo]

    if link_strategy == NativeLinkStrategy("native"):
        entry_point = "runtime/bin/{}".format(ctx.attrs.executable_name)
        build_args.append(cmd_args("--passthrough=--runtime-binary={}".format(entry_point)))

    if dbg_source_db_output:
        extra_artifacts["dbg-db.json"] = dbg_source_db_output

    # Run lazy import analysis using the existing dbg-db.json only if the attribute is enabled
    if getattr(ctx.attrs, "lazy_imports_analyzer", None):
        lazy_import_analysis_output = ctx.actions.declare_output("safer_lazy_imports/lazy-import-analysis.json")
        run_lazy_imports_analyzer(ctx, dbg_source_db.other_outputs, lazy_import_analysis_output, dbg_source_db_output)
        if lazy_import_analysis_output:
            extra_artifacts["safer_lazy_imports/lazy-import-analysis.json"] = lazy_import_analysis_output

    extra_artifacts["sitecustomize.py"] = python_internal_tools.default_sitecustomize

    # Add bundled runtime
    if ctx.attrs.runtime_bundle:
        bundle = ctx.attrs.runtime_bundle[PythonRuntimeBundleInfo]
        build_args.append(cmd_args("--passthrough=--python-home=runtime"))
        extra_artifacts["runtime/bin/{}".format(bundle.py_bin.basename)] = bundle.py_bin
        extra_artifacts["runtime/lib/{}".format(bundle.stdlib.basename)] = bundle.stdlib
        if bundle.libpython:
            if link_strategy != NativeLinkStrategy("native"):
                build_args.append(cmd_args("--passthrough=--preload=runtime/lib/{}".format(bundle.libpython.basename)))
            extra_artifacts["runtime/lib/{}".format(bundle.libpython.basename)] = bundle.libpython
        if link_strategy != NativeLinkStrategy("native"):
            entry_point = "runtime/bin/{}".format(bundle.py_bin.basename)
            build_args.append(cmd_args(["--passthrough=--runtime-binary={}".format(entry_point)]))
            extra_artifacts[entry_point] = bundle.py_bin
        if ctx.attrs.runtime_bundle_full:
            extra_artifacts["runtime/include/{}".format(bundle.include.basename)] = bundle.include

    # Add additional runtime libs
    for name, dep in ctx.attrs.runtime_libs.items():
        dep_info = dep.get(DefaultInfo)
        if dep_info != None:
            extra_artifacts["runtime/lib/{}".format(name)] = dep_info.default_outputs[0]

    extra_manifests = create_manifest_for_source_map(ctx, "extra_manifests", extra_artifacts)
    package_style = get_package_style(ctx)

    # Strip native libraries and extensions and update the .gnu_debuglink references if we are extracting
    # debug symbols from the par
    debuginfo_files = []
    debuginfos = {}
    if ctx.attrs.strip_libpar == "extract" and package_style == PackageStyle("standalone") and cxx_is_gnu(ctx):
        stripped_shlibs = []
        for shlib, libdir in shared_libs:
            name = paths.join(
                libdir,
                value_or(
                    shlib.soname.as_str(),
                    shlib.lib.unstripped_output.short_path,
                ),
            )
            existing = debuginfos.get(name)
            if existing == None:
                stripped, debuginfo = strip_debug_with_gnu_debuglink(
                    ctx = ctx,
                    name = name,
                    obj = shlib.lib.unstripped_output,
                )
                debuginfos[name] = (stripped, debuginfo)
            else:
                stripped, debuginfo = existing
            shlib = SharedLibrary(
                soname = shlib.soname,
                label = shlib.label,
                lib = LinkedObject(
                    output = stripped,
                    unstripped_output = shlib.lib.unstripped_output,
                    dwp = shlib.lib.dwp,
                ),
            )
            stripped_shlibs.append((shlib, libdir))
            debuginfo_files.append(((libdir, shlib, ".debuginfo"), debuginfo))
        shared_libs = stripped_shlibs
        stripped_extensions = {}
        for name, (extension, label) in extensions.items():
            stripped, debuginfo = strip_debug_with_gnu_debuglink(
                ctx = ctx,
                name = name,
                obj = extension.unstripped_output,
            )
            stripped_extensions[name] = (
                LinkedObject(
                    output = stripped,
                    unstripped_output = extension.unstripped_output,
                    dwp = extension.dwp,
                ),
                label,
            )
            debuginfo_files.append((name + ".debuginfo", debuginfo))
        extensions = stripped_extensions

    # Combine sources and extensions into a map of all modules.
    pex_modules = PexModules(
        manifests = manifests_to_interface(library.manifests),
        extra_manifests = extra_manifests,
        compile = compile,
        extensions = create_manifest_for_extensions(
            ctx,
            extensions,
            dwp = ctx.attrs.package_split_dwarf_dwp,
        ) if extensions else None,
    )

    # Convert preloaded deps to a set of their names to be loaded by.
    preload_labels = set([_linkable_graph(d).label for d in ctx.attrs.preload_deps if _linkable_graph(d)])

    # Build the PEX.
    pex = make_py_package(
        ctx = ctx,
        python_toolchain = python_toolchain,
        python_internal_tools = python_internal_tools,
        make_py_package_cmd = ctx.attrs.make_py_package[RunInfo] if ctx.attrs.make_py_package != None else None,
        package_style = package_style,
        build_args = build_args,
        pex_modules = pex_modules,
        shared_libraries = shared_libs,
        preload_labels = preload_labels,
        main = main,
        allow_cache_upload = allow_cache_upload,
        debuginfo_files = debuginfo_files,
        link_args = link_args,
    )

    pex.sub_targets.update(extra)

    if linker_map_data != None:
        pex.sub_targets["linker-map"] = [DefaultInfo(
            default_output = linker_map_data.map,
            other_outputs = [linker_map_data.binary],
        )]

    updated_pex = _add_executable_subtargets(ctx, pex, dbg_source_db, dbg_source_db_output, library, main, source_db_no_deps, src_manifest, python_deps)

    return compute_providers(ctx, updated_pex, executable_type)

def _convert_python_library_to_executable(
        ctx: AnalysisContext,
        main: EntryPoint,
        library: PythonLibraryInfo,
        deps: list[Dependency],
        compile: bool,
        allow_cache_upload: bool,
        src_manifest: ManifestInfo | None,
        python_deps: list[PythonLibraryInfo],
        source_db_no_deps: DefaultInfo,
        executable_type: ExecutableType) -> list[Provider] | Promise:
    extra = {}

    python_toolchain = ctx.attrs._python_toolchain[PythonToolchainInfo]
    python_internal_tools = ctx.attrs._python_internal_tools[PythonInternalToolsInfo]
    package_style = get_package_style(ctx)

    extra_artifacts = {}
    link_args = []
    link_strategy = compute_link_strategy(ctx)

    if link_strategy == NativeLinkStrategy("native"):
        use_anon_target = getattr(ctx.attrs, "use_anon_target_for_analysis", False)
        if use_anon_target:
            # For caching link groups, we just need to pass cxx_deps
            native_deps = {}
            for dep in library.native_deps.traverse():
                native_deps.update(dep.native_deps)

            explicit_attrs = {
                "allow_cache_upload": allow_cache_upload,
                "deps": list(native_deps.values()),
                "name": "python_linking:" + ctx.attrs.name,
                "package_style": package_style,
                "python_toolchain": ctx.attrs._python_toolchain,
                "rpath": ctx.attrs.name,
                "static_extension_utils": ctx.attrs.static_extension_utils,
                "transformation_spec": ctx.attrs.transformation_spec,
                "_cxx_toolchain": ctx.attrs._cxx_toolchain,
                "_python_internal_tools": ctx.attrs._python_internal_tools,
            }
            implicit_attrs = {
                a: getattr(ctx.attrs, a)
                for a in (set(cxx_implicit_attrs.keys()) | set(python_implicit_attrs.keys())) - set(explicit_attrs.keys())
            }
            return ctx.actions.anon_target(
                process_native_linking_rule,
                explicit_attrs | implicit_attrs,
            ).promise.map(lambda providers: _compute_pex_providers(
                ctx,
                src_manifest,
                python_deps,
                source_db_no_deps,
                main,
                compile,
                library,
                allow_cache_upload,
                providers[LinkProviders].shared_libraries,
                providers[LinkProviders].extensions,
                providers[LinkProviders].link_args,
                providers[LinkProviders].extra,
                providers[LinkProviders].extra_artifacts,
                executable_type,
                providers[LinkProviders].linker_map_data,
            ))
        else:
            shared_libs, extensions, link_args, extra, extra_artifacts, linker_map_data = process_native_linking(
                ctx,
                deps,
                python_toolchain,
                python_internal_tools,
                package_style,
                allow_cache_upload,
            )
            if ctx.attrs.runtime_bundle:
                runtime_bundle = ctx.attrs.runtime_bundle[PythonRuntimeBundleInfo]

                # On some platforms libpython does not link with system libraries. Include libraries
                # in the PAR in this case.
                for dep in runtime_bundle.shared_libs:
                    lib = dep.get(DefaultInfo).default_outputs[0]
                    shared_libs.append((
                        # There's probably a smarter way to get the shared library object out of the
                        # dependency, but I'm not sure what that is.
                        SharedLibrary(
                            soname = to_soname(lib.basename),
                            label = dep.label,
                            lib = LinkedObject(output = lib, unstripped_output = lib),
                        ),
                        "",
                    ))

    else:
        linker_map_data = None
        extensions = {}
        for manifest in library.manifests.traverse():
            if manifest.extensions:
                _merge_extensions(extensions, manifest.label, manifest.extensions)
        if link_strategy == NativeLinkStrategy("merged"):
            shared_libs, extensions = process_omnibus_linking(ctx, deps, extensions, python_toolchain, extra)
        else:
            shared_libs = [
                (shared_lib, "")
                for shared_lib in traverse_shared_library_info(library.shared_libraries, transformation_provider = None)
            ]

            # darwin and windows expect self-contained dynamically linked
            # python extensions without additional transitive shared libraries
            shared_libs += [
                (extension_shared_lib, "")
                for extension_shared_lib in traverse_shared_library_info(library.extension_shared_libraries, transformation_provider = None)
            ]

    return _compute_pex_providers(
        ctx,
        src_manifest,
        python_deps,
        source_db_no_deps,
        main,
        compile,
        library,
        allow_cache_upload,
        shared_libs,
        extensions,
        link_args,
        extra,
        extra_artifacts,
        executable_type,
        linker_map_data = linker_map_data if link_strategy == NativeLinkStrategy("native") else None,
    )

def python_binary_impl(ctx: AnalysisContext) -> list[Provider] | Promise:
    main_module = ctx.attrs.main_module
    main_function = ctx.attrs.main_function
    if main_module != None and ctx.attrs.main != None:
        fail("Only one of main_module or main may be set. Prefer main_function as main and main_module are considered deprecated")
    elif main_module != None and main_function != None:
        fail("Only one of main_module or main_function may be set. Prefer main_function.")
    elif ctx.attrs.main != None and main_function == None:
        main_module = "." + ctx.attrs.main.short_path.replace("/", ".")
        if main_module.endswith(".py"):
            main_module = main_module[:-3]

    # if "python-version=3.8" in ctx.attrs.labels:
    #     # buildifier: disable=print
    #     print((
    #         "\033[1;33m \u26A0 [Warning] " +
    #         "{0} 3.8 is EOL, and is going away by the end of H1 2024. " +
    #         "This build triggered //{1}:{2} which still uses {0} 3.8. " +
    #         "Make sure someone (you or the appropriate maintainers) upgrades it to {0} 3.10 soon to avoid breakages. " +
    #         "https://fburl.com/python-eol \033[0m"
    #     ).format(
    #         "Cinder" if "python-flavor=cinder" in ctx.attrs.labels else "Python",
    #         ctx.label.package,
    #         ctx.attrs.name,
    #     ))

    if main_module != None:
        main = (EntryPointKind("module"), main_module)
    else:
        main = (EntryPointKind("function"), main_function)

    srcs = {}
    if ctx.attrs.main != None:
        srcs[ctx.attrs.main.short_path] = ctx.attrs.main
    srcs = qualify_srcs(ctx.label, ctx.attrs.base_module, srcs)
    default_resources_map, standalone_resources_map = py_attr_resources(ctx)
    standalone_resources = qualify_srcs(ctx.label, ctx.attrs.base_module, standalone_resources_map)
    default_resources = qualify_srcs(ctx.label, ctx.attrs.base_module, default_resources_map)

    cxx_toolchain_info = get_opt_cxx_toolchain_info(ctx)
    toolchain_allow_cache_upload = cxx_toolchain_info.cxx_compiler_info.allow_cache_upload if cxx_toolchain_info else None

    return python_executable(
        ctx,
        main,
        srcs,
        default_resources,
        standalone_resources,
        compile = value_or(ctx.attrs.compile, False),
        allow_cache_upload = cxx_attrs_get_allow_cache_upload(ctx.attrs, toolchain_allow_cache_upload),
        executable_type = ExecutableType("binary"),
    )

# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(
    "@prelude//:artifacts.bzl",
    "ArtifactGroupInfo",
    "ArtifactOutputs",  # @unused Used as a type
)
load("@prelude//:paths.bzl", "paths")
load("@prelude//:resources.bzl", "gather_resources")
load(
    "@prelude//cxx:cxx_library_utility.bzl",
    "cxx_is_gnu",
)
load("@prelude//cxx:cxx_toolchain_types.bzl", "CxxToolchainInfo")
load("@prelude//cxx:cxx_utility.bzl", "cxx_attrs_get_allow_cache_upload")
load(
    "@prelude//linking:link_info.bzl",
    "LinkedObject",
)
load(
    "@prelude//linking:linkable_graph.bzl",
    _linkable_graph = "linkable_graph",
)
load(
    "@prelude//linking:shared_libraries.bzl",
    "SharedLibrary",
    "traverse_shared_library_info",
)
load("@prelude//linking:strip.bzl", "strip_debug_with_gnu_debuglink")
load("@prelude//python/linking:native.bzl", "process_native_linking")
load("@prelude//python/linking:omnibus.bzl", "process_omnibus_linking")
load("@prelude//utils:utils.bzl", "flatten", "value_or")
load(":compile.bzl", "compile_manifests")
load(
    ":interface.bzl",
    "EntryPoint",
    "EntryPointKind",
)
load(":make_py_package.bzl", "PexModules", "PexProviders", "make_default_info", "make_py_package", "make_run_info")
load(
    ":manifest.bzl",
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
load(":source_db.bzl", "create_dbg_source_db", "create_python_source_db_info", "create_source_db_no_deps")
load(":toolchain.bzl", "NativeLinkStrategy", "PackageStyle", "PythonPlatformInfo", "PythonToolchainInfo", "get_package_style", "get_platform_attr")
load(":typing.bzl", "create_per_target_type_check")
load(":versions.bzl", "LibraryName", "LibraryVersion", "gather_versioned_dependencies", "resolve_versions")

def _link_strategy(ctx: AnalysisContext) -> NativeLinkStrategy | None:
    if ctx.attrs._cxx_toolchain.get(CxxToolchainInfo) == None:
        # cxx toolchain is required
        return None

    return NativeLinkStrategy(
        ctx.attrs.native_link_strategy or ctx.attrs._python_toolchain[PythonToolchainInfo].native_link_strategy,
    )

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
        allow_cache_upload: bool) -> PexProviders:
    # Returns a three tuple: the Python binary, all its potential runtime files,
    # and a provider for its source DB.

    # TODO(nmj): See if people are actually setting cxx_platform here. Really
    #                 feels like it should be a property of the python platform
    python_platform = ctx.attrs._python_toolchain[PythonPlatformInfo]
    cxx_toolchain = ctx.attrs._cxx_toolchain

    raw_deps = ctx.attrs.deps

    raw_deps.extend(flatten(
        get_platform_attr(python_platform, cxx_toolchain, ctx.attrs.platform_deps),
    ))

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

    python_toolchain = ctx.attrs._python_toolchain[PythonToolchainInfo]
    if python_toolchain.runtime_library and ArtifactGroupInfo in python_toolchain.runtime_library:
        for artifact in python_toolchain.runtime_library[ArtifactGroupInfo].artifacts:
            srcs[artifact.short_path] = artifact

    if srcs:
        src_manifest = create_manifest_for_source_map(ctx, "srcs", srcs)
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
    )

    source_db_no_deps = create_source_db_no_deps(ctx, srcs)

    dbg_source_db_output = ctx.actions.declare_output("dbg-db.json")
    dbg_source_db = create_dbg_source_db(ctx, dbg_source_db_output, src_manifest, python_deps)

    exe = _convert_python_library_to_executable(
        ctx,
        _qualify_entry_point(
            main,
            ctx.attrs.base_module if ctx.attrs.base_module != None else ctx.label.package.replace("/", "."),
        ),
        library_info,
        raw_deps,
        compile,
        allow_cache_upload,
        dbg_source_db_output,
    )

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

def _convert_python_library_to_executable(
        ctx: AnalysisContext,
        main: EntryPoint,
        library: PythonLibraryInfo,
        deps: list[Dependency],
        compile: bool,
        allow_cache_upload: bool,
        dbg_source_db: [Artifact, None]) -> PexProviders:
    extra = {}

    python_toolchain = ctx.attrs._python_toolchain[PythonToolchainInfo]
    package_style = get_package_style(ctx)

    # Convert preloaded deps to a set of their names to be loaded by.
    preload_labels = {_linkable_graph(d).label: None for d in ctx.attrs.preload_deps if _linkable_graph(d)}

    extra_artifacts = {}
    link_args = []
    link_strategy = _link_strategy(ctx)

    if link_strategy == NativeLinkStrategy("native"):
        shared_libs, extensions, link_args = process_native_linking(
            ctx,
            deps,
            python_toolchain,
            extra,
            package_style,
            allow_cache_upload,
            extra_artifacts,
        )
    else:
        extensions = {}
        for manifest in library.manifests.traverse():
            if manifest.extensions:
                _merge_extensions(extensions, manifest.label, manifest.extensions)
        if link_strategy == NativeLinkStrategy("merged"):
            shared_libs, extensions = process_omnibus_linking(ctx, deps, extensions, python_toolchain, extra)
        else:
            shared_libs = [
                ("", shared_lib)
                for shared_lib in traverse_shared_library_info(library.shared_libraries)
            ]

            # darwin and windows expect self-contained dynamically linked
            # python extensions without additional transitive shared libraries
            shared_libs += [
                ("", extension_shared_lib)
                for extension_shared_lib in traverse_shared_library_info(library.extension_shared_libraries)
            ]

    if dbg_source_db:
        extra_artifacts["dbg-db.json"] = dbg_source_db

    if python_toolchain.default_sitecustomize != None:
        extra_artifacts["sitecustomize.py"] = python_toolchain.default_sitecustomize

    extra_manifests = create_manifest_for_source_map(ctx, "extra_manifests", extra_artifacts)

    # Create the map of native libraries to their artifacts and whether they
    # need to be preloaded.  Note that we merge preload deps into regular deps
    # above, before gathering up all native libraries, so we're guaranteed to
    # have all preload libraries (and their transitive deps) here.
    shared_libs = [
        (libdir, shlib, shlib.label in preload_labels)
        for libdir, shlib in shared_libs
    ]

    # Strip native libraries and extensions and update the .gnu_debuglink references if we are extracting
    # debug symbols from the par
    debuginfo_files = []
    debuginfos = {}
    if ctx.attrs.strip_libpar == "extract" and package_style == PackageStyle("standalone") and cxx_is_gnu(ctx):
        stripped_shlibs = []
        for libdir, shlib, preload in shared_libs:
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
            stripped_shlibs.append((libdir, shlib, preload))
            debuginfo_files.append(((libdir, shlib, ".debuginfo"), debuginfo))
        shared_libs = stripped_shlibs
        for name, (extension, label) in extensions.items():
            stripped, debuginfo = strip_debug_with_gnu_debuglink(
                ctx = ctx,
                name = name,
                obj = extension.unstripped_output,
            )
            extensions[name] = (
                LinkedObject(
                    output = stripped,
                    unstripped_output = extension.unstripped_output,
                    dwp = extension.dwp,
                ),
                label,
            )
            debuginfo_files.append((name + ".debuginfo", debuginfo))

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

    # Build the PEX.
    pex = make_py_package(
        ctx = ctx,
        python_toolchain = python_toolchain,
        make_py_package_cmd = ctx.attrs.make_py_package[RunInfo] if ctx.attrs.make_py_package != None else None,
        package_style = package_style,
        build_args = ctx.attrs.build_args,
        pex_modules = pex_modules,
        shared_libraries = shared_libs,
        main = main,
        allow_cache_upload = allow_cache_upload,
        debuginfo_files = debuginfo_files,
        link_args = link_args,
    )

    pex.sub_targets.update(extra)

    return pex

def python_binary_impl(ctx: AnalysisContext) -> list[Provider]:
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

    pex = python_executable(
        ctx,
        main,
        srcs,
        default_resources,
        standalone_resources,
        compile = value_or(ctx.attrs.compile, False),
        allow_cache_upload = cxx_attrs_get_allow_cache_upload(ctx.attrs),
    )
    return [
        make_default_info(pex),
        make_run_info(pex, ctx.attrs.run_with_inplace),
    ]

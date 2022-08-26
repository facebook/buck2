load("@prelude//:paths.bzl", "paths")
load("@prelude//:resources.bzl", "gather_resources")
load("@prelude//cxx:cxx_toolchain_types.bzl", "CxxPlatformInfo")
load(
    "@prelude//cxx:omnibus.bzl",
    "add_omnibus_exclusions",
    "add_omnibus_roots",
    "create_omnibus_libraries",
)
load(
    "@prelude//linking:linkable_graph.bzl",
    "create_merged_linkable_graph",
)
load(
    "@prelude//utils:types.bzl",
    "unchecked",  # @unused Used as a type
)
load("@prelude//utils:utils.bzl", "expect", "flatten", "value_or")
load(":compile.bzl", "compile_manifests")
load(
    ":interface.bzl",
    "PythonLibraryInterface",  # @unused Used as a type
)
load(":make_pex.bzl", "PexModules", "make_pex")
load(
    ":manifest.bzl",
    "create_manifest_for_extensions",
    "create_manifest_for_source_dir",
    "create_manifest_for_source_map",
)
load(":python.bzl", "info_to_interface")
load(
    ":python_library.bzl",
    "create_python_library_info",
    "gather_dep_libraries",
    "py_resources",
    "qualify_srcs",
)
load(":source_db.bzl", "create_source_db_deps")
load(":toolchain.bzl", "NativeLinkStrategy", "PackageStyle", "PythonPlatformInfo", "PythonToolchainInfo", "get_platform_attr")

def _link_strategy(ctx: "context") -> NativeLinkStrategy.type:
    if ctx.attrs.native_link_strategy != None:
        return NativeLinkStrategy(ctx.attrs.native_link_strategy)
    return NativeLinkStrategy(ctx.attrs._python_toolchain[PythonToolchainInfo].native_link_strategy)

def _package_style(ctx: "context") -> PackageStyle.type:
    if ctx.attrs.package_style != None:
        return PackageStyle(ctx.attrs.package_style.lower())
    return PackageStyle(ctx.attrs._python_toolchain[PythonToolchainInfo].package_style)

# We do a lot of merging extensions, so don't use O(n) type annotations
def _merge_extensions(
        extensions: unchecked({str.type: ("_a", "label")}),
        incoming_label: unchecked("label"),
        incoming_extensions: unchecked({str.type: "_a"})) -> None:
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

def python_executable(
        ctx: "context",
        main_module: str.type,
        srcs: {str.type: "artifact"},
        resources: {str.type: ("artifact", ["_arglike"])},
        compile: bool.type = False) -> ("artifact", ["_arglike"], {str.type: ["provider"]}):
    # Returns a three tuple: the Python binary, all its potential runtime files,
    # and a provider for its source DB.

    # TODO(nmj): See if people are actually setting cxx_platform here. Really
    #                 feels like it should be a property of the python platform
    python_platform = ctx.attrs._python_toolchain[PythonPlatformInfo]
    cxx_platform = ctx.attrs._cxx_toolchain[CxxPlatformInfo]

    raw_deps = (
        [ctx.attrs.deps] +
        get_platform_attr(python_platform, cxx_platform, ctx.attrs.platform_deps)
    )

    # `preload_deps` is used later to configure `LD_PRELOAD` environment variable,
    # here we make the actual libraries to appear in the distribution.
    # TODO: make fully consistent with its usage later
    raw_deps.append(ctx.attrs.preload_deps)
    python_deps, shared_deps = gather_dep_libraries(raw_deps)

    src_manifest = None
    bytecode_manifest = None
    if srcs:
        src_manifest = create_manifest_for_source_map(ctx, "srcs", srcs)
        bytecode_manifest = create_manifest_for_source_dir(
            ctx,
            "bytecode",
            compile_manifests(ctx, [src_manifest]),
        )

    all_resources = {}
    all_resources.update(resources)
    for cxx_resources in gather_resources(ctx.label, deps = flatten(raw_deps)).values():
        for name, resource in cxx_resources.items():
            all_resources[paths.join("__cxx_resources__", name)] = resource

    library_info = create_python_library_info(
        ctx.actions,
        ctx.label,
        srcs = src_manifest,
        resources = py_resources(ctx, all_resources) if all_resources else None,
        bytecode = bytecode_manifest,
        deps = python_deps,
        shared_libraries = shared_deps,
    )

    source_db = create_source_db_deps(ctx, src_manifest, python_deps)

    output, runtime_files, extra = convert_python_library_to_executable(
        ctx,
        main_module,
        info_to_interface(library_info),
        flatten(raw_deps),
        compile,
    )

    extra["source-db"] = [source_db]

    return (output, runtime_files, extra)

# Note that this is used by rules outside of prelude (e.g., aienv_layer).
# aienv_layer potentially uses all of this except omnibus linking.
def convert_python_library_to_executable(
        ctx: "context",
        main_module: "string",
        library: PythonLibraryInterface.type,
        deps: ["dependency"],
        compile: bool.type = False):
    extra = {}

    # Returns a three tuple: the Python binary, all its potential runtime files,
    # and subtarget providers.
    python_toolchain = ctx.attrs._python_toolchain[PythonToolchainInfo]
    package_style = _package_style(ctx)

    # TODO(nmj): base_module / main should probably just not be supported
    #                 they were deprecated before, so leave it at that.

    output = ctx.actions.declare_output("{}{}".format(ctx.attrs.name, python_toolchain.pex_extension))

    # Convert preloaded deps to a set of their names to be loaded by.
    preload_labels = {d.label: None for d in ctx.attrs.preload_deps}
    preload_names = {
        name: None
        for name, shared_lib in library.shared_libraries().items()
        if shared_lib.label in preload_labels
    }

    extensions = {}
    for manifest in library.iter_manifests():
        if manifest.extensions:
            _merge_extensions(extensions, manifest.label, manifest.extensions)

    # If we're using omnibus linking, re-link libraries and extensions and
    # update the libraries we'll pull into the final binary.
    if _link_strategy(ctx) == NativeLinkStrategy("merged"):
        # Collect omnibus info from deps.
        linkable_graph = create_merged_linkable_graph(ctx.label, deps)

        # Add in any potential native root targets from our first-order deps.
        add_omnibus_roots(linkable_graph, deps)

        # Exclude preloaded deps from omnibus linking, to prevent preloading
        # the monolithic omnibus library.
        add_omnibus_exclusions(linkable_graph, ctx.attrs.preload_deps)

        # Link omnibus libraries.
        omnibus_libs = create_omnibus_libraries(
            ctx,
            linkable_graph,
            ctx.attrs.linker_flags,
            prefer_stripped_objects = ctx.attrs.prefer_stripped_native_objects,
        )

        # Extract re-linked extensions.
        extensions = {
            dest: (omnibus_libs.roots[label].shared_library, label)
            for dest, (_, label) in extensions.items()
        }
        native_libs = omnibus_libs.libraries

        if python_toolchain.emit_omnibus_metadata:
            exclusion_roots = ctx.actions.write_json("omnibus/exclusion_roots.json", omnibus_libs.exclusion_roots)
            extra["omnibus-exclusion-roots"] = [DefaultInfo(default_outputs = [exclusion_roots])]

            roots = ctx.actions.write_json("omnibus/roots.json", omnibus_libs.roots)
            extra["omnibus-roots"] = [DefaultInfo(default_outputs = [roots])]

            omnibus_excluded = ctx.actions.write_json("omnibus/excluded.json", omnibus_libs.excluded)
            extra["omnibus-excluded"] = [DefaultInfo(default_outputs = [omnibus_excluded])]

            linkable_graph = ctx.actions.write_json("linkable_graph.json", linkable_graph)
            extra["linkable-graph"] = [DefaultInfo(default_outputs = [linkable_graph])]
    else:
        native_libs = {name: shared_lib.lib for name, shared_lib in library.shared_libraries().items()}

    # Combine sources and extensions into a map of all modules.
    pex_modules = PexModules(
        manifests = library.manifests(),
        compile = compile,
        extensions = create_manifest_for_extensions(
            ctx,
            extensions,
            dwp = ctx.attrs.package_split_dwarf_dwp,
        ) if extensions else None,
    )

    has_hidden_resources = library.has_hidden_resources()
    hidden_resources = library.hidden_resources()

    # Create the map of native libraries to their artifacts and whether they
    # need to be preloaded.  Note that we merge preload deps into regular deps
    # above, before gathering up all native libraries, so we're guaranteed to
    # have all preload libraries (and their transitive deps) here.
    shared_libraries = {}
    for name, lib in native_libs.items():
        shared_libraries[name] = lib, name in preload_names

    # If we're building an inplace binary, create a symlink tree and record all
    # inputs as runtime files.
    symlink_tree_path = None
    runtime_files = []
    if package_style != PackageStyle("standalone"):
        if not ctx.attrs.bundled_runtime:
            # TODO(nmj): Not quite the right name, deal with legacy_output_path if necessary
            symlink_tree_path = ctx.actions.declare_output("{}#link-tree".format(ctx.attrs.name))
            runtime_files.append(symlink_tree_path)
        runtime_files.extend(hidden_resources)

    else:
        # Standalone PEXs don't know how to handle hidden/extra manifest outputs.
        expect(
            not has_hidden_resources,
            "Cannot package hidden srcs/resources in a standalone python_binary. " +
            'Eliminate resources in non-Python dependencies of this binary, or use `package_style = "inplace"`.',
        )

    # Build the PEX.
    hidden = make_pex(
        ctx,
        python_toolchain,
        ctx.attrs.bundled_runtime,
        package_style,
        ctx.attrs.build_args,
        pex_modules,
        shared_libraries,
        main_module,
        output,
        symlink_tree_path,
    )

    runtime_files.extend(hidden)

    return output, runtime_files, extra

def python_binary_impl(ctx: "context") -> ["provider"]:
    main_module = ctx.attrs.main_module
    if ctx.attrs.main_module != None and ctx.attrs.main != None:
        fail("Only one of main_module or main may be set. Prefer main_module as main is considered deprecated")
    elif ctx.attrs.main != None:
        base_module = ctx.attrs.base_module
        if base_module == None:
            base_module = ctx.label.package.replace("/", ".")
        if base_module != "":
            base_module += "."
        main_module = base_module + ctx.attrs.main.short_path.replace("/", ".")
        if main_module.endswith(".py"):
            main_module = main_module[:-3]

    srcs = {}
    if ctx.attrs.main != None:
        srcs[ctx.attrs.main.short_path] = ctx.attrs.main
    srcs = qualify_srcs(ctx.label, ctx.attrs.base_module, srcs)

    output, runtime_files, extra = python_executable(
        ctx,
        main_module,
        srcs,
        {},
        compile = value_or(ctx.attrs.compile, False),
    )

    return [
        DefaultInfo(
            default_outputs = [output],
            other_outputs = runtime_files,
            sub_targets = extra,
        ),
        RunInfo(cmd_args(output).hidden(runtime_files)),
    ]

# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//cxx:compile.bzl", "CxxSrcWithFlags")
load("@prelude//cxx:cxx.bzl", "create_shared_lib_link_group_specs")
load("@prelude//cxx:cxx_context.bzl", "get_cxx_toolchain_info")
load("@prelude//cxx:cxx_executable.bzl", "cxx_executable")
load("@prelude//cxx:cxx_toolchain_types.bzl", "CxxPlatformInfo")
load(
    "@prelude//cxx:cxx_types.bzl",
    "CxxRuleConstructorParams",
)
load(
    "@prelude//cxx:groups.bzl",
    "Group",
    "GroupAttrs",
    "GroupMapping",
    "GroupRoot",
    "Traversal",
    "compute_mappings",
)
load("@prelude//cxx:headers.bzl", "cxx_get_regular_cxx_headers_layout")
load(
    "@prelude//cxx:link_groups.bzl",
    "LinkGroupInfo",  # @unused Used as a type
    "LinkGroupLibSpec",
    "get_link_group_info",
    "make_link_group_info",
)
load("@prelude//cxx:linker.bzl", "get_rpath_origin")
load(
    "@prelude//cxx:omnibus.bzl",
    "create_omnibus_libraries",
    "get_excluded",
    "get_omnibus_graph",
    "get_roots",
)
load(
    "@prelude//cxx:preprocessor.bzl",
    "CPreprocessor",
    "cxx_inherited_preprocessor_infos",
)
load(
    "@prelude//linking:link_info.bzl",
    "LinkedObject",  # @unused Used as a type
)
load(
    "@prelude//linking:linkable_graph.bzl",
    "DlopenableLibraryInfo",
    "LinkableGraph",
    "LinkableGraphTSet",
    "create_linkable_graph",
    "get_linkable_graph_node_map_func",
)
load(
    "@prelude//linking:linkables.bzl",
    "LinkableProviders",  # @unused Used as a type
    "linkables",
)
load(
    "@prelude//utils:types.bzl",
    "unchecked",  # @unused Used as a type
)
load("@prelude//utils:utils.bzl", "flatten", "value_or")
load("@prelude//paths.bzl", "paths")
load("@prelude//resources.bzl", "gather_resources")
load(":compile.bzl", "compile_manifests")
load(
    ":interface.bzl",
    "PythonLibraryInterface",  # @unused Used as a type
)
load(":make_pex.bzl", "PexModules", "PexProviders", "make_pex")
load(
    ":manifest.bzl",
    "create_manifest_for_extensions",
    "create_manifest_for_source_dir",
    "create_manifest_for_source_map",
)
load(":native_python_util.bzl", "merge_cxx_extension_info")
load(":python.bzl", "info_to_interface")
load(
    ":python_library.bzl",
    "create_python_library_info",
    "gather_dep_libraries",
    "py_resources",
    "qualify_srcs",
)
load(":source_db.bzl", "create_source_db", "create_source_db_no_deps")
load(":toolchain.bzl", "NativeLinkStrategy", "PackageStyle", "PythonPlatformInfo", "PythonToolchainInfo", "get_platform_attr")

OmnibusMetadataInfo = provider(fields = ["omnibus_libs", "omnibus_graph"])

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

def _get_root_link_group_specs(
        libs: [LinkableProviders.type],
        extensions: {str.type: LinkableProviders.type}) -> [LinkGroupLibSpec.type]:
    """
    Walk the linkable graph finding dlopen-able C++ libs.
    """

    # TODO(agallagher): We should handle `allow_embedding = False` C++ extensions
    # here too.

    specs = []

    # Add link group specs for dlopen-able libs.
    for dep in libs:
        specs.append(
            LinkGroupLibSpec(
                name = dep.linkable_root_info.name,
                is_shared_lib = True,
                root = dep.linkable_root_info,
                group = Group(
                    name = dep.linkable_root_info.name,
                    mappings = [
                        GroupMapping(
                            root = GroupRoot(
                                label = dep.linkable_graph.nodes.value.label,
                                node = dep,
                            ),
                            traversal = Traversal("node"),
                        ),
                    ],
                    # TODO(@christylee): Add attributes to python dlopen-able libs
                    attrs = GroupAttrs(
                        enable_distributed_thinlto = False,
                    ),
                ),
            ),
        )

    # Add link group specs for extensions.
    for name, extension in extensions.items():
        specs.append(
            LinkGroupLibSpec(
                name = name,
                is_shared_lib = False,
                group = Group(
                    name = name,
                    mappings = [
                        GroupMapping(
                            root = GroupRoot(
                                label = extension.linkable_graph.nodes.value.label,
                                node = extension,
                            ),
                            traversal = Traversal("node"),
                        ),
                    ],
                    # TODO(@christylee): Add attributes to extensions
                    attrs = GroupAttrs(
                        enable_distributed_thinlto = False,
                    ),
                ),
            ),
        )

    return specs

def _get_link_group_info(
        ctx: "context",
        link_deps: [LinkableProviders.type],
        libs: [LinkableProviders.type],
        extensions: {str.type: LinkableProviders.type}) -> (LinkGroupInfo.type, [LinkGroupLibSpec.type]):
    """
    Return the `LinkGroupInfo` and link group lib specs to use for this binary.
    This will handle parsing the various user-specific parameters and automatic
    link group lib spec generation for dlopen-enabled native libraries and,
    eventually, extensions.
    """

    link_group_info = get_link_group_info(ctx, [d.linkable_graph for d in link_deps])
    link_group_specs = []

    # Add link group specs from user-provided link group info.
    if link_group_info != None:
        link_group_specs.extend(create_shared_lib_link_group_specs(ctx, link_group_info))

    # Add link group specs from dlopenable C++ libraries.
    root_specs = _get_root_link_group_specs(libs, extensions)

    # (Re-)build the link group info
    if root_specs or link_group_info == None:
        # We prepend the dlopen roots, so that they take precedence over
        # user-specific ones.
        link_group_specs = root_specs + link_group_specs

        # Regenerate the new `LinkGroupInfo` with the new link group lib
        # groups.
        linkable_graph = LinkableGraph(
            #label = ctx.label,
            nodes = ctx.actions.tset(
                LinkableGraphTSet,
                children = (
                    [d.linkable_graph.nodes for d in link_deps] +
                    [d.linkable_graph.nodes for d in libs] +
                    [d.linkable_graph.nodes for d in extensions.values()]
                ),
            ),
        )
        linkable_graph_node_map = get_linkable_graph_node_map_func(linkable_graph)()

        # We add user-defined mappings last, so that our auto-generated
        # ones get precedence (as we rely on this for things to work).
        link_groups = [s.group for s in root_specs]
        if link_group_info != None:
            link_groups += link_group_info.groups

        mappings = compute_mappings(
            groups = link_groups,
            graph_map = linkable_graph_node_map,
        )
        link_group_info = make_link_group_info(
            groups = link_groups,
            mappings = mappings,
        )

    return (link_group_info, link_group_specs)

def python_executable(
        ctx: "context",
        main_module: str.type,
        srcs: {str.type: "artifact"},
        resources: {str.type: ("artifact", ["_arglike"])},
        compile: bool.type = False) -> PexProviders.type:
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

    source_db = create_source_db(ctx, src_manifest, python_deps)
    source_db_no_deps = create_source_db_no_deps(ctx, srcs)

    exe = convert_python_library_to_executable(
        ctx,
        main_module,
        info_to_interface(library_info),
        flatten(raw_deps),
        compile,
    )
    exe.sub_targets.update({
        "source-db": [source_db],
        "source-db-no-deps": [source_db_no_deps, library_info],
    })

    return exe

def convert_python_library_to_executable(
        ctx: "context",
        main_module: "string",
        library: PythonLibraryInterface.type,
        deps: ["dependency"],
        compile: bool.type = False) -> PexProviders.type:
    extra = {}

    python_toolchain = ctx.attrs._python_toolchain[PythonToolchainInfo]
    package_style = _package_style(ctx)

    # Convert preloaded deps to a set of their names to be loaded by.
    preload_labels = {d.label: None for d in ctx.attrs.preload_deps}
    preload_names = {
        name: None
        for name, shared_lib in library.shared_libraries().items()
        if shared_lib.label in preload_labels
    }

    extensions = {}
    extra_manifests = None
    for manifest in library.iter_manifests():
        if manifest.extensions:
            _merge_extensions(extensions, manifest.label, manifest.extensions)

    # If we're using omnibus linking, re-link libraries and extensions and
    # update the libraries we'll pull into the final binary.
    if _link_strategy(ctx) == NativeLinkStrategy("merged"):
        # Collect omnibus info from deps.
        linkable_graph = create_linkable_graph(
            ctx,
            deps = deps,
        )

        omnibus_graph = get_omnibus_graph(
            graph = linkable_graph,
            # Add in any potential native root targets from our first-order deps.
            roots = get_roots(ctx.label, deps),
            # Exclude preloaded deps from omnibus linking, to prevent preloading
            # the monolithic omnibus library.
            excluded = get_excluded(deps = ctx.attrs.preload_deps),
        )

        # Link omnibus libraries.
        omnibus_libs = create_omnibus_libraries(
            ctx,
            omnibus_graph,
            ctx.attrs.linker_flags,
            prefer_stripped_objects = ctx.attrs.prefer_stripped_native_objects,
        )

        # Extract re-linked extensions.
        extensions = {
            dest: (omnibus_libs.roots[label].product.shared_library, label)
            for dest, (_, label) in extensions.items()
        }
        native_libs = omnibus_libs.libraries

        if python_toolchain.emit_omnibus_metadata:
            omnibus_linked_obj = omnibus_libs.omnibus
            omnibus_info = DefaultInfo()
            if omnibus_linked_obj:
                omnibus_info = DefaultInfo(
                    default_output = omnibus_linked_obj.output,
                    sub_targets = {
                        "dwp": [DefaultInfo(default_output = omnibus_linked_obj.dwp if omnibus_linked_obj.dwp else None)],
                    },
                )
            extra["omnibus"] = [
                omnibus_info,
                OmnibusMetadataInfo(
                    omnibus_libs = omnibus_libs,
                    omnibus_graph = omnibus_graph,
                ),
            ]

            exclusion_roots = ctx.actions.write_json("omnibus/exclusion_roots.json", omnibus_libs.exclusion_roots)
            extra["omnibus-exclusion-roots"] = [DefaultInfo(default_output = exclusion_roots)]

            roots = ctx.actions.write_json("omnibus/roots.json", omnibus_libs.roots)
            extra["omnibus-roots"] = [DefaultInfo(default_output = roots)]

            omnibus_excluded = ctx.actions.write_json("omnibus/excluded.json", omnibus_libs.excluded)
            extra["omnibus-excluded"] = [DefaultInfo(default_output = omnibus_excluded)]

            omnibus_graph_json = ctx.actions.write_json("omnibus_graph.json", omnibus_graph)
            extra["linkable-graph"] = [DefaultInfo(default_output = omnibus_graph_json)]
    elif _link_strategy(ctx) == NativeLinkStrategy("native"):
        executable_deps = ctx.attrs.executable_deps
        extension_info = merge_cxx_extension_info(
            ctx.actions,
            deps + executable_deps,
            # Add in dlopen-enabled libs from first-order deps.
            dlopen_deps = [
                dep
                for dep in ctx.attrs.deps + ctx.attrs.preload_deps
                if DlopenableLibraryInfo in dep
            ],
        )
        inherited_preprocessor_info = cxx_inherited_preprocessor_infos(executable_deps)

        # Generate an additional C file as input
        static_extension_info_out = ctx.actions.declare_output("static_extension_info.cpp")
        cmd = cmd_args(python_toolchain.generate_static_extension_info[RunInfo])
        cmd.add(cmd_args(static_extension_info_out.as_output(), format = "--output={}"))
        cmd.add(cmd_args(["{}:{}".format(k, v) for k, v in extension_info.python_module_names.items()], format = "--extension={}"))

        # TODO we don't need to do this ...
        ctx.actions.run(cmd, category = "generate_static_extension_info")

        extra["static_extension_info"] = [DefaultInfo(default_output = static_extension_info_out)]

        cxx_executable_srcs = [
            CxxSrcWithFlags(file = ctx.attrs.cxx_main, flags = []),
            CxxSrcWithFlags(file = ctx.attrs.static_extension_utils, flags = []),
            CxxSrcWithFlags(file = static_extension_info_out, flags = []),
        ]
        extra_preprocessors = []
        if ctx.attrs.par_style == "native":
            extra_preprocessors.append(CPreprocessor(args = ["-DNATIVE_PAR_STYLE=1"]))

        # All deps inolved in the link.
        link_deps = (
            linkables(executable_deps + ctx.attrs.preload_deps) +
            list(extension_info.linkable_providers.traverse())
        )

        link_group_info, auto_link_group_specs = _get_link_group_info(
            ctx,
            link_deps,
            extension_info.dlopen_deps.values(),
            extension_info.unembeddable_extensions,
        )

        # Set rpaths to find 1) the shared libs dir and the 2) runtime libs dir.
        rpath_ref = get_rpath_origin(get_cxx_toolchain_info(ctx).linker_info.type)
        rpath_ldflag = "-Wl,-rpath,{}/".format(rpath_ref)
        extra_binary_link_flags = []
        if package_style == PackageStyle("standalone"):
            extra_binary_link_flags.append(rpath_ldflag + "../..")
            extra_binary_link_flags.append(rpath_ldflag + "../lib")
        else:
            rpath_ldflag_prefix = rpath_ldflag + "{}#link-tree".format(ctx.attrs.name)
            extra_binary_link_flags.append(rpath_ldflag_prefix + "/runtime/lib")
            extra_binary_link_flags.append(rpath_ldflag_prefix)

        impl_params = CxxRuleConstructorParams(
            rule_type = "python_binary",
            headers_layout = cxx_get_regular_cxx_headers_layout(ctx),
            srcs = cxx_executable_srcs,
            extra_link_flags = ctx.attrs.linker_flags,
            extra_binary_link_flags = extra_binary_link_flags,
            extra_preprocessors = extra_preprocessors,
            extra_preprocessors_info = inherited_preprocessor_info,
            extra_link_deps = link_deps,
            exe_shared_libs_link_tree = False,
            force_full_hybrid_if_capable = True,
            prefer_stripped_objects = ctx.attrs.prefer_stripped_native_objects,
            link_group_info = link_group_info,
            auto_link_group_specs = auto_link_group_specs,
            exe_category_suffix = "python_exe",
            extra_link_roots = (
                extension_info.unembeddable_extensions.values() +
                extension_info.dlopen_deps.values()
            ),
        )

        executable_info, _, _ = cxx_executable(ctx, impl_params)
        extra["native-executable"] = [DefaultInfo(default_output = executable_info.binary, sub_targets = executable_info.sub_targets)]

        native_libs = executable_info.shared_libs

        # Add sub-targets for libs.
        for name, lib in native_libs.items():
            extra[name] = [DefaultInfo(default_output = lib.output)]

        for name, group in executable_info.auto_link_groups.items():
            extra[name] = [DefaultInfo(default_output = group.output)]

        # Unembeddable extensions.
        extensions = {
            name: (
                executable_info.auto_link_groups[name],
                link.linkable_graph.nodes.value.label,
            )
            for name, link in extension_info.unembeddable_extensions.items()
        }

        # TODO expect(len(executable_info.runtime_files) == 0, "OH NO THERE ARE RUNTIME FILES")
        artifacts = dict(extension_info.artifacts)
        native_libs["runtime/bin/{}".format(ctx.attrs.executable_name)] = LinkedObject(
            output = executable_info.binary,
            dwp = executable_info.dwp,
        )
        artifacts["static_extension_finder.py"] = ctx.attrs.static_extension_finder
        extra_manifests = create_manifest_for_source_map(ctx, "extension_stubs", artifacts)

    else:
        native_libs = {name: shared_lib.lib for name, shared_lib in library.shared_libraries().items()}

    # Combine sources and extensions into a map of all modules.
    pex_modules = PexModules(
        manifests = library.manifests(),
        extra_manifests = extra_manifests,
        compile = compile,
        extensions = create_manifest_for_extensions(
            ctx,
            extensions,
            dwp = ctx.attrs.package_split_dwarf_dwp,
        ) if extensions else None,
    )

    # Create the map of native libraries to their artifacts and whether they
    # need to be preloaded.  Note that we merge preload deps into regular deps
    # above, before gathering up all native libraries, so we're guaranteed to
    # have all preload libraries (and their transitive deps) here.
    shared_libraries = {}
    for name, lib in native_libs.items():
        shared_libraries[name] = lib, name in preload_names

    hidden_resources = library.hidden_resources() if library.has_hidden_resources() else None

    # Build the PEX.
    pex = make_pex(
        ctx,
        python_toolchain,
        ctx.attrs.make_pex[RunInfo] if ctx.attrs.make_pex != None else None,
        package_style,
        ctx.attrs.build_args,
        pex_modules,
        shared_libraries,
        main_module,
        hidden_resources,
    )

    pex.sub_targets.update(extra)

    return pex

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

    pex = python_executable(
        ctx,
        main_module,
        srcs,
        {},
        compile = value_or(ctx.attrs.compile, False),
    )
    return [
        DefaultInfo(
            default_output = pex.default_output,
            other_outputs = pex.other_outputs,
            sub_targets = pex.sub_targets,
        ),
        RunInfo(pex.run_cmd),
    ]

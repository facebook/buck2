# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(
    "@prelude//:artifact_tset.bzl",
    "ArtifactTSet",
    "make_artifact_tset",
    "project_artifacts",
)
load("@prelude//:local_only.bzl", "get_resolved_cxx_binary_link_execution_preference")
load(
    "@prelude//:resources.bzl",
    "create_resource_db",
    "gather_resources",
)
load(
    "@prelude//apple:apple_frameworks.bzl",
    "apple_build_link_args_with_deduped_flags",
    "apple_create_frameworks_linkable",
    "apple_get_link_info_by_deduping_link_infos",
)
load(
    "@prelude//apple:xcode.bzl",
    "get_project_root_file",
)
load(
    "@prelude//cxx:cxx_bolt.bzl",
    "cxx_use_bolt",
)
load(
    "@prelude//ide_integrations:xcode.bzl",
    "XCODE_DATA_SUB_TARGET",
    "XcodeDataInfo",
    "generate_xcode_data",
)
load(
    "@prelude//linking:link_groups.bzl",
    "gather_link_group_libs",
)
load(
    "@prelude//linking:link_info.bzl",
    "LinkArgs",
    "LinkCommandDebugOutput",
    "LinkInfo",
    "LinkOrdering",  # @unused Used as a type
    "LinkStrategy",
    "LinkedObject",  # @unused Used as a type
    "ObjectsLinkable",
    "make_link_command_debug_output",
    "make_link_command_debug_output_json_info",
    "to_link_strategy",
)
load(
    "@prelude//linking:linkable_graph.bzl",
    "create_linkable_graph",
    "get_linkable_graph_node_map_func",
)
load(
    "@prelude//linking:linkables.bzl",
    "linkables",
)
load(
    "@prelude//linking:shared_libraries.bzl",
    "merge_shared_libraries",
    "traverse_shared_library_info",
)
load("@prelude//utils:arglike.bzl", "ArgLike")  # @unused Used as a type
load("@prelude//utils:set.bzl", "set")
load(
    "@prelude//utils:utils.bzl",
    "flatten_dict",
    "map_val",
)
load(
    ":argsfiles.bzl",
    "ABS_ARGSFILES_SUBTARGET",
    "ARGSFILES_SUBTARGET",
    "get_argsfiles_output",
)
load(
    ":comp_db.bzl",
    "CxxCompilationDbInfo",  # @unused Used as a type
    "create_compilation_database",
    "make_compilation_db_info",
)
load(
    ":compile.bzl",
    "compile_cxx",
    "create_compile_cmds",
)
load(":cxx_context.bzl", "get_cxx_platform_info", "get_cxx_toolchain_info")
load(
    ":cxx_library_utility.bzl",
    "OBJECTS_SUBTARGET",
    "cxx_attr_deps",
    "cxx_attr_link_style",
    "cxx_attr_linker_flags",
    "cxx_attr_resources",
    "cxx_is_gnu",
    "cxx_objects_sub_targets",
)
load(
    ":cxx_link_utility.bzl",
    "executable_shared_lib_arguments",
)
load(
    ":cxx_types.bzl",
    "CxxRuleConstructorParams",  # @unused Used as a type
)
load(
    ":link.bzl",
    "CxxLinkerMapData",
    "cxx_link_into",
)
load(
    ":link_groups.bzl",
    "LINK_GROUP_MAPPINGS_FILENAME_SUFFIX",
    "LINK_GROUP_MAPPINGS_SUB_TARGET",
    "LINK_GROUP_MAP_DATABASE_SUB_TARGET",
    "LinkGroupContext",
    "create_link_groups",
    "find_relevant_roots",
    "get_filtered_labels_to_links_map",
    "get_filtered_links",
    "get_filtered_targets",
    "get_link_group",
    "get_link_group_map_json",
    "get_link_group_preferred_linkage",
    "get_transitive_deps_matching_labels",
    "is_link_group_shlib",
)
load(
    ":link_types.bzl",
    "CxxLinkResultType",
    "LinkOptions",
    "link_options",
    "merge_link_options",
)
load(
    ":linker.bzl",
    "DUMPBIN_SUB_TARGET",
    "PDB_SUB_TARGET",
    "get_dumpbin_providers",
    "get_pdb_providers",
)
load(
    ":preprocessor.bzl",
    "cxx_inherited_preprocessor_infos",
    "cxx_private_preprocessor_info",
)

CxxExecutableOutput = record(
    binary = Artifact,
    unstripped_binary = Artifact,
    bitcode_bundle = field([Artifact, None], None),
    dwp = field([Artifact, None]),
    # Files that will likely need to be included as .hidden() arguments
    # when executing the executable (ex. RunInfo())
    runtime_files = list[ArgLike],
    sub_targets = dict[str, list[DefaultInfo]],
    # The LinkArgs used to create the final executable in 'binary'.
    link_args = list[LinkArgs],
    # External components needed to debug the executable.
    external_debug_info = field(ArtifactTSet, ArtifactTSet()),
    # The projection of `external_debug_info`
    external_debug_info_artifacts = list[TransitiveSetArgsProjection],
    shared_libs = dict[str, LinkedObject],
    # All link group links that were generated in the executable.
    auto_link_groups = field(dict[str, LinkedObject], {}),
    compilation_db = CxxCompilationDbInfo,
    xcode_data = XcodeDataInfo,
    linker_map_data = [CxxLinkerMapData, None],
    link_command_debug_output = field([LinkCommandDebugOutput, None], None),
)

def cxx_executable(ctx: AnalysisContext, impl_params: CxxRuleConstructorParams, is_cxx_test: bool = False) -> CxxExecutableOutput:
    project_root_file = get_project_root_file(ctx)

    # Gather preprocessor inputs.
    preprocessor_deps = cxx_attr_deps(ctx) + filter(None, [ctx.attrs.precompiled_header])
    (own_preprocessor_info, test_preprocessor_infos) = cxx_private_preprocessor_info(
        ctx,
        impl_params.headers_layout,
        project_root_file = project_root_file,
        raw_headers = ctx.attrs.raw_headers,
        extra_preprocessors = impl_params.extra_preprocessors,
        non_exported_deps = preprocessor_deps,
        is_test = is_cxx_test,
    )
    inherited_preprocessor_infos = cxx_inherited_preprocessor_infos(preprocessor_deps) + impl_params.extra_preprocessors_info

    # The link style to use.
    link_strategy = to_link_strategy(cxx_attr_link_style(ctx))

    sub_targets = {}

    # Compile objects.
    compile_cmd_output = create_compile_cmds(
        ctx,
        impl_params,
        [own_preprocessor_info] + test_preprocessor_infos,
        inherited_preprocessor_infos,
    )
    cxx_outs = compile_cxx(ctx, compile_cmd_output.src_compile_cmds, pic = link_strategy != LinkStrategy("static"))

    sub_targets[ARGSFILES_SUBTARGET] = [get_argsfiles_output(ctx, compile_cmd_output.argsfiles.relative, "argsfiles")]
    sub_targets[ABS_ARGSFILES_SUBTARGET] = [get_argsfiles_output(ctx, compile_cmd_output.argsfiles.absolute, "abs-argsfiles")]
    sub_targets[OBJECTS_SUBTARGET] = [DefaultInfo(sub_targets = cxx_objects_sub_targets(cxx_outs))]

    # Compilation DB.
    comp_db = create_compilation_database(ctx, compile_cmd_output.src_compile_cmds, "compilation-database")
    sub_targets["compilation-database"] = [comp_db]

    # Compilation DB including headers.
    comp_db = create_compilation_database(ctx, compile_cmd_output.comp_db_compile_cmds, "full-compilation-database")
    sub_targets["full-compilation-database"] = [comp_db]

    # comp_db_compile_cmds can include header files being compiled as C++ which should not be exposed in the [compilation-database] subtarget
    comp_db_info = make_compilation_db_info(compile_cmd_output.comp_db_compile_cmds, get_cxx_toolchain_info(ctx), get_cxx_platform_info(ctx))

    # Link deps
    link_deps = linkables(cxx_attr_deps(ctx)) + impl_params.extra_link_deps

    # Link Groups
    link_group = get_link_group(ctx)
    link_group_info = impl_params.link_group_info

    if link_group_info:
        link_groups = link_group_info.groups
        link_group_mappings = link_group_info.mappings
        link_group_deps = [link_group_info.graph]
    else:
        link_groups = {}
        link_group_mappings = {}
        link_group_deps = []
    link_group_preferred_linkage = get_link_group_preferred_linkage(link_groups.values())

    # Create the linkable graph with the binary's deps and any link group deps.
    linkable_graph = create_linkable_graph(
        ctx,
        deps = filter(
            None,
            # Some subtargets (like :Foo[headers]) do not have a linkable_graph.
            [d.linkable_graph for d in link_deps] +
            # We also need to include additional link roots, so that we find
            # deps that might need to be linked into the main executable.
            [d.linkable_graph for d in impl_params.extra_link_roots] +
            # For non-auto-link-group cases, also search the targets specified
            # in the link group mappings, as they won't be available in other
            # ways.
            link_group_deps,
        ),
    )

    # Gather link inputs.
    own_link_flags = cxx_attr_linker_flags(ctx) + impl_params.extra_link_flags + impl_params.extra_exported_link_flags

    # ctx.attrs.binary_linker_flags should come after default link flags so it can be used to override default settings
    own_binary_link_flags = impl_params.extra_binary_link_flags + own_link_flags + ctx.attrs.binary_linker_flags
    deps_merged_link_infos = [d.merged_link_info for d in link_deps]
    frameworks_linkable = apple_create_frameworks_linkable(ctx)
    swiftmodule_linkable = impl_params.swiftmodule_linkable

    # `apple_binary()` / `cxx_binary()` _itself_ cannot contain Swift, so it does not
    # _directly_ contribute a Swift runtime linkable
    swift_runtime_linkable = None

    # Link group libs.
    link_group_libs = {}
    auto_link_groups = {}
    labels_to_links_map = {}

    if not link_group_mappings:
        # We cannot support deriving link execution preference off the included links, as we've already
        # lost the information on what is in the link.
        # TODO(T152860998): Derive link_execution_preference based upon the included links
        link_execution_preference = get_resolved_cxx_binary_link_execution_preference(ctx, [], impl_params.force_full_hybrid_if_capable)

        dep_links = apple_build_link_args_with_deduped_flags(
            ctx,
            deps_merged_link_infos,
            frameworks_linkable,
            link_strategy,
            swiftmodule_linkable,
            prefer_stripped = impl_params.prefer_stripped_objects,
            swift_runtime_linkable = swift_runtime_linkable,
        )
    else:
        linkable_graph_node_map = get_linkable_graph_node_map_func(linkable_graph)()

        # Although these aren't really deps, we need to search from the
        # extra link group roots to make sure we find additional libs
        # that should be linked into the main link group.
        link_group_extra_link_roots = [d.linkable_graph.nodes.value.label for d in impl_params.extra_link_roots if d.linkable_graph != None]

        exec_dep_roots = [d.linkable_graph.nodes.value.label for d in link_deps if d.linkable_graph != None]

        # If we're using auto-link-groups, where we generate the link group links
        # in the prelude, the link group map will give us the link group libs.
        # Otherwise, pull them from the `LinkGroupLibInfo` provider from out deps.
        if impl_params.auto_link_group_specs != None:
            linked_link_groups = create_link_groups(
                ctx = ctx,
                link_groups = link_groups,
                link_group_mappings = link_group_mappings,
                link_group_preferred_linkage = link_group_preferred_linkage,
                executable_deps = exec_dep_roots,
                linker_flags = own_link_flags,
                link_group_specs = impl_params.auto_link_group_specs,
                root_link_group = link_group,
                linkable_graph_node_map = linkable_graph_node_map,
                other_roots = link_group_extra_link_roots,
                prefer_stripped_objects = impl_params.prefer_stripped_objects,
                anonymous = ctx.attrs.anonymous_link_groups,
            )
            for name, linked_link_group in linked_link_groups.libs.items():
                auto_link_groups[name] = linked_link_group.artifact
                if linked_link_group.library != None:
                    link_group_libs[name] = linked_link_group.library
            own_binary_link_flags += linked_link_groups.symbol_ldflags

        else:
            # NOTE(agallagher): We don't use version scripts and linker scripts
            # for non-auto-link-group flow, as it's note clear it's useful (e.g.
            # it's mainly for supporting dlopen-enabled libs and extensions).
            link_group_libs = gather_link_group_libs(
                children = [d.link_group_lib_info for d in link_deps],
            )

        pic_behavior = get_cxx_toolchain_info(ctx).pic_behavior

        # TODO(T110378098): Similar to shared libraries, we need to identify all the possible
        # scenarios for which we need to propagate up link info and simplify this logic. For now
        # base which links to use based on whether link groups are defined.
        labels_to_links_map = get_filtered_labels_to_links_map(
            linkable_graph_node_map,
            link_group,
            link_groups,
            link_group_mappings,
            link_group_preferred_linkage,
            pic_behavior = pic_behavior,
            link_group_libs = {
                name: (lib.label, lib.shared_link_infos)
                for name, lib in link_group_libs.items()
            },
            link_strategy = link_strategy,
            roots = (
                exec_dep_roots +
                find_relevant_roots(
                    link_group = link_group,
                    linkable_graph_node_map = linkable_graph_node_map,
                    link_group_mappings = link_group_mappings,
                    roots = link_group_extra_link_roots,
                )
            ),
            is_executable_link = True,
            prefer_stripped = impl_params.prefer_stripped_objects,
            force_static_follows_dependents = impl_params.link_groups_force_static_follows_dependents,
        )

        if is_cxx_test and link_group != None:
            # if a cpp_unittest is part of the link group, we need to traverse through all deps
            # from the root again to ensure we link in gtest deps
            labels_to_links_map = labels_to_links_map | get_filtered_labels_to_links_map(
                linkable_graph_node_map,
                None,
                link_groups,
                link_group_mappings,
                link_group_preferred_linkage,
                link_strategy,
                pic_behavior = pic_behavior,
                roots = [d.linkable_graph.nodes.value.label for d in link_deps],
                is_executable_link = True,
                prefer_stripped = impl_params.prefer_stripped_objects,
            )

        # NOTE: Our Haskell DLL support impl currently links transitive haskell
        # deps needed by DLLs which get linked into the main executable as link-
        # whole.  To emulate this, we mark Haskell rules with a special label
        # and traverse this to find all the nodes we need to link whole.
        public_nodes = []
        if ctx.attrs.link_group_public_deps_label != None:
            public_nodes = get_transitive_deps_matching_labels(
                linkable_graph_node_map = linkable_graph_node_map,
                label = ctx.attrs.link_group_public_deps_label,
                roots = [
                    mapping.root
                    for group in link_group_info.groups.values()
                    for mapping in group.mappings
                    if mapping.root != None
                ],
            )

        filtered_links = get_filtered_links(labels_to_links_map, set(public_nodes))
        filtered_targets = get_filtered_targets(labels_to_links_map)

        link_execution_preference = get_resolved_cxx_binary_link_execution_preference(ctx, labels_to_links_map.keys(), impl_params.force_full_hybrid_if_capable)

        # Unfortunately, link_groups does not use MergedLinkInfo to represent the args
        # for the resolved nodes in the graph.
        # Thus, we have no choice but to traverse all the nodes to dedupe the framework linker args.
        additional_links = apple_get_link_info_by_deduping_link_infos(ctx, filtered_links, frameworks_linkable, swiftmodule_linkable, swift_runtime_linkable)
        if additional_links:
            filtered_links.append(additional_links)

        dep_links = LinkArgs(infos = filtered_links)
        sub_targets[LINK_GROUP_MAP_DATABASE_SUB_TARGET] = [get_link_group_map_json(ctx, filtered_targets)]

    # Set up shared libraries symlink tree only when needed
    shared_libs = {}

    # Add in extra, rule-specific shared libs.
    for name, shlib in impl_params.extra_shared_libs.items():
        shared_libs[name] = shlib.lib

    # Only setup a shared library symlink tree when shared linkage or link_groups is used
    gnu_use_link_groups = cxx_is_gnu(ctx) and link_group_mappings
    if link_strategy == LinkStrategy("shared") or gnu_use_link_groups:
        shlib_info = merge_shared_libraries(
            ctx.actions,
            deps = (
                [d.shared_library_info for d in link_deps] +
                [d.shared_library_info for d in impl_params.extra_link_roots]
            ),
        )

        link_group_ctx = LinkGroupContext(
            link_group_mappings = link_group_mappings,
            link_group_libs = link_group_libs,
            link_group_preferred_linkage = link_group_preferred_linkage,
            labels_to_links_map = labels_to_links_map,
        )

        def shlib_filter(_name, shared_lib):
            return not gnu_use_link_groups or is_link_group_shlib(shared_lib.label, link_group_ctx)

        for name, shared_lib in traverse_shared_library_info(shlib_info, filter_func = shlib_filter).items():
            shared_libs[name] = shared_lib.lib

    if gnu_use_link_groups:
        # When there are no matches for a pattern based link group,
        # `link_group_mappings` will not have an entry associated with the lib.
        for _name, link_group_lib in link_group_libs.items():
            shared_libs.update(link_group_lib.shared_libs)

    toolchain_info = get_cxx_toolchain_info(ctx)
    linker_info = toolchain_info.linker_info
    links = [
        LinkArgs(infos = [
            LinkInfo(
                pre_flags = own_binary_link_flags,
                linkables = [ObjectsLinkable(
                    objects = [out.object for out in cxx_outs] + impl_params.extra_link_input,
                    linker_type = linker_info.type,
                    link_whole = True,
                )],
                external_debug_info = make_artifact_tset(
                    actions = ctx.actions,
                    label = ctx.label,
                    artifacts = (
                        [out.object for out in cxx_outs if out.object_has_external_debug_info] +
                        [out.external_debug_info for out in cxx_outs if out.external_debug_info != None] +
                        (impl_params.extra_link_input if impl_params.extra_link_input_has_external_debug_info else [])
                    ),
                ),
            ),
        ]),
        dep_links,
    ] + impl_params.extra_link_args

    link_result = _link_into_executable(
        ctx,
        # If shlib lib tree generation is enabled, pass in the shared libs (which
        # will trigger the necessary link tree and link args).
        shared_libs if impl_params.exe_shared_libs_link_tree else {},
        linker_info.binary_extension,
        link_options(
            links = links,
            link_weight = linker_info.link_weight,
            link_ordering = map_val(LinkOrdering, ctx.attrs.link_ordering),
            link_execution_preference = link_execution_preference,
            enable_distributed_thinlto = ctx.attrs.enable_distributed_thinlto,
            strip = impl_params.strip_executable,
            strip_args_factory = impl_params.strip_args_factory,
            category_suffix = impl_params.exe_category_suffix,
            allow_cache_upload = impl_params.exe_allow_cache_upload,
        ),
    )
    binary = link_result.exe
    runtime_files = link_result.runtime_files
    shared_libs_symlink_tree = link_result.shared_libs_symlink_tree
    linker_map_data = link_result.linker_map_data

    # Define the xcode data sub target
    xcode_data_default_info, xcode_data_info = generate_xcode_data(
        ctx,
        rule_type = impl_params.rule_type,
        output = binary.output,
        populate_rule_specific_attributes_func = impl_params.cxx_populate_xcode_attributes_func,
        srcs = impl_params.srcs + impl_params.additional.srcs,
        argsfiles = compile_cmd_output.argsfiles.absolute,
        product_name = get_cxx_executable_product_name(ctx),
    )
    sub_targets[XCODE_DATA_SUB_TARGET] = xcode_data_default_info

    # Info about dynamic-linked libraries for fbpkg integration:
    # - the symlink dir that's part of RPATH
    # - sub-sub-targets that reference shared library dependencies and their respective dwp
    # - [shared-libraries] - a json map that references the above rules.
    if isinstance(shared_libs_symlink_tree, Artifact):
        sub_targets["rpath-tree"] = [DefaultInfo(
            default_output = shared_libs_symlink_tree,
            other_outputs = [
                lib.output
                for lib in shared_libs.values()
            ] + [
                lib.dwp
                for lib in shared_libs.values()
                if lib.dwp
            ],
        )]
    sub_targets["shared-libraries"] = [DefaultInfo(
        default_output = ctx.actions.write_json(
            binary.output.basename + ".shared-libraries.json",
            {
                "libraries": ["{}:{}[shared-libraries][{}]".format(ctx.label.path, ctx.label.name, name) for name in shared_libs.keys()],
                "librariesdwp": ["{}:{}[shared-libraries][{}][dwp]".format(ctx.label.path, ctx.label.name, name) for name, lib in shared_libs.items() if lib.dwp],
                "rpathtree": ["{}:{}[rpath-tree]".format(ctx.label.path, ctx.label.name)] if shared_libs_symlink_tree else [],
            },
        ),
        sub_targets = {
            name: [DefaultInfo(
                default_output = lib.output,
                sub_targets = {"dwp": [DefaultInfo(default_output = lib.dwp)]} if lib.dwp else {},
            )]
            for name, lib in shared_libs.items()
        },
    )]
    if link_group_mappings:
        readable_mappings = {}
        for node, group in link_group_mappings.items():
            readable_mappings[group] = readable_mappings.get(group, []) + ["{}//{}:{}".format(node.cell, node.package, node.name)]

        sub_targets[LINK_GROUP_MAPPINGS_SUB_TARGET] = [DefaultInfo(
            default_output = ctx.actions.write_json(
                binary.output.basename + LINK_GROUP_MAPPINGS_FILENAME_SUFFIX,
                readable_mappings,
            ),
        )]

        linkable_graph_node_map = get_linkable_graph_node_map_func(linkable_graph)()
        sub_targets["binary_node_count"] = [DefaultInfo(
            default_output = ctx.actions.write_json(
                binary.output.basename + ".binary_node_count.json",
                {
                    "binary_node_count": len(linkable_graph_node_map),
                },
            ),
        )]

    # TODO(T110378140): We can't really enable this yet, as Python binaries
    # consuming C++ binaries as resources don't know how to handle the
    # extraneous debug paths and will crash.  We probably need to add a special
    # exported resources provider and make sure we handle the workflows.
    # Add any referenced debug paths to runtime files.
    #runtime_files.extend(binary.external_debug_info)

    # If we have some resources, write it to the resources JSON file and add
    # it and all resources to "runtime_files" so that we make to materialize
    # them with the final binary.
    resources = flatten_dict(gather_resources(
        label = ctx.label,
        resources = cxx_attr_resources(ctx),
        deps = cxx_attr_deps(ctx),
    ).values())
    if resources:
        runtime_files.append(create_resource_db(
            ctx = ctx,
            name = binary.output.basename + ".resources.json",
            binary = binary.output,
            resources = resources,
        ))
        for resource, other in resources.values():
            runtime_files.append(resource)
            runtime_files.extend(other)

    if binary.dwp:
        # A `dwp` sub-target which generates the `.dwp` file for this binary.
        sub_targets["dwp"] = [DefaultInfo(default_output = binary.dwp)]

    if binary.pdb:
        # A `pdb` sub-target which generates the `.pdb` file for this binary.
        sub_targets[PDB_SUB_TARGET] = get_pdb_providers(binary.pdb)

    if toolchain_info.dumpbin_toolchain_path:
        sub_targets[DUMPBIN_SUB_TARGET] = get_dumpbin_providers(ctx, binary.output, toolchain_info.dumpbin_toolchain_path)

    # If bolt is not ran, binary.prebolt_output will be the same as binary.output. Only
    # expose binary.prebolt_output if cxx_use_bolt(ctx) is True to avoid confusion
    if cxx_use_bolt(ctx):
        sub_targets["prebolt"] = [DefaultInfo(default_output = binary.prebolt_output)]

    if linker_map_data:
        sub_targets["linker-map"] = [DefaultInfo(default_output = linker_map_data.map, other_outputs = [linker_map_data.binary])]

    sub_targets["linker.argsfile"] = [DefaultInfo(
        default_output = binary.linker_argsfile,
    )]
    sub_targets["linker.filelist"] = [DefaultInfo(
        default_outputs = filter(None, [binary.linker_filelist]),
    )]

    link_cmd_debug_output = make_link_command_debug_output(binary)
    if link_cmd_debug_output != None:
        link_cmd_debug_output_file = make_link_command_debug_output_json_info(ctx, [link_cmd_debug_output])
        sub_targets["linker.command"] = [DefaultInfo(
            default_outputs = filter(None, [link_cmd_debug_output_file]),
        )]

    if linker_info.supports_distributed_thinlto and ctx.attrs.enable_distributed_thinlto:
        sub_targets["index.argsfile"] = [DefaultInfo(
            default_output = binary.index_argsfile,
        )]

    # Provide a debug info target to make sure debug info is materialized.
    external_debug_info = make_artifact_tset(
        actions = ctx.actions,
        children = (
            [binary.external_debug_info] +
            [s.external_debug_info for s in shared_libs.values()] +
            impl_params.additional.static_external_debug_info
        ),
    )
    external_debug_info_artifacts = project_artifacts(ctx.actions, [external_debug_info])
    materialize_external_debug_info = ctx.actions.write(
        "debuginfo.artifacts",
        external_debug_info_artifacts,
        with_inputs = True,
    )
    sub_targets["debuginfo"] = [DefaultInfo(
        default_output = materialize_external_debug_info,
    )]

    return CxxExecutableOutput(
        binary = binary.output,
        unstripped_binary = binary.unstripped_output,
        dwp = binary.dwp,
        runtime_files = runtime_files,
        sub_targets = sub_targets,
        link_args = links,
        external_debug_info = external_debug_info,
        external_debug_info_artifacts = external_debug_info_artifacts,
        shared_libs = shared_libs,
        auto_link_groups = auto_link_groups,
        compilation_db = comp_db_info,
        xcode_data = xcode_data_info,
        linker_map_data = linker_map_data,
        link_command_debug_output = link_cmd_debug_output,
    )

_CxxLinkExecutableResult = record(
    # The resulting executable
    exe = LinkedObject,
    # List of files/directories that should be present for executable to be run successfully
    runtime_files = list[ArgLike],
    # Optional shared libs symlink tree symlinked_dir action
    shared_libs_symlink_tree = [list[Artifact], Artifact, None],
    linker_map_data = [CxxLinkerMapData, None],
)

def _link_into_executable(
        ctx: AnalysisContext,
        shared_libs: dict[str, LinkedObject],
        binary_extension: str,
        opts: LinkOptions) -> _CxxLinkExecutableResult:
    output = ctx.actions.declare_output("{}{}".format(get_cxx_executable_product_name(ctx), "." + binary_extension if binary_extension else ""))
    extra_args, runtime_files, shared_libs_symlink_tree = executable_shared_lib_arguments(
        ctx.actions,
        get_cxx_toolchain_info(ctx),
        output,
        shared_libs,
    )

    link_result = cxx_link_into(
        ctx = ctx,
        output = output,
        result_type = CxxLinkResultType("executable"),
        opts = merge_link_options(
            opts,
            links = [LinkArgs(flags = extra_args)] + opts.links,
        ),
    )

    return _CxxLinkExecutableResult(
        exe = link_result.linked_object,
        runtime_files = runtime_files,
        shared_libs_symlink_tree = shared_libs_symlink_tree,
        linker_map_data = link_result.linker_map_data,
    )

def get_cxx_executable_product_name(ctx: AnalysisContext) -> str:
    return ctx.label.name + ("-wrapper" if cxx_use_bolt(ctx) else "")

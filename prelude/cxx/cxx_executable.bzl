load("@fbcode//buck2/prelude:local_only.bzl", "link_cxx_binary_locally")
load(
    "@fbcode//buck2/prelude/apple:apple_frameworks.bzl",
    "build_link_args_with_deduped_framework_flags",
    "create_frameworks_linkable",
    "get_frameworks_link_info_by_deduping_link_infos",
)
load(
    "@fbcode//buck2/prelude/apple:link_groups.bzl",
    "LINK_GROUP_MAP_DATABASE_SUB_TARGET",
    "LinkGroupInfo",  # @unused Used as a type
    "get_filtered_labels_to_links_map",
    "get_filtered_links",
    "get_filtered_targets",
    "get_link_group",
    "get_link_group_map_json",
    "get_link_group_mappings",
    "get_link_groups",
)
load(
    "@fbcode//buck2/prelude/cxx:cxx_bolt.bzl",
    "cxx_use_bolt",
)
load(
    "@fbcode//buck2/prelude/ide_integrations:xcode.bzl",
    "XCODE_DATA_SUB_TARGET",
    "generate_xcode_data",
)
load(
    "@fbcode//buck2/prelude/linking:link_info.bzl",
    "LinkArgs",
    "LinkInfo",
    "LinkStyle",
    "LinkedObject",  # @unused Used as a type
    "ObjectsLinkable",
)
load(
    "@fbcode//buck2/prelude/linking:linkable_graph.bzl",
    "create_merged_linkable_graph",
)
load(
    "@fbcode//buck2/prelude/linking:shared_libraries.bzl",
    "SharedLibraryInfo",
    "merge_shared_libraries",
    "traverse_shared_library_info",
)
load(
    "@fbcode//buck2/prelude/utils:utils.bzl",
    "flatten_dict",
    "map_idx",
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
    "ARGSFILES_SUBTARGET",
    "cxx_attr_deps",
    "cxx_attr_link_style",
    "cxx_attr_linker_flags",
    "cxx_attr_resources",
    "cxx_inherited_link_info",
    "cxx_use_link_groups",
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
    "cxx_link",
)
load(
    ":preprocessor.bzl",
    "cxx_inherited_preprocessor_infos",
    "cxx_private_preprocessor_info",
)
load(
    ":resources.bzl",
    "create_resource_db",
    "gather_cxx_resources",
)

_CxxExecutableOutput = record(
    binary = "artifact",
    # Files that will likely need to be included as .hidden() arguments
    # when executing the executable (ex. RunInfo())
    runtime_files = ["_arglike"],
    sub_targets = {str.type: [DefaultInfo.type]},
    # The LinkArgs used to create the final executable in 'binary'.
    link_args = [LinkArgs.type],
)

# returns a tuple of the runnable binary as an artifact, a list of its runtime files as artifacts and a sub targets map, and the CxxCompilationDbInfo provider
def cxx_executable(ctx: "context", impl_params: CxxRuleConstructorParams.type, is_cxx_test: bool.type = False) -> (_CxxExecutableOutput.type, CxxCompilationDbInfo.type):
    first_order_deps = cxx_attr_deps(ctx) + filter(None, [ctx.attr.precompiled_header])

    # Gather preprocessor inputs.
    (own_preprocessor_info, test_preprocessor_infos) = cxx_private_preprocessor_info(
        ctx,
        impl_params.headers_layout,
        raw_headers = ctx.attr.raw_headers,
        extra_preprocessors = impl_params.extra_preprocessors,
        non_exported_deps = first_order_deps,
        is_test = is_cxx_test,
    )
    inherited_preprocessor_infos = cxx_inherited_preprocessor_infos(first_order_deps)

    # The link style to use.
    link_style = cxx_attr_link_style(ctx)

    sub_targets = {}

    # Compile objects.
    compile_cmd_output = create_compile_cmds(
        ctx,
        impl_params,
        [own_preprocessor_info] + test_preprocessor_infos,
        inherited_preprocessor_infos,
    )
    objects = compile_cxx(ctx, compile_cmd_output.source_commands.src_compile_cmds, pic = link_style != LinkStyle("static"))
    sub_targets[ARGSFILES_SUBTARGET] = [compile_cmd_output.source_commands.argsfiles_info]

    # Compilation DB.
    comp_db = create_compilation_database(ctx, compile_cmd_output.comp_db_commands.src_compile_cmds)
    sub_targets["compilation-database"] = [comp_db]
    comp_db_info = make_compilation_db_info(compile_cmd_output.comp_db_commands.src_compile_cmds, get_cxx_toolchain_info(ctx), get_cxx_platform_info(ctx))

    # Link Groups
    link_group = get_link_group(ctx)
    link_groups = get_link_groups(ctx)
    link_group_deps = [mapping.target for group in link_groups for mapping in group.mappings]

    # Create the linkable graph with the binary's deps and any link group deps.
    linkable_graph = create_merged_linkable_graph(
        ctx.label,
        first_order_deps + link_group_deps,
    )

    # Calculate link group mappings now that all relevant nodes exist in the linkable graph.
    link_group_mappings = get_link_group_mappings(link_groups, linkable_graph)

    # Gather link inputs.
    own_link_flags = cxx_attr_linker_flags(ctx)
    inherited_link = cxx_inherited_link_info(ctx, first_order_deps)

    filtered_labels_to_links_map = {}
    rest_labels_to_links_map = {}
    shared_libs = {}

    frameworks_linkable = create_frameworks_linkable(ctx)

    # TODO(T110378098): Similar to shared libraries, we need to identify all the possible
    # scenarios for which we need to propagate up link info and simplify this logic. For now
    # base which links to use based on whether link groups are defined.
    if link_group_mappings:
        filtered_labels_to_links_map = get_filtered_labels_to_links_map(linkable_graph, link_group, link_group_mappings, link_style, first_order_deps, is_library = False)
        filtered_links = get_filtered_links(filtered_labels_to_links_map)
        filtered_targets = get_filtered_targets(filtered_labels_to_links_map)

        # Unfortunately, link_groups does not use MergedLinkInfo to represent the args
        # for the resolved nodes in the graph.
        # Thus, we have no choice but to traverse all the nodes to dedupe the framework linker args.
        frameworks_link_info = get_frameworks_link_info_by_deduping_link_infos(filtered_links, frameworks_linkable)
        if frameworks_link_info:
            filtered_links.append(frameworks_link_info)

        if is_cxx_test and link_group != None:
            # if a cpp_unittest is part of the link group, we need to traverse through all deps
            # from the root again to ensure we link in gtest deps
            rest_labels_to_links_map = get_filtered_labels_to_links_map(linkable_graph, None, link_group_mappings, link_style, first_order_deps, is_library = False)
            filtered_links.extend(get_filtered_links(rest_labels_to_links_map))
            filtered_targets.extend(get_filtered_targets(rest_labels_to_links_map))

        dep_links = LinkArgs(infos = filtered_links)
        sub_targets[LINK_GROUP_MAP_DATABASE_SUB_TARGET] = [get_link_group_map_json(ctx, filtered_targets)]
    else:
        dep_links = build_link_args_with_deduped_framework_flags(
            ctx,
            inherited_link,
            frameworks_linkable,
            link_style,
            prefer_stripped = ctx.attr.prefer_stripped_objects,
        )

    # Only setup a shared library symlink tree when shared linkage or link_groups is used
    use_link_groups = cxx_use_link_groups(ctx)
    if link_style == LinkStyle("shared") or use_link_groups:
        shlib_info = merge_shared_libraries(
            ctx.actions,
            deps = filter(None, map_idx(SharedLibraryInfo, first_order_deps)),
        )

        def is_link_group_shlib(label: "label", labels_to_links_map: {"label": LinkGroupInfo.type}):
            # if using link_groups, only materialize the link_group shlibs
            return label in labels_to_links_map and labels_to_links_map[label].link_style == LinkStyle("shared")  # buildifier: disable=uninitialized

        for name, shared_lib in traverse_shared_library_info(shlib_info).items():
            label = shared_lib.label
            if not use_link_groups or is_link_group_shlib(label, filtered_labels_to_links_map) or is_link_group_shlib(label, rest_labels_to_links_map):
                shared_libs[name] = shared_lib.lib

    toolchain_info = get_cxx_toolchain_info(ctx)
    linker_info = toolchain_info.linker_info
    links = [
        LinkArgs(infos = [
            LinkInfo(
                pre_flags = own_link_flags + impl_params.extra_link_flags + impl_params.extra_exported_link_flags,
                linkables = [ObjectsLinkable(objects = objects, linker_type = linker_info.type, link_whole = True)],
            ),
        ]),
        dep_links,
    ]

    binary, runtime_files, extra_args = _link_into_executable(
        ctx,
        links,
        shared_libs,
        linker_info.link_weight,
        prefer_local = link_cxx_binary_locally(ctx),
        enable_distributed_thinlto = ctx.attr.enable_distributed_thinlto,
        strip = impl_params.strip_executable,
        strip_args_factory = impl_params.strip_args_factory,
    )

    # Define the xcode data sub target
    sub_targets[XCODE_DATA_SUB_TARGET] = generate_xcode_data(
        ctx,
        rule_type = impl_params.rule_type,
        output = binary.output,
        populate_rule_specific_attributes_func = impl_params.cxx_populate_xcode_attributes_func,
        srcs = impl_params.srcs + impl_params.additional_srcs,
        argsfiles_by_ext = compile_cmd_output.source_commands.argsfile_by_ext,
        product_name = get_cxx_excutable_product_name(ctx),
    )

    # TODO(T110378140): We can't really enable this yet, as Python binaries
    # consuming C++ binaries as resources don't know how to handle the
    # extraneous debug paths and will crash.  We probably need to add a special
    # exported resources provider and make sure we handle the workflows.
    # Add any referenced debug paths to runtime files.
    #runtime_files.extend(binary.external_debug_paths)

    # If we have some resources, write it to the resources JSON file and add
    # it and all resources to "runtime_files" so that we make to materialize
    # them with the final binary.
    resources = flatten_dict(gather_cxx_resources(
        label = ctx.label,
        resources = cxx_attr_resources(ctx),
        deps = cxx_attr_deps(ctx),
    ).values())
    if resources:
        runtime_files.append(create_resource_db(ctx, binary.output, resources))
        for resource, other in resources.values():
            runtime_files.append(resource)
            runtime_files.extend(other)

    # Provide a `dwp` sub-target which generates the `.dwp` file for this
    # binary.  We always provide it, but this only really makes sense in
    # split-dwarf builds.
    # TODO(T110378141): We should probably set whether we're using split-dwarf
    # on the toolchian or target.
    if binary.dwp:
        sub_targets["dwp"] = [DefaultInfo(default_outputs = [binary.dwp])]

    # If bolt is not ran, binary.prebolt_output will be the same as binary.output. Only
    # expose binary.prebolt_output if cxx_use_bolt(ctx) is True to avoid confusion
    if cxx_use_bolt(ctx):
        sub_targets["prebolt"] = [DefaultInfo(default_outputs = [binary.prebolt_output])]

    sub_targets["linker-map"] = [DefaultInfo(default_outputs = _linker_map(
        ctx,
        binary,
        [LinkArgs(flags = extra_args)] + links,
        prefer_local = link_cxx_binary_locally(ctx, toolchain_info),
        link_weight = linker_info.link_weight,
    ))]

    sub_targets["linker.argsfile"] = [DefaultInfo(
        default_outputs = [binary.linker_argsfile],
    )]

    if linker_info.supports_distributed_thinlto and ctx.attr.enable_distributed_thinlto:
        sub_targets["index.argsfile"] = [DefaultInfo(
            default_outputs = [binary.index_argsfile],
        )]

    return _CxxExecutableOutput(
        binary = binary.output,
        runtime_files = runtime_files,
        sub_targets = sub_targets,
        link_args = links,
    ), comp_db_info

# Returns a tuple of:
# - the resulting executable
# - list of files/directories that should be present for executable to be run successfully
# - extra linking args (for the shared_libs)
def _link_into_executable(
        ctx: "context",
        links: [LinkArgs.type],
        shared_libs: {str.type: LinkedObject.type},
        link_weight: int.type,
        prefer_local: bool.type = False,
        enable_distributed_thinlto = False,
        strip: bool.type = False,
        strip_args_factory = None) -> (LinkedObject.type, ["artifact"], [""]):
    output = ctx.actions.declare_output(get_cxx_excutable_product_name(ctx))
    extra_args, runtime_files = executable_shared_lib_arguments(ctx, output, shared_libs)
    exe = cxx_link(
        ctx,
        [LinkArgs(flags = extra_args)] + links,
        output,
        prefer_local = prefer_local,
        link_weight = link_weight,
        enable_distributed_thinlto = enable_distributed_thinlto,
        category_suffix = "executable",
        strip = strip,
        strip_args_factory = strip_args_factory,
        executable_link = True,
    )
    return (exe, runtime_files, extra_args)

def _linker_map(
        ctx: "context",
        binary: LinkedObject.type,
        links: [LinkArgs.type],
        prefer_local: bool.type,
        link_weight: int.type) -> ["artifact"]:
    identifier = binary.output.short_path + ".linker-map-binary"
    binary_for_linker_map = ctx.actions.declare_output(identifier)
    linker_map = ctx.actions.declare_output(binary.output.short_path + ".linker-map")
    cxx_link(
        ctx,
        links,
        binary_for_linker_map,
        category_suffix = "linker_map",
        linker_map = linker_map,
        prefer_local = prefer_local,
        link_weight = link_weight,
        identifier = identifier,
        generate_dwp = False,
    )
    return [
        binary_for_linker_map,
        linker_map,
    ]

def get_cxx_excutable_product_name(ctx: "context") -> str.type:
    return ctx.label.name + ("-wrapper" if cxx_use_bolt(ctx) else "")

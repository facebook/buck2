load("@fbcode//buck2/prelude/apple:apple_utility.bzl", "get_apple_frameworks_linker_flags")
load(
    "@fbcode//buck2/prelude/linking:link_info.bzl",
    "Archive",
    "ArchiveLinkable",
    "LinkArgs",
    "LinkInfo",
    "LinkInfos",
    "LinkStyle",
    "Linkage",
    "LinkedObject",
    "SharedLibLinkable",
    "create_merged_link_info",
    "get_actual_link_style",
    "get_link_args",
    "get_link_styles_for_linkage",
)
load(
    "@fbcode//buck2/prelude/linking:linkable_graph.bzl",
    "add_linkable_node",
    "create_merged_linkable_graph",
)
load("@fbcode//buck2/prelude/linking:shared_libraries.bzl", "SharedLibraryInfo", "create_shared_libraries", "merge_shared_libraries")
load(
    "@fbcode//buck2/prelude/utils:utils.bzl",
    "expect",
    "flatten",
    "map_idx",
    "value_or",
)
load(
    ":compile.bzl",
    "CxxSrcWithFlags",  # @unused Used as a type
)
load(":cxx_context.bzl", "get_cxx_toolchain_info")
load(":cxx_executable.bzl", "cxx_executable")
load(":cxx_library.bzl", "cxx_library_parameterized")
load(
    ":cxx_library_utility.bzl",
    "cxx_attr_exported_deps",
    "cxx_attr_exported_linker_flags",
    "cxx_attr_exported_post_linker_flags",
    "cxx_attr_preferred_linkage",
    "cxx_inherited_link_info",
    "cxx_mk_shlib_intf",
    "cxx_platform_supported",
    "cxx_use_shlib_intfs",
)
load(
    ":cxx_types.bzl",
    "CxxRuleConstructorParams",
)
load(
    ":headers.bzl",
    "CPrecompiledHeaderInfo",
    "cxx_get_regular_cxx_headers_layout",
)
load(
    ":link.bzl",
    _cxx_link_into_shared_library = "cxx_link_into_shared_library",
)
load(
    ":linker.bzl",
    "get_link_whole_args",
    "get_shared_library_name",
)
load(
    ":omnibus.bzl",
    "create_native_link_target",
)
load(":platform.bzl", "cxx_by_platform")
load(
    ":preprocessor.bzl",
    "CPreprocessor",
    "cxx_attr_exported_preprocessor_flags",
    "cxx_exported_preprocessor_info",
    "cxx_inherited_preprocessor_infos",
    "cxx_merge_cpreprocessors",
)

cxx_link_into_shared_library = _cxx_link_into_shared_library

#####################################################################
# Attributes

# The source files
def get_srcs_with_flags(ctx: "context") -> [CxxSrcWithFlags.type]:
    all_srcs = ctx.attr.srcs + flatten(cxx_by_platform(ctx, ctx.attr.platform_srcs))
    src_to_flags = {}
    for x in all_srcs:
        if type(x) == type(()):
            artifact = x[0]
            flags = x[1]
        else:
            artifact = x
            flags = []

        old_flags = src_to_flags.get(artifact)

        if old_flags != None:
            if old_flags != flags:
                fail("Duplicate source {} with different flags {} and {}".format(artifact, old_flags, flags))
        else:
            src_to_flags[artifact] = flags

    return [CxxSrcWithFlags(file = artifact, flags = flags) for (artifact, flags) in src_to_flags.items()]

#####################################################################
# Operations

def cxx_library_impl(ctx: "context") -> ["provider"]:
    params = CxxRuleConstructorParams(
        rule_type = "cxx_library",
        headers_layout = cxx_get_regular_cxx_headers_layout(ctx),
        srcs = get_srcs_with_flags(ctx),
        extra_exported_link_flags = flatten(get_apple_frameworks_linker_flags(ctx)),
    )
    output = cxx_library_parameterized(ctx, params)
    return output.providers

def cxx_binary_impl(ctx: "context") -> ["provider"]:
    params = CxxRuleConstructorParams(
        rule_type = "cxx_binary",
        headers_layout = cxx_get_regular_cxx_headers_layout(ctx),
        srcs = get_srcs_with_flags(ctx),
    )
    output, comp_db_info = cxx_executable(ctx, params)

    return [
        DefaultInfo(
            default_outputs = [output.binary],
            other_outputs = output.runtime_files,
            sub_targets = output.sub_targets,
        ),
        RunInfo(args = cmd_args(output.binary).hidden(output.runtime_files)),
        comp_db_info,
    ]

def _prebuilt_item(
        ctx: "context",
        item: ["", None],
        platform_items: [[(str.type, "_a")], None]) -> ["_a", None]:
    """
    Parse the given item that can be specified by regular and platform-specific
    parameters.
    """

    if item != None:
        return item

    if platform_items != None:
        items = cxx_by_platform(ctx, platform_items)
        if len(items) == 0:
            return None
        if len(items) != 1:
            fail("expected single platform match: name={}//{}:{}, platform_items={}, items={}".format(ctx.label.cell, ctx.label.package, ctx.label.name, str(platform_items), str(items)))
        return items[0]

    return None

def _prebuilt_linkage(ctx: "context") -> Linkage.type:
    """
    Construct the preferred linkage to use for the given prebuilt library.
    """
    if ctx.attr.header_only:
        return Linkage("any")
    if ctx.attr.force_static:
        return Linkage("static")
    preferred_linkage = cxx_attr_preferred_linkage(ctx)
    if preferred_linkage != Linkage("any"):
        return preferred_linkage
    if ctx.attr.provided:
        return Linkage("shared")
    return Linkage("any")

def prebuilt_cxx_library_impl(ctx: "context") -> ["provider"]:
    # Versioned params should be intercepted and converted away via the stub.
    expect(not ctx.attr.versioned_exported_lang_platform_preprocessor_flags)
    expect(not ctx.attr.versioned_exported_lang_preprocessor_flags)
    expect(not ctx.attr.versioned_exported_platform_preprocessor_flags)
    expect(not ctx.attr.versioned_exported_preprocessor_flags)
    expect(not ctx.attr.versioned_header_dirs)
    expect(not ctx.attr.versioned_shared_lib)
    expect(not ctx.attr.versioned_static_lib)
    expect(not ctx.attr.versioned_static_pic_lib)

    if not cxx_platform_supported(ctx):
        return [DefaultInfo(default_outputs = [])]

    providers = []

    linker_type = get_cxx_toolchain_info(ctx).linker_info.type

    # Parse library parameters.
    static_lib = _prebuilt_item(
        ctx,
        ctx.attr.static_lib,
        ctx.attr.platform_static_lib,
    )
    static_pic_lib = _prebuilt_item(
        ctx,
        ctx.attr.static_pic_lib,
        ctx.attr.platform_static_pic_lib,
    )
    shared_lib = _prebuilt_item(
        ctx,
        ctx.attr.shared_lib,
        ctx.attr.platform_shared_lib,
    )
    header_dirs = _prebuilt_item(
        ctx,
        ctx.attr.header_dirs,
        ctx.attr.platform_header_dirs,
    )
    soname = value_or(ctx.attr.soname, get_shared_library_name(linker_type, ctx.label.name))
    preferred_linkage = _prebuilt_linkage(ctx)

    first_order_deps = cxx_attr_exported_deps(ctx)

    # Exported preprocessor info.
    inherited_pp_infos = cxx_inherited_preprocessor_infos(first_order_deps)
    generic_exported_pre = cxx_exported_preprocessor_info(ctx, cxx_get_regular_cxx_headers_layout(ctx), [])
    args = cxx_attr_exported_preprocessor_flags(ctx)
    if header_dirs != None:
        for x in header_dirs:
            args += ["-isystem", x]
    specific_exportd_pre = CPreprocessor(args = args)
    providers.append(cxx_merge_cpreprocessors(
        ctx,
        [generic_exported_pre, specific_exportd_pre],
        inherited_pp_infos,
    ))

    inherited_link = cxx_inherited_link_info(ctx, first_order_deps)
    exported_linker_flags = cxx_attr_exported_linker_flags(ctx)

    # Gather link infos, outputs, and shared libs for effective link style.
    outputs = {}
    libraries = {}
    solibs = {}
    for link_style in get_link_styles_for_linkage(preferred_linkage):
        args = []
        outs = []

        # Add exported linker flags first.
        args.extend(cxx_attr_exported_linker_flags(ctx))
        post_link_flags = cxx_attr_exported_post_linker_flags(ctx)
        linkable = None

        # If we have sources to compile, generate the necessary libraries and
        # add them to the exported link info.
        if not ctx.attr.header_only:
            def archive_linkable(lib):
                return ArchiveLinkable(
                    archive = Archive(artifact = lib),
                    linker_type = linker_type,
                    link_whole = ctx.attr.link_whole,
                    do_not_inspect_for_thinlto = True,
                )

            if link_style == LinkStyle("static"):
                if static_lib:
                    outs.append(static_lib)
                    linkable = archive_linkable(static_lib)
            elif link_style == LinkStyle("static_pic"):
                lib = static_pic_lib or static_lib
                if lib:
                    outs.append(lib)
                    linkable = archive_linkable(lib)
            else:  # shared
                # If no shared library was provided, link one from the static libraries.
                if shared_lib != None:
                    shared_lib = LinkedObject(output = shared_lib)
                else:
                    lib = static_pic_lib or static_lib
                    if lib:
                        shlink_args = []

                        # TODO(T110378143): Support post link flags properly.
                        shlink_args.extend(exported_linker_flags)
                        shlink_args.extend(get_link_whole_args(linker_type, [lib]))
                        shared_lib = cxx_link_into_shared_library(
                            ctx,
                            soname,
                            [
                                LinkArgs(flags = shlink_args),
                                # TODO(T110378118): As per v1, we always link against "shared"
                                # dependencies when building a shaerd library.
                                get_link_args(inherited_link, LinkStyle("shared")),
                            ],
                        )

                if shared_lib:
                    outs.append(shared_lib.output)

                    # Some prebuilt shared libs don't set a SONAME (e.g.
                    # IntelComposerXE), so we can't link them via just the shared
                    # lib (otherwise, we'll may embed buid-time paths in `DT_NEEDED`
                    # tags).
                    if ctx.attr.link_without_soname:
                        if ctx.attr.supports_shared_library_interface:
                            fail("cannot use `link_without_soname` with shlib interfaces")
                        linkable = SharedLibLinkable(
                            lib = shared_lib.output,
                            link_without_soname = True,
                        )
                    else:
                        shared_lib_for_linking = shared_lib.output

                        # Generate a shared library interface if the rule supports it.
                        if ctx.attr.supports_shared_library_interface and cxx_use_shlib_intfs(ctx):
                            shared_lib_for_linking = cxx_mk_shlib_intf(ctx, ctx.attr.name, shared_lib.output)
                        linkable = SharedLibLinkable(lib = shared_lib_for_linking)

                    # Provided means something external to the build will provide
                    # the libraries, so we don't need to propagate anything.
                    if not ctx.attr.provided:
                        solibs[soname] = shared_lib

        # TODO(cjhopman): is it okay that we sometimes don't have a linkable?
        outputs[link_style] = outs
        libraries[link_style] = LinkInfos(
            default = LinkInfo(
                name = ctx.attr.name,
                pre_flags = args,
                post_flags = post_link_flags,
                linkables = [linkable] if linkable else [],
            ),
        )

    # Create the default ouput for the library rule given it's link style and preferred linkage
    link_style = get_cxx_toolchain_info(ctx).linker_info.link_style
    actual_link_style = get_actual_link_style(link_style, preferred_linkage)
    output = outputs[actual_link_style]
    providers.append(DefaultInfo(default_outputs = output))

    # Propagate link info provider.
    providers.append(create_merged_link_info(
        ctx,
        # Add link info for each link style,
        libraries,
        preferred_linkage = preferred_linkage,
        # Export link info from out (exported) deps.
        exported_deps = [inherited_link],
    ))

    # Propagate shared libraries up the tree.
    providers.append(merge_shared_libraries(
        ctx.actions,
        create_shared_libraries(ctx, solibs),
        filter(None, map_idx(SharedLibraryInfo, first_order_deps)),
    ))

    # Create, augment and provide the linkable graph.
    linkable_graph = create_merged_linkable_graph(ctx.label, first_order_deps)
    add_linkable_node(
        linkable_graph,
        ctx,
        preferred_linkage = preferred_linkage,
        # If we don't have link input for this link style, we pass in `None` so
        # that omnibus knows to avoid it.
        link_infos = libraries,
        shared_libs = solibs,
        excluded = not value_or(ctx.attr.supports_merged_linking, True),
        exported_deps = first_order_deps,
    )
    providers.append(linkable_graph)

    # Omnibus root provider.
    if LinkStyle("static_pic") in libraries and (static_pic_lib or static_lib) and not ctx.attr.header_only:
        # TODO(cjhopman): This doesn't support thin archives
        providers.append(create_native_link_target(
            name = soname,
            link_info = LinkInfo(
                name = soname,
                pre_flags = cxx_attr_exported_linker_flags(ctx),
                linkables = [ArchiveLinkable(
                    archive = Archive(
                        artifact = static_pic_lib or static_lib,
                    ),
                    linker_type = linker_type,
                    link_whole = True,
                )],
                post_flags = cxx_attr_exported_post_linker_flags(ctx),
            ),
            deps = first_order_deps,
        ))

    return providers

def cxx_precompiled_header_impl(ctx: "context") -> ["provider"]:
    inherited_pp_infos = cxx_inherited_preprocessor_infos(ctx.attr.deps)
    inherited_link = cxx_inherited_link_info(ctx, ctx.attr.deps)
    return [
        DefaultInfo(default_outputs = [ctx.attr.src]),
        cxx_merge_cpreprocessors(ctx, [], inherited_pp_infos),
        inherited_link,
        CPrecompiledHeaderInfo(header = ctx.attr.src),
    ]

def cxx_test_impl(ctx: "context") -> ["provider"]:
    # TODO(T110378115): have the runinfo contain the correct test running args
    params = CxxRuleConstructorParams(
        rule_type = "cxx_test",
        headers_layout = cxx_get_regular_cxx_headers_layout(ctx),
        srcs = get_srcs_with_flags(ctx),
    )
    output, comp_db_info = cxx_executable(ctx, params, is_cxx_test = True)

    command = [cmd_args(output.binary).hidden(output.runtime_files)] + ctx.attr.args

    return [
        DefaultInfo(default_outputs = [output.binary], other_outputs = output.runtime_files, sub_targets = output.sub_targets),
        # TODO(T110378106): handle env vars
        RunInfo(args = cmd_args(command)),
        ExternalRunnerTestInfo(
            type = "gtest",
            command = command,
            env = ctx.attr.env,
            labels = ctx.attr.labels,
            contacts = ctx.attr.contacts,
        ),
        comp_db_info,
    ]

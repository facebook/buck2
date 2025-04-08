# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//:paths.bzl", "paths")
load(
    "@prelude//android:android_providers.bzl",
    "merge_android_packageable_info",
)
load("@prelude//apple:resource_groups.bzl", "create_resource_graph")
load("@prelude//cxx:cxx_sources.bzl", "get_srcs_with_flags")
load("@prelude//cxx:cxx_utility.bzl", "cxx_attrs_get_allow_cache_upload")
load(
    "@prelude//cxx:link_groups_types.bzl",
    "LinkGroupInfo",  # @unused Used as a type
)
load(
    "@prelude//cxx:runtime_dependency_handling.bzl",
    "cxx_attr_runtime_dependency_handling",
)
load("@prelude//linking:execution_preference.bzl", "LinkExecutionPreference")
load(
    "@prelude//linking:link_groups.bzl",
    "merge_link_group_lib_info",
)
load(
    "@prelude//linking:link_info.bzl",
    "Archive",
    "ArchiveLinkable",
    "CxxSanitizerRuntimeInfo",
    "LibOutputStyle",
    "LinkArgs",
    "LinkCommandDebugOutputInfo",
    "LinkInfo",
    "LinkInfos",
    "LinkStrategy",
    "LinkedObject",
    "SharedLibLinkable",
    "create_merged_link_info",
    "create_merged_link_info_for_propagation",
    "get_lib_output_style",
    "get_link_args_for_strategy",
    "get_output_styles_for_linkage",
    "subtarget_for_output_style",
    "to_link_strategy",
)
load(
    "@prelude//linking:linkable_graph.bzl",
    "DlopenableLibraryInfo",
    "LinkableGraph",
    "create_linkable_graph",
    "create_linkable_graph_node",
    "create_linkable_node",
)
load(
    "@prelude//linking:linkables.bzl",
    "linkables",
)
load(
    "@prelude//linking:shared_libraries.bzl",
    "SharedLibraries",
    "SharedLibraryInfo",
    "create_shlib_from_ctx",
    "extract_soname_from_shlib",
    "merge_shared_libraries",
    "to_soname",
)
load("@prelude//linking:strip.bzl", "strip_debug_info")
load("@prelude//linking:types.bzl", "Linkage")
load("@prelude//os_lookup:defs.bzl", "OsLookup")
load("@prelude//python:manifest.bzl", "create_manifest_for_entries")
load("@prelude//test:inject_test_run_info.bzl", "inject_test_run_info")
load(
    "@prelude//tests:re_utils.bzl",
    "get_re_executors_from_props",
)
load(
    "@prelude//third-party:build.bzl",
    "create_third_party_build_info",
)
load("@prelude//unix:providers.bzl", "UnixEnv", "create_unix_env_info")
load("@prelude//utils:expect.bzl", "expect")
load(
    "@prelude//utils:utils.bzl",
    "filter_and_map_idx",
    "value_or",
)
load(":cxx_context.bzl", "get_cxx_toolchain_info")
load(":cxx_executable.bzl", "cxx_executable")
load(
    ":cxx_library.bzl",
    "CxxLibraryOutput",  # @unused Used as a type
    "cxx_library_parameterized",
)
load(
    ":cxx_library_utility.bzl",
    "cxx_attr_dep_metadata",
    "cxx_attr_deps",
    "cxx_attr_exported_deps",
    "cxx_attr_link_style",
    "cxx_attr_linker_flags_all",
    "cxx_attr_preferred_linkage",
    "cxx_inherited_link_info",
    "cxx_platform_supported",
    "cxx_use_shlib_intfs",
)
load(
    ":cxx_types.bzl",
    "CxxRuleConstructorParams",
    "CxxRuleProviderParams",
    "CxxRuleSubTargetParams",
)
load(
    ":groups_types.bzl",
    "Group",
    "MATCH_ALL_LABEL",
    "NO_MATCH_LABEL",
)
load(
    ":headers.bzl",
    "CPrecompiledHeaderInfo",
    "cxx_get_regular_cxx_headers_layout",
)
load(
    ":link.bzl",
    "cxx_link_shared_library",
)
load(
    ":link_groups.bzl",
    "LinkGroupLibSpec",
    "get_link_group_info",
)
load(
    ":link_types.bzl",
    "link_options",
)
load(
    ":linker.bzl",
    "DUMPBIN_SUB_TARGET",
    "PDB_SUB_TARGET",
    "get_dumpbin_providers",
    "get_link_whole_args",
    "get_pdb_providers",
    "get_shared_library_name",
    "get_shared_library_name_for_param",
)
load(
    ":omnibus.bzl",
    "create_linkable_root",
)
load(":platform.bzl", "cxx_by_platform")
load(
    ":preprocessor.bzl",
    "CPreprocessor",
    "CPreprocessorArgs",
    "cxx_exported_preprocessor_info",
    "cxx_inherited_preprocessor_infos",
    "cxx_merge_cpreprocessors",
    "format_system_include_arg",
)
load(
    ":shared_library_interface.bzl",
    "shared_library_interface",
)

#####################################################################
# Operations

def _get_shared_link_style_sub_targets_and_providers(
        output_style: LibOutputStyle,
        ctx: AnalysisContext,
        output: [CxxLibraryOutput, None]) -> (dict[str, list[Provider]], list[Provider]):
    if output_style != LibOutputStyle("shared_lib") or output == None:
        return ({}, [])
    sub_targets = {}
    providers = []
    if output.dwp != None:
        sub_targets["dwp"] = [DefaultInfo(default_output = output.dwp)]
    if output.pdb != None:
        sub_targets[PDB_SUB_TARGET] = get_pdb_providers(pdb = output.pdb, binary = output.default)
    cxx_toolchain = get_cxx_toolchain_info(ctx)
    if cxx_toolchain.dumpbin_toolchain_path != None:
        sub_targets[DUMPBIN_SUB_TARGET] = get_dumpbin_providers(ctx, output.default, cxx_toolchain.dumpbin_toolchain_path)
    if output.linker_map != None:
        sub_targets["linker-map"] = [DefaultInfo(default_output = output.linker_map.map, other_outputs = [output.linker_map.binary])]
    if output.implib != None:
        sub_targets["implib"] = [DefaultInfo(default_output = output.implib)]
    return (sub_targets, providers)

def cxx_library_impl(ctx: AnalysisContext) -> list[Provider]:
    if ctx.attrs.can_be_asset and ctx.attrs.used_by_wrap_script:
        fail("Cannot use `can_be_asset` and `used_by_wrap_script` in the same rule")

    if ctx.attrs._is_building_android_binary:
        sub_target_params, provider_params = _get_params_for_android_binary_cxx_library()
    else:
        sub_target_params = CxxRuleSubTargetParams()
        provider_params = CxxRuleProviderParams(
            third_party_build = True,
        )

    params = CxxRuleConstructorParams(
        rule_type = "cxx_library",
        headers_layout = cxx_get_regular_cxx_headers_layout(ctx),
        srcs = get_srcs_with_flags(ctx),
        output_style_sub_targets_and_providers_factory = _get_shared_link_style_sub_targets_and_providers,
        generate_sub_targets = sub_target_params,
        generate_providers = provider_params,
        compiler_flags = ctx.attrs.compiler_flags,
        lang_compiler_flags = ctx.attrs.lang_compiler_flags,
        platform_compiler_flags = ctx.attrs.platform_compiler_flags,
        lang_platform_compiler_flags = ctx.attrs.lang_platform_compiler_flags,
        preprocessor_flags = ctx.attrs.preprocessor_flags,
        lang_preprocessor_flags = ctx.attrs.lang_preprocessor_flags,
        platform_preprocessor_flags = ctx.attrs.platform_preprocessor_flags,
        lang_platform_preprocessor_flags = ctx.attrs.lang_platform_preprocessor_flags,
        use_header_units = ctx.attrs.use_header_units,
        export_header_unit = ctx.attrs.export_header_unit,
        export_header_unit_filter = ctx.attrs.export_header_unit_filter,
        error_handler = get_cxx_toolchain_info(ctx).cxx_error_handler,
    )
    output = cxx_library_parameterized(ctx, params)
    return output.providers

def _only_shared_mappings(group: Group) -> bool:
    """
    Return whether this group only has explicit "shared" linkage mappings,
    which indicates a group that re-uses pre-linked libs.
    """
    for mapping in group.mappings:
        if mapping.preferred_linkage != Linkage("shared"):
            return False
    return True

def create_shared_lib_link_group_specs(ctx: AnalysisContext, link_group_definitions: list[Group]) -> list[LinkGroupLibSpec]:
    specs = []
    linker_info = get_cxx_toolchain_info(ctx).linker_info
    for group in link_group_definitions:
        if group.name in (MATCH_ALL_LABEL, NO_MATCH_LABEL):
            continue

        # TODO(agallagher): We should probably add proper handling for "provided"
        # system handling to avoid needing this special case.
        if _only_shared_mappings(group):
            continue
        specs.append(
            LinkGroupLibSpec(
                name = get_shared_library_name(linker_info, group.name, apply_default_prefix = True),
                is_shared_lib = True,
                group = group,
            ),
        )
    return specs

def get_auto_link_group_specs(ctx: AnalysisContext, link_group_info: [LinkGroupInfo, None]) -> [list[LinkGroupLibSpec], None]:
    if link_group_info == None or not ctx.attrs.auto_link_groups:
        return None
    return create_shared_lib_link_group_specs(ctx, link_group_info.groups.values())

def cxx_binary_impl(ctx: AnalysisContext) -> list[Provider]:
    link_strategy = to_link_strategy(cxx_attr_link_style(ctx))
    link_group_info = get_link_group_info(
        ctx,
        filter_and_map_idx(LinkableGraph, cxx_attr_deps(ctx)),
        link_strategy,
    )
    params = CxxRuleConstructorParams(
        rule_type = "cxx_binary",
        executable_name = ctx.attrs.executable_name,
        headers_layout = cxx_get_regular_cxx_headers_layout(ctx),
        srcs = get_srcs_with_flags(ctx),
        link_group_info = link_group_info,
        auto_link_group_specs = get_auto_link_group_specs(ctx, link_group_info),
        prefer_stripped_objects = ctx.attrs.prefer_stripped_objects,
        exe_allow_cache_upload = cxx_attrs_get_allow_cache_upload(ctx.attrs),
        extra_link_roots = linkables(ctx.attrs.link_group_deps),
        compiler_flags = ctx.attrs.compiler_flags,
        lang_compiler_flags = ctx.attrs.lang_compiler_flags,
        platform_compiler_flags = ctx.attrs.platform_compiler_flags,
        lang_platform_compiler_flags = ctx.attrs.lang_platform_compiler_flags,
        preprocessor_flags = ctx.attrs.preprocessor_flags,
        lang_preprocessor_flags = ctx.attrs.lang_preprocessor_flags,
        platform_preprocessor_flags = ctx.attrs.platform_preprocessor_flags,
        lang_platform_preprocessor_flags = ctx.attrs.lang_platform_preprocessor_flags,
        use_header_units = ctx.attrs.use_header_units,
        runtime_dependency_handling = cxx_attr_runtime_dependency_handling(ctx),
        error_handler = get_cxx_toolchain_info(ctx).cxx_error_handler,
    )
    output = cxx_executable(ctx, params)

    extra_providers = []
    if output.link_command_debug_output:
        extra_providers.append(LinkCommandDebugOutputInfo(debug_outputs = [output.link_command_debug_output]))
    if output.sanitizer_runtime_files:
        extra_providers.append(CxxSanitizerRuntimeInfo(runtime_files = output.sanitizer_runtime_files))

    # Unix env provider.
    extra_providers.append(
        create_unix_env_info(
            actions = ctx.actions,
            env = UnixEnv(
                label = ctx.label,
                binaries = [
                    create_manifest_for_entries(
                        ctx = ctx,
                        name = "unix_env",
                        entries = [
                            (ctx.label.name, output.binary, ""),
                        ],
                    ),
                ],
            ),
            # TODO(agallagher): We only want to traverse deps when dynamically
            # linking.
            #deps = ctx.attrs.deps,
        ),
    )

    # When an executable is the output of a build, also materialize all the
    # unpacked external debuginfo that goes with it. This makes `buck2 build
    # :main` equivalent to `buck2 build :main :main[debuginfo]`.
    #
    # This is wasted work if we are building an executable together with its dwp
    # subtarget (`buck2 build :main :main[dwp]`) in which case a large number of
    # unpacked debuginfo files can end up being materialized redundantly. LLDB
    # will ignore them and obtain debuginfo via the single packed debuginfo file
    # instead.
    #
    # But materializing unpacked debuginfo is the right tradeoff because it
    # means the output of `buck2 build :main` is always immediately usable in a
    # debugger.
    #
    # External debuginfo is *not* materialized when an executable is depended on
    # by another rule, such as by $(exe ...) or exec_dep.
    other_outputs = output.runtime_files + output.external_debug_info_artifacts

    return [
        DefaultInfo(
            default_output = output.binary,
            other_outputs = other_outputs,
            sub_targets = output.sub_targets,
        ),
        RunInfo(args = cmd_args(output.binary, hidden = output.runtime_files)),
        output.compilation_db,
        output.xcode_data,
        output.dist_info,
    ] + extra_providers

def _prebuilt_item(
        ctx: AnalysisContext,
        item: [typing.Any, None],
        platform_items: [list[(str, typing.Any)], None]) -> [typing.Any, None]:
    """
    Parse the given item that can be specified by regular and platform-specific
    parameters.
    """

    if item != None:
        return item

    if platform_items != None:
        items = dedupe(cxx_by_platform(ctx, platform_items))
        if len(items) == 0:
            return None
        if len(items) != 1:
            fail("expected single platform match: name={}//{}:{}, platform_items={}, items={}".format(ctx.label.cell, ctx.label.package, ctx.label.name, str(platform_items), str(items)))
        return items[0]

    return None

def _prebuilt_linkage(ctx: AnalysisContext) -> Linkage:
    """
    Construct the preferred linkage to use for the given prebuilt library.
    """
    if ctx.attrs.header_only:
        return Linkage("any")
    if ctx.attrs.force_static:
        return Linkage("static")
    preferred_linkage = cxx_attr_preferred_linkage(ctx)
    if preferred_linkage != Linkage("any"):
        return preferred_linkage
    if ctx.attrs.provided:
        return Linkage("shared")
    return Linkage("any")

def prebuilt_cxx_library_impl(ctx: AnalysisContext) -> list[Provider]:
    # Versioned params should be intercepted and converted away via the stub.
    expect(not ctx.attrs.versioned_exported_lang_platform_preprocessor_flags)
    expect(not ctx.attrs.versioned_exported_lang_preprocessor_flags)
    expect(not ctx.attrs.versioned_exported_platform_preprocessor_flags)
    expect(not ctx.attrs.versioned_exported_preprocessor_flags)
    expect(not ctx.attrs.versioned_header_dirs)
    expect(not ctx.attrs.versioned_shared_lib)
    expect(not ctx.attrs.versioned_static_lib)
    expect(not ctx.attrs.versioned_static_pic_lib)

    if not cxx_platform_supported(ctx):
        return [DefaultInfo(default_output = None)]

    providers = []

    linker_info = get_cxx_toolchain_info(ctx).linker_info
    linker_type = linker_info.type

    # Parse library parameters.
    static_lib = _prebuilt_item(
        ctx,
        ctx.attrs.static_lib,
        ctx.attrs.platform_static_lib,
    )
    static_pic_lib = _prebuilt_item(
        ctx,
        ctx.attrs.static_pic_lib,
        ctx.attrs.platform_static_pic_lib,
    )
    shared_lib = _prebuilt_item(
        ctx,
        ctx.attrs.shared_lib,
        ctx.attrs.platform_shared_lib,
    )
    header_dirs = _prebuilt_item(
        ctx,
        ctx.attrs.header_dirs,
        ctx.attrs.platform_header_dirs,
    )
    preferred_linkage = _prebuilt_linkage(ctx)

    # Prepare the stripped static lib.
    static_lib_stripped = None
    if not ctx.attrs.prestripped and static_lib != None:
        static_lib_stripped = strip_debug_info(ctx, static_lib.short_path, static_lib)

    # Prepare the stripped static PIC lib.  If the static PIC lib is the same
    # artifact as the static lib, then just re-use the stripped static lib.
    static_pic_lib_stripped = None
    if not ctx.attrs.prestripped:
        if static_lib == static_pic_lib:
            static_pic_lib_stripped = static_lib_stripped
        elif static_pic_lib != None:
            static_pic_lib_stripped = strip_debug_info(ctx, static_pic_lib.short_path, static_pic_lib)

    if ctx.attrs.soname != None:
        soname = get_shared_library_name_for_param(linker_info, ctx.attrs.soname)
    elif shared_lib != None and ctx.attrs.extract_soname:
        soname = extract_soname_from_shlib(
            actions = ctx.actions,
            name = "__soname__.txt",
            shared_lib = shared_lib,
        )
    else:
        soname = get_shared_library_name(linker_info, ctx.label.name, apply_default_prefix = True)
    soname = to_soname(soname)

    # Use ctx.attrs.deps instead of cxx_attr_deps, since prebuilt rules don't have platform_deps.
    first_order_deps = ctx.attrs.deps
    exported_first_order_deps = cxx_attr_exported_deps(ctx)

    # Exported preprocessor info.
    inherited_pp_infos = cxx_inherited_preprocessor_infos(exported_first_order_deps)
    generic_exported_pre = cxx_exported_preprocessor_info(ctx, cxx_get_regular_cxx_headers_layout(ctx), [])
    args = []
    compiler_type = get_cxx_toolchain_info(ctx).cxx_compiler_info.compiler_type
    if header_dirs != None:
        for x in header_dirs:
            args.append(format_system_include_arg(cmd_args(x), compiler_type))
    exported_items = [generic_exported_pre]
    if args:
        exported_items.append(CPreprocessor(args = CPreprocessorArgs(args = args)))
    propagated_preprocessor = cxx_merge_cpreprocessors(
        ctx,
        exported_items,
        inherited_pp_infos,
    )
    providers.append(propagated_preprocessor)

    inherited_link = cxx_inherited_link_info(first_order_deps)
    inherited_exported_link = cxx_inherited_link_info(exported_first_order_deps)

    linker_flags = cxx_attr_linker_flags_all(ctx)

    # Gather link infos, outputs, and shared libs for effective link style.
    outputs = {}
    libraries = {}
    solibs = []
    sub_targets = {}
    for output_style in get_output_styles_for_linkage(preferred_linkage):
        out = None
        linkable = None
        linkable_stripped = None

        # If we have sources to compile, generate the necessary libraries and
        # add them to the exported link info.
        if not ctx.attrs.header_only:
            def archive_linkable(lib):
                return ArchiveLinkable(
                    archive = Archive(artifact = lib),
                    linker_type = linker_type,
                    link_whole = ctx.attrs.link_whole,
                    supports_lto = ctx.attrs.supports_lto,
                )

            if output_style == LibOutputStyle("archive"):
                if static_lib:
                    out = static_lib
                    linkable = archive_linkable(static_lib)
                    linkable_stripped = archive_linkable(static_lib_stripped) if static_lib_stripped else None
            elif output_style == LibOutputStyle("pic_archive"):
                lib = static_pic_lib or static_lib
                if lib:
                    out = lib
                    linkable = archive_linkable(lib)
                    linkable_stripped = archive_linkable(static_pic_lib_stripped or static_lib_stripped) if (static_pic_lib_stripped or static_lib_stripped) else None
            else:  # shared
                # If no shared library was provided, link one from the static libraries.
                if shared_lib != None:
                    shared_lib = LinkedObject(output = shared_lib, unstripped_output = shared_lib)
                else:
                    lib = static_pic_lib or static_lib
                    if lib:
                        shlink_args = []

                        # TODO(T110378143): Support post link flags properly.
                        shlink_args.extend(linker_flags.exported_flags)
                        shlink_args.extend(linker_flags.flags)
                        shlink_args.extend(get_link_whole_args(linker_type, [lib]))
                        link_result = cxx_link_shared_library(
                            ctx = ctx,
                            output = soname.ensure_str(),
                            name = soname.ensure_str(),
                            opts = link_options(
                                links = [
                                    LinkArgs(flags = shlink_args),
                                    # TODO(T110378118): As per v1, we always link against "shared"
                                    # dependencies when building a shaerd library.
                                    get_link_args_for_strategy(ctx, inherited_exported_link, LinkStrategy("shared")),
                                ],
                                link_execution_preference = LinkExecutionPreference("any"),
                            ),
                        )
                        shared_lib = link_result.linked_object

                if shared_lib:
                    out = shared_lib.output

                    shared_lib_for_linking = shared_lib.output

                    # Generate a shared library interface if the rule supports it.
                    if ctx.attrs.supports_shared_library_interface and cxx_use_shlib_intfs(ctx):
                        shared_lib_for_linking = shared_library_interface(
                            ctx = ctx,
                            shared_lib = shared_lib.output,
                        )
                    if ctx.attrs._target_os_type[OsLookup].platform == "windows":
                        shared_lib_for_linking = ctx.attrs.import_lib

                    linkable = None
                    if shared_lib_for_linking != None:
                        linkable = SharedLibLinkable(
                            lib = shared_lib_for_linking,
                            # Some prebuilt shared libs don't set a SONAME (e.g.
                            # IntelComposerXE), so we can't link them via just the shared
                            # lib (otherwise, we may embed build-time paths in `DT_NEEDED`
                            # tags).
                            link_without_soname = ctx.attrs.link_without_soname,
                        )

                    # Provided means something external to the build will provide
                    # the libraries, so we don't need to propagate anything.
                    if not ctx.attrs.provided:
                        solibs.append(
                            create_shlib_from_ctx(
                                ctx = ctx,
                                lib = shared_lib,
                                soname = soname,
                            ),
                        )

                    # Provide a sub-target that always provides the shared lib
                    # using the soname.
                    if soname and soname.is_str and shared_lib.output.basename != paths.basename(soname.ensure_str()):
                        soname_lib = ctx.actions.copy_file(soname.ensure_str(), shared_lib.output)
                    else:
                        soname_lib = shared_lib.output
                    sub_targets["soname-lib"] = [DefaultInfo(default_output = soname_lib)]

                    if shared_lib.pdb:
                        sub_targets[PDB_SUB_TARGET] = get_pdb_providers(pdb = shared_lib.pdb, binary = shared_lib.output)
                    dumpbin_toolchain_path = get_cxx_toolchain_info(ctx).dumpbin_toolchain_path
                    if dumpbin_toolchain_path != None:
                        sub_targets[DUMPBIN_SUB_TARGET] = get_dumpbin_providers(ctx, shared_lib.output, dumpbin_toolchain_path)
                    if shared_lib.dwp != None:
                        sub_targets["dwp"] = [DefaultInfo(default_output = shared_lib.dwp)]

        # TODO(cjhopman): is it okay that we sometimes don't have a linkable?
        outputs[output_style] = out
        dep_metadata = cxx_attr_dep_metadata(ctx)
        libraries[output_style] = LinkInfos(
            default = LinkInfo(
                name = ctx.attrs.name,
                pre_flags = linker_flags.exported_flags,
                post_flags = linker_flags.exported_post_flags,
                linkables = [linkable] if linkable else [],
                metadata = dep_metadata,
            ),
            stripped = None if linkable_stripped == None else LinkInfo(
                name = ctx.attrs.name,
                pre_flags = linker_flags.exported_flags,
                post_flags = linker_flags.exported_post_flags,
                linkables = [linkable_stripped],
                metadata = dep_metadata,
            ),
        )

        sub_targets[subtarget_for_output_style(output_style)] = [DefaultInfo(
            default_output = outputs[output_style],
        )]

    cxx_toolchain = get_cxx_toolchain_info(ctx)
    pic_behavior = cxx_toolchain.pic_behavior

    # Propagate link info provider.
    providers.append(create_merged_link_info(
        ctx,
        pic_behavior,
        # Add link info for each link style,
        libraries,
        preferred_linkage = preferred_linkage,
        # Export link info from non-exported deps (when necessary).
        deps = inherited_link,
        # Export link info from out (exported) deps.
        exported_deps = inherited_exported_link,
    ))

    # Propagate shared libraries up the tree.
    shared_libs = SharedLibraries(libraries = solibs)
    providers.append(merge_shared_libraries(
        ctx.actions,
        shared_libs,
        filter(None, [x.get(SharedLibraryInfo) for x in exported_first_order_deps]),
    ))

    providers.append(
        create_unix_env_info(
            actions = ctx.actions,
            env = UnixEnv(
                label = ctx.label,
                native_libs = [shared_libs],
            ),
            deps = ctx.attrs.deps + ctx.attrs.exported_deps,
        ),
    )

    # Third-party provider.
    third_party_build_info = create_third_party_build_info(
        ctx = ctx,
        paths = [] if header_dirs == None else [(d.short_path, d) for d in header_dirs],
        cxx_headers = [propagated_preprocessor],
        shared_libs = shared_libs.libraries,
        cxx_header_dirs = ["include"] + ([] if header_dirs == None else [d.short_path for d in header_dirs]),
        deps = ctx.attrs.deps + cxx_attr_exported_deps(ctx),
    )
    providers.append(third_party_build_info)
    sub_targets["third-party-build"] = [
        DefaultInfo(
            default_output = third_party_build_info.build.root.artifact,
            sub_targets = dict(
                manifest = [DefaultInfo(default_output = third_party_build_info.build.manifest)],
            ),
        ),
    ]

    # Create the default output for the library rule given it's link style and preferred linkage
    link_strategy = to_link_strategy(cxx_toolchain.linker_info.link_style)
    actual_output_style = get_lib_output_style(link_strategy, preferred_linkage, pic_behavior)
    output = outputs[actual_output_style]
    providers.append(DefaultInfo(
        default_output = output,
        sub_targets = sub_targets,
    ))

    # Omnibus root provider.
    if LibOutputStyle("pic_archive") in libraries and (static_pic_lib or static_lib) and not ctx.attrs.header_only and soname.is_str:
        # TODO(cjhopman): This doesn't support thin archives
        linkable_root = create_linkable_root(
            label = ctx.label,
            name = soname.ensure_str(),
            link_infos = LinkInfos(default = LinkInfo(
                name = soname.ensure_str(),
                pre_flags = (
                    linker_flags.exported_flags +
                    linker_flags.flags
                ),
                linkables = [ArchiveLinkable(
                    archive = Archive(
                        artifact = static_pic_lib or static_lib,
                    ),
                    linker_type = linker_type,
                    link_whole = True,
                )],
                post_flags = linker_flags.exported_post_flags,
                metadata = cxx_attr_dep_metadata(ctx),
            )),
            deps = exported_first_order_deps,
        )
        providers.append(linkable_root)

        # Mark libraries that support `dlopen`.
        if ctx.attrs.supports_python_dlopen:
            providers.append(DlopenableLibraryInfo())

    linkable_graph = create_linkable_graph(
        ctx,
        node = create_linkable_graph_node(
            ctx,
            linkable_node = create_linkable_node(
                ctx = ctx,
                default_soname = soname.as_str(),
                preferred_linkage = preferred_linkage,
                default_link_strategy = to_link_strategy(cxx_toolchain.linker_info.link_style),
                exported_deps = exported_first_order_deps,
                # If we don't have link input for this link style, we pass in `None` so
                # that omnibus knows to avoid it.
                link_infos = libraries,
                shared_libs = shared_libs,
                linker_flags = linker_flags,
                can_be_asset = getattr(ctx.attrs, "can_be_asset", False) or False,
                stub = getattr(ctx.attrs, "stub", False),
            ),
            excluded = {ctx.label: None} if not value_or(ctx.attrs.supports_merged_linking, True) else {},
        ),
        deps = exported_first_order_deps,
    )

    providers.append(linkable_graph)

    providers.append(
        merge_link_group_lib_info(
            deps = first_order_deps + exported_first_order_deps,
        ),
    )

    # TODO(T107163344) this shouldn't be in prebuilt_cxx_library itself, use overlays to remove it.
    providers.append(merge_android_packageable_info(ctx.label, ctx.actions, first_order_deps + exported_first_order_deps))

    apple_resource_graph = create_resource_graph(
        ctx = ctx,
        labels = ctx.attrs.labels,
        deps = first_order_deps,
        exported_deps = exported_first_order_deps,
    )
    providers += [apple_resource_graph]

    return providers

def cxx_precompiled_header_impl(ctx: AnalysisContext) -> list[Provider]:
    inherited_pp_infos = cxx_inherited_preprocessor_infos(ctx.attrs.deps)
    inherited_link = cxx_inherited_link_info(ctx.attrs.deps)
    return [
        DefaultInfo(default_output = ctx.attrs.src),
        cxx_merge_cpreprocessors(ctx, [], inherited_pp_infos),
        create_merged_link_info_for_propagation(ctx, inherited_link),
        CPrecompiledHeaderInfo(header = ctx.attrs.src),
    ]

def cxx_test_impl(ctx: AnalysisContext) -> list[Provider]:
    link_strategy = to_link_strategy(cxx_attr_link_style(ctx))
    link_group_info = get_link_group_info(
        ctx,
        filter_and_map_idx(LinkableGraph, cxx_attr_deps(ctx)),
        link_strategy,
    )

    # TODO(T110378115): have the runinfo contain the correct test running args
    params = CxxRuleConstructorParams(
        rule_type = "cxx_test",
        headers_layout = cxx_get_regular_cxx_headers_layout(ctx),
        srcs = get_srcs_with_flags(ctx),
        link_group_info = link_group_info,
        auto_link_group_specs = get_auto_link_group_specs(ctx, link_group_info),
        prefer_stripped_objects = ctx.attrs.prefer_stripped_objects,
        extra_link_roots = linkables(ctx.attrs.link_group_deps),
        compiler_flags = ctx.attrs.compiler_flags,
        lang_compiler_flags = ctx.attrs.lang_compiler_flags,
        platform_compiler_flags = ctx.attrs.platform_compiler_flags,
        lang_platform_compiler_flags = ctx.attrs.lang_platform_compiler_flags,
        preprocessor_flags = ctx.attrs.preprocessor_flags,
        lang_preprocessor_flags = ctx.attrs.lang_preprocessor_flags,
        platform_preprocessor_flags = ctx.attrs.platform_preprocessor_flags,
        lang_platform_preprocessor_flags = ctx.attrs.lang_platform_preprocessor_flags,
        use_header_units = ctx.attrs.use_header_units,
        runtime_dependency_handling = cxx_attr_runtime_dependency_handling(ctx),
        error_handler = get_cxx_toolchain_info(ctx).cxx_error_handler,
    )
    output = cxx_executable(ctx, params, is_cxx_test = True)

    command = [cmd_args(output.binary, hidden = output.runtime_files)] + ctx.attrs.args

    # Setup RE executors based on the `remote_execution` param.
    re_executor, executor_overrides = get_re_executors_from_props(ctx)

    return inject_test_run_info(
        ctx,
        ExternalRunnerTestInfo(
            type = "gtest",
            command = command,
            env = ctx.attrs.env,
            labels = ctx.attrs.labels,
            contacts = ctx.attrs.contacts,
            default_executor = re_executor,
            executor_overrides = executor_overrides,
            # We implicitly make this test via the project root, instead of
            # the cell root (e.g. fbcode root).
            run_from_project_root = (
                "buck2_run_from_project_root" in (ctx.attrs.labels or []) or
                re_executor != None
            ),
            use_project_relative_paths = re_executor != None,
        ),
    ) + [
        DefaultInfo(
            default_output = output.binary,
            other_outputs = output.runtime_files + output.external_debug_info_artifacts,
            sub_targets = output.sub_targets,
        ),
        output.compilation_db,
        output.xcode_data,
        output.dist_info,
    ]

def _get_params_for_android_binary_cxx_library() -> (CxxRuleSubTargetParams, CxxRuleProviderParams):
    sub_target_params = CxxRuleSubTargetParams(
        argsfiles = False,
        compilation_database = False,
        headers = False,
        link_group_map = False,
        xcode_data = False,
        clang_traces = False,
        objects = False,
        bitcode_bundle = False,
    )
    provider_params = CxxRuleProviderParams(
        compilation_database = False,
        omnibus_root = False,
        preprocessor_for_tests = False,
        cxx_resources_as_apple_resources = False,
    )

    return sub_target_params, provider_params

# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(
    "@prelude//:artifact_tset.bzl",
    "ArtifactTSet",  # @unused Used as a type
    "make_artifact_tset",
)
load("@prelude//:paths.bzl", "paths")
load(
    "@prelude//:resources.bzl",
    "ResourceInfo",
    "gather_resources",
)
load(
    "@prelude//android:android_providers.bzl",
    "merge_android_packageable_info",
)
load(
    "@prelude//apple:apple_frameworks.bzl",
    "apple_build_link_args_with_deduped_flags",
    "apple_create_frameworks_linkable",
    "apple_get_link_info_by_deduping_link_infos",
)
load(
    "@prelude//apple:apple_resource_types.bzl",
    "CxxResourceSpec",
)
load("@prelude//apple:resource_groups.bzl", "create_resource_graph")
load("@prelude//cxx:headers.bzl", "cxx_attr_exported_headers")
load(
    "@prelude//ide_integrations/xcode:argsfiles.bzl",
    "XCODE_ARGSFILES_SUB_TARGET",
)
load(
    "@prelude//ide_integrations/xcode:data.bzl",
    "XCODE_DATA_SUB_TARGET",
    "XcodeDataInfo",
    "generate_xcode_data",
)
load(
    "@prelude//java:java_providers.bzl",
    "get_java_packaging_info",
    "propagate_global_code_info",
)
load("@prelude//linking:execution_preference.bzl", "LinkExecutionPreference", "get_link_execution_preference")
load(
    "@prelude//linking:link_groups.bzl",
    "LinkGroupLib",  # @unused Used as a type
    "LinkGroupLibInfo",
    "gather_link_group_libs",
    "merge_link_group_lib_info",
)
load(
    "@prelude//linking:link_info.bzl",
    "ArchiveContentsType",
    "ArchiveLinkable",
    "FrameworksLinkable",  # @unused Used as a type
    "LibOutputStyle",
    "LinkArgs",
    "LinkCommandDebugOutputInfo",
    "LinkInfo",
    "LinkInfos",
    "LinkOrdering",
    "LinkStrategy",
    "LinkedObject",  # @unused Used as a type
    "ObjectsLinkable",
    "SharedLibLinkable",
    "SwiftmoduleLinkable",  # @unused Used as a type
    "UnstrippedLinkOutputInfo",
    "create_merged_link_info",
    "get_lib_output_style",
    "get_link_args_for_strategy",
    "get_output_styles_for_linkage",
    "make_link_command_debug_output",
    "make_link_command_debug_output_json_info",
    "process_link_strategy_for_pic_behavior",
    "subtarget_for_output_style",
    "to_link_strategy",
    "unpack_link_args",
    "wrap_link_info",
)
load(
    "@prelude//linking:linkable_graph.bzl",
    "DlopenableLibraryInfo",
    "LinkableRootInfo",
    "ReducedLinkableGraph",  # @unused used as a type
    "create_linkable_graph",
    "create_linkable_graph_node",
    "create_linkable_node",
    "linkable_deps",
    "reduce_linkable_graph",
)
load("@prelude//linking:shared_libraries.bzl", "SharedLibraryInfo", "create_shared_libraries", "merge_shared_libraries")
load("@prelude//linking:strip.bzl", "strip_debug_info")
load("@prelude//linking:types.bzl", "Linkage")
load(
    "@prelude//third-party:build.bzl",
    "create_third_party_build_info",
)
load("@prelude//unix:providers.bzl", "UnixEnv", "create_unix_env_info")
load("@prelude//utils:expect.bzl", "expect")
load("@prelude//utils:lazy.bzl", "lazy")
load(
    "@prelude//utils:utils.bzl",
    "flatten",
    "map_val",
    "value_or",
)
load(":archive.bzl", "make_archive")
load(
    ":argsfiles.bzl",
    "ARGSFILES_SUBTARGET",
    "get_argsfiles_output",
)
load(":bitcode.bzl", "BitcodeBundle", "BitcodeBundleInfo", "BitcodeTSet", "make_bitcode_bundle")
load(
    ":comp_db.bzl",
    "CxxCompilationDbInfo",
    "create_compilation_database",
    "make_compilation_db_info",
)
load(
    ":compile.bzl",
    "compile_cxx",
    "create_compile_cmds",
    "cxx_objects_sub_targets",
    "precompile_cxx",
)
load(
    ":compile_types.bzl",
    "CxxCompileCommandOutput",
    "CxxCompileFlavor",
    "CxxCompileOutput",  # @unused Used as a type
    "CxxSrcCompileCommand",
)
load(":cxx_context.bzl", "get_cxx_platform_info", "get_cxx_toolchain_info")
load(
    ":cxx_instrumentation.bzl",
    "build_exported_needs_coverage",
    "needs_coverage",
)
load(
    ":cxx_library_utility.bzl",
    "OBJECTS_SUBTARGET",
    "cxx_attr_dep_metadata",
    "cxx_attr_deps",
    "cxx_attr_exported_deps",
    "cxx_attr_link_strategy",
    "cxx_attr_link_style",
    "cxx_attr_linker_flags_all",
    "cxx_attr_preferred_linkage",
    "cxx_attr_resources",
    "cxx_inherited_link_info",
    "cxx_is_gnu",
    "cxx_platform_supported",
    "cxx_use_shlib_intfs",
    "cxx_use_shlib_intfs_mode",
)
load(
    ":cxx_toolchain_types.bzl",
    "LinkerType",
    "ShlibInterfacesMode",
    "is_bitcode_format",
)
load(
    ":cxx_types.bzl",
    "CxxLibraryInfo",
    "CxxRuleConstructorParams",  # @unused Used as a type
)
load(":diagnostics.bzl", "check_sub_target")
load(":gcno.bzl", "GcnoFilesInfo")
load(":index_store.bzl", "create_index_store_subtargets_and_provider")
load(
    ":link.bzl",
    "CxxLinkResult",  # @unused Used as a type
    "CxxLinkerMapData",
    "cxx_link_shared_library",
)
load(
    ":link_groups.bzl",
    "BuildLinkGroupsContext",
    "LINK_GROUP_MAP_DATABASE_SUB_TARGET",
    "collect_linkables",
    "get_filtered_labels_to_links_map",
    "get_filtered_links",
    "get_filtered_targets",
    "get_link_group",
    "get_link_group_info",
    "get_link_group_map_json",
    "get_link_group_preferred_linkage",
)
load(
    ":link_types.bzl",
    "link_options",
)
load(
    ":linker.bzl",
    "get_default_shared_library_name",
    "get_ignore_undefined_symbols_flags",
    "get_shared_library_name",
    "get_shared_library_name_for_param",
)
load(
    ":omnibus.bzl",
    "create_linkable_root",
)
load(
    ":preprocessor.bzl",
    "CPreprocessor",  # @unused Used as a type
    "CPreprocessorForTestsInfo",
    "CPreprocessorInfo",  # @unused Used as a type
    "cxx_exported_preprocessor_info",
    "cxx_inherited_preprocessor_infos",
    "cxx_merge_cpreprocessors",
    "cxx_private_preprocessor_info",
)
load(
    ":shared_library_interface.bzl",
    "SharedInterfaceInfo",  # @unused Used as a type
    "create_shared_interface_info",
    "create_shared_interface_info_with_children",
    "generate_exported_symbols",
    "generate_tbd_with_symbols",
    "shared_library_interface",
)

# A possible output of a `cxx_library`. This could be an archive or a shared library. Generally for an archive
# it represents just the sources of the library target itself, while a shared library will bundle multiple libraries
# together.
CxxLibraryOutput = record(
    # The output style of this output.
    output_style = field(LibOutputStyle),

    # The main output.
    default = field(Artifact),
    unstripped = field(Artifact),

    # Additional outputs that are implicitly used along with the above output
    # (e.g. external object files referenced by a thin archive).
    #
    # Note: It's possible that this can contain some of the artifacts which are
    # also present in object_files.
    other = field(list[Artifact], []),
    # The bitcode bundle. This only available for archive outputs.
    # TODO(cjhopman): always available? when is it/is it not available?
    bitcode_bundle = field([BitcodeBundle, None], None),
    # Additional debug info which referenced by but not included in the library output.
    external_debug_info = field(ArtifactTSet, ArtifactTSet()),
    # A shared shared library may have an associated dwp file with
    # its corresponding DWARF debug info.
    # May be None when Split DWARF is disabled, for static/static-pic libraries,
    # for some types of synthetic link objects or for pre-built shared libraries.
    dwp = field(Artifact | None, None),

    # A shared shared library may have an associated PDB file with
    # its corresponding Windows debug info.
    pdb = field(Artifact | None, None),
    # The import library is the linkable output of a Windows shared library build.
    implib = field(Artifact | None, None),
    # Data about the linker map, only available on shared libraries
    # TODO(cjhopman): always available? when is it/is it not available?
    linker_map = field([CxxLinkerMapData, None], None),

    # Extra sub targets to be returned as outputs of this rule, by link style.
    sub_targets = field(dict[str, list[DefaultInfo]]),

    # The object files used to create the artifact in `default`. This only includes the object files
    # for this library itself, so it's not particularly meaningful for a shared lib output.
    object_files = field(list[Artifact], []),
)

# The outputs of either archiving or linking the outputs of the library
_CxxAllLibraryOutputs = record(
    # The outputs for each lib output style.
    # For 'static'/'static_pic', these will be archives containing just this library's object files.
    # For 'shared', the output as a shared library. That output will be built using either this library's link_strategy link strategy
    # or via the link group strategy if this library has link group mapping.
    # A header-only lib won't produce any outputs (but it may still provide LinkInfos below).
    # TODO(cjhopman): make library-level link_group shared lib not put its output here
    outputs = field(dict[LibOutputStyle, CxxLibraryOutput]),

    # The link infos for linking against this lib of each output style. It's possible for a library to
    # add link_infos even when it doesn't produce an output itself.
    link_infos = field(dict[LibOutputStyle, LinkInfos]),

    # Extra providers to be returned consumers of this rule.
    providers = field(list[Provider], default = []),
    # Shared object name to shared library mapping if this target produces a shared library.
    solib = field([(str, LinkedObject), None]),
    sanitizer_runtime_files = field(list[Artifact], []),
)

_CxxLibraryCompileOutput = record(
    # object files
    objects = field(list[Artifact]),
    # object files stripped of debug information
    stripped_objects = field(list[Artifact]),
    # Those outputs which are bitcode
    bitcode_objects = field([list[Artifact], None]),
    # yaml file with optimization remarks about clang compilation
    clang_remarks = field([list[Artifact], None]),
    gcno_files = field([list[Artifact], None]),
    # json file with trace information about clang compilation
    clang_traces = field(list[Artifact]),
    # Externally referenced debug info, which doesn't get linked with the
    # object (e.g. the above `.o` when using `-gsplit-dwarf=single` or the
    # the `.dwo` when using `-gsplit-dwarf=split`).
    external_debug_info = field(list[Artifact]),
    # Whether there is any debug info
    objects_have_external_debug_info = field(bool),
    # sub_target for each object
    objects_sub_targets = field(dict[str, list[DefaultInfo]]),
    # the generated index stores
    index_stores = field(list[Artifact]),
    # diagnostics produced by a typecheck-only build (-fsyntax-only)
    diagnostics = field(dict[str, Artifact]),
)

# The output of compiling all the source files in the library, containing
# the commands use to compile them and all the object file variants.
_CxxCompiledSourcesOutput = record(
    # Compile commands used to compile the source files or generate object files
    compile_cmds = field(CxxCompileCommandOutput),
    # PIC compile outputs
    pic = field(_CxxLibraryCompileOutput),
    # PIC optimized compile outputs
    pic_optimized = field([_CxxLibraryCompileOutput, None]),
    # Non PIC compile outputs
    non_pic = field([_CxxLibraryCompileOutput, None]),
    # Header unit outputs
    header_unit_preprocessors = field(list[CPreprocessor]),
)

# The outputs of a cxx_library_parameterized rule.
_CxxLibraryParameterizedOutput = record(
    # The default output of a cxx library rule
    default_output = field([CxxLibraryOutput, None], None),
    # The other outputs available
    all_outputs = field([_CxxAllLibraryOutputs, None], None),
    # Any generated sub targets as requested by impl_params. Most of these will just be a
    # DefaultInfo to expose outputs to be consumed or built individually, but some (like "headers")
    # will have richer providers.
    sub_targets = field(dict[str, list[Provider]]),
    # A bundle of all bitcode files as a subtarget
    bitcode_bundle = field([BitcodeBundle, None], None),
    # Any generated providers as requested by impl_params.
    providers = field(list[Provider]),
    # XcodeDataInfo provider, returned separately as we cannot check
    # provider type from providers above
    xcode_data_info = field([XcodeDataInfo, None], None),
    # CxxCompilationDbInfo provider, returned separately as we cannot check
    # provider type from providers above
    cxx_compilationdb_info = field([CxxCompilationDbInfo, None], None),
    # LinkableRootInfo provider, same as above.
    linkable_root = field([LinkableRootInfo, None], None),
    # List of shared libraries for the sanitizer runtime linked into the library
    sanitizer_runtime_files = field(list[Artifact], []),
)

def cxx_library_parameterized(ctx: AnalysisContext, impl_params: CxxRuleConstructorParams) -> _CxxLibraryParameterizedOutput:
    """
    Defines the outputs for a cxx library, return the default output and any subtargets and providers based upon the requested params.
    """

    if not cxx_platform_supported(ctx):
        sub_targets = {}

        # Needed to handle cases of the named output (e.g. [static-pic]) being called directly.
        for output_style in get_output_styles_for_linkage(cxx_attr_preferred_linkage(ctx)):
            sub_targets[subtarget_for_output_style(output_style)] = [DefaultInfo(default_output = None)]

        return _CxxLibraryParameterizedOutput(
            providers = [
                DefaultInfo(default_output = None, sub_targets = sub_targets),
                SharedLibraryInfo(set = None),
            ],
            sub_targets = sub_targets,
        )

    non_exported_deps = cxx_attr_deps(ctx)
    exported_deps = cxx_attr_exported_deps(ctx)

    # TODO(T110378095) right now we implement reexport of exported_* flags manually, we should improve/automate that in the macro layer

    # Gather preprocessor inputs.
    (own_non_exported_preprocessor_info, test_preprocessor_infos) = cxx_private_preprocessor_info(
        ctx = ctx,
        raw_headers = ctx.attrs.raw_headers,
        headers_layout = impl_params.headers_layout,
        extra_preprocessors = impl_params.extra_preprocessors,
        non_exported_deps = non_exported_deps,
        is_test = impl_params.is_test,
    )
    own_exported_preprocessor_info = cxx_exported_preprocessor_info(ctx, impl_params.headers_layout, impl_params.extra_exported_preprocessors)
    own_preprocessors = [own_non_exported_preprocessor_info, own_exported_preprocessor_info] + test_preprocessor_infos
    own_exported_preprocessors = [own_exported_preprocessor_info]

    inherited_non_exported_preprocessor_infos = cxx_inherited_preprocessor_infos(
        non_exported_deps + filter(None, [ctx.attrs.precompiled_header]),
    )
    inherited_exported_preprocessor_infos = cxx_inherited_preprocessor_infos(exported_deps)

    preferred_linkage = cxx_attr_preferred_linkage(ctx)

    exported_needs_coverage = build_exported_needs_coverage(ctx, exported_deps + non_exported_deps)
    compiled_srcs = cxx_compile_srcs(
        ctx = ctx,
        impl_params = impl_params,
        own_preprocessors = own_preprocessors,
        own_exported_preprocessors = own_exported_preprocessors,
        inherited_non_exported_preprocessor_infos = inherited_non_exported_preprocessor_infos,
        inherited_exported_preprocessor_infos = inherited_exported_preprocessor_infos,
        preferred_linkage = preferred_linkage,
        add_coverage_instrumentation_compiler_flags = needs_coverage(exported_needs_coverage),
    )

    sub_targets = {}
    providers = []

    providers.append(exported_needs_coverage)

    if len(ctx.attrs.tests) > 0 and impl_params.generate_providers.preprocessor_for_tests:
        providers.append(
            CPreprocessorForTestsInfo(
                test_names = [test_target.name for test_target in ctx.attrs.tests],
                own_non_exported_preprocessor = own_non_exported_preprocessor_info,
            ),
        )

    if impl_params.generate_sub_targets.argsfiles:
        sub_targets[ARGSFILES_SUBTARGET] = [get_argsfiles_output(ctx, compiled_srcs.compile_cmds.argsfiles.relative, ARGSFILES_SUBTARGET)]
        sub_targets[XCODE_ARGSFILES_SUB_TARGET] = [get_argsfiles_output(ctx, compiled_srcs.compile_cmds.argsfiles.xcode, XCODE_ARGSFILES_SUB_TARGET)]

    if impl_params.generate_sub_targets.clang_remarks:
        if compiled_srcs.non_pic and compiled_srcs.non_pic.clang_remarks:
            sub_targets["clang-remarks"] = [DefaultInfo(
                default_outputs = compiled_srcs.non_pic.clang_remarks,
            )]

        if compiled_srcs.pic.clang_remarks:
            sub_targets["pic-clang-remarks"] = [DefaultInfo(
                default_outputs = compiled_srcs.pic.clang_remarks,
            )]

    if impl_params.generate_sub_targets.clang_traces:
        if compiled_srcs.non_pic and compiled_srcs.non_pic.clang_traces:
            sub_targets["clang-trace"] = [DefaultInfo(
                default_outputs = compiled_srcs.non_pic.clang_traces,
            )]

        if compiled_srcs.pic.clang_traces:
            sub_targets["pic-clang-trace"] = [DefaultInfo(
                default_outputs = compiled_srcs.pic.clang_traces,
            )]

    if impl_params.generate_sub_targets.objects:
        objects_sub_targets = compiled_srcs.pic.objects_sub_targets
        if compiled_srcs.non_pic:
            objects_sub_targets = objects_sub_targets | compiled_srcs.non_pic.objects_sub_targets
        sub_targets[OBJECTS_SUBTARGET] = [DefaultInfo(sub_targets = objects_sub_targets)]
        if len(compiled_srcs.pic.diagnostics) > 0:
            sub_targets["check"] = check_sub_target(ctx, compiled_srcs.pic.diagnostics)

    # Compilation DB.
    if impl_params.generate_sub_targets.compilation_database:
        comp_db = create_compilation_database(ctx, compiled_srcs.compile_cmds.src_compile_cmds, "compilation-database")
        sub_targets["compilation-database"] = [comp_db]

        # Compilation DB including headers.
        comp_db = create_compilation_database(ctx, compiled_srcs.compile_cmds.comp_db_compile_cmds, "full-compilation-database")
        sub_targets["full-compilation-database"] = [comp_db]

    # comp_db_compile_cmds can include header files being compiled as C++ which should not be exposed in the [compilation-database] subtarget
    comp_db_info = None
    if impl_params.generate_providers.compilation_database:
        comp_db_info = make_compilation_db_info(compiled_srcs.compile_cmds.comp_db_compile_cmds, get_cxx_toolchain_info(ctx), get_cxx_platform_info(ctx))
        providers.append(comp_db_info)

    # Shared library interfaces are partial lists of exported symbols that are merged at link time.
    exported_symbol_outputs = impl_params.extra_shared_library_interfaces if impl_params.extra_shared_library_interfaces else []
    if cxx_use_shlib_intfs_mode(ctx, ShlibInterfacesMode("stub_from_headers")):
        transitive_pp = inherited_exported_preprocessor_infos
        if _attr_reexport_all_header_dependencies(ctx):
            transitive_pp += inherited_non_exported_preprocessor_infos

        cxx_exported_symbols = generate_exported_symbols(
            ctx,
            cxx_attr_exported_headers(ctx, impl_params.headers_layout),
            own_exported_preprocessor_info,
            transitive_pp,
        )
        exported_symbol_outputs.append(cxx_exported_symbols)
        sub_targets["exported-symbols"] = [DefaultInfo(default_outputs = exported_symbol_outputs)]

    # Link Groups
    link_group = get_link_group(ctx)
    link_group_info = get_link_group_info(ctx)

    if link_group_info:
        link_groups = link_group_info.groups
        link_group_mappings = link_group_info.mappings
        link_group_deps = [link_group_info.graph]
        link_group_libs = gather_link_group_libs(
            deps = non_exported_deps + exported_deps,
        )
        providers.append(link_group_info)
    else:
        link_groups = {}
        link_group_mappings = {}
        link_group_deps = []
        link_group_libs = {}
    link_group_preferred_linkage = get_link_group_preferred_linkage(link_groups.values())

    # Create the linkable graph from the library's deps, exported deps and any link group deps.
    linkable_graph_deps = non_exported_deps + exported_deps + link_group_deps
    deps_linkable_graph = create_linkable_graph(
        ctx,
        deps = linkable_graph_deps,
    )

    frameworks_linkable = apple_create_frameworks_linkable(ctx)
    swiftmodule_linkable = impl_params.swiftmodule_linkable
    dep_infos, link_group_map, link_execution_preference, shared_interface_info = _get_shared_library_links(
        ctx,
        partial(reduce_linkable_graph, deps_linkable_graph),
        link_group,
        link_group_mappings,
        link_group_preferred_linkage,
        link_group_libs,
        exported_deps,
        non_exported_deps,
        impl_params.force_link_group_linking,
        frameworks_linkable,
        swiftmodule_linkable,
        force_static_follows_dependents = impl_params.link_groups_force_static_follows_dependents,
        exported_symbol_outputs = exported_symbol_outputs,
    )
    if impl_params.generate_sub_targets.link_group_map and link_group_map:
        sub_targets[LINK_GROUP_MAP_DATABASE_SUB_TARGET] = [link_group_map]

    extra_static_linkables = []
    if frameworks_linkable:
        extra_static_linkables.append(frameworks_linkable)
    if swiftmodule_linkable:
        extra_static_linkables.append(swiftmodule_linkable)

    library_outputs = _form_library_outputs(
        ctx = ctx,
        impl_params = impl_params,
        compiled_srcs = compiled_srcs,
        preferred_linkage = preferred_linkage,
        dep_infos = dep_infos,
        extra_static_linkables = extra_static_linkables,
        gnu_use_link_groups = cxx_is_gnu(ctx) and bool(link_group_mappings),
        link_execution_preference = link_execution_preference,
        shared_interface_info = shared_interface_info,
    )
    solib_as_dict = {library_outputs.solib[0]: library_outputs.solib[1]} if library_outputs.solib else {}
    shared_libs = create_shared_libraries(ctx, solib_as_dict)

    for _, link_style_output in library_outputs.outputs.items():
        for key in link_style_output.sub_targets.keys():
            expect(not key in sub_targets, "The subtarget `{}` already exists!".format(key))
        sub_targets.update(link_style_output.sub_targets)

    providers.extend(library_outputs.providers)

    pic_behavior = get_cxx_toolchain_info(ctx).pic_behavior

    # This is the output style for the library's own link_strategy+preferred_linkage.
    default_output_style = get_lib_output_style(to_link_strategy(cxx_attr_link_style(ctx)), preferred_linkage, pic_behavior)

    # Output sub-targets for all output-styles.
    if impl_params.generate_sub_targets.link_style_outputs or impl_params.generate_providers.link_style_outputs:
        default_output_style_providers = []
        for output_style in get_output_styles_for_linkage(preferred_linkage):
            output = library_outputs.outputs.get(output_style, None)
            output_style_sub_targets, output_style_providers = impl_params.output_style_sub_targets_and_providers_factory(
                output_style,
                ctx,
                output,
            )
            if output != None and output.unstripped != None:
                output_style_providers.append(
                    UnstrippedLinkOutputInfo(artifact = output.unstripped),
                )

            if output:
                # Add any subtargets for this output style.
                output_style_sub_targets.update(output.sub_targets)

            # TBD outputs are collected for each link unit, so propagate whenever
            # a library is being linked statically.
            if output_style != LibOutputStyle("shared_lib") and shared_interface_info != None:
                output_style_providers.append(shared_interface_info)

            if impl_params.generate_sub_targets.link_style_outputs:
                if output:
                    sub_targets[subtarget_for_output_style(output_style)] = [DefaultInfo(
                        default_output = output.default,
                        other_outputs = output.other,
                        sub_targets = output_style_sub_targets,
                    )] + (output_style_providers if output_style_providers else [])

                if output_style == default_output_style:
                    # If we have additional providers for the current link style,
                    # add them to the list of default providers
                    # TODO(cjhopman): This looks like a bug, adding to providers probably shouldn't depend on generate_sub_target.link_style_outputs
                    default_output_style_providers += output_style_providers

                    # In addition, ensure any subtargets for the active link style
                    # can be accessed as a default subtarget
                    for output_style_sub_target_name, output_style_sub_target_providers in output_style_sub_targets.items():
                        sub_targets[output_style_sub_target_name] = output_style_sub_target_providers

        if impl_params.generate_providers.link_style_outputs:
            providers += default_output_style_providers

    # Create the default output for the library rule given it's link style and preferred linkage
    # It's possible for a library to not produce any output, for example, a header only library doesn't produce any archive or shared lib
    default_output = library_outputs.outputs[default_output_style] if default_output_style in library_outputs.outputs else None

    if default_output and default_output.bitcode_bundle:
        sub_targets["bitcode"] = [DefaultInfo(default_output = default_output.bitcode_bundle.artifact)]

    # Define the xcode data sub target
    xcode_data_info = None
    if impl_params.generate_sub_targets.xcode_data:
        xcode_data_default_info, xcode_data_info = generate_xcode_data(
            ctx,
            rule_type = impl_params.rule_type,
            output = default_output.default if default_output else None,
            populate_rule_specific_attributes_func = impl_params.cxx_populate_xcode_attributes_func,
            srcs = impl_params.srcs + impl_params.additional.srcs,
            argsfiles = compiled_srcs.compile_cmds.argsfiles.xcode,
            product_name = get_default_cxx_library_product_name(ctx, impl_params),
        )
        sub_targets[XCODE_DATA_SUB_TARGET] = xcode_data_default_info
        providers.append(xcode_data_info)

    # Propagate link info provider.
    if impl_params.generate_providers.merged_native_link_info or impl_params.generate_providers.template_placeholders:
        # Gather link inputs.
        inherited_non_exported_link = cxx_inherited_link_info(non_exported_deps)
        inherited_exported_link = cxx_inherited_link_info(exported_deps)

        merged_native_link_info = create_merged_link_info(
            ctx,
            pic_behavior,
            # Add link info for each link style,
            library_outputs.link_infos,
            preferred_linkage = preferred_linkage,
            # Export link info from non-exported deps (when necessary).
            deps = inherited_non_exported_link,
            # Export link info from out (exported) deps.
            exported_deps = inherited_exported_link,
            frameworks_linkable = frameworks_linkable,
            swiftmodule_linkable = swiftmodule_linkable,
        )
        if impl_params.generate_providers.merged_native_link_info:
            providers.append(merged_native_link_info)
    else:
        # This code sets merged_native_link_info only in some cases, leaving it unassigned in others.
        # Add a fake definition set to None so the assignment checker is satisfied.
        merged_native_link_info = None

    # Propagate shared libraries up the tree.
    if impl_params.generate_providers.shared_libraries:
        providers.append(merge_shared_libraries(
            ctx.actions,
            shared_libs,
            filter(None, [x.get(SharedLibraryInfo) for x in non_exported_deps]) +
            filter(None, [x.get(SharedLibraryInfo) for x in exported_deps]),
        ))
        providers.append(
            create_unix_env_info(
                actions = ctx.actions,
                env = UnixEnv(
                    label = ctx.label,
                    native_libs = [shared_libs],
                ),
                deps = exported_deps + non_exported_deps,
            ),
        )

    propagated_preprocessor_merge_list = inherited_exported_preprocessor_infos
    if _attr_reexport_all_header_dependencies(ctx):
        propagated_preprocessor_merge_list = inherited_non_exported_preprocessor_infos + propagated_preprocessor_merge_list

    # Header unit PCM.
    if impl_params.generate_sub_targets.header_unit:
        if compiled_srcs.header_unit_preprocessors:
            header_unit_preprocessors = []
            header_unit_sub_targets = []
            header_unit_clang_traces = []

            def header_unit_clang_trace_sub_target(clang_traces):
                if impl_params.generate_sub_targets.clang_traces:
                    return {
                        "clang-trace": [
                            DefaultInfo(default_outputs = clang_traces),
                        ],
                    }
                else:
                    return {}

            def add_header_unit_clang_trace_sub_target(clang_traces):
                header_unit_clang_traces.extend(clang_traces)
                return header_unit_clang_trace_sub_target(clang_traces)

            for x in compiled_srcs.header_unit_preprocessors:
                header_unit_preprocessors.append(x)
                header_unit_sub_targets.append([
                    DefaultInfo(
                        default_outputs = [h.module for h in x.header_units],
                        sub_targets = add_header_unit_clang_trace_sub_target(filter(None, [h.clang_trace for h in x.header_units])),
                    ),
                    cxx_merge_cpreprocessors(
                        ctx,
                        own_exported_preprocessors + header_unit_preprocessors,
                        propagated_preprocessor_merge_list,
                    ),
                ])
            sub_targets["header-unit"] = [
                DefaultInfo(
                    default_outputs = [
                        h.module
                        for x in header_unit_preprocessors
                        for h in x.header_units
                    ],
                    sub_targets = {
                        str(i): x
                        for i, x in enumerate(header_unit_sub_targets)
                    } | header_unit_clang_trace_sub_target(header_unit_clang_traces),
                ),
                header_unit_sub_targets[-1][1],
            ]
            if impl_params.export_header_unit:
                own_exported_preprocessors.extend(header_unit_preprocessors)
        else:
            sub_targets["header-unit"] = [
                DefaultInfo(),
                cxx_merge_cpreprocessors(
                    ctx,
                    own_exported_preprocessors,
                    propagated_preprocessor_merge_list,
                ),
            ]

    propagated_preprocessor = cxx_merge_cpreprocessors(
        ctx,
        own_exported_preprocessors,
        propagated_preprocessor_merge_list,
    )
    if impl_params.generate_providers.preprocessors:
        providers.append(propagated_preprocessor)

    # Propagated_exported_preprocessor_info is used for pcm compilation, which isn't possible for non-modular targets.
    propagated_exported_preprocessor_info = propagated_preprocessor if impl_params.rule_type == "apple_library" and ctx.attrs.modular else None
    additional_providers = impl_params.additional.additional_providers_factory(propagated_exported_preprocessor_info) if impl_params.additional.additional_providers_factory else []

    if impl_params.generate_providers.third_party_build:
        third_party_build_info = create_third_party_build_info(
            ctx = ctx,
            cxx_headers = [propagated_preprocessor],
            shared_libs = shared_libs.libraries,
            deps = exported_deps + non_exported_deps,
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

    # For v1's `#headers` functionality.
    if impl_params.generate_sub_targets.headers:
        sub_targets["headers"] = [propagated_preprocessor, create_merged_link_info(
            ctx,
            pic_behavior = pic_behavior,
            preferred_linkage = Linkage("static"),
            frameworks_linkable = frameworks_linkable,
            swiftmodule_linkable = swiftmodule_linkable,
        ), LinkGroupLibInfo(libs = {}), SharedLibraryInfo(set = None)] + additional_providers

    if getattr(ctx.attrs, "supports_header_symlink_subtarget", False):
        header_symlink_mapping = {}
        for records in propagated_preprocessor.set.traverse():
            for record in records:
                for header in record.headers:
                    header_path = header.name
                    if header.namespace:
                        header_path = paths.join(header.namespace, header_path)
                    header_symlink_mapping[paths.normalize(header_path)] = header.artifact

        sub_targets["header-symlink-tree"] = [DefaultInfo(
            default_output = ctx.actions.symlinked_dir("header_symlink_tree", header_symlink_mapping),
        )]

    for additional_subtarget, subtarget_providers in impl_params.additional.subtargets.items():
        sub_targets[additional_subtarget] = subtarget_providers

    index_stores = []
    swift_index_stores = []
    if impl_params.index_stores:
        # Index store from swift compile
        index_stores.extend(impl_params.index_stores)
        swift_index_stores.extend(impl_params.index_stores)

    # Index stores from cxx compile. We only generate the index store for pic
    if compiled_srcs.pic:
        index_stores.extend(compiled_srcs.pic.index_stores)

    index_store_subtargets, index_store_info = create_index_store_subtargets_and_provider(ctx, index_stores, swift_index_stores, non_exported_deps + exported_deps)
    sub_targets.update(index_store_subtargets)
    providers.append(index_store_info)

    linker_flags = cxx_attr_linker_flags_all(ctx)

    # Omnibus root provider.
    linkable_root = None
    if impl_params.generate_providers.omnibus_root:
        if impl_params.use_soname:
            soname = _soname(ctx, impl_params)
        else:
            soname = None
        linker_type = get_cxx_toolchain_info(ctx).linker_info.type
        linkable_root = create_linkable_root(
            label = ctx.label,
            name = soname,
            link_infos = LinkInfos(
                default = LinkInfo(
                    pre_flags = linker_flags.flags + linker_flags.exported_flags,
                    post_flags = linker_flags.post_flags + linker_flags.exported_post_flags,
                    linkables = [ObjectsLinkable(
                        objects = compiled_srcs.pic.objects,
                        linker_type = linker_type,
                        link_whole = True,
                    )],
                    external_debug_info = make_artifact_tset(
                        actions = ctx.actions,
                        label = ctx.label,
                        artifacts = (
                            compiled_srcs.pic.external_debug_info +
                            (compiled_srcs.pic.objects if compiled_srcs.pic.objects_have_external_debug_info else [])
                        ),
                        children = impl_params.additional.static_external_debug_info,
                    ),
                    metadata = cxx_attr_dep_metadata(ctx),
                ),
                stripped = LinkInfo(
                    pre_flags = linker_flags.flags + linker_flags.exported_flags,
                    post_flags = linker_flags.post_flags + linker_flags.exported_post_flags,
                    linkables = [ObjectsLinkable(
                        objects = compiled_srcs.pic.stripped_objects,
                        linker_type = linker_type,
                        link_whole = True,
                    )],
                    metadata = cxx_attr_dep_metadata(ctx),
                ),
            ),
            deps = non_exported_deps + exported_deps,
        )
        providers.append(linkable_root)

        # Mark libraries that support `dlopen`.
        if getattr(ctx.attrs, "supports_python_dlopen", False):
            providers.append(DlopenableLibraryInfo())

    # Augment and provide the linkable graph.
    if impl_params.generate_providers.linkable_graph:
        merged_linkable_graph = create_linkable_graph(
            ctx,
            node = create_linkable_graph_node(
                ctx,
                linkable_node = create_linkable_node(
                    ctx = ctx,
                    default_soname = _soname(ctx, impl_params),
                    preferred_linkage = preferred_linkage,
                    default_link_strategy = cxx_attr_link_strategy(ctx.attrs),
                    deps = non_exported_deps,
                    exported_deps = exported_deps,
                    # If we don't have link input for this link style, we pass in `None` so
                    # that omnibus knows to avoid it.
                    include_in_android_mergemap = getattr(ctx.attrs, "include_in_android_merge_map_output", True) and default_output != None,
                    link_infos = library_outputs.link_infos,
                    shared_libs = shared_libs,
                    linker_flags = linker_flags,
                    can_be_asset = getattr(ctx.attrs, "can_be_asset", False) or False,
                    # We don't want to propagate shared interaces across shared library boundaries.
                    shared_interface_info = None if preferred_linkage == Linkage("shared") else create_shared_interface_info(ctx, exported_symbol_outputs, []),
                    stub = getattr(ctx.attrs, "stub", False),
                ),
                excluded = {ctx.label: None} if not value_or(ctx.attrs.supports_merged_linking, True) else {},
            ),
            deps = linkable_graph_deps,
        )
        providers.append(merged_linkable_graph)

    # C++ resource.
    if impl_params.generate_providers.resources:
        resources = cxx_attr_resources(ctx)
        cxx_resource_info = ResourceInfo(resources = gather_resources(
            label = ctx.label,
            resources = resources,
            deps = non_exported_deps + exported_deps,
        ))
        providers += [cxx_resource_info]
        if impl_params.generate_providers.cxx_resources_as_apple_resources:
            apple_resource_graph = create_resource_graph(
                ctx = ctx,
                labels = ctx.attrs.labels,
                deps = non_exported_deps,
                exported_deps = exported_deps,
                cxx_resource_spec = CxxResourceSpec(resources = resources) if resources else None,
            )
            providers += [apple_resource_graph]

    if impl_params.generate_providers.template_placeholders:
        templ_vars = {}

        # Some rules, e.g. fbcode//thrift/lib/cpp:thrift-core-module
        # define preprocessor flags as things like: -DTHRIFT_PLATFORM_CONFIG=<thrift/facebook/PlatformConfig.h>
        # and unless they get quoted, they break shell syntax.
        cxx_compiler_info = get_cxx_toolchain_info(ctx).cxx_compiler_info
        cxx_preprocessor_flags = cmd_args(
            cmd_args(cxx_compiler_info.preprocessor_flags or [], quote = "shell"),
            cmd_args(propagated_preprocessor.set.project_as_args("args"), quote = "shell"),
            propagated_preprocessor.set.project_as_args("include_dirs"),
        )
        templ_vars["cxxppflags"] = cxx_preprocessor_flags

        c_compiler_info = get_cxx_toolchain_info(ctx).c_compiler_info
        c_preprocessor_flags = cmd_args(
            cmd_args(c_compiler_info.preprocessor_flags or [], quote = "shell"),
            cmd_args(propagated_preprocessor.set.project_as_args("args"), quote = "shell"),
            propagated_preprocessor.set.project_as_args("include_dirs"),
        )
        templ_vars["cppflags"] = c_preprocessor_flags

        # Add in ldflag macros.
        for link_strategy in (LinkStrategy("static"), LinkStrategy("static_pic")):
            name = "ldflags-" + link_strategy.value.replace("_", "-")
            args = []
            linker_info = get_cxx_toolchain_info(ctx).linker_info
            args.append(linker_info.linker_flags or [])

            # Normally, we call get_link_args_for_strategy for getting the args for our own link from our
            # deps. This case is a bit different as we are effectively trying to get the args for how this library
            # would be represented on a dependent's link line and so it is appropriate to use our own merged_native_link_info.
            link_args = get_link_args_for_strategy(
                ctx,
                [merged_native_link_info],
                link_strategy,
            )
            args.append(unpack_link_args(link_args))
            templ_vars[name] = cmd_args(args)

        # TODO(T110378127): To implement `$(ldflags-shared ...)` properly, we'd need
        # to setup a symink tree rule for all transitive shared libs.  Since this
        # currently would be pretty costly (O(N^2)?), and since it's not that
        # commonly used anyway, just use `static-pic` instead.  Longer-term, once
        # v1 is gone, macros that use `$(ldflags-shared ...)` (e.g. Haskell's
        # hsc2hs) can move to a v2 rules-based API to avoid needing this macro.
        templ_vars["ldflags-shared"] = templ_vars["ldflags-static-pic"]

        providers.append(TemplatePlaceholderInfo(keyed_variables = templ_vars))

    # It is possible (e.g. in a java binary or an Android APK) to have C++ libraries that depend
    # upon Java libraries (through JNI). In some cases those Java libraries are not depended upon
    # anywhere else, so we need to expose them here to ensure that they are packaged into the
    # final binary.
    if impl_params.generate_providers.java_packaging_info:
        providers.append(get_java_packaging_info(ctx, non_exported_deps + exported_deps))

    if impl_params.generate_providers.java_global_code_info:
        providers.append(propagate_global_code_info(ctx, ctx.attrs.deps + getattr(ctx.attrs, "exported_deps", [])))

    # TODO(T107163344) this shouldn't be in cxx_library itself, use overlays to remove it.
    if impl_params.generate_providers.android_packageable_info:
        providers.append(merge_android_packageable_info(ctx.label, ctx.actions, non_exported_deps + exported_deps))

    bitcode_bundle = default_output.bitcode_bundle if default_output != None else None
    if bitcode_bundle:
        bc_provider = BitcodeBundleInfo(bitcode = bitcode_bundle, bitcode_bundle = ctx.actions.tset(BitcodeTSet, value = bitcode_bundle))
        additional_providers.append(bc_provider)

    if impl_params.generate_providers.default:
        if False:
            # TODO(nga): `default_output.unstripped` is never `None`.
            def unknown():
                pass

            default_output = unknown()

        default_info = DefaultInfo(
            default_output = default_output.default if default_output != None else None,
            other_outputs = default_output.other if default_output != None else [],
            sub_targets = sub_targets,
        )
        providers.append(default_info)

    # Propagate all transitive link group lib roots up the tree, so that the
    # final executable can use them.
    if impl_params.generate_providers.merged_native_link_info:
        providers.append(
            merge_link_group_lib_info(
                label = ctx.label,
                name = link_group,
                shared_libs = shared_libs,
                shared_link_infos = library_outputs.link_infos.get(LibOutputStyle("shared_lib")),
                deps = exported_deps + non_exported_deps,
            ),
        )

    if getattr(ctx.attrs, "_meta_apple_library_validation_enabled", False):
        providers.append(
            CxxLibraryInfo(
                target = ctx.label,
                labels = ctx.attrs.labels,
            ),
        )

    return _CxxLibraryParameterizedOutput(
        default_output = default_output,
        all_outputs = library_outputs,
        sub_targets = sub_targets,
        bitcode_bundle = bitcode_bundle,
        providers = providers + additional_providers,
        xcode_data_info = xcode_data_info,
        cxx_compilationdb_info = comp_db_info,
        linkable_root = linkable_root,
        sanitizer_runtime_files = library_outputs.sanitizer_runtime_files,
    )

def get_default_cxx_library_product_name(ctx, impl_params) -> str:
    preferred_linkage = cxx_attr_preferred_linkage(ctx)
    output_style = get_lib_output_style(
        to_link_strategy(cxx_attr_link_style(ctx)),
        preferred_linkage,
        get_cxx_toolchain_info(ctx).pic_behavior,
    )
    if output_style == LibOutputStyle("shared_lib"):
        return _soname(ctx, impl_params)
    else:
        return _base_static_library_name(ctx, optimized = False, stripped = False)

def _get_library_compile_output(
        ctx: AnalysisContext,
        src_compile_cmds: list[CxxSrcCompileCommand],
        outs: list[CxxCompileOutput],
        extra_link_input: list[Artifact]) -> _CxxLibraryCompileOutput:
    objects = [out.object for out in outs]
    stripped_objects = _strip_objects(ctx, objects)

    bitcode_objects = [
        out.object
        for out in outs
        if is_bitcode_format(out.object_format)
    ]
    if len(bitcode_objects) == 0:
        bitcode_objects = None

    objects_sub_targets = cxx_objects_sub_targets(outs)

    # add this after setting up sub_targets
    objects += extra_link_input
    stripped_objects += extra_link_input

    index_stores = [
        out.index_store
        for out in outs
        if out.index_store
    ]

    diagnostics = {
        compile_cmd.src.short_path: out.diagnostics
        for compile_cmd, out in zip(src_compile_cmds, outs)
        if out.diagnostics != None
    }

    return _CxxLibraryCompileOutput(
        objects = objects,
        stripped_objects = stripped_objects,
        bitcode_objects = bitcode_objects,
        clang_traces = [out.clang_trace for out in outs if out.clang_trace != None],
        clang_remarks = [out.clang_remarks for out in outs if out.clang_remarks != None],
        gcno_files = [out.gcno_file for out in outs if out.gcno_file != None],
        external_debug_info = [out.external_debug_info for out in outs if out.external_debug_info != None],
        objects_have_external_debug_info = lazy.is_any(lambda out: out.object_has_external_debug_info, outs),
        objects_sub_targets = objects_sub_targets,
        index_stores = index_stores,
        diagnostics = diagnostics,
    )

def cxx_compile_srcs(
        ctx: AnalysisContext,
        impl_params: CxxRuleConstructorParams,
        own_preprocessors: list[CPreprocessor],
        inherited_non_exported_preprocessor_infos: list[CPreprocessorInfo],
        inherited_exported_preprocessor_infos: list[CPreprocessorInfo],
        preferred_linkage: Linkage,
        add_coverage_instrumentation_compiler_flags: bool,
        own_exported_preprocessors: list[CPreprocessor] = []) -> _CxxCompiledSourcesOutput:
    """
    Compile objects we'll need for archives and shared libraries.
    """

    # Create the commands and argsfiles to use for compiling each source file
    compile_cmd_output = create_compile_cmds(
        ctx = ctx,
        impl_params = impl_params,
        own_preprocessors = own_preprocessors,
        inherited_preprocessor_infos = inherited_non_exported_preprocessor_infos + inherited_exported_preprocessor_infos,
        add_coverage_instrumentation_compiler_flags = add_coverage_instrumentation_compiler_flags,
    )

    # Define header unit.
    header_unit_preprocessors = []
    if own_exported_preprocessors:
        header_preprocessor_info = cxx_merge_cpreprocessors(
            ctx,
            own_exported_preprocessors,
            inherited_exported_preprocessor_infos,
        )
        header_unit_preprocessors.extend(precompile_cxx(
            ctx = ctx,
            impl_params = impl_params,
            preprocessors = own_exported_preprocessors,
            header_preprocessor_info = header_preprocessor_info,
        ))

    # Define object files.
    pic_cxx_outs = compile_cxx(
        ctx = ctx,
        src_compile_cmds = compile_cmd_output.src_compile_cmds,
        flavor = CxxCompileFlavor("pic"),
        provide_syntax_only = True,
        use_header_units = impl_params.use_header_units,
    )
    pic = _get_library_compile_output(
        ctx = ctx,
        src_compile_cmds = compile_cmd_output.src_compile_cmds,
        outs = pic_cxx_outs,
        extra_link_input = impl_params.extra_link_input,
    )

    non_pic = None
    pic_optimized = None
    if preferred_linkage != Linkage("shared"):
        non_pic_cxx_outs = compile_cxx(
            ctx = ctx,
            src_compile_cmds = compile_cmd_output.src_compile_cmds,
            flavor = CxxCompileFlavor("default"),
            # Diagnostics from the pic and non-pic compilation would be
            # identical. We can avoid instantiating a second set of actions.
            provide_syntax_only = False,
            use_header_units = impl_params.use_header_units,
        )
        non_pic = _get_library_compile_output(
            ctx = ctx,
            src_compile_cmds = compile_cmd_output.src_compile_cmds,
            outs = non_pic_cxx_outs,
            extra_link_input = impl_params.extra_link_input,
        )

        if get_cxx_toolchain_info(ctx).optimization_compiler_flags_EXPERIMENTAL:
            optimized_cxx_outs = compile_cxx(
                ctx = ctx,
                src_compile_cmds = compile_cmd_output.src_compile_cmds,
                flavor = CxxCompileFlavor("pic_optimized"),
                # Diagnostics from the pic and non-pic compilation would be
                # identical. We can avoid instantiating a second set of actions.
                provide_syntax_only = False,
            )
            pic_optimized = _get_library_compile_output(
                ctx = ctx,
                src_compile_cmds = compile_cmd_output.src_compile_cmds,
                outs = optimized_cxx_outs,
                extra_link_input = impl_params.extra_link_input,
            )

    return _CxxCompiledSourcesOutput(
        compile_cmds = compile_cmd_output,
        pic = pic,
        pic_optimized = pic_optimized,
        non_pic = non_pic,
        header_unit_preprocessors = header_unit_preprocessors,
    )

def _form_library_outputs(
        ctx: AnalysisContext,
        impl_params: CxxRuleConstructorParams,
        compiled_srcs: _CxxCompiledSourcesOutput,
        preferred_linkage: Linkage,
        dep_infos: LinkArgs,
        extra_static_linkables: list[[FrameworksLinkable, SwiftmoduleLinkable]],
        gnu_use_link_groups: bool,
        link_execution_preference: LinkExecutionPreference,
        shared_interface_info: [SharedInterfaceInfo, None]) -> _CxxAllLibraryOutputs:
    # Build static/shared libs and the link info we use to export them to dependents.
    outputs = {}
    solib = None
    link_infos = {}
    providers = []
    sanitizer_runtime_files = []
    gcno_files = []

    linker_flags = cxx_attr_linker_flags_all(ctx)

    # Add in exported linker flags.
    def ldflags(inner: LinkInfo) -> LinkInfo:
        return wrap_link_info(
            inner = inner,
            pre_flags = linker_flags.exported_flags,
            post_flags = linker_flags.exported_post_flags,
        )

    # We don't know which outputs consumers may want, so we define all the possibilities given our preferred linkage.
    for output_style in get_output_styles_for_linkage(preferred_linkage):
        output = None
        optimized_info = None
        stripped = None
        info = None

        # Generate the necessary libraries and
        # add them to the exported link info.
        if output_style != LibOutputStyle("shared_lib"):
            pic = _use_pic(output_style)
            lib_compile_output = compiled_srcs.pic
            if not pic:
                lib_compile_output = compiled_srcs.non_pic
                if not lib_compile_output:
                    fail("output_style {} requires non_pic compiled srcs, but didn't have any in {}".format(output_style, compiled_srcs))

            gcno_files += lib_compile_output.gcno_files

            if pic and compiled_srcs.pic_optimized and compiled_srcs.pic_optimized.objects:
                _, optimized_info = _static_library(
                    ctx,
                    impl_params,
                    compiled_srcs.pic_optimized.objects,
                    objects_have_external_debug_info = compiled_srcs.pic_optimized.objects_have_external_debug_info,
                    external_debug_info = make_artifact_tset(
                        ctx.actions,
                        label = ctx.label,
                        artifacts = compiled_srcs.pic_optimized.external_debug_info,
                        children = impl_params.additional.static_external_debug_info,
                    ),
                    pic = pic,
                    optimized = True,
                    stripped = False,
                    extra_linkables = extra_static_linkables,
                    bitcode_objects = compiled_srcs.pic_optimized.bitcode_objects,
                )

            # Only generate an archive if we have objects to include
            if lib_compile_output.objects:
                output, info = _static_library(
                    ctx,
                    impl_params,
                    lib_compile_output.objects,
                    objects_have_external_debug_info = lib_compile_output.objects_have_external_debug_info,
                    external_debug_info = make_artifact_tset(
                        ctx.actions,
                        label = ctx.label,
                        artifacts = lib_compile_output.external_debug_info,
                        children = impl_params.additional.static_external_debug_info,
                    ),
                    pic = pic,
                    stripped = False,
                    optimized = False,
                    extra_linkables = extra_static_linkables,
                    bitcode_objects = lib_compile_output.bitcode_objects,
                )
                _, stripped = _static_library(
                    ctx,
                    impl_params,
                    lib_compile_output.stripped_objects,
                    pic = pic,
                    stripped = True,
                    optimized = False,
                    extra_linkables = extra_static_linkables,
                    bitcode_objects = lib_compile_output.bitcode_objects,
                )
            else:
                # Header only libraries can have `extra_static_linkables`
                info = LinkInfo(
                    name = ctx.label.name,
                    linkables = extra_static_linkables,
                    metadata = cxx_attr_dep_metadata(ctx),
                )
        else:  # shared
            # If requested (by build_empty_so), we still generate a shared library even if there's no source objects.
            # This could be useful because it can still point to dependencies.
            # i.e. a rust_python_extension is an empty .so depending on a rust shared object
            if compiled_srcs.pic.objects or impl_params.build_empty_so:
                external_debug_artifacts = compiled_srcs.pic.external_debug_info
                if compiled_srcs.pic.objects_have_external_debug_info:
                    external_debug_artifacts.extend(compiled_srcs.pic.objects)
                if impl_params.extra_link_input_has_external_debug_info:
                    external_debug_artifacts.extend(impl_params.extra_link_input)
                external_debug_info = make_artifact_tset(
                    actions = ctx.actions,
                    label = ctx.label,
                    artifacts = external_debug_artifacts,
                    children = impl_params.additional.shared_external_debug_info,
                    tags = impl_params.additional.external_debug_info_tags,
                )

                gcno_files += compiled_srcs.pic.gcno_files

                result = _shared_library(
                    ctx = ctx,
                    impl_params = impl_params,
                    objects = compiled_srcs.pic.objects,
                    external_debug_info = external_debug_info,
                    dep_infos = dep_infos,
                    gnu_use_link_groups = gnu_use_link_groups,
                    link_ordering = map_val(LinkOrdering, ctx.attrs.link_ordering),
                    link_execution_preference = link_execution_preference,
                    shared_interface_info = shared_interface_info,
                )
                shlib = result.link_result.linked_object
                info = result.info
                extra_outputs = result.link_result.extra_outputs

                link_cmd_debug_output_file = None
                link_cmd_debug_output = make_link_command_debug_output(shlib)
                if link_cmd_debug_output != None:
                    link_cmd_debug_output_file = make_link_command_debug_output_json_info(ctx, [link_cmd_debug_output])
                    providers.append(LinkCommandDebugOutputInfo(debug_outputs = [link_cmd_debug_output]))

                unstripped = shlib.unstripped_output
                output = CxxLibraryOutput(
                    output_style = LibOutputStyle("shared_lib"),
                    default = shlib.output,
                    unstripped = unstripped,
                    object_files = compiled_srcs.pic.objects,
                    external_debug_info = shlib.external_debug_info,
                    dwp = shlib.dwp,
                    linker_map = result.link_result.linker_map_data,
                    sub_targets = extra_outputs | {
                        "linker.argsfile": [DefaultInfo(
                            default_output = shlib.linker_argsfile,
                        )],
                        "linker.command": [DefaultInfo(
                            default_outputs = filter(None, [link_cmd_debug_output_file]),
                        )],
                        "unstripped": [DefaultInfo(
                            default_output = unstripped,
                        )],
                    },
                    pdb = shlib.pdb,
                    implib = shlib.import_library,
                )
                solib = (result.soname, shlib)

                providers.append(result.link_result.link_execution_preference_info)

                link_sanitizer_runtime_files = result.link_result.sanitizer_runtime_files
                if link_sanitizer_runtime_files:
                    if sanitizer_runtime_files:
                        fail("Cannot specify sanitizer runtime files multiple times")
                    sanitizer_runtime_files = link_sanitizer_runtime_files

        # you cannot link against header only libraries so create an empty link info
        info = info if info != None else LinkInfo()
        if output:
            outputs[output_style] = output
        link_infos[output_style] = LinkInfos(
            default = ldflags(info),
            stripped = ldflags(stripped) if stripped != None else None,
            optimized = ldflags(optimized_info) if optimized_info != None else None,
        )

    if get_cxx_toolchain_info(ctx).gcno_files:
        deps_gcno_files = [
            x[GcnoFilesInfo].gcno_files
            for x in ctx.attrs.deps + ctx.attrs.exported_deps
            if GcnoFilesInfo in x
        ]
        providers.append(GcnoFilesInfo(
            gcno_files = dedupe(flatten(deps_gcno_files) + gcno_files),
        ))

    return _CxxAllLibraryOutputs(
        outputs = outputs,
        link_infos = link_infos,
        providers = providers,
        solib = solib,
        sanitizer_runtime_files = sanitizer_runtime_files,
    )

def _strip_objects(ctx: AnalysisContext, objects: list[Artifact]) -> list[Artifact]:
    """
    Return new objects with debug info stripped.
    """

    cxx_toolchain_info = get_cxx_toolchain_info(ctx)

    # Stripping is not supported on Windows
    linker_type = cxx_toolchain_info.linker_info.type
    if linker_type == LinkerType("windows"):
        return objects

    # Disable stripping if no `strip` binary was provided by the toolchain.
    if cxx_toolchain_info.binary_utilities_info == None or \
       cxx_toolchain_info.binary_utilities_info.strip == None:
        return objects

    outs = []

    for obj in objects:
        base, ext = paths.split_extension(obj.short_path)
        expect(ext == ".o")
        outs.append(strip_debug_info(ctx, base + ".stripped.o", obj))

    return outs

def _get_shared_library_links(
        ctx: AnalysisContext,
        reduce_linkable_graph_func: typing.Callable[[], ReducedLinkableGraph],
        link_group: [str, None],
        link_group_mappings: [dict[Label, str], None],
        link_group_preferred_linkage: dict[Label, Linkage],
        link_group_libs: dict[str, LinkGroupLib],
        exported_deps: list[Dependency],
        non_exported_deps: list[Dependency],
        force_link_group_linking,
        frameworks_linkable: [FrameworksLinkable, None],
        swiftmodule_linkable: [SwiftmoduleLinkable, None],
        exported_symbol_outputs: list[Artifact],
        force_static_follows_dependents: bool = True) -> (LinkArgs, [DefaultInfo, None], LinkExecutionPreference, [SharedInterfaceInfo, None]):
    """
    Returns LinkArgs with the content to link, and a link group map json output if applicable.

    TODO(T110378116): Omnibus linking always creates shared libraries by linking
    against shared dependencies. This is not true for link groups and possibly
    other forms of shared libraries. Ideally we consolidate this logic and
    propagate up only the expected links. Until we determine the comprehensive
    logic here, simply diverge behavior on whether link groups are defined.
    """

    pic_behavior = get_cxx_toolchain_info(ctx).pic_behavior

    # If we're not filtering for link groups, link against the shared dependencies
    if not link_group_mappings and not force_link_group_linking:
        deps = dedupe(flatten([non_exported_deps, exported_deps]))
        deps_merged_link_infos = cxx_inherited_link_info(deps)

        link_strategy = cxx_attr_link_strategy(ctx.attrs)

        # We cannot support deriving link execution preference off the included links, as we've already
        # lost the information on what is in the link.
        # TODO(T152860998): Derive link_execution_preference based upon the included links
        # Not all rules calling `cxx_library_parameterized` have `link_execution_preference`. Notably `cxx_python_extension`.
        link_execution_preference = get_link_execution_preference(ctx, []) if hasattr(ctx.attrs, "link_execution_preference") else LinkExecutionPreference("any")

        # Collect the shared interface providers for this link unit and strategy.
        # These are merged when linking shared library output.
        shared_interface_info = create_shared_interface_info(ctx, exported_symbol_outputs, deps)

        return apple_build_link_args_with_deduped_flags(
            ctx,
            deps_merged_link_infos,
            frameworks_linkable,
            # fPIC behaves differently on various combinations of toolchains + platforms.
            # To get the link_strategy, we have to check the link_strategy against the toolchain's pic_behavior.
            #
            # For more info, check the PicBehavior docs.
            process_link_strategy_for_pic_behavior(link_strategy, pic_behavior),
            swiftmodule_linkable,
        ), None, link_execution_preference, shared_interface_info

    # Else get filtered link group links
    prefer_stripped = cxx_is_gnu(ctx) and ctx.attrs.prefer_stripped_objects

    # TODO(cjhopman): Why is this different than where we compute just above for the non link-group case?
    link_strategy = to_link_strategy(cxx_attr_link_style(ctx))
    if link_strategy == LinkStrategy("static"):
        link_strategy = LinkStrategy("static_pic")
    link_strategy = process_link_strategy_for_pic_behavior(link_strategy, pic_behavior)
    reduced_linkable_graph = reduce_linkable_graph_func()

    roots = set(linkable_deps(non_exported_deps + exported_deps))
    is_executable_link = False
    lib_linkables = collect_linkables(
        reduced_linkable_graph,
        is_executable_link,
        link_strategy,
        link_group_preferred_linkage,
        pic_behavior,
        roots,
    )

    build_context = BuildLinkGroupsContext(
        public_nodes = set(),
        linkable_graph = reduced_linkable_graph,
        link_groups = {},
        link_group_mappings = link_group_mappings,
        link_group_preferred_linkage = link_group_preferred_linkage,
        link_strategy = link_strategy,
        pic_behavior = pic_behavior,
        link_group_libs = {
            name: (lib.label, lib.shared_link_infos)
            for name, lib in link_group_libs.items()
        },
        prefer_stripped = prefer_stripped,
        prefer_optimized = False,
    )

    filtered_labels_to_links = get_filtered_labels_to_links_map(
        link_group = link_group,
        linkables = lib_linkables,
        is_executable_link = False,
        build_context = build_context,
        force_static_follows_dependents = force_static_follows_dependents,
    )
    filtered_links = get_filtered_links(filtered_labels_to_links.map)
    filtered_targets = get_filtered_targets(filtered_labels_to_links.map)

    link_execution_preference = get_link_execution_preference(ctx, filtered_labels_to_links.map.keys())

    # Unfortunately, link_groups does not use MergedLinkInfo to represent the args
    # for the resolved nodes in the graph.
    additional_links = apple_get_link_info_by_deduping_link_infos(ctx, filtered_links, frameworks_linkable, swiftmodule_linkable)
    if additional_links:
        filtered_links.append(additional_links)

    # Collect the interface providers from the targets in this link group, these will
    # be merged when linking shared library output. If this library has no
    # interface output then interface generation is disabled and we can skip collection.
    shared_interface_infos = []
    if len(exported_symbol_outputs) > 0:
        for label in filtered_labels_to_links.map.keys():
            linkable_node = reduced_linkable_graph.nodes[label]
            if linkable_node.shared_interface_info != None:
                shared_interface_infos.append(linkable_node.shared_interface_info)

    shared_interface_info = create_shared_interface_info_with_children(ctx, exported_symbol_outputs, shared_interface_infos)
    return LinkArgs(infos = filtered_links), get_link_group_map_json(ctx, filtered_targets), link_execution_preference, shared_interface_info

def _use_pic(output_style: LibOutputStyle) -> bool:
    """
    Whether this output style requires PIC objects.
    """
    return output_style != LibOutputStyle("archive")

# Create the objects/archive to use for static linking this rule.
# Returns a tuple of objects/archive to use as the default output for the link
# style(s) it's used in and the `LinkInfo` to export to dependents.
def _static_library(
        ctx: AnalysisContext,
        impl_params: CxxRuleConstructorParams,
        objects: list[Artifact],
        pic: bool,
        optimized: bool,
        stripped: bool,
        extra_linkables: list[[FrameworksLinkable, SwiftmoduleLinkable]],
        objects_have_external_debug_info: bool = False,
        external_debug_info: ArtifactTSet = ArtifactTSet(),
        bitcode_objects: [list[Artifact], None] = None) -> (CxxLibraryOutput, LinkInfo):
    if len(objects) == 0:
        fail("empty objects")

    # No reason to create a static library with just a single object file. We
    # still want to create a static lib to expose as the default output because
    # it's the contract/expectation of external clients of the cmd line
    # interface. Any tools consuming `buck build` outputs should get a
    # consistent output type when building a library, not static lib or object
    # file depending on number of source files.
    linker_info = get_cxx_toolchain_info(ctx).linker_info
    linker_type = linker_info.type

    base_name = _base_static_library_name(ctx, optimized, stripped)
    name = _archive_name(base_name, pic = pic, extension = linker_info.static_library_extension)

    # If we have extra hidden deps of this target add them to the archive action
    # so they are forced to build for static library output.
    archive = make_archive(ctx, name, objects, impl_params.extra_hidden)

    bitcode_bundle = _bitcode_bundle(ctx, bitcode_objects, optimized, pic, stripped)
    if False:
        # TODO(nga): bitcode_bundle.artifact
        def unknown():
            pass

        bitcode_bundle = unknown()
    if bitcode_bundle != None and bitcode_bundle.artifact != None:
        bitcode_artifact = bitcode_bundle.artifact
    else:
        bitcode_artifact = None

    if use_archives(ctx):
        linkable = ArchiveLinkable(
            archive = archive,
            bitcode_bundle = bitcode_artifact,
            linker_type = linker_type,
            link_whole = _attr_link_whole(ctx),
        )
    else:
        linkable = ObjectsLinkable(
            objects = objects,
            bitcode_bundle = bitcode_artifact,
            linker_type = linker_type,
            link_whole = _attr_link_whole(ctx),
        )

    post_flags = []

    if pic:
        post_flags.extend(linker_info.static_pic_dep_runtime_ld_flags or [])
    else:
        post_flags.extend(linker_info.static_dep_runtime_ld_flags or [])

    # On darwin, the linked output references the archive that contains the
    # object files instead of the originating objects.
    object_external_debug_info = []
    if linker_type == LinkerType("darwin"):
        if archive.archive_contents_type == ArchiveContentsType("virtual"):
            # If using thin archives or passing archives --start-lib --end-lib style (virtual), we only need to provide the underlying objects
            object_external_debug_info.extend(archive.external_objects)
        elif archive.archive_contents_type == ArchiveContentsType("thin"):
            # If passing the archive to the linker conventionally, we need to provide both the thin archive
            # and the underlying object files if using thin archives
            object_external_debug_info.append(archive.artifact)
            object_external_debug_info.extend(archive.external_objects)
        else:
            # And only the archive if using regular archives
            object_external_debug_info.append(archive.artifact)
    elif archive.archive_contents_type == ArchiveContentsType("thin") and objects_have_external_debug_info:
        object_external_debug_info.extend(objects)

    all_external_debug_info = make_artifact_tset(
        actions = ctx.actions,
        label = ctx.label,
        artifacts = object_external_debug_info,
        children = [external_debug_info],
        tags = impl_params.additional.external_debug_info_tags,
    )

    return (
        CxxLibraryOutput(
            output_style = LibOutputStyle("pic_archive") if pic else LibOutputStyle("archive"),
            default = archive.artifact,
            unstripped = archive.artifact,
            object_files = objects,
            bitcode_bundle = bitcode_bundle,
            other = (archive.external_objects if archive.archive_contents_type == ArchiveContentsType("thin") else []),
            sub_targets = {},
        ),
        LinkInfo(
            name = name,
            # We're propagating object code for linking up the dep tree,
            # so we need to also propagate any necessary link flags required for
            # the object code.
            pre_flags = impl_params.extra_exported_link_flags,
            post_flags = post_flags,
            # Extra linkables are propagated here so they are available to link_groups
            # when they are deducing linker args.
            linkables = [linkable] + extra_linkables,
            external_debug_info = all_external_debug_info,
            metadata = cxx_attr_dep_metadata(ctx),
        ),
    )

# A bitcode bundle is very much like a static library and is generated from object file
# inputs, except the output is a combined bitcode file, which is not machine code.
def _bitcode_bundle(
        ctx: AnalysisContext,
        objects: [list[Artifact], None],
        optimized: bool,
        pic: bool,
        stripped: bool,
        name_extra = "") -> [BitcodeBundle, None]:
    if objects == None or len(objects) == 0:
        return None

    base_name = _base_static_library_name(ctx, optimized, stripped = False)
    name = name_extra + _bitcode_bundle_name(base_name, pic, stripped)
    return make_bitcode_bundle(ctx, name, objects)

_CxxSharedLibraryResult = record(
    # Result from link, includes the shared lib, linker map data etc
    link_result = CxxLinkResult,
    # Shared library name (e.g. SONAME)
    soname = str,
    objects_bitcode_bundle = Artifact | None,
    # `LinkInfo` used to link against the shared library.
    info = LinkInfo,
)

def _shared_library(
        ctx: AnalysisContext,
        impl_params: CxxRuleConstructorParams,
        objects: list[Artifact],
        external_debug_info: ArtifactTSet,
        dep_infos: LinkArgs,
        gnu_use_link_groups: bool,
        link_execution_preference: LinkExecutionPreference,
        link_ordering: [LinkOrdering, None],
        shared_interface_info: [SharedInterfaceInfo, None]) -> _CxxSharedLibraryResult:
    """
    Generate a shared library and the associated native link info used by
    dependents to link against it.
    """

    soname = _soname(ctx, impl_params)
    cxx_toolchain = get_cxx_toolchain_info(ctx)
    linker_info = cxx_toolchain.linker_info

    local_bitcode_bundle = _bitcode_bundle(ctx, objects, optimized = False, pic = False, stripped = False, name_extra = "objects-")

    # NOTE(agallagher): We add exported link flags here because it's what v1
    # does, but the intent of exported link flags are to wrap the link output
    # that we propagate up the tree, rather than being used locally when
    # generating a link product.
    linker_flags = cxx_attr_linker_flags_all(ctx)
    link_info = LinkInfo(
        dist_thin_lto_codegen_flags = getattr(ctx.attrs, "dist_thin_lto_codegen_flags", []),
        pre_flags = (
            linker_flags.flags +
            linker_flags.exported_flags +
            getattr(ctx.attrs, "local_linker_flags", [])
        ),
        linkables = [ObjectsLinkable(
            objects = objects,
            bitcode_bundle = local_bitcode_bundle.artifact if local_bitcode_bundle else None,
            linker_type = linker_info.type,
            link_whole = True,
        )],
        post_flags = (
            impl_params.extra_exported_link_flags +
            impl_params.extra_link_flags +
            linker_flags.post_flags +
            (linker_info.shared_dep_runtime_ld_flags or [])
            # TODO(cjhopman): Why doesn't this add exported_linker_flags.post_flags?
        ),
        external_debug_info = external_debug_info,
        metadata = cxx_attr_dep_metadata(ctx),
    )

    # If we have extra hidden deps here, add them as hidden inputs
    # to the link action so that they are forced to build before linking.
    links = [LinkArgs(infos = [link_info]), dep_infos]
    if impl_params.extra_hidden:
        links.append(
            LinkArgs(flags = cmd_args(hidden = impl_params.extra_hidden)),
        )

    link_result = cxx_link_shared_library(
        ctx = ctx,
        output = soname,
        opts = link_options(
            enable_distributed_thinlto = getattr(ctx.attrs, "enable_distributed_thinlto", False),
            links = links,
            identifier = soname,
            link_ordering = link_ordering,
            strip = impl_params.strip_executable,
            strip_args_factory = impl_params.strip_args_factory,
            link_execution_preference = link_execution_preference,
            error_handler = impl_params.error_handler,
            extra_linker_outputs_factory = impl_params.extra_linker_outputs_factory,
            extra_linker_outputs_flags_factory = impl_params.extra_linker_outputs_flags_factory,
        ),
        name = soname if impl_params.use_soname else None,
        shared_library_flags = impl_params.shared_library_flags,
    )
    exported_shlib = link_result.linked_object.output

    # If shared library interfaces are enabled, link that and use it as
    # the shared lib that dependents will link against.
    if cxx_use_shlib_intfs(ctx):
        mode = get_cxx_toolchain_info(ctx).linker_info.shlib_interfaces
        if mode == ShlibInterfacesMode("stub_from_library"):
            # Generate a library interface from the linked library output.
            # This will prevent relinking rdeps when changes do not affect
            # the library symbols.
            exported_shlib = shared_library_interface(
                ctx = ctx,
                shared_lib = exported_shlib,
            )
        elif mode == ShlibInterfacesMode("stub_from_headers"):
            # Generate a library interface from its deps exported_headers.
            # This will allow for linker parallelisation as we do not have
            # to wait for dependent libraries to link.
            # If the provider is missing this is a non apple_library target,
            # so skip producing the interface.
            if shared_interface_info != None:
                # collect the linker args which are required
                # to correctly set symbol visibility.
                link_args = [unpack_link_args(link) for link in links]
                exported_shlib = generate_tbd_with_symbols(ctx, soname, shared_interface_info.interfaces, link_args)
        elif not gnu_use_link_groups:
            # TODO(agallagher): There's a bug in shlib intfs interacting with link
            # groups, where we don't include the symbols we're meant to export from
            # deps that get statically linked in.
            link_info = LinkInfo(
                pre_flags = link_info.pre_flags,
                linkables = link_info.linkables,
                post_flags = (
                    (link_info.post_flags or []) +
                    get_ignore_undefined_symbols_flags(linker_info.type) +
                    (linker_info.independent_shlib_interface_linker_flags or [])
                ),
                external_debug_info = link_info.external_debug_info,
                metadata = cxx_attr_dep_metadata(ctx),
            )
            intf_link_result = cxx_link_shared_library(
                ctx = ctx,
                output = get_shared_library_name(
                    linker_info,
                    ctx.label.name + "-for-interface",
                    apply_default_prefix = True,
                ),
                opts = link_options(
                    category_suffix = "interface",
                    link_ordering = link_ordering,
                    links = [LinkArgs(infos = [link_info])],
                    identifier = soname + "-interface",
                    link_execution_preference = link_execution_preference,
                    strip = impl_params.strip_executable,
                    error_handler = impl_params.error_handler,
                ),
                name = soname,
            )
            exported_shlib = shared_library_interface(
                ctx = ctx,
                shared_lib = intf_link_result.linked_object.output,
            )

    # Link against import library on Windows.
    if link_result.linked_object.import_library:
        exported_shlib = link_result.linked_object.import_library

    return _CxxSharedLibraryResult(
        link_result = link_result,
        soname = soname,
        objects_bitcode_bundle = local_bitcode_bundle.artifact if local_bitcode_bundle else None,
        info = LinkInfo(
            name = soname,
            linkables = [SharedLibLinkable(
                lib = exported_shlib,
            )],
            metadata = cxx_attr_dep_metadata(ctx),
        ),
    )

def _attr_reexport_all_header_dependencies(ctx: AnalysisContext) -> bool:
    return value_or(ctx.attrs.reexport_all_header_dependencies, False)

def _soname(ctx: AnalysisContext, impl_params) -> str:
    """
    Get the shared library name to set for the given C++ library.
    """
    linker_info = get_cxx_toolchain_info(ctx).linker_info
    explicit_soname = value_or(ctx.attrs.soname, impl_params.soname)
    if explicit_soname != None:
        return get_shared_library_name_for_param(linker_info, explicit_soname)
    return get_default_shared_library_name(linker_info, ctx.label)

def _base_static_library_name(ctx: AnalysisContext, optimized: bool, stripped: bool) -> str:
    return "{}{}{}".format(ctx.label.name, ".optimized" if optimized else "", ".stripped" if stripped else "")

def _archive_name(name: str, pic: bool, extension: str) -> str:
    return "lib{}{}.{}".format(name, ".pic" if pic else "", extension)

def _bitcode_bundle_name(name: str, pic: bool, stripped: bool = False) -> str:
    return "{}{}{}.bc".format(name, ".pic" if pic else "", ".stripped" if stripped else "")

def _attr_link_whole(ctx: AnalysisContext) -> bool:
    return value_or(ctx.attrs.link_whole, False)

def use_archives(ctx: AnalysisContext) -> bool:
    """
    Whether this rule should use archives to package objects when producing
    link input for dependents.
    """
    linker_info = get_cxx_toolchain_info(ctx).linker_info
    requires_archives = linker_info.requires_archives
    requires_objects = linker_info.requires_objects

    if requires_archives and requires_objects:
        fail("In cxx linker_info, only one of `requires_archives` and `requires_objects` can be enabled")

    # If the toolchain requires them, then always use them.
    if requires_archives:
        return True

    if requires_objects:
        return False

    # Otherwise, fallback to the rule-specific setting.
    return value_or(getattr(ctx.attrs, "use_archive", True), True)

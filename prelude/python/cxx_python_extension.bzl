# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//:paths.bzl", "paths")
load(
    "@prelude//cxx:cuda.bzl",
    "CudaCompileStyle",
)
load("@prelude//cxx:cxx_context.bzl", "get_cxx_toolchain_info")
load(
    "@prelude//cxx:cxx_library.bzl",
    "cxx_library_parameterized",
)
load(
    "@prelude//cxx:cxx_library_utility.bzl",
    "cxx_attr_deps",
)
load(
    "@prelude//cxx:cxx_sources.bzl",
    "get_srcs_with_flags",
)
load(
    "@prelude//cxx:cxx_types.bzl",
    "CxxRuleConstructorParams",
    "CxxRuleProviderParams",
    "CxxRuleSubTargetParams",
)
load("@prelude//cxx:cxx_utility.bzl", "cxx_attrs_get_allow_cache_upload")
load("@prelude//cxx:headers.bzl", "cxx_get_regular_cxx_headers_layout")
load("@prelude//cxx:linker.bzl", "DUMPBIN_SUB_TARGET", "PDB_SUB_TARGET", "get_dumpbin_providers", "get_pdb_providers")
load(
    "@prelude//cxx:omnibus.bzl",
    "create_linkable_root",
    "get_roots",
)
load(
    "@prelude//linking:link_groups.bzl",
    "merge_link_group_lib_info",
)
load(
    "@prelude//linking:link_info.bzl",
    "LibOutputStyle",
    "LinkInfo",
    "LinkInfos",
    "LinkableFlavor",
    "create_merged_link_info",
    "wrap_link_infos",
)
load(
    "@prelude//linking:linkable_graph.bzl",
    "create_linkable_graph",
    "create_linkable_graph_node",
    "create_linkable_node",
)
load(
    "@prelude//linking:linkables.bzl",
    "LinkableProviders",
    "linkables",
)
load(
    "@prelude//linking:shared_libraries.bzl",
    "merge_shared_libraries",
)
load("@prelude//linking:types.bzl", "Linkage")
load("@prelude//os_lookup:defs.bzl", "Os", "OsLookup")
load("@prelude//python:toolchain.bzl", "PythonToolchainInfo")
load(
    "@prelude//python/linking:native_python_util.bzl",
    "merge_cxx_extension_info",
    "rewrite_static_symbols",
)
load(
    "@prelude//third-party:build.bzl",
    "create_third_party_build_root",
    "prefix_from_label",
)
load("@prelude//third-party:providers.bzl", "ThirdPartyBuild", "third_party_build_info")
load("@prelude//unix:providers.bzl", "UnixEnv", "create_unix_env_info")
load("@prelude//utils:utils.bzl", "value_or")
load(":manifest.bzl", "create_manifest_for_source_map")
load(":python.bzl", "NativeDepsInfo", "NativeDepsInfoTSet", "PythonLibraryInfo")
load(":python_library.bzl", "create_python_library_info", "dest_prefix", "gather_dep_libraries", "qualify_srcs")
load(":source_db.bzl", "create_python_source_db_info", "create_source_db_no_deps")
load(":versions.bzl", "gather_versioned_dependencies")

# This extension is basically cxx_library, plus base_module.
# So we augment with default attributes so it has everything cxx_library has, and then call cxx_library_parameterized and work from that.
def cxx_python_extension_impl(ctx: AnalysisContext) -> list[Provider]:
    providers = []

    if ctx.attrs._target_os_type[OsLookup].os == Os("windows"):
        library_extension = ".pyd"
    else:
        library_extension = ".so"
    module_name = value_or(ctx.attrs.module_name, ctx.label.name)
    name = module_name + library_extension
    base_module = dest_prefix(ctx.label, ctx.attrs.base_module)

    sub_targets = CxxRuleSubTargetParams(
        argsfiles = True,
        compilation_database = True,
        headers = False,
        link_group_map = False,
        link_style_outputs = False,
        xcode_data = False,
    )

    cxx_providers = CxxRuleProviderParams(
        compilation_database = True,
        default = False,  # We need to do some postprocessing to make sure the shared library is our default output
        java_packaging_info = False,
        java_global_code_info = False,
        linkable_graph = False,  # We create this here so we can correctly apply exclusions
        link_style_outputs = False,
        merged_native_link_info = False,
        omnibus_root = True,
        preprocessors = False,
        resources = True,
        shared_libraries = False,
        template_placeholders = False,
        preprocessor_for_tests = False,
    )

    cxx_toolchain = get_cxx_toolchain_info(ctx)
    python_toolchain = ctx.attrs._python_toolchain[PythonToolchainInfo]

    impl_params = CxxRuleConstructorParams(
        build_empty_so = True,
        rule_type = "cxx_python_extension",
        headers_layout = cxx_get_regular_cxx_headers_layout(ctx),
        srcs = get_srcs_with_flags(ctx),
        soname = name,
        use_soname = False,
        generate_providers = cxx_providers,
        generate_sub_targets = sub_targets,
        compiler_flags = ctx.attrs.compiler_flags,
        lang_compiler_flags = ctx.attrs.lang_compiler_flags,
        extra_link_flags = python_toolchain.extension_linker_flags,
        preprocessor_flags = ctx.attrs.preprocessor_flags,
        lang_preprocessor_flags = ctx.attrs.lang_preprocessor_flags,
        error_handler = cxx_toolchain.cxx_error_handler,
        allow_cache_upload = cxx_attrs_get_allow_cache_upload(ctx.attrs, get_cxx_toolchain_info(ctx).cxx_compiler_info.allow_cache_upload),
        precompiled_header = ctx.attrs.precompiled_header,
        prefix_header = ctx.attrs.prefix_header,
        _cxx_toolchain = ctx.attrs._cxx_toolchain,
        coverage_instrumentation_compiler_flags = ctx.attrs.coverage_instrumentation_compiler_flags,
        separate_debug_info = ctx.attrs.separate_debug_info,
        cuda_compile_style = CudaCompileStyle(ctx.attrs.cuda_compile_style),
        supports_stripping = ctx.attrs.supports_stripping,
        use_content_based_paths = cxx_toolchain.cxx_compiler_info.supports_content_based_paths,
    )

    cxx_library_info = cxx_library_parameterized(ctx, impl_params)
    libraries = cxx_library_info.all_outputs
    shared_output = libraries.outputs[LibOutputStyle("shared_lib")][LinkableFlavor("default")]

    solib_default = libraries.solibs.get(LinkableFlavor("default"), None)
    if not solib_default:
        fail("Expected cxx_python_extension to produce a solib: {}".format(ctx.label))
    extension = solib_default.linked_object

    sub_targets = cxx_library_info.sub_targets
    if extension.pdb:
        sub_targets[PDB_SUB_TARGET] = get_pdb_providers(pdb = extension.pdb, binary = extension.output)

    dumpbin_toolchain_path = cxx_toolchain.dumpbin_toolchain_path
    if dumpbin_toolchain_path:
        sub_targets[DUMPBIN_SUB_TARGET] = get_dumpbin_providers(ctx, extension.output, dumpbin_toolchain_path)

    cxx_deps = cxx_attr_deps(ctx)

    extension_artifacts = {}
    python_module_names = {}
    unembeddable_extensions = {}

    link_infos = libraries.link_infos

    # For python_cxx_extensions we need to mangle the symbol names in order to avoid collisions
    # when linking into the main binary
    embeddable = ctx.attrs.allow_embedding and LibOutputStyle("archive") in libraries.outputs
    if embeddable:
        pyinit_prefix = "PyInit"
        if ctx.attrs._target_os_type[OsLookup].os == Os("macos"):
            pyinit_prefix = "_PyInit"
        if not ctx.attrs.allow_suffixing:
            pyinit_symbol = "{}_{}".format(pyinit_prefix, module_name)
        else:
            suffix = base_module.replace("/", "$") + module_name
            static_output = libraries.outputs[LibOutputStyle("archive")][LinkableFlavor("default")]
            static_pic_output = libraries.outputs[LibOutputStyle("pic_archive")][LinkableFlavor("default")]

            debuggable_static_pic_objects = []
            if LinkableFlavor("debug") in libraries.outputs[LibOutputStyle("pic_archive")]:
                debuggable_static_pic_objects = libraries.outputs[LibOutputStyle("pic_archive")][LinkableFlavor("debug")].object_files

            link_infos = rewrite_static_symbols(
                ctx,
                suffix,
                pic_objects = static_pic_output.object_files,
                non_pic_objects = static_output.object_files,
                debuggable_pic_objects = debuggable_static_pic_objects,
                libraries = link_infos,
                cxx_toolchain = cxx_toolchain,
                suffix_all = ctx.attrs.suffix_all,
                suffix_exclude_rtti = ctx.attrs.suffix_exclude_rtti,
            )
            pyinit_symbol = "{}_{}_{}".format(pyinit_prefix, module_name, suffix)

        if base_module != "":
            lines = ["# auto generated stub for {}\n".format(ctx.label.raw_target())]
            stub_name = module_name + ".empty_stub"
            extension_artifacts.update(qualify_srcs(ctx.label, ctx.attrs.base_module, {stub_name: ctx.actions.write(stub_name, lines)}))

        python_module_names[base_module.replace("/", ".") + module_name] = pyinit_symbol

    # Add a dummy shared link info to avoid marking this node as preferred
    # linkage being "static", which has a special meaning for various link
    # strategies
    link_infos[LibOutputStyle("shared_lib")] = LinkInfos(default = LinkInfo())

    # Create linkable providers for the extension.
    link_deps = linkables(cxx_deps)
    linkable_providers = LinkableProviders(
        link_group_lib_info = merge_link_group_lib_info(deps = cxx_deps),
        linkable_graph = create_linkable_graph(
            ctx = ctx,
            node = create_linkable_graph_node(
                ctx = ctx,
                linkable_node = create_linkable_node(
                    ctx = ctx,
                    deps = cxx_deps,
                    preferred_linkage = Linkage("any"),
                    link_infos = link_infos,
                    default_soname = name,
                ),
            ),
            deps = [d.linkable_graph for d in link_deps if d.linkable_graph != None],
        ),
        merged_link_info = create_merged_link_info(
            ctx = ctx,
            pic_behavior = cxx_toolchain.pic_behavior,
            link_infos = link_infos,
            preferred_linkage = Linkage("static"),
            deps = [d.merged_link_info for d in link_deps],
        ),
        shared_library_info = merge_shared_libraries(
            actions = ctx.actions,
            deps = [d.shared_library_info for d in link_deps],
        ),
        linkable_root_info = create_linkable_root(
            label = ctx.label,
            link_infos = wrap_link_infos(
                link_infos[LibOutputStyle("pic_archive")],
                pre_flags = ctx.attrs.linker_flags,
                post_flags = ctx.attrs.post_linker_flags,
            ),
            deps = cxx_deps,
        ),
    )

    if not embeddable:
        unembeddable_extensions[base_module + name] = linkable_providers
        linkable_providers = None

    providers.append(merge_cxx_extension_info(
        actions = ctx.actions,
        deps = cxx_deps,
        linkable_providers = linkable_providers,
        artifacts = extension_artifacts,
        python_module_names = python_module_names,
        unembeddable_extensions = unembeddable_extensions,
    ))
    providers.extend(cxx_library_info.providers)

    # If a type stub was specified, create a manifest for export.
    src_types = None
    src_type_manifest = None
    if ctx.attrs.type_stub != None:
        src_types = qualify_srcs(
            ctx.label,
            ctx.attrs.base_module,
            {module_name + ".pyi": ctx.attrs.type_stub},
        )
        src_type_manifest = create_manifest_for_source_map(
            ctx,
            "type_stub",
            src_types,
        )

    # Export library info.
    raw_deps = ctx.attrs.deps

    deps, shared_deps = gather_dep_libraries(raw_deps, resolve_versioned_deps = False)
    providers.append(gather_versioned_dependencies(raw_deps))

    # We dont process anything for cxx_extensions, we just add an empty set
    native_deps = ctx.actions.tset(
        NativeDepsInfoTSet,
        value = NativeDepsInfo(native_deps = {}),
        children = [],
    )
    library_info = create_python_library_info(
        ctx.actions,
        ctx.label,
        extensions = qualify_srcs(ctx.label, ctx.attrs.base_module, {name: extension}),
        deps = deps,
        extension_shared_libraries = shared_deps,
        src_types = src_type_manifest,
        native_deps = native_deps,
        is_native_dep = True,
    )
    providers.append(library_info)

    # Source DBs.
    if src_types != None:
        sub_targets["source-db-no-deps"] = [create_source_db_no_deps(ctx, src_types), create_python_source_db_info(library_info.manifests)]

    providers.append(DefaultInfo(
        default_output = shared_output.default,
        other_outputs = shared_output.other,
        sub_targets = sub_targets,
    ))

    # Omnibus providers

    # Handle the case where C++ Python extensions depend on other C++ Python
    # extensions, which should also be treated as roots.
    roots = get_roots([
        dep
        for dep in raw_deps
        # We only want to handle C++ Python extension deps, but not other native
        # linkable deps like C++ libraries.
        if PythonLibraryInfo in dep
    ])

    linkable_graph = create_linkable_graph(
        ctx,
        node = create_linkable_graph_node(
            ctx,
            roots = roots,
        ),
        deps = raw_deps,
    )
    providers.append(linkable_graph)

    # Allow third-party-build rules to depend on Python rules.
    tp_prefix = prefix_from_label(ctx.label)
    providers.append(
        third_party_build_info(
            actions = ctx.actions,
            build = ThirdPartyBuild(
                prefix = tp_prefix,
                root = create_third_party_build_root(
                    ctx = ctx,
                    paths = [(paths.join("lib/python", base_module + name), extension.output)],
                ),
                manifest = ctx.actions.write_json(
                    "third_party_build_manifest.json",
                    dict(
                        bin_paths = [],
                        c_include_paths = [],
                        cxx_include_paths = [],
                        lib_paths = [],
                        libs = [],
                        prefix = tp_prefix,
                        py_lib_paths = ["lib/python"],
                        runtime_lib_paths = [],
                    ),
                ),
            ),
            deps = raw_deps,
        ),
    )

    providers.append(
        create_unix_env_info(
            actions = ctx.actions,
            env = UnixEnv(
                label = ctx.label,
                python_libs = [library_info],
            ),
            deps = raw_deps,
        ),
    )

    return providers

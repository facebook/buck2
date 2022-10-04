load(
    "@prelude//cxx:cxx.bzl",
    "get_srcs_with_flags",
)
load("@prelude//cxx:cxx_context.bzl", "get_cxx_toolchain_info")
load(
    "@prelude//cxx:cxx_library.bzl",
    "cxx_library_parameterized",
)
load(
    "@prelude//cxx:cxx_library_utility.bzl",
    "cxx_attr_deps",
    "cxx_inherited_link_info",
)
load("@prelude//cxx:cxx_toolchain_types.bzl", "CxxPlatformInfo")
load(
    "@prelude//cxx:cxx_types.bzl",
    "CxxRuleConstructorParams",
    "CxxRuleProviderParams",
    "CxxRuleSubTargetParams",
)
load("@prelude//cxx:headers.bzl", "cxx_get_regular_cxx_headers_layout")
load(
    "@prelude//cxx:omnibus.bzl",
    "explicit_roots_enabled",
    "get_roots",
)
load(
    "@prelude//linking:link_info.bzl",
    "LinkInfo",
    "LinkInfos",
    "LinkInfosTSet",
    "LinkStyle",
)
load(
    "@prelude//linking:linkable_graph.bzl",
    "AnnotatedLinkableRoot",
    "create_linkable_graph",
    "create_linkable_graph_node",
)
load("@prelude//linking:shared_libraries.bzl", "SharedLibrariesTSet", "SharedLibraryInfo")
load("@prelude//python:toolchain.bzl", "PythonPlatformInfo", "get_platform_attr")
load("@prelude//utils:utils.bzl", "expect", "flatten", "value_or")
load(":manifest.bzl", "create_manifest_for_source_map")
load(
    ":native_python_util.bzl",
    "merge_cxx_extension_info",
    "suffix_symbols",
)
load(":python.bzl", "PythonLibraryInfo")
load(":python_library.bzl", "create_python_library_info", "dest_prefix", "gather_dep_libraries", "qualify_srcs")

# This extension is basically cxx_library, plus base_module.
# So we augment with default attributes so it has everything cxx_library has, and then call cxx_library_parameterized and work from that.
def cxx_python_extension_impl(ctx: "context") -> ["provider"]:
    providers = []

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

    impl_params = CxxRuleConstructorParams(
        build_empty_so = True,
        rule_type = "cxx_python_extension",
        headers_layout = cxx_get_regular_cxx_headers_layout(ctx),
        srcs = get_srcs_with_flags(ctx),
        use_soname = False,
        generate_providers = cxx_providers,
        generate_sub_targets = sub_targets,
        is_omnibus_root = explicit_roots_enabled(ctx),
    )

    cxx_library_info = cxx_library_parameterized(ctx, impl_params)
    libraries = cxx_library_info.all_outputs
    shared_output = libraries.outputs[LinkStyle("shared")]

    shared_objects = libraries.solibs.values()
    expect(len(shared_objects) == 1, "Expected exactly 1 so for cxx_python_extension: {}".format(ctx.label))
    extension = shared_objects[0]

    providers.append(DefaultInfo(
        default_outputs = [shared_output.default],
        other_outputs = shared_output.other,
        sub_targets = cxx_library_info.sub_targets,
    ))

    module_name = value_or(ctx.attrs.module_name, ctx.label.name)
    name = module_name + ".so"
    cxx_deps = [dep for dep in cxx_attr_deps(ctx)]

    static_output = None
    shared_libraries = []
    link_infos = []
    python_module_names = {}
    if ctx.attrs.allow_embedding:
        static_output = libraries.outputs[LinkStyle("static")]

    # For python_cxx_extensions we need to mangle the symbol names in order to avoid collisions
    # when linking into the main binary
    if static_output != None:
        qualified_name = dest_prefix(ctx.label, ctx.attrs.base_module)
        static_info = libraries.libraries[LinkStyle("static")].default
        if qualified_name == "":
            static_link_info = static_info
            extension_artifacts = {}
            python_module_names[module_name] = True
        else:
            suffix = qualified_name.replace("/", "_") + module_name
            cxx_toolchain = get_cxx_toolchain_info(ctx)
            new_linkable = suffix_symbols(
                ctx,
                suffix,
                static_output.object_files,
                cxx_toolchain,
            )
            static_link_info = LinkInfo(
                name = static_info.name,
                pre_flags = static_info.pre_flags,
                post_flags = static_info.post_flags,
                linkables = [new_linkable],
                use_link_groups = static_info.use_link_groups,
            )
            python_module_names[qualified_name.replace("/", ".") + module_name] = True

            lines = ["# auto generated stub\n"]
            stub_name = module_name + ".empty_stub"
            extension_artifacts = qualify_srcs(ctx.label, ctx.attrs.base_module, {stub_name: ctx.actions.write(stub_name, lines)})
        inherited_link = cxx_inherited_link_info(ctx, cxx_deps)

        # We need to dynamically export the modules PyInit function so that we
        # can find and import it with dlsym. TODO (T129253406) Remove this when
        # we statically register symbols
        link_infos.append(ctx.actions.tset(
            LinkInfosTSet,
            value = LinkInfos(default = static_link_info),
            children = [inherited_link._infos[LinkStyle("static")]],
        ))

    else:
        # If we cannot link this extension statically we need to include it's shared libraries
        shared_library_infos = filter(None, [x.get(SharedLibraryInfo) for x in cxx_deps])
        shared_libraries.append(ctx.actions.tset(
            SharedLibrariesTSet,
            children = filter(
                None,
                [dep.set for dep in shared_library_infos],
            ),
        ))
        extension_artifacts = qualify_srcs(ctx.label, ctx.attrs.base_module, {name: extension.output})

    providers.append(merge_cxx_extension_info(
        ctx.actions,
        cxx_deps,
        link_infos = link_infos,
        shared_libraries = shared_libraries,
        artifacts = extension_artifacts,
        python_module_names = python_module_names,
    ))
    providers.extend(cxx_library_info.providers)

    # If a type stub was specified, create a manifest for export.
    src_type_manifest = None
    if ctx.attrs.type_stub != None:
        src_type_manifest = create_manifest_for_source_map(
            ctx,
            "type_stub",
            qualify_srcs(
                ctx.label,
                ctx.attrs.base_module,
                {module_name + ".pyi": ctx.attrs.type_stub},
            ),
        )

    # Export library info.
    python_platform = ctx.attrs._python_toolchain[PythonPlatformInfo]
    cxx_platform = ctx.attrs._cxx_toolchain[CxxPlatformInfo]
    raw_deps = (
        [ctx.attrs.deps] +
        get_platform_attr(python_platform, cxx_platform, ctx.attrs.platform_deps)
    )
    deps, shared_deps = gather_dep_libraries(raw_deps)
    providers.append(create_python_library_info(
        ctx.actions,
        ctx.label,
        extensions = qualify_srcs(ctx.label, ctx.attrs.base_module, {name: extension}),
        deps = deps,
        shared_libraries = shared_deps,
        src_types = src_type_manifest,
    ))

    # Omnibus providers

    # Handle the case where C++ Python extensions depend on other C++ Python
    # extensions, which should also be treated as roots.
    roots = get_roots(ctx.label, [
        dep
        for dep in flatten(raw_deps)
        # We only want to handle C++ Python extension deps, but not other native
        # linkable deps like C++ libraries.
        if PythonLibraryInfo in dep
    ])
    roots[ctx.label] = AnnotatedLinkableRoot(root = cxx_library_info.linkable_root)

    linkable_graph = create_linkable_graph(
        ctx,
        node = create_linkable_graph_node(
            ctx,
            roots = roots,
        ),
        deps = flatten(raw_deps),
    )
    providers.append(linkable_graph)
    return providers

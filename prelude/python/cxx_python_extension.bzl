load(
    "@fbcode//buck2/prelude/cxx:cxx.bzl",
    "get_srcs_with_flags",
)
load(
    "@fbcode//buck2/prelude/cxx:cxx_library.bzl",
    "cxx_library_parameterized",
)
load("@fbcode//buck2/prelude/cxx:cxx_toolchain_types.bzl", "CxxPlatformInfo")
load(
    "@fbcode//buck2/prelude/cxx:cxx_types.bzl",
    "CxxRuleConstructorParams",
    "CxxRuleProviderParams",
    "CxxRuleSubTargetParams",
)
load("@fbcode//buck2/prelude/cxx:headers.bzl", "cxx_get_regular_cxx_headers_layout")
load(
    "@fbcode//buck2/prelude/cxx:omnibus.bzl",
    "add_omnibus_roots",
)
load(
    "@fbcode//buck2/prelude/linking:link_info.bzl",
    "LinkStyle",
    "MergedLinkInfo",
)
load(
    "@fbcode//buck2/prelude/linking:linkable_graph.bzl",
    "ForceConsiderationOfOmnibusRoots",
    "LinkableGraph",
    "create_merged_linkable_graph",
)
load("@fbcode//buck2/prelude/python:toolchain.bzl", "PythonPlatformInfo", "get_platform_attr")
load("@fbcode//buck2/prelude/utils:utils.bzl", "expect", "flatten", "value_or")
load(":manifest.bzl", "create_manifest_for_source_map")
load(":python_library.bzl", "create_python_library_info", "gather_dep_libraries", "qualify_srcs")

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
                {value_or(ctx.attrs.module_name, ctx.label.name) + ".pyi": ctx.attrs.type_stub},
            ),
        )

    # Export library info.
    name = value_or(ctx.attrs.module_name, ctx.label.name) + ".so"
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
    linkable_graph = create_merged_linkable_graph(ctx.label, flatten(raw_deps))

    # Handle the case where C++ Python extensions depend on other C++ Python
    # extensions, which should also be treated as roots.
    add_omnibus_roots(
        linkable_graph,
        [
            dep
            for dep in flatten(raw_deps)
            # We only want to handle C++ Python extension deps, but not other native
            # linkable deps like C++ libraries.
            if dep[ForceConsiderationOfOmnibusRoots] != None or (dep[LinkableGraph] != None and dep[MergedLinkInfo] == None)
        ],
    )
    providers.append(linkable_graph)
    return providers

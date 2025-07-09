# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//cxx:cxx.bzl", "create_shared_lib_link_group_specs")
load("@prelude//cxx:cxx_context.bzl", "get_cxx_toolchain_info")
load("@prelude//cxx:cxx_executable.bzl", "CxxExecutableOutput", "cxx_executable")
load("@prelude//cxx:cxx_sources.bzl", "CxxSrcWithFlags")
load(
    "@prelude//cxx:cxx_types.bzl",
    "CxxRuleConstructorParams",
)
load(
    "@prelude//cxx:groups_types.bzl",
    "Group",
    "GroupAttrs",
    "GroupMapping",
    "Traversal",
)
load("@prelude//cxx:headers.bzl", "cxx_get_regular_cxx_headers_layout")
load(
    "@prelude//cxx:link_groups.bzl",
    "LinkGroupLibSpec",
    "build_link_group_info",
    "get_link_group_info",
)
load(
    "@prelude//cxx:link_groups_types.bzl",
    "LinkGroupDefinitions",
    "LinkGroupInfo",  # @unused Used as a type
)
load("@prelude//cxx:linker.bzl", "get_rpath_origin")
load(
    "@prelude//cxx:preprocessor.bzl",
    "cxx_inherited_preprocessor_infos",
)
load(
    "@prelude//linking:link_info.bzl",
    "LinkArgs",  # @unused Used as a type
    "LinkStrategy",
    "LinkedObject",
)
load(
    "@prelude//linking:linkable_graph.bzl",
    "LinkableGraph",
    "LinkableGraphTSet",
)
load(
    "@prelude//linking:linkables.bzl",
    "LinkableProviders",
    "linkables",
)
load(
    "@prelude//linking:shared_libraries.bzl",
    "SharedLibrary",  # @unused Used as a type
    "create_shlib",
    "merge_shared_libraries",
    "traverse_shared_library_info",
)
load("@prelude//linking:types.bzl", "Linkage")
load("@prelude//python:internal_tools.bzl", "PythonInternalToolsInfo")
load("@prelude//python:toolchain.bzl", "PackageStyle")
load("@prelude//utils:argfile.bzl", "at_argfile")
load(":native_python_util.bzl", "CxxExtensionLinkInfo", "CxxExtensionLinkInfoReduced", "merge_cxx_extension_info", "reduce_cxx_extension_info")  # @unused Used as a type

def _get_root_link_group_specs(
        libs: list[LinkableProviders],
        extensions: dict[str, LinkableProviders]) -> list[LinkGroupLibSpec]:
    """
    Walk the linkable graph finding dlopen-able C++ libs.
    """

    # TODO(agallagher): We should handle `allow_embedding = False` C++ extensions
    # here too.

    specs = []

    # Add link group specs for dlopen-able libs.
    found_libs = set()
    for dep in libs:
        if dep.linkable_graph.label in found_libs:
            continue
        else:
            found_libs.add(dep.linkable_graph.label)
        specs.append(
            LinkGroupLibSpec(
                name = dep.linkable_root_info.name,
                is_shared_lib = True,
                root = dep.linkable_root_info,
                label = dep.linkable_graph.nodes.value.label,
                group = Group(
                    name = dep.linkable_root_info.name,
                    mappings = [
                        GroupMapping(
                            roots = [dep.linkable_graph.nodes.value.label],
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

    found_extensions = set()

    # Add link group specs for extensions.
    for name, extension in extensions.items():
        if extension.linkable_graph.label in found_extensions:
            continue
        else:
            found_extensions.add(extension.linkable_graph.label)
        specs.append(
            LinkGroupLibSpec(
                name = name,
                is_shared_lib = False,
                root = extension.linkable_root_info,
                group = Group(
                    name = name,
                    mappings = [
                        GroupMapping(
                            roots = [extension.linkable_graph.nodes.value.label],
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

def _get_shared_only_groups(shared_only_libs: list[LinkableProviders]) -> list[Group]:
    """
    Create link group mappings for shared-only libs that'll force the link to
    link them dynamically.
    """

    groups = []

    # Add link group specs for dlopen-able libs.
    for dep in shared_only_libs:
        if dep.linkable_graph == None:
            continue
        groups.append(
            Group(
                name = str(dep.linkable_graph.nodes.value.label.raw_target()),
                mappings = [
                    GroupMapping(
                        roots = [dep.linkable_graph.nodes.value.label],
                        traversal = Traversal("node"),
                        preferred_linkage = Linkage("shared"),
                    ),
                ],
                # TODO(@christylee): Add attributes to python dlopen-able libs
                attrs = GroupAttrs(
                    enable_distributed_thinlto = False,
                ),
            ),
        )
    return groups

def _get_link_group_info(
        ctx: AnalysisContext,
        link_deps: list[LinkableProviders],
        libs: list[LinkableProviders],
        extensions: dict[str, LinkableProviders],
        shared_only_libs: list[LinkableProviders]) -> ([LinkGroupInfo, None], list[LinkGroupLibSpec]):
    """
    Return the `LinkGroupInfo` and link group lib specs to use for this binary.
    This will handle parsing the various user-specific parameters and automatic
    link group lib spec generation for dlopen-enabled native libraries and,
    eventually, extensions.
    """

    link_group_map = ctx.attrs.link_group_map
    definitions = []

    # Native python is only ever used with static-pic linking
    link_strategy = LinkStrategy("static_pic")
    if isinstance(link_group_map, Dependency):
        if LinkGroupDefinitions in link_group_map:
            definitions = link_group_map[LinkGroupDefinitions].definitions(ctx.label, link_strategy)

    if not definitions:
        link_group_info = get_link_group_info(ctx, [d.linkable_graph for d in link_deps], link_strategy)

        # Add link group specs from user-provided link group info.
        if link_group_info != None:
            definitions = link_group_info.groups.values()

    # Add link group specs from dlopenable C++ libraries.
    root_specs = _get_root_link_group_specs(libs, extensions)

    # We prepend the dlopen roots, so that they take precedence over
    # user-specific ones.
    link_group_specs = root_specs + create_shared_lib_link_group_specs(ctx, definitions)

    # We add the auto-generated root specs first so it takes precedence (as
    # we really rely on this for things to work), followed by shared-only libs
    # to make sure we link against them dynamically. Add user-defined mappings
    # last.
    link_groups = [s.group for s in root_specs] + _get_shared_only_groups(shared_only_libs) + definitions

    linkable_graph = LinkableGraph(
        #label = ctx.label,
        nodes = ctx.actions.tset(
            LinkableGraphTSet,
            children = (
                [d.linkable_graph.nodes for d in link_deps] +
                [d.linkable_graph.nodes for d in libs] +
                [d.linkable_graph.nodes for d in extensions.values()] +
                [d.linkable_graph.nodes for d in shared_only_libs]
            ),
        ),
    )

    link_group_info = build_link_group_info(
        graph = linkable_graph,
        groups = link_groups,
        min_node_count = ctx.attrs.link_group_min_binary_node_count,
    )

    return (link_group_info, link_group_specs)

def _compute_cxx_extension_info(ctx, deps) -> (CxxExtensionLinkInfo, CxxExtensionLinkInfoReduced):
    executable_deps = ctx.attrs.executable_deps
    extension_info = merge_cxx_extension_info(
        ctx.actions,
        deps + executable_deps,
        # Add in dlopen-enabled libs from first-order deps.
        shared_deps = ctx.attrs.deps + ctx.attrs.preload_deps,
    )
    extension_info_reduced = reduce_cxx_extension_info(extension_info)
    return extension_info, extension_info_reduced

def _cxx_exe_allow_cache_upload(ctx) -> bool:
    return hasattr(ctx.attrs, "exe_allow_cache_uplod") and bool(ctx.attrs.exe_allow_cache_uplod)

def _compute_cxx_executable_info(
        ctx,
        extension_info_reduced,
        static_extension_info_out,
        inherited_preprocessor_info,
        python_toolchain,
        package_style,
        allow_cache_upload) -> CxxExecutableOutput:
    cxx_executable_srcs = [
        CxxSrcWithFlags(file = ctx.attrs.cxx_main, flags = []),
        CxxSrcWithFlags(file = ctx.attrs.static_extension_utils, flags = ["-DOSS_PYTHON=1"] if ctx.attrs.use_oss_python else []),
        CxxSrcWithFlags(file = static_extension_info_out, flags = []),
    ]

    # All deps inolved in the link.
    link_deps = (
        linkables(ctx.attrs.executable_deps + ctx.attrs.preload_deps) +
        extension_info_reduced.linkable_providers
    )

    link_group_info, auto_link_group_specs = _get_link_group_info(
        ctx,
        link_deps,
        extension_info_reduced.dlopen_deps,
        extension_info_reduced.unembeddable_extensions,
        extension_info_reduced.shared_only_libs,
    )

    extra_binary_link_flags = []

    extra_binary_link_flags.extend(python_toolchain.binary_linker_flags)

    # Set rpaths to find 1) the shared libs dir and the 2) runtime libs dir.
    rpath_ref = get_rpath_origin(get_cxx_toolchain_info(ctx).linker_info.type)
    rpath_ldflag = "-Wl,-rpath,{}/".format(rpath_ref)
    if package_style == PackageStyle("standalone"):
        extra_binary_link_flags.append(rpath_ldflag + "../..")
        extra_binary_link_flags.append(rpath_ldflag + "../lib")
    else:
        use_anon_target = getattr(ctx.attrs, "use_anon_target_for_analysis", False)
        if use_anon_target:
            link_tree_name = getattr(ctx.attrs, "name", ctx.attrs.rpath)
        else:
            link_tree_name = ctx.attrs.name

        rpath_ldflag_prefix = rpath_ldflag + "{}#link-tree".format(link_tree_name)
        extra_binary_link_flags.append(rpath_ldflag_prefix + "/runtime/lib")
        extra_binary_link_flags.append(rpath_ldflag_prefix)

    impl_params = CxxRuleConstructorParams(
        rule_type = "python_binary",
        headers_layout = cxx_get_regular_cxx_headers_layout(ctx),
        srcs = cxx_executable_srcs,
        extra_binary_link_flags = extra_binary_link_flags,
        extra_link_flags = python_toolchain.linker_flags,
        extra_preprocessors = [],
        extra_preprocessors_info = inherited_preprocessor_info,
        extra_link_deps = link_deps,
        exe_shared_libs_link_tree = False,
        force_full_hybrid_if_capable = True,
        prefer_stripped_objects = ctx.attrs.prefer_stripped_native_objects,
        link_group_info = link_group_info,
        auto_link_group_specs = auto_link_group_specs,
        exe_category_suffix = "python_exe",
        extra_shared_libs = traverse_shared_library_info(
            merge_shared_libraries(
                actions = ctx.actions,
                deps =
                    [d.shared_library_info for d in extension_info_reduced.shared_only_libs],
            ),
        ),
        extra_link_roots = (
            extension_info_reduced.unembeddable_extensions.values() +
            extension_info_reduced.dlopen_deps +
            extension_info_reduced.shared_only_libs +
            linkables(ctx.attrs.link_group_deps)
        ),
        exe_allow_cache_upload = bool(allow_cache_upload) or _cxx_exe_allow_cache_upload(ctx),
        compiler_flags = ctx.attrs.compiler_flags,
        lang_compiler_flags = ctx.attrs.lang_compiler_flags,
        platform_compiler_flags = ctx.attrs.platform_compiler_flags,
        lang_platform_compiler_flags = ctx.attrs.lang_platform_compiler_flags,
        preprocessor_flags = ctx.attrs.preprocessor_flags,
        lang_preprocessor_flags = ctx.attrs.lang_preprocessor_flags,
        platform_preprocessor_flags = ctx.attrs.platform_preprocessor_flags,
        lang_platform_preprocessor_flags = ctx.attrs.lang_platform_preprocessor_flags,
        error_handler = python_toolchain.python_error_handler,
    )

    return cxx_executable(ctx, impl_params)

def process_native_linking(
        ctx,
        deps,
        python_toolchain,
        python_internal_tools: PythonInternalToolsInfo,
        package_style,
        allow_cache_upload) -> (
    list[(SharedLibrary, str)],
    dict[str, (LinkedObject, Label)],
    list[LinkArgs],
    dict[str, typing.Any],
    dict[str, typing.Any],
):
    #TODO @dcssiva: fix types
    extra = {}
    extra_artifacts = {}

    extension_info, extension_info_reduced = _compute_cxx_extension_info(ctx, deps)

    executable_deps = ctx.attrs.executable_deps

    inherited_preprocessor_info = cxx_inherited_preprocessor_infos(executable_deps)

    # Generate an additional C file as input
    static_extension_info_out = ctx.actions.declare_output("static_extension_info.cpp")
    argfile = at_argfile(
        actions = ctx.actions,
        name = "generate_static_extension_info.argsfile",
        args = cmd_args(
            extension_info.set.project_as_args("python_module_names"),
        ),
    )
    cmd = cmd_args()
    cmd.add(cmd_args(python_internal_tools.generate_static_extension_info[RunInfo]))
    cmd.add(cmd_args(argfile))
    cmd.add(cmd_args(static_extension_info_out.as_output(), format = "--output={}"))

    # TODO we don't need to do this ...
    ctx.actions.run(cmd, category = "generate_static_extension_info")

    extra["static_extension_info"] = [DefaultInfo(default_output = static_extension_info_out)]

    executable_info = _compute_cxx_executable_info(
        ctx,
        extension_info_reduced,
        static_extension_info_out,
        inherited_preprocessor_info,
        python_toolchain,
        package_style,
        allow_cache_upload,
    )
    extra["native-executable"] = [DefaultInfo(default_output = executable_info.binary, sub_targets = executable_info.sub_targets)]

    # Add sub-targets for libs.
    for shlib in executable_info.shared_libs:
        # TODO(agallagher) There appears to be pre-existing soname conflicts
        # when building this (when using link groups), which prevents using
        # `with_unique_str_sonames`.
        if shlib.soname.is_str:
            extra[shlib.soname.ensure_str()] = [DefaultInfo(default_output = shlib.lib.output)]

    for name, group in executable_info.auto_link_groups.items():
        extra[name] = [DefaultInfo(default_output = group.output)]

    # Unembeddable extensions.
    extensions = {
        name: (
            executable_info.auto_link_groups[name],
            link.linkable_graph.nodes.value.label,
        )
        for name, link in extension_info_reduced.unembeddable_extensions.items()
    }

    # Put native libraries into the runtime location, as we need to unpack
    # potentially all of them before startup.
    shared_libs = [(s, "runtime/lib") for s in executable_info.shared_libs]

    # TODO expect(len(executable_info.runtime_files) == 0, "OH NO THERE ARE RUNTIME FILES")
    extra_artifacts.update(extension_info_reduced.artifacts)
    shared_libs.append((
        create_shlib(
            soname = ctx.attrs.executable_name,
            label = ctx.label,
            lib = LinkedObject(
                output = executable_info.binary,
                unstripped_output = executable_info.binary,
                dwp = executable_info.dwp,
            ),
        ),
        "runtime/bin",
    ))

    link_args = executable_info.link_args
    extra_artifacts["static_extension_finder.py"] = ctx.attrs.static_extension_finder
    return shared_libs, extensions, link_args, extra, extra_artifacts

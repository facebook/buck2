# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(
    "@prelude//:artifact_tset.bzl",
    "project_artifacts",
)
load(
    "@prelude//:resources.bzl",
    "create_resource_db",
    "gather_resources",
)
load(
    "@prelude//cxx:cxx_library_utility.bzl",
    "cxx_attr_deps",
)
load("@prelude//cxx:cxx_link_utility.bzl", "executable_shared_lib_arguments")
load("@prelude//cxx:cxx_utility.bzl", "cxx_attrs_get_allow_cache_upload")
load(
    "@prelude//cxx:link_groups.bzl",
    "LINK_GROUP_MAPPINGS_FILENAME_SUFFIX",
    "LINK_GROUP_MAPPINGS_SUB_TARGET",
    "LINK_GROUP_MAP_DATABASE_SUB_TARGET",
    "LinkGroupContext",
    "get_link_group_map_json",
    "is_link_group_shlib",
)
load("@prelude//cxx:linker.bzl", "DUMPBIN_SUB_TARGET", "PDB_SUB_TARGET", "get_dumpbin_providers", "get_pdb_providers")
load(
    "@prelude//dist:dist_info.bzl",
    "DistInfo",
)
load(
    "@prelude//linking:link_info.bzl",
    "LinkStrategy",
)
load(
    "@prelude//linking:shared_libraries.bzl",
    "merge_shared_libraries",
    "traverse_shared_library_info",
)
load("@prelude//os_lookup:defs.bzl", "OsLookup")
load(
    "@prelude//tests:re_utils.bzl",
    "get_re_executors_from_props",
)
load("@prelude//utils:arglike.bzl", "ArgLike")  # @unused Used as a type
load("@prelude//utils:utils.bzl", "flatten_dict")
load("@prelude//test/inject_test_run_info.bzl", "inject_test_run_info")
load(
    ":build.bzl",
    "compile_context",
    "generate_rustdoc",
    "rust_compile",
    "rust_compile_multi",
)
load(
    ":build_params.bzl",
    "Emit",
    "LinkageLang",
    "RuleType",
    "build_params",
    "output_filename",
)
load(":context.bzl", "CompileContext")
load(
    ":link_info.bzl",
    "DEFAULT_STATIC_LINK_STRATEGY",
    "attr_simple_crate_for_filenames",
    "enable_link_groups",
    "inherited_external_debug_info",
    "inherited_rust_cxx_link_group_info",
    "inherited_shared_libs",
)
load(":resources.bzl", "rust_attr_resources")

_CompileOutputs = record(
    link = field(Artifact),
    args = field(ArgLike),
    extra_targets = field(list[(str, Artifact)]),
    runtime_files = field(list[ArgLike]),
    external_debug_info = field(list[TransitiveSetArgsProjection]),
    sub_targets = field(dict[str, list[DefaultInfo]]),
    dist_info = DistInfo,
)

def _rust_binary_common(
        ctx: AnalysisContext,
        compile_ctx: CompileContext,
        default_roots: list[str],
        extra_flags: list[str],
        allow_cache_upload: bool) -> (list[Provider], cmd_args):
    toolchain_info = compile_ctx.toolchain_info

    simple_crate = attr_simple_crate_for_filenames(ctx)

    styles = {}
    dwp_target = None
    pdb = None
    strategy_param = {}  # strategy -> param
    sub_targets = {}

    specified_link_strategy = LinkStrategy(ctx.attrs.link_style) if ctx.attrs.link_style else DEFAULT_STATIC_LINK_STRATEGY

    target_os_type = ctx.attrs._target_os_type[OsLookup]
    linker_type = compile_ctx.cxx_toolchain_info.linker_info.type

    resources = flatten_dict(gather_resources(
        label = ctx.label,
        resources = rust_attr_resources(ctx),
        deps = cxx_attr_deps(ctx),
    ).values())

    for link_strategy in LinkStrategy:
        # Unlike for libraries, there's no possibility of different link styles
        # resulting in the same build params, so no need to deduplicate.
        params = build_params(
            rule = RuleType("binary"),
            proc_macro = False,
            link_strategy = link_strategy,
            lib_output_style = None,
            lang = LinkageLang("rust"),
            linker_type = linker_type,
            target_os_type = target_os_type,
        )
        strategy_param[link_strategy] = params
        name = link_strategy.value + "/" + output_filename(simple_crate, Emit("link"), params)
        output = ctx.actions.declare_output(name)

        # Gather and setup symlink tree of transitive shared library deps.
        shared_libs = {}

        rust_cxx_link_group_info = None
        link_group_mappings = {}
        link_group_libs = {}
        link_group_preferred_linkage = {}
        labels_to_links_map = {}
        filtered_targets = []

        if enable_link_groups(ctx, link_strategy, specified_link_strategy, is_binary = True):
            rust_cxx_link_group_info = inherited_rust_cxx_link_group_info(
                ctx,
                compile_ctx.dep_ctx,
                link_strategy = link_strategy,
            )
            link_group_mappings = rust_cxx_link_group_info.link_group_info.mappings
            link_group_libs = rust_cxx_link_group_info.link_group_libs
            link_group_preferred_linkage = rust_cxx_link_group_info.link_group_preferred_linkage
            labels_to_links_map = rust_cxx_link_group_info.labels_to_links_map
            filtered_targets = rust_cxx_link_group_info.filtered_targets

        # As per v1, we only setup a shared library symlink tree for the shared
        # link style.
        # XXX need link tree for dylib crates
        shlib_deps = []
        if link_strategy == LinkStrategy("shared") or rust_cxx_link_group_info != None:
            shlib_deps = inherited_shared_libs(ctx, compile_ctx.dep_ctx)

        shlib_info = merge_shared_libraries(ctx.actions, deps = shlib_deps)

        link_group_ctx = LinkGroupContext(
            link_group_mappings = link_group_mappings,
            link_group_libs = link_group_libs,
            link_group_preferred_linkage = link_group_preferred_linkage,
            labels_to_links_map = labels_to_links_map,
        )

        def shlib_filter(_name, shared_lib):
            return not rust_cxx_link_group_info or is_link_group_shlib(shared_lib.label, link_group_ctx)

        for soname, shared_lib in traverse_shared_library_info(shlib_info, filter_func = shlib_filter).items():
            shared_libs[soname] = shared_lib.lib

        if rust_cxx_link_group_info:
            # When there are no matches for a pattern based link group,
            # `link_group_mappings` will not have an entry associated with the lib.
            for _name, link_group_lib in link_group_libs.items():
                shared_libs.update(link_group_lib.shared_libs)

        # link groups shared libraries link args are directly added to the link command,
        # we don't have to add them here
        executable_args = executable_shared_lib_arguments(
            ctx,
            compile_ctx.cxx_toolchain_info,
            output,
            shared_libs,
        )

        extra_flags = toolchain_info.rustc_binary_flags + (extra_flags or [])

        # Compile rust binary.
        link, meta = rust_compile_multi(
            ctx = ctx,
            compile_ctx = compile_ctx,
            emits = [Emit("link"), Emit("metadata")],
            params = params,
            default_roots = default_roots,
            extra_link_args = executable_args.extra_link_args,
            predeclared_outputs = {Emit("link"): output},
            extra_flags = extra_flags,
            is_binary = True,
            allow_cache_upload = allow_cache_upload,
            rust_cxx_link_group_info = rust_cxx_link_group_info,
        )

        args = cmd_args(link.output).hidden(executable_args.runtime_files)
        extra_targets = [("check", meta.output)] + meta.diag.items()
        external_debug_info = project_artifacts(
            actions = ctx.actions,
            tsets = [inherited_external_debug_info(
                ctx,
                compile_ctx.dep_ctx,
                link.dwo_output_directory,
                link_strategy,
            )],
        )

        # If we have some resources, write it to the resources JSON file and add
        # it and all resources to "runtime_files" so that we make to materialize
        # them with the final binary.
        runtime_files = list(executable_args.runtime_files)
        if resources:
            resources_hidden = [create_resource_db(
                ctx = ctx,
                name = name + ".resources.json",
                binary = output,
                resources = resources,
            )]
            for resource in resources.values():
                resources_hidden.append(resource.default_output)
                resources_hidden.extend(resource.other_outputs)
            args.hidden(resources_hidden)
            runtime_files.extend(resources_hidden)

        sub_targets_for_link_strategy = {}

        sub_targets_for_link_strategy["shared-libraries"] = [DefaultInfo(
            default_output = ctx.actions.write_json(
                name + ".shared-libraries.json",
                {
                    "libraries": ["{}:{}[shared-libraries][{}]".format(ctx.label.path, ctx.label.name, name) for name in shared_libs.keys()],
                    "librariesdwp": ["{}:{}[shared-libraries][{}][dwp]".format(ctx.label.path, ctx.label.name, name) for name, lib in shared_libs.items() if lib.dwp],
                    "rpathtree": ["{}:{}[rpath-tree]".format(ctx.label.path, ctx.label.name)] if executable_args.shared_libs_symlink_tree else [],
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

        if isinstance(executable_args.shared_libs_symlink_tree, Artifact):
            sub_targets_for_link_strategy["rpath-tree"] = [DefaultInfo(
                default_output = executable_args.shared_libs_symlink_tree,
                other_outputs = [
                    lib.output
                    for lib in shared_libs.values()
                ] + [
                    lib.dwp
                    for lib in shared_libs.values()
                    if lib.dwp
                ],
            )]

        if rust_cxx_link_group_info:
            sub_targets_for_link_strategy[LINK_GROUP_MAP_DATABASE_SUB_TARGET] = [get_link_group_map_json(ctx, filtered_targets)]
            readable_mappings = {}
            for node, group in link_group_mappings.items():
                readable_mappings[group] = readable_mappings.get(group, []) + ["{}//{}:{}".format(node.cell, node.package, node.name)]
            sub_targets_for_link_strategy[LINK_GROUP_MAPPINGS_SUB_TARGET] = [DefaultInfo(
                default_output = ctx.actions.write_json(
                    name + LINK_GROUP_MAPPINGS_FILENAME_SUFFIX,
                    readable_mappings,
                ),
            )]

        styles[link_strategy] = _CompileOutputs(
            link = link.output,
            args = args,
            extra_targets = extra_targets,
            runtime_files = runtime_files,
            external_debug_info = executable_args.external_debug_info + external_debug_info,
            sub_targets = sub_targets_for_link_strategy,
            dist_info = DistInfo(
                shared_libs = shlib_info.set,
                nondebug_runtime_files = runtime_files,
            ),
        )

        if link_strategy == specified_link_strategy and link.dwp_output:
            dwp_target = link.dwp_output
        if link_strategy == specified_link_strategy and link.pdb:
            pdb = link.pdb

    expand = rust_compile(
        ctx = ctx,
        compile_ctx = compile_ctx,
        emit = Emit("expand"),
        params = strategy_param[DEFAULT_STATIC_LINK_STRATEGY],
        default_roots = default_roots,
        extra_flags = extra_flags,
    )

    compiled_outputs = styles[specified_link_strategy]
    extra_compiled_targets = (compiled_outputs.extra_targets + [
        ("doc", generate_rustdoc(
            ctx = ctx,
            compile_ctx = compile_ctx,
            params = strategy_param[DEFAULT_STATIC_LINK_STRATEGY],
            default_roots = default_roots,
            document_private_items = True,
        )),
        ("expand", expand.output),
        ("sources", compile_ctx.symlinked_srcs),
    ])
    sub_targets.update({k: [DefaultInfo(default_output = v)] for k, v in extra_compiled_targets})
    sub_targets.update(compiled_outputs.sub_targets)
    for (k, sub_compiled_outputs) in styles.items():
        sub_targets[k.value] = [
            DefaultInfo(
                default_output = sub_compiled_outputs.link,
                other_outputs = sub_compiled_outputs.runtime_files + sub_compiled_outputs.external_debug_info,
                # Check/save-analysis for each link style?
                sub_targets = sub_compiled_outputs.sub_targets,
            ),
            RunInfo(args = sub_compiled_outputs.args),
            sub_compiled_outputs.dist_info,
        ]

    if dwp_target:
        sub_targets["dwp"] = [
            DefaultInfo(
                default_output = dwp_target,
            ),
        ]

    if pdb:
        sub_targets[PDB_SUB_TARGET] = get_pdb_providers(pdb = pdb, binary = compiled_outputs.link)

    dupmbin_toolchain = compile_ctx.cxx_toolchain_info.dumpbin_toolchain_path
    if dupmbin_toolchain:
        sub_targets[DUMPBIN_SUB_TARGET] = get_dumpbin_providers(ctx, compiled_outputs.link, dupmbin_toolchain)

    providers = [
        DefaultInfo(
            default_output = compiled_outputs.link,
            other_outputs = compiled_outputs.runtime_files + compiled_outputs.external_debug_info,
            sub_targets = sub_targets,
        ),
        compiled_outputs.dist_info,
    ]
    return (providers, compiled_outputs.args)

def rust_binary_impl(ctx: AnalysisContext) -> list[Provider]:
    compile_ctx = compile_context(ctx)

    providers, args = _rust_binary_common(
        ctx = ctx,
        compile_ctx = compile_ctx,
        default_roots = ["main.rs"],
        extra_flags = [],
        allow_cache_upload = cxx_attrs_get_allow_cache_upload(ctx.attrs),
    )

    return providers + [RunInfo(args = args)]

def rust_test_impl(ctx: AnalysisContext) -> list[Provider]:
    compile_ctx = compile_context(ctx)
    toolchain_info = compile_ctx.toolchain_info

    extra_flags = toolchain_info.rustc_test_flags or []
    if ctx.attrs.framework:
        extra_flags += ["--test"]

    providers, args = _rust_binary_common(
        ctx = ctx,
        compile_ctx = compile_ctx,
        default_roots = ["main.rs", "lib.rs"],
        extra_flags = extra_flags,
        allow_cache_upload = False,
    )

    # Setup RE executors based on the `remote_execution` param.
    re_executor, executor_overrides = get_re_executors_from_props(ctx)

    return inject_test_run_info(
        ctx,
        ExternalRunnerTestInfo(
            type = "rust",
            command = [args],
            env = ctx.attrs.env,
            labels = ctx.attrs.labels,
            contacts = ctx.attrs.contacts,
            default_executor = re_executor,
            executor_overrides = executor_overrides,
            run_from_project_root = True,
            use_project_relative_paths = True,
        ),
    ) + providers

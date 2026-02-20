# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load(
    "@prelude//:artifact_tset.bzl",
    "project_artifacts",
)
load(
    "@prelude//:resources.bzl",
    "create_relocatable_resources_info",
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
    "build_shared_libs_for_symlink_tree",
    "get_link_group_map_json",
)
load("@prelude//cxx:linker.bzl", "DUMPBIN_SUB_TARGET", "PDB_SUB_TARGET", "get_dumpbin_providers", "get_pdb_providers")
load("@prelude//cxx:transformation_spec.bzl", "build_transformation_spec_context")
load(
    "@prelude//dist:dist_info.bzl",
    "DistInfo",
)
load(
    "@prelude//linking:link_info.bzl",
    "LinkStrategy",
    "process_link_strategy_for_pic_behavior",
)
load(
    "@prelude//linking:shared_libraries.bzl",
    "merge_shared_libraries",
    "traverse_shared_library_info",
)
load("@prelude//linking:stamp_build_info.bzl", "cxx_stamp_build_info", "stamp_build_info")
load("@prelude//os_lookup:defs.bzl", "OsLookup")
load("@prelude//rust/rust-analyzer:provider.bzl", "rust_analyzer_provider")
load("@prelude//test:inject_test_run_info.bzl", "inject_test_run_info")
load(
    "@prelude//tests:re_utils.bzl",
    "get_re_executors_from_props",
)
load(
    "@prelude//utils:build_graph_pattern.bzl",
    "new_build_graph_info",
)
load("@prelude//utils:utils.bzl", "flatten_dict")
load(
    ":build.bzl",
    "generate_rustdoc",
    "rust_compile",
)
load(
    ":build_params.bzl",
    "BuildParams",  # @unused Used as a type
    "Emit",
    "LinkageLang",
    "ProfileMode",  # @unused Used as a type
    "RuleType",
    "build_params",
)
load(
    ":context.bzl",
    "CompileContext",
    "compile_context",
    "output_filename",
)
load(
    ":link_info.bzl",
    "DEFAULT_STATIC_LINK_STRATEGY",
    "attr_simple_crate_for_filenames",
    "inherited_external_debug_info",
    "inherited_rust_cxx_link_group_info",
    "inherited_shared_libs",
)
load(":named_deps.bzl", "write_named_deps_names")
load(":outputs.bzl", "RustcExtraOutputsInfo", "output_as_diag_subtargets")
load(":profile.bzl", "make_profile_providers")
load(":resources.bzl", "rust_attr_resources")

def _strategy_params(
        ctx: AnalysisContext,
        compile_ctx: CompileContext) -> dict[LinkStrategy, BuildParams]:
    target_os_type = ctx.attrs._target_os_type[OsLookup]
    linker_type = compile_ctx.cxx_toolchain_info.linker_info.type

    params = {}
    for link_strategy in LinkStrategy:
        params[link_strategy] = build_params(
            rule = RuleType("binary"),
            proc_macro = False,
            link_strategy = link_strategy,
            lib_output_style = None,
            lang = LinkageLang("rust"),
            linker_type = linker_type,
            target_os_type = target_os_type,
        )
    return params

def _rust_binary_common(
        ctx: AnalysisContext,
        compile_ctx: CompileContext,
        default_roots: list[str],
        extra_flags: list[str],
        allow_cache_upload: bool) -> (list[Provider], cmd_args):
    toolchain_info = compile_ctx.toolchain_info

    simple_crate = attr_simple_crate_for_filenames(ctx)

    link_strategy = LinkStrategy(ctx.attrs.link_style) if ctx.attrs.link_style else DEFAULT_STATIC_LINK_STRATEGY
    link_strategy = process_link_strategy_for_pic_behavior(link_strategy, compile_ctx.cxx_toolchain_info.pic_behavior)

    cxx_deps = cxx_attr_deps(ctx)
    resources = flatten_dict(gather_resources(
        label = ctx.label,
        resources = rust_attr_resources(ctx),
        deps = cxx_deps,
    ).values())

    extra_flags = toolchain_info.rustc_binary_flags + (extra_flags or [])

    strategy_param = _strategy_params(ctx, compile_ctx)

    params = strategy_param[link_strategy]
    name = output_filename(compile_ctx, simple_crate, Emit("link"), params)

    enable_late_build_info_stamping = cxx_stamp_build_info(ctx)
    if enable_late_build_info_stamping:
        allow_cache_upload = True
        unstamped_name = output_filename(compile_ctx, simple_crate, Emit("link"), params, "-unstamped")
        predeclared_output = ctx.actions.declare_output(unstamped_name)
        final_output = ctx.actions.declare_output(name)
    else:
        # If not using late build info stamping, then the output will be stamped eagerly in rust_compile
        predeclared_output = ctx.actions.declare_output(name)
        final_output = predeclared_output

    build_graph_info = new_build_graph_info(ctx, cxx_deps)
    transformation_spec_context = build_transformation_spec_context(ctx, build_graph_info)

    rust_cxx_link_group_info = inherited_rust_cxx_link_group_info(
        ctx,
        compile_ctx.dep_ctx,
        link_strategy = link_strategy,
        transformation_spec_context = transformation_spec_context,
    )
    if rust_cxx_link_group_info != None:
        link_group_mappings = rust_cxx_link_group_info.link_group_info.mappings
        link_group_libs = rust_cxx_link_group_info.link_group_libs
        link_group_preferred_linkage = rust_cxx_link_group_info.link_group_preferred_linkage
        labels_to_links_map = rust_cxx_link_group_info.labels_to_links_map
        targets_consumed_by_link_groups = rust_cxx_link_group_info.targets_consumed_by_link_groups
        filtered_targets = rust_cxx_link_group_info.filtered_targets
    else:
        link_group_mappings = {}
        link_group_libs = {}
        link_group_preferred_linkage = {}
        labels_to_links_map = {}
        targets_consumed_by_link_groups = {}
        filtered_targets = []

    shlib_deps = []
    if link_strategy == LinkStrategy("shared") or rust_cxx_link_group_info != None:
        shlib_deps = inherited_shared_libs(ctx, compile_ctx.dep_ctx)

    shlib_info = merge_shared_libraries(ctx.actions, deps = shlib_deps)

    link_group_ctx = LinkGroupContext(
        link_group_mappings = link_group_mappings,
        link_group_libs = link_group_libs,
        link_group_preferred_linkage = link_group_preferred_linkage,
        labels_to_links_map = labels_to_links_map,
        targets_consumed_by_link_groups = targets_consumed_by_link_groups,
    )

    # Gather and setup symlink tree of transitive shared library deps.
    shared_libs = build_shared_libs_for_symlink_tree(
        use_link_groups = rust_cxx_link_group_info != None,
        link_group_ctx = link_group_ctx,
        link_strategy = link_strategy,
        shared_libraries = traverse_shared_library_info(shlib_info, transformation_provider = transformation_spec_context),
        extra_shared_libraries = [],
    )

    # link groups shared libraries link args are directly added to the link command,
    # we don't have to add them here
    executable_shlib_args = executable_shared_lib_arguments(
        ctx,
        compile_ctx.cxx_toolchain_info,
        final_output,
        shared_libs,
    )

    # Compile rust binary.
    link = rust_compile(
        ctx = ctx,
        compile_ctx = compile_ctx,
        emit = Emit("link"),
        params = params,
        default_roots = default_roots,
        extra_link_args = executable_shlib_args.extra_link_args,
        predeclared_output = predeclared_output,
        extra_flags = extra_flags,
        allow_cache_upload = allow_cache_upload,
        rust_cxx_link_group_info = rust_cxx_link_group_info,
        transformation_spec_context = transformation_spec_context,
        incremental_enabled = ctx.attrs.incremental_enabled,
    )

    if enable_late_build_info_stamping:
        stamp_build_info(ctx, link.output, final_output)

    args = cmd_args(final_output, hidden = executable_shlib_args.runtime_files)
    external_debug_info = project_artifacts(
        actions = ctx.actions,
        tsets = inherited_external_debug_info(
            ctx,
            compile_ctx.dep_ctx,
            link.compile_output.dwo_output_directory,
            link_strategy,
        ),
    )

    # If we have some resources, write it to the resources JSON file and add
    # it and all resources to "runtime_files" so that we make to materialize
    # them with the final binary.
    runtime_files = list(executable_shlib_args.runtime_files)
    relocatable_resources_json = None
    relocatable_resources_contents = None
    if resources:
        resources_hidden = [create_resource_db(
            ctx = ctx,
            name = name + ".resources.json",
            binary = final_output,
            resources = resources,
        )]
        for resource in resources.values():
            resources_hidden.append(resource.default_output)
            resources_hidden.extend(resource.other_outputs)
        args.add(cmd_args(hidden = resources_hidden))
        runtime_files.extend(resources_hidden)
        relocatable_resources_json, relocatable_resources_contents = create_relocatable_resources_info(
            ctx = ctx,
            name = name,
            resources = resources,
        )

    # A simple dict of sub-target key to artifact, which we'll convert to
    # DefaultInfo providers at the end
    extra_compiled_targets = {
        "sources": compile_ctx.symlinked_srcs,
    }
    sub_targets = {}

    # TODO(agallagher) There appears to be pre-existing soname conflicts
    # when building this (when using link groups), which prevents using
    # `with_unique_str_sonames`.
    str_soname_shlibs = {
        shlib.soname.ensure_str(): shlib
        for shlib in shared_libs
        if shlib.soname.is_str
    }
    sub_targets["shared-libraries"] = [DefaultInfo(
        default_output = ctx.actions.write_json(
            name + ".shared-libraries.json",
            {
                "libraries": [
                    "{}:{}[shared-libraries][{}]".format(ctx.label.path, ctx.label.name, soname)
                    for soname in str_soname_shlibs
                ],
                "librariesdwp": [
                    "{}:{}[shared-libraries][{}][dwp]".format(ctx.label.path, ctx.label.name, soname)
                    for soname, shlib in str_soname_shlibs.items()
                    if shlib.lib.dwp
                ],
                "rpathtree": ["{}:{}[rpath-tree]".format(ctx.label.path, ctx.label.name)] if executable_shlib_args.shared_libs_symlink_tree else [],
            },
        ),
        sub_targets = {
            soname: [DefaultInfo(
                default_output = shlib.lib.output,
                sub_targets = {"dwp": [DefaultInfo(default_output = shlib.lib.dwp)]} if shlib.lib.dwp else {},
            )]
            for soname, shlib in str_soname_shlibs.items()
        },
    )]

    if isinstance(executable_shlib_args.shared_libs_symlink_tree, Artifact):
        sub_targets["rpath-tree"] = [DefaultInfo(
            default_output = executable_shlib_args.shared_libs_symlink_tree,
            other_outputs = [
                shlib.lib.output
                for shlib in shared_libs
            ] + [
                shlib.lib.dwp
                for shlib in shared_libs
                if shlib.lib.dwp
            ],
        )]

    if rust_cxx_link_group_info:
        sub_targets[LINK_GROUP_MAP_DATABASE_SUB_TARGET] = [get_link_group_map_json(ctx, filtered_targets)]
        readable_mappings = {}
        for node, group in link_group_mappings.items():
            readable_mappings[group] = readable_mappings.get(group, []) + ["{}//{}:{}".format(node.cell, node.package, node.name)]
        sub_targets[LINK_GROUP_MAPPINGS_SUB_TARGET] = [DefaultInfo(
            default_output = ctx.actions.write_json(
                name + LINK_GROUP_MAPPINGS_FILENAME_SUFFIX,
                readable_mappings,
            ),
        )]

    # `infallible_diagnostics` allows us to circumvent compilation failures and
    # treat the resulting rustc action as a success, even if a metadata
    # artifact was not generated. This allows us to generate diagnostics
    # even when the target has bugs.
    diag_artifacts = {}
    clippy_artifacts = {}
    for incr in (True, False):
        diag_artifacts[incr] = rust_compile(
            ctx = ctx,
            compile_ctx = compile_ctx,
            emit = Emit("metadata-fast"),
            params = strategy_param[DEFAULT_STATIC_LINK_STRATEGY],
            default_roots = default_roots,
            extra_flags = extra_flags,
            infallible_diagnostics = True,
            incremental_enabled = incr,
            transformation_spec_context = transformation_spec_context,
        )
        clippy_artifacts[incr] = rust_compile(
            ctx = ctx,
            compile_ctx = compile_ctx,
            emit = Emit("clippy"),
            params = strategy_param[DEFAULT_STATIC_LINK_STRATEGY],
            default_roots = default_roots,
            extra_flags = extra_flags,
            infallible_diagnostics = True,
            incremental_enabled = incr,
            transformation_spec_context = transformation_spec_context,
        )

    providers = [RustcExtraOutputsInfo(
        metadata = diag_artifacts[False],
        metadata_incr = diag_artifacts[True],
        clippy = clippy_artifacts[False],
        clippy_incr = clippy_artifacts[True],
        remarks = None,  # Exposed via subtargets, not this provider
    )]

    incr_enabled = ctx.attrs.incremental_enabled
    extra_compiled_targets.update(output_as_diag_subtargets(diag_artifacts[incr_enabled], clippy_artifacts[incr_enabled]))

    # Add remarks subtargets (lazy - only built when subtarget requested)
    # Uses `params` to match the actual binary's link strategy
    remarks = rust_compile(
        ctx = ctx,
        compile_ctx = compile_ctx,
        emit = Emit("link"),
        params = params,
        default_roots = default_roots,
        extra_flags = extra_flags,
        incremental_enabled = False,
        profile_mode = ProfileMode("remarks"),
        transformation_spec_context = transformation_spec_context,
    )
    if remarks.compile_output.remarks_txt:
        extra_compiled_targets["remarks.txt"] = remarks.compile_output.remarks_txt
    if remarks.compile_output.remarks_json:
        extra_compiled_targets["remarks.json"] = remarks.compile_output.remarks_json

    extra_compiled_targets["expand"] = rust_compile(
        ctx = ctx,
        compile_ctx = compile_ctx,
        emit = Emit("expand"),
        params = strategy_param[DEFAULT_STATIC_LINK_STRATEGY],
        default_roots = default_roots,
        extra_flags = extra_flags,
        incremental_enabled = ctx.attrs.incremental_enabled,
    ).output

    llvm_ir_noopt = rust_compile(
        ctx = ctx,
        compile_ctx = compile_ctx,
        emit = Emit("llvm-ir-noopt"),
        params = strategy_param[DEFAULT_STATIC_LINK_STRATEGY],
        default_roots = default_roots,
        extra_flags = extra_flags,
        incremental_enabled = ctx.attrs.incremental_enabled,
    ).output
    llvm_time_trace = rust_compile(
        ctx = ctx,
        compile_ctx = compile_ctx,
        emit = Emit("link"),
        params = params,
        default_roots = default_roots,
        extra_link_args = executable_shlib_args.extra_link_args,
        extra_flags = extra_flags,
        rust_cxx_link_group_info = rust_cxx_link_group_info,
        incremental_enabled = ctx.attrs.incremental_enabled,
        profile_mode = ProfileMode("llvm-time-trace"),
    )
    self_profile = rust_compile(
        ctx = ctx,
        compile_ctx = compile_ctx,
        emit = Emit("link"),
        params = params,
        default_roots = default_roots,
        extra_link_args = executable_shlib_args.extra_link_args,
        extra_flags = extra_flags,
        rust_cxx_link_group_info = rust_cxx_link_group_info,
        incremental_enabled = ctx.attrs.incremental_enabled,
        profile_mode = ProfileMode("self-profile"),
    )
    profiles = make_profile_providers(
        ctx = ctx,
        compile_ctx = compile_ctx,
        llvm_ir_noopt = llvm_ir_noopt,
        llvm_time_trace = llvm_time_trace,
        self_profile = self_profile,
    )
    sub_targets["profile"] = profiles

    for emit_type in ["asm", "llvm-ir", "mir"]:
        extra_compiled_targets[emit_type] = rust_compile(
            ctx = ctx,
            compile_ctx = compile_ctx,
            emit = Emit(emit_type),
            params = params,
            default_roots = default_roots,
            extra_flags = extra_flags,
            incremental_enabled = ctx.attrs.incremental_enabled,
        ).output

    doc_output = generate_rustdoc(
        ctx = ctx,
        compile_ctx = compile_ctx,
        params = strategy_param[DEFAULT_STATIC_LINK_STRATEGY],
        default_roots = default_roots,
        document_private_items = True,
    )
    extra_compiled_targets["doc"] = doc_output

    named_deps_names = write_named_deps_names(ctx, compile_ctx)
    if named_deps_names:
        extra_compiled_targets["named_deps"] = named_deps_names

    if link.link_output.dwp_output:
        sub_targets["dwp"] = [
            DefaultInfo(
                default_output = link.link_output.dwp_output,
                other_outputs = [
                    shlib.lib.dwp
                    for shlib in shared_libs
                    if shlib.lib.dwp
                ] + ([executable_shlib_args.dwp_symlink_tree] if executable_shlib_args.dwp_symlink_tree else []),
            ),
        ]

    if link.link_output.pdb:
        sub_targets[PDB_SUB_TARGET] = get_pdb_providers(pdb = link.link_output.pdb, binary = final_output)

    dupmbin_toolchain = compile_ctx.cxx_toolchain_info.dumpbin_toolchain_path
    if dupmbin_toolchain:
        sub_targets[DUMPBIN_SUB_TARGET] = get_dumpbin_providers(ctx, final_output, dupmbin_toolchain)

    sub_targets.update({
        k: [DefaultInfo(default_output = v)]
        for (k, v) in extra_compiled_targets.items()
    })

    providers += [
        DefaultInfo(
            default_output = final_output,
            other_outputs = runtime_files + executable_shlib_args.external_debug_info + external_debug_info,
            sub_targets = sub_targets,
        ),
        DistInfo(
            shared_libs = shlib_info.set,
            nondebug_runtime_files = runtime_files,
            relocatable_resources_contents = relocatable_resources_contents,
            relocatable_resources_json = relocatable_resources_json,
        ),
    ]
    providers.append(rust_analyzer_provider(
        ctx = ctx,
        compile_ctx = compile_ctx,
        default_roots = default_roots,
    ))
    return (providers, args)

def rust_binary_impl(ctx: AnalysisContext) -> list[Provider]:
    compile_ctx = compile_context(ctx, binary = True)

    providers, args = _rust_binary_common(
        ctx = ctx,
        compile_ctx = compile_ctx,
        default_roots = ["main.rs"],
        extra_flags = [],
        allow_cache_upload = cxx_attrs_get_allow_cache_upload(ctx.attrs),
    )

    return providers + [RunInfo(args = args)]

def rust_test_impl(ctx: AnalysisContext) -> list[Provider]:
    compile_ctx = compile_context(ctx, binary = True)
    toolchain_info = compile_ctx.toolchain_info

    extra_flags = toolchain_info.rustc_test_flags or []
    if ctx.attrs.framework:
        extra_flags += ["--test"]

    providers, args = _rust_binary_common(
        ctx = ctx,
        compile_ctx = compile_ctx,
        # Unless default_roots are provided, it is ambiguous whether this test rule is invoked
        # to test a binary, or to test a library. As such, we must consider both main.rs and
        # lib.rs as potential candidates.
        default_roots = ctx.attrs.default_roots or ["main.rs", "lib.rs"],
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
            env = ctx.attrs.env | ctx.attrs.run_env,
            labels = ctx.attrs.labels,
            contacts = ctx.attrs.contacts,
            default_executor = re_executor,
            executor_overrides = executor_overrides,
            run_from_project_root = True,
            use_project_relative_paths = True,
        ),
    ) + providers

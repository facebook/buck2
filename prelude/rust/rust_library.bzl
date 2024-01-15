# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(
    "@prelude//:artifact_tset.bzl",
    "ArtifactTSet",
    "make_artifact_tset",
)
load("@prelude//:resources.bzl", "ResourceInfo", "gather_resources")
load(
    "@prelude//android:android_providers.bzl",
    "merge_android_packageable_info",
)
load(
    "@prelude//cxx:cxx_context.bzl",
    "get_cxx_toolchain_info",
)
load("@prelude//cxx:cxx_toolchain_types.bzl", "CxxToolchainInfo", "PicBehavior")
load(
    "@prelude//cxx:linker.bzl",
    "PDB_SUB_TARGET",
    "get_default_shared_library_name",
    "get_pdb_providers",
)
load(
    "@prelude//cxx:omnibus.bzl",
    "create_linkable_root",
)
load(
    "@prelude//linking:link_groups.bzl",
    "merge_link_group_lib_info",
)
load(
    "@prelude//linking:link_info.bzl",
    "Archive",
    "ArchiveLinkable",
    "LibOutputStyle",
    "LinkInfo",
    "LinkInfos",
    "LinkStrategy",
    "Linkage",
    "LinkedObject",
    "MergedLinkInfo",
    "SharedLibLinkable",
    "create_merged_link_info",
    "create_merged_link_info_for_propagation",
    "get_lib_output_style",
    "legacy_output_style_to_link_style",
)
load(
    "@prelude//linking:linkable_graph.bzl",
    "DlopenableLibraryInfo",
    "LinkableGraph",  # @unused Used as a type
    "create_linkable_graph",
    "create_linkable_graph_node",
    "create_linkable_node",
)
load(
    "@prelude//linking:shared_libraries.bzl",
    "SharedLibraryInfo",
    "create_shared_libraries",
    "merge_shared_libraries",
)
load("@prelude//linking:strip.bzl", "strip_debug_info")
load("@prelude//os_lookup:defs.bzl", "OsLookup")
load(
    ":build.bzl",
    "RustcOutput",  # @unused Used as a type
    "compile_context",
    "generate_rustdoc",
    "generate_rustdoc_test",
    "rust_compile",
    "rust_compile_multi",
)
load(
    ":build_params.bzl",
    "BuildParams",  # @unused Used as a type
    "CrateType",
    "Emit",
    "LinkageLang",
    "RuleType",
    "build_params",
)
load(
    ":context.bzl",
    "CompileContext",  # @unused Used as a type
    "CrateName",  # @unused Used as a type
    "DepCollectionContext",
)
load(
    ":link_info.bzl",
    "DEFAULT_STATIC_LINK_STRATEGY",
    "RustLinkInfo",
    "RustLinkStrategyInfo",
    "RustProcMacroMarker",  # @unused Used as a type
    "attr_crate",
    "inherited_exported_link_deps",
    "inherited_link_group_lib_infos",
    "inherited_linkable_graphs",
    "inherited_merged_link_infos",
    "inherited_shared_libs",
    "resolve_deps",
    "resolve_rust_deps",
    "strategy_info",
)
load(":proc_macro_alias.bzl", "rust_proc_macro_alias")
load(":resources.bzl", "rust_attr_resources")
load(":rust_toolchain.bzl", "RustToolchainInfo")
load(":targets.bzl", "targets")

def prebuilt_rust_library_impl(ctx: AnalysisContext) -> list[Provider]:
    providers = []

    # Default output.
    providers.append(
        DefaultInfo(
            default_output = ctx.attrs.rlib,
        ),
    )

    rust_toolchain = ctx.attrs._rust_toolchain[RustToolchainInfo]
    dep_ctx = DepCollectionContext(
        advanced_unstable_linking = rust_toolchain.advanced_unstable_linking,
        include_doc_deps = False,
        is_proc_macro = False,
        explicit_sysroot_deps = rust_toolchain.explicit_sysroot_deps,
        panic_runtime = rust_toolchain.panic_runtime,
    )

    cxx_toolchain = get_cxx_toolchain_info(ctx)
    linker_info = cxx_toolchain.linker_info

    archive_info = LinkInfos(
        default = LinkInfo(
            linkables = [
                ArchiveLinkable(
                    archive = Archive(artifact = ctx.attrs.rlib),
                    linker_type = linker_info.type,
                ),
            ],
        ),
        stripped = LinkInfo(
            linkables = [
                ArchiveLinkable(
                    archive = Archive(
                        artifact = strip_debug_info(
                            ctx = ctx,
                            out = ctx.attrs.rlib.short_path,
                            obj = ctx.attrs.rlib,
                        ),
                    ),
                    linker_type = linker_info.type,
                ),
            ],
        ),
    )
    link_infos = {LibOutputStyle("archive"): archive_info, LibOutputStyle("pic_archive"): archive_info}

    # Rust link provider.
    crate = attr_crate(ctx)
    strategies = {}
    for link_strategy in LinkStrategy:
        tdeps, tmetadeps, external_debug_info, tprocmacrodeps = _compute_transitive_deps(ctx, dep_ctx, link_strategy)
        external_debug_info = make_artifact_tset(
            actions = ctx.actions,
            children = external_debug_info,
        )
        strategies[link_strategy] = RustLinkStrategyInfo(
            rlib = ctx.attrs.rlib,
            transitive_deps = tdeps,
            rmeta = ctx.attrs.rlib,
            transitive_rmeta_deps = tmetadeps,
            transitive_proc_macro_deps = tprocmacrodeps,
            pdb = None,
            external_debug_info = external_debug_info,
        )

    merged_link_info, shared_libs, inherited_graphs, inherited_link_deps = _rust_link_providers(ctx, dep_ctx, cxx_toolchain, link_infos)
    providers.append(
        RustLinkInfo(
            crate = crate,
            strategies = strategies,
            exported_link_deps = inherited_link_deps,
            merged_link_info = merged_link_info,
            shared_libs = shared_libs,
            linkable_graphs = inherited_graphs,
        ),
    )

    # Native link provier.
    providers.append(
        create_merged_link_info(
            ctx,
            PicBehavior("supported"),
            link_infos,
            exported_deps = [d[MergedLinkInfo] for d in ctx.attrs.deps],
            # TODO(agallagher): This matches v1 behavior, but some of these libs
            # have prebuilt DSOs which might be usable.
            preferred_linkage = Linkage("static"),
        ),
    )

    # Native link graph setup.
    linkable_graph = create_linkable_graph(
        ctx,
        node = create_linkable_graph_node(
            ctx,
            linkable_node = create_linkable_node(
                ctx = ctx,
                preferred_linkage = Linkage("static"),
                exported_deps = ctx.attrs.deps,
                link_infos = link_infos,
                default_soname = get_default_shared_library_name(linker_info, ctx.label),
            ),
        ),
        deps = ctx.attrs.deps,
    )
    providers.append(linkable_graph)

    providers.append(merge_link_group_lib_info(children = inherited_link_group_lib_infos(ctx, dep_ctx)))

    # FIXME(JakobDegen): I am about 85% confident that this matches what C++
    # does for prebuilt libraries if they don't have a shared variant and have
    # preferred linkage static. C++ doesn't require static preferred linkage on
    # their prebuilt libraries, and so they incur extra complexity here that we
    # don't have to deal with.
    #
    # However, Rust linking is not the same as C++ linking. If Rust were
    # disciplined about its use of `LibOutputStyle`, `Linkage` and
    # `LinkStrategy`, then this would at least be no more wrong than what C++
    # does. In the meantime however...
    providers.append(SharedLibraryInfo(set = None))

    providers.append(merge_android_packageable_info(ctx.label, ctx.actions, ctx.attrs.deps))

    return providers

def rust_library_impl(ctx: AnalysisContext) -> list[Provider]:
    compile_ctx = compile_context(ctx)
    toolchain_info = compile_ctx.toolchain_info

    # Multiple styles and language linkages could generate the same crate types
    # (eg procmacro or using preferred_linkage), so we need to see how many
    # distinct kinds of build we actually need to deal with.
    param_lang, lang_style_param = _build_params_for_styles(ctx, compile_ctx)

    artifacts = _build_library_artifacts(ctx, compile_ctx, param_lang.keys())

    rust_param_artifact = {}
    native_param_artifact = {}
    check_artifacts = None

    for params, (link, meta) in artifacts.items():
        if LinkageLang("rust") in param_lang[params]:
            # Grab the check output for all kinds of builds to use
            # in the check subtarget. The link style doesn't matter
            # so pick the first.
            if check_artifacts == None:
                check_artifacts = {"check": meta.output}
                check_artifacts.update(meta.diag)

            rust_param_artifact[params] = (link, meta)
        if LinkageLang("native") in param_lang[params] or LinkageLang("native-unbundled") in param_lang[params]:
            native_param_artifact[params] = link

    # For doctests, we need to know two things to know how to link them. The
    # first is that we need a link strategy, which affects how deps of this
    # target are handled
    if ctx.attrs.doc_link_style:
        doc_link_strategy = LinkStrategy(ctx.attrs.doc_link_style)
    else:
        # FIXME(JakobDegen): In this position, a binary would just fall back to
        # the default link style. However, we have a little bit of additional
        # information in the form of the preferred linkage that we can use to
        # make a different decision. There's nothing technically wrong with
        # that, but a comment explaining why we want to do it would be nice
        doc_link_strategy = {
            "any": LinkStrategy("shared"),
            "shared": LinkStrategy("shared"),
            "static": DEFAULT_STATIC_LINK_STRATEGY,
        }[ctx.attrs.preferred_linkage]

    # The second thing we need is a lib output style of the regular, non-doctest
    # version of this target that we want. Rustdoc does not handle this library
    # being built in a "shared" way well, so this must be a static output style.
    if ctx.attrs.doc_link_style:
        doc_output_style = {
            "shared": LibOutputStyle("pic_archive"),
            "static": LibOutputStyle("archive"),
            "static_pic": LibOutputStyle("pic_archive"),
        }[ctx.attrs.doc_link_style]
    else:
        doc_output_style = LibOutputStyle("pic_archive")
    static_library_params = lang_style_param[(LinkageLang("rust"), doc_output_style)]

    # Among {rustdoc, doctests, macro expand}, doctests are the only one which
    # cares about linkage. So whatever build params we picked for the doctests,
    # reuse them for the other two as well
    default_roots = ["lib.rs"]
    rustdoc = generate_rustdoc(
        ctx = ctx,
        compile_ctx = compile_ctx,
        params = static_library_params,
        default_roots = default_roots,
        document_private_items = False,
    )

    expand = rust_compile(
        ctx = ctx,
        compile_ctx = compile_ctx,
        emit = Emit("expand"),
        params = static_library_params,
        default_roots = default_roots,
    )

    # If doctests=True or False is set on the individual target, respect that.
    # Otherwise look at the global setting on the toolchain.
    doctests_enabled = \
        (ctx.attrs.doctests if ctx.attrs.doctests != None else toolchain_info.doctests) and \
        toolchain_info.rustc_target_triple == targets.exec_triple(ctx)

    rustdoc_test_params = build_params(
        rule = RuleType("binary"),
        proc_macro = ctx.attrs.proc_macro,
        link_strategy = doc_link_strategy,
        lib_output_style = None,
        lang = LinkageLang("rust"),
        linker_type = compile_ctx.cxx_toolchain_info.linker_info.type,
        target_os_type = ctx.attrs._target_os_type[OsLookup],
    )
    rustdoc_test = generate_rustdoc_test(
        ctx = ctx,
        compile_ctx = compile_ctx,
        link_strategy = rustdoc_test_params.dep_link_strategy,
        rlib = rust_param_artifact[static_library_params][0].output,
        params = rustdoc_test_params,
        default_roots = default_roots,
    )

    providers = []

    link_infos = _link_infos(
        ctx = ctx,
        compile_ctx = compile_ctx,
        lang_style_param = lang_style_param,
        param_artifact = native_param_artifact,
    )

    providers += _default_providers(
        lang_style_param = lang_style_param,
        param_artifact = rust_param_artifact,
        rustdoc = rustdoc,
        rustdoc_test = rustdoc_test,
        doctests_enabled = doctests_enabled,
        check_artifacts = check_artifacts,
        expand = expand.output,
        sources = compile_ctx.symlinked_srcs,
    )
    rust_link_info = _rust_providers(
        ctx = ctx,
        compile_ctx = compile_ctx,
        lang_style_param = lang_style_param,
        param_artifact = rust_param_artifact,
        link_infos = link_infos,
    )
    providers.append(rust_link_info)
    providers += _native_providers(
        ctx = ctx,
        compile_ctx = compile_ctx,
        lang_style_param = lang_style_param,
        param_artifact = native_param_artifact,
        link_infos = link_infos,
        rust_link_info = rust_link_info,
    )

    deps = [dep.dep for dep in resolve_deps(ctx, compile_ctx.dep_ctx)]
    providers.append(ResourceInfo(resources = gather_resources(
        label = ctx.label,
        resources = rust_attr_resources(ctx),
        deps = deps,
    )))

    providers.append(merge_android_packageable_info(ctx.label, ctx.actions, deps))

    return providers

def _build_params_for_styles(
        ctx: AnalysisContext,
        compile_ctx: CompileContext) -> (
    dict[BuildParams, list[LinkageLang]],
    dict[(LinkageLang, LibOutputStyle), BuildParams],
):
    """
    For a given rule, return two things:
    - a set of build params we need for all combinations of linkage langages and
      link styles, mapped to which languages they apply to
    - a mapping from linkage language and link style to build params

    This is needed because different combinations may end up using the same set
    of params, and we want to minimize invocations to rustc, both for
    efficiency's sake, but also to avoid duplicate objects being linked
    together.
    """

    param_lang = {}  # param -> linkage_lang
    style_param = {}  # (linkage_lang, output_style) -> param

    target_os_type = ctx.attrs._target_os_type[OsLookup]
    linker_type = compile_ctx.cxx_toolchain_info.linker_info.type

    # Styles+lang linkage to params
    for linkage_lang in LinkageLang:
        # Skip proc_macro + non-rust combinations
        if ctx.attrs.proc_macro and linkage_lang != LinkageLang("rust"):
            continue

        for lib_output_style in LibOutputStyle:
            params = build_params(
                rule = RuleType("library"),
                proc_macro = ctx.attrs.proc_macro,
                link_strategy = None,
                lib_output_style = lib_output_style,
                lang = linkage_lang,
                linker_type = linker_type,
                target_os_type = target_os_type,
            )
            if params not in param_lang:
                param_lang[params] = []
            param_lang[params] = param_lang[params] + [linkage_lang]
            style_param[(linkage_lang, lib_output_style)] = params

    return (param_lang, style_param)

def _build_library_artifacts(
        ctx: AnalysisContext,
        compile_ctx: CompileContext,
        params: list[BuildParams]) -> dict[BuildParams, (RustcOutput, RustcOutput)]:
    """
    Generate the actual actions to build various output artifacts. Given the set
    parameters we need, return a mapping to the linkable and metadata artifacts.
    """
    param_artifact = {}

    for params in params:
        # Separate actions for each emit type
        #
        # In principle we don't really need metadata for C++-only artifacts, but I don't think it hurts
        link, meta = rust_compile_multi(
            ctx = ctx,
            compile_ctx = compile_ctx,
            emits = [Emit("link"), Emit("metadata")],
            params = params,
            default_roots = ["lib.rs"],
        )

        param_artifact[params] = (link, meta)

    return param_artifact

def _handle_rust_artifact(
        ctx: AnalysisContext,
        dep_ctx: DepCollectionContext,
        crate_type: CrateType,
        link_strategy: LinkStrategy,
        link: RustcOutput,
        meta: RustcOutput) -> RustLinkStrategyInfo:
    """
    Return the RustLinkInfo for a given set of artifacts. The main consideration
    is computing the right set of dependencies.
    """

    # If we're a crate where our consumers should care about transitive deps,
    # then compute them (specifically, not proc-macro).
    if crate_type != CrateType("proc-macro"):
        tdeps, tmetadeps, external_debug_info, tprocmacrodeps = _compute_transitive_deps(ctx, dep_ctx, link_strategy)
    else:
        tdeps, tmetadeps, external_debug_info, tprocmacrodeps = {}, {}, [], {}

    if not ctx.attrs.proc_macro:
        external_debug_info = make_artifact_tset(
            actions = ctx.actions,
            label = ctx.label,
            artifacts = filter(None, [link.dwo_output_directory]),
            children = external_debug_info,
        )
        return RustLinkStrategyInfo(
            rlib = link.output,
            transitive_deps = tdeps,
            rmeta = meta.output,
            transitive_rmeta_deps = tmetadeps,
            transitive_proc_macro_deps = tprocmacrodeps,
            pdb = link.pdb,
            external_debug_info = external_debug_info,
        )
    else:
        # Proc macro deps are always the real thing
        return RustLinkStrategyInfo(
            rlib = link.output,
            transitive_deps = tdeps,
            rmeta = link.output,
            transitive_rmeta_deps = tdeps,
            transitive_proc_macro_deps = tprocmacrodeps,
            pdb = link.pdb,
            external_debug_info = ArtifactTSet(),
        )

def _default_providers(
        lang_style_param: dict[(LinkageLang, LibOutputStyle), BuildParams],
        param_artifact: dict[BuildParams, (RustcOutput, RustcOutput)],
        rustdoc: Artifact,
        rustdoc_test: (cmd_args, dict[str, cmd_args]),
        doctests_enabled: bool,
        check_artifacts: dict[str, Artifact],
        expand: Artifact,
        sources: Artifact) -> list[Provider]:
    targets = {}
    targets.update(check_artifacts)
    targets["sources"] = sources
    targets["expand"] = expand
    targets["doc"] = rustdoc
    sub_targets = {
        k: [DefaultInfo(default_output = v)]
        for (k, v) in targets.items()
    }

    # Add provider for default output, and for each lib output style...
    # FIXME(JakobDegen): C++ rules only provide some of the output styles,
    # determined by `get_output_styles_for_linkage` in `linking/link_info.bzl`.
    # Do we want to do the same?
    for output_style in LibOutputStyle:
        link, _ = param_artifact[lang_style_param[(LinkageLang("rust"), output_style)]]
        nested_sub_targets = {}
        if link.pdb:
            nested_sub_targets[PDB_SUB_TARGET] = get_pdb_providers(pdb = link.pdb, binary = link.output)

        # FIXME(JakobDegen): Ideally we'd use the same
        # `subtarget_for_output_style` as C++, but that uses `static-pic`
        # instead of `static_pic`. Would be nice if that were consistent
        name = legacy_output_style_to_link_style(output_style).value
        sub_targets[name] = [DefaultInfo(
            default_output = link.output,
            sub_targets = nested_sub_targets,
        )]

    providers = []

    (rustdoc_cmd, rustdoc_env) = rustdoc_test
    rustdoc_test_info = ExternalRunnerTestInfo(
        type = "rustdoc",
        command = [rustdoc_cmd],
        run_from_project_root = True,
        env = rustdoc_env,
    )

    # Always let the user run doctests via `buck2 test :crate[doc]`
    sub_targets["doc"].append(rustdoc_test_info)

    # But only run it as a part of `buck2 test :crate` if it's not disabled
    if doctests_enabled:
        providers.append(rustdoc_test_info)

    providers.append(DefaultInfo(
        default_output = check_artifacts["check"],
        sub_targets = sub_targets,
    ))

    return providers

def _rust_link_providers(
        ctx: AnalysisContext,
        dep_ctx: DepCollectionContext,
        cxx_toolchain: CxxToolchainInfo,
        link_infos: dict[LibOutputStyle, LinkInfos]) -> (
    MergedLinkInfo,
    SharedLibraryInfo,
    list[LinkableGraph],
    list[Dependency],
):
    # These are never accessed in the case of proc macros, so just return some dummy
    # values
    if ctx.attrs.proc_macro:
        return (
            create_merged_link_info_for_propagation(ctx, []),
            merge_shared_libraries(ctx.actions),
            [],
            [],
        )

    inherited_link_infos = inherited_merged_link_infos(ctx, dep_ctx)
    inherited_shlibs = inherited_shared_libs(ctx, dep_ctx)
    inherited_graphs = inherited_linkable_graphs(ctx, dep_ctx)
    inherited_link_deps = inherited_exported_link_deps(ctx, dep_ctx)

    if dep_ctx.advanced_unstable_linking:
        # We have to produce a version of the providers that are defined in such
        # a way that native rules looking at these providers will also pick up
        # the `FORCE_RLIB` behavior. The general approach to that will be to
        # claim that we have `preferred_linkage = "static"`.
        #
        # Note that all of this code is FORCE_RLIB specific. Disabling that
        # setting requires replacing this with the "real" native providers
        merged_link_info = create_merged_link_info(
            ctx,
            cxx_toolchain.pic_behavior,
            link_infos,
            exported_deps = inherited_link_infos,
            preferred_linkage = Linkage("static"),
        )
        shared_libs = merge_shared_libraries(
            # We never actually have any shared libraries to add
            ctx.actions,
            deps = inherited_shlibs,
        )

        # The link graph representation is a little bit weird, since instead of
        # just building up a graph via tsets, it uses a flat list of labeled
        # nodes, each with a list of labels for dependency edges. The node that
        # we create here cannot just use this target's label, since that would
        # conflict with the node created for the native providers. As a result,
        # we make up a fake subtarget to get a distinct label
        new_label = ctx.label.configured_target().with_sub_target((ctx.label.sub_target or []) + ["fake_force_rlib_subtarget"])
        linkable_graph = create_linkable_graph(
            ctx,
            node = create_linkable_graph_node(
                ctx,
                linkable_node = create_linkable_node(
                    ctx = ctx,
                    preferred_linkage = Linkage("static"),
                    exported_deps = inherited_graphs,
                    link_infos = link_infos,
                    default_soname = get_default_shared_library_name(cxx_toolchain.linker_info, ctx.label),
                ),
                label = new_label,
            ),
            deps = inherited_graphs,
        )
        inherited_graphs = [linkable_graph]
    else:
        merged_link_info = create_merged_link_info_for_propagation(ctx, inherited_link_infos)
        shared_libs = merge_shared_libraries(
            ctx.actions,
            deps = inherited_shlibs,
        )
    return (merged_link_info, shared_libs, inherited_graphs, inherited_link_deps)

def _rust_providers(
        ctx: AnalysisContext,
        compile_ctx: CompileContext,
        lang_style_param: dict[(LinkageLang, LibOutputStyle), BuildParams],
        param_artifact: dict[BuildParams, (RustcOutput, RustcOutput)],
        link_infos: dict[LibOutputStyle, LinkInfos]) -> RustLinkInfo:
    """
    Return the set of providers for Rust linkage.
    """
    crate = attr_crate(ctx)

    pic_behavior = compile_ctx.cxx_toolchain_info.pic_behavior
    preferred_linkage = Linkage(ctx.attrs.preferred_linkage)

    strategy_info = {}
    for link_strategy in LinkStrategy:
        params = lang_style_param[(LinkageLang("rust"), get_lib_output_style(link_strategy, preferred_linkage, pic_behavior))]
        link, meta = param_artifact[params]
        strategy_info[link_strategy] = _handle_rust_artifact(ctx, compile_ctx.dep_ctx, params.crate_type, link_strategy, link, meta)

    merged_link_info, shared_libs, inherited_graphs, inherited_link_deps = _rust_link_providers(ctx, compile_ctx.dep_ctx, compile_ctx.cxx_toolchain_info, link_infos)

    # Create rust library provider.
    rust_link_info = RustLinkInfo(
        crate = crate,
        strategies = strategy_info,
        merged_link_info = merged_link_info,
        exported_link_deps = inherited_link_deps,
        shared_libs = shared_libs,
        linkable_graphs = inherited_graphs,
    )

    return rust_link_info

def _link_infos(
        ctx: AnalysisContext,
        compile_ctx: CompileContext,
        lang_style_param: dict[(LinkageLang, LibOutputStyle), BuildParams],
        param_artifact: dict[BuildParams, RustcOutput]) -> dict[LibOutputStyle, LinkInfos]:
    if ctx.attrs.proc_macro:
        # Don't need any of this for proc macros
        return {}

    advanced_unstable_linking = compile_ctx.toolchain_info.advanced_unstable_linking
    lang = LinkageLang("native-unbundled") if advanced_unstable_linking else LinkageLang("native")
    linker_type = compile_ctx.cxx_toolchain_info.linker_info.type

    link_infos = {}
    for output_style in LibOutputStyle:
        lib = param_artifact[lang_style_param[(lang, output_style)]]
        external_debug_info = make_artifact_tset(
            actions = ctx.actions,
            label = ctx.label,
            artifacts = filter(None, [lib.dwo_output_directory]),
            children = lib.extra_external_debug_info,
        )
        if output_style == LibOutputStyle("shared_lib"):
            link_infos[output_style] = LinkInfos(
                default = LinkInfo(
                    linkables = [SharedLibLinkable(lib = lib.output)],
                    external_debug_info = external_debug_info,
                ),
                stripped = LinkInfo(
                    linkables = [SharedLibLinkable(lib = lib.stripped_output)],
                    external_debug_info = external_debug_info,
                ),
            )
        else:
            link_infos[output_style] = LinkInfos(
                default = LinkInfo(
                    linkables = [ArchiveLinkable(
                        archive = Archive(artifact = lib.output),
                        linker_type = linker_type,
                    )],
                    external_debug_info = external_debug_info,
                ),
                stripped = LinkInfo(
                    linkables = [ArchiveLinkable(
                        archive = Archive(artifact = lib.stripped_output),
                        linker_type = linker_type,
                    )],
                ),
            )
    return link_infos

def _native_providers(
        ctx: AnalysisContext,
        compile_ctx: CompileContext,
        lang_style_param: dict[(LinkageLang, LibOutputStyle), BuildParams],
        param_artifact: dict[BuildParams, RustcOutput],
        link_infos: dict[LibOutputStyle, LinkInfos],
        rust_link_info: RustLinkInfo) -> list[Provider]:
    """
    Return the set of providers needed to link Rust as a dependency for native
    (ie C/C++) code, along with relevant dependencies.
    """

    if ctx.attrs.proc_macro:
        # Proc-macros never have a native form
        return []

    # If advanced_unstable_linking is set on the the rust toolchain, then build this artifact
    # using the "native-unbundled" linkage language. See LinkageLang docs for more details
    advanced_unstable_linking = compile_ctx.toolchain_info.advanced_unstable_linking
    lang = LinkageLang("native-unbundled") if advanced_unstable_linking else LinkageLang("native")

    if advanced_unstable_linking:
        # The rust link providers already contain the linkables for the `archive` and `pic_archive`
        # cases
        link_infos = {
            LibOutputStyle("shared_lib"): link_infos[LibOutputStyle("shared_lib")],
            LibOutputStyle("archive"): LinkInfos(default = LinkInfo()),
            LibOutputStyle("pic_archive"): LinkInfos(default = LinkInfo()),
        }

    # We collected transitive deps in the Rust link providers
    inherited_link_infos = [rust_link_info.merged_link_info]
    inherited_shlibs = [rust_link_info.shared_libs]
    inherited_link_graphs = rust_link_info.linkable_graphs

    linker_info = compile_ctx.cxx_toolchain_info.linker_info
    linker_type = linker_info.type

    providers = []

    shared_lib_params = lang_style_param[(lang, LibOutputStyle("shared_lib"))]
    shared_lib_output = param_artifact[shared_lib_params].output

    preferred_linkage = Linkage(ctx.attrs.preferred_linkage)

    # Native link provider.
    providers.append(create_merged_link_info(
        ctx,
        compile_ctx.cxx_toolchain_info.pic_behavior,
        link_infos,
        exported_deps = inherited_link_infos,
        preferred_linkage = preferred_linkage,
    ))

    solibs = {}

    # Add the shared library to the list of shared libs.
    shlib_name = get_default_shared_library_name(linker_info, ctx.label)

    # Only add a shared library if we generated one.
    # TODO(cjhopman): This is strange. Normally (like in c++) the link_infos passed to create_merged_link_info above would only have
    # a value for LibOutputStyle("shared_lib") if that were created and we could just check for that key. Given that I intend
    # to remove the SharedLibraries provider, maybe just wait for that to resolve this.
    if get_lib_output_style(LinkStrategy("shared"), preferred_linkage, compile_ctx.cxx_toolchain_info.pic_behavior) == LibOutputStyle("shared_lib"):
        solibs[shlib_name] = LinkedObject(
            output = shared_lib_output,
            unstripped_output = shared_lib_output,
            external_debug_info = link_infos[LibOutputStyle("shared_lib")].default.external_debug_info,
        )

    # Native shared library provider.
    providers.append(merge_shared_libraries(
        ctx.actions,
        create_shared_libraries(ctx, solibs),
        inherited_shlibs,
    ))

    # Omnibus root provider.
    linkable_root = create_linkable_root(
        name = shlib_name,
        link_infos = LinkInfos(
            default = LinkInfo(
                linkables = [ArchiveLinkable(
                    archive = Archive(
                        artifact = shared_lib_output,
                    ),
                    linker_type = linker_type,
                    link_whole = True,
                )],
                external_debug_info = link_infos[LibOutputStyle("pic_archive")].default.external_debug_info,
            ),
        ),
        deps = inherited_link_graphs,
    )
    providers.append(linkable_root)

    # Mark libraries that support `dlopen`.
    if getattr(ctx.attrs, "supports_python_dlopen", False):
        providers.append(DlopenableLibraryInfo())

    linkable_graph = create_linkable_graph(
        ctx,
        node = create_linkable_graph_node(
            ctx,
            linkable_node = create_linkable_node(
                ctx = ctx,
                preferred_linkage = preferred_linkage,
                exported_deps = inherited_link_graphs,
                link_infos = link_infos,
                shared_libs = solibs,
                default_soname = shlib_name,
            ),
        ),
        deps = inherited_link_graphs,
    )

    providers.append(linkable_graph)

    # We never need to add anything to this provider because Rust libraries
    # cannot act as link group libs, especially given that they only support
    # auto link groups anyway
    providers.append(merge_link_group_lib_info(children = inherited_link_group_lib_infos(ctx, compile_ctx.dep_ctx)))

    return providers

# Compute transitive deps. Caller decides whether this is necessary.
def _compute_transitive_deps(
        ctx: AnalysisContext,
        dep_ctx: DepCollectionContext,
        dep_link_strategy: LinkStrategy) -> (
    dict[Artifact, CrateName],
    dict[Artifact, CrateName],
    list[ArtifactTSet],
    dict[RustProcMacroMarker, ()],
):
    transitive_deps = {}
    transitive_rmeta_deps = {}
    external_debug_info = []
    transitive_proc_macro_deps = {}

    for dep in resolve_rust_deps(ctx, dep_ctx):
        if dep.proc_macro_marker != None:
            transitive_proc_macro_deps[dep.proc_macro_marker] = ()

            # We don't want to propagate proc macros directly, and they have no transitive deps
            continue
        strategy = strategy_info(dep.info, dep_link_strategy)
        transitive_deps[strategy.rlib] = dep.info.crate
        transitive_deps.update(strategy.transitive_deps)

        transitive_rmeta_deps[strategy.rmeta] = dep.info.crate
        transitive_rmeta_deps.update(strategy.transitive_rmeta_deps)

        external_debug_info.append(strategy.external_debug_info)

        transitive_proc_macro_deps.update(strategy.transitive_proc_macro_deps)

    return transitive_deps, transitive_rmeta_deps, external_debug_info, transitive_proc_macro_deps

def rust_library_macro_wrapper(rust_library: typing.Callable) -> typing.Callable:
    def wrapper(**kwargs):
        if not kwargs.pop("_use_legacy_proc_macros", False) and kwargs.get("proc_macro") == True:
            name = kwargs["name"]
            if kwargs.get("crate", None) == None and kwargs.get("crate_dynamic", None) == None:
                kwargs["crate"] = name.replace("-", "_")

            rust_proc_macro_alias(
                name = name,
                actual_exec = ":_" + name,
                actual_plugin = ":_" + name,
                visibility = kwargs.pop("visibility", []),
            )
            kwargs["name"] = "_" + name

        rust_library(**kwargs)

    return wrapper

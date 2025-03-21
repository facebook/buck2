# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# Implementation of the Rust build rules.

load(
    "@prelude//:artifact_tset.bzl",
    "ArtifactTSet",
    "make_artifact_tset",
)
load(
    "@prelude//cxx:cxx.bzl",
    "get_auto_link_group_specs",
)
load("@prelude//cxx:cxx_context.bzl", "get_cxx_toolchain_info")
load(
    "@prelude//cxx:cxx_library_utility.bzl",
    "cxx_is_gnu",
)
load("@prelude//cxx:cxx_toolchain_types.bzl", "PicBehavior")
load(
    "@prelude//cxx:link_groups.bzl",
    "BuildLinkGroupsContext",
    "LinkGroupLinkInfo",  # @unused Used as a type
    "collect_linkables",
    "create_link_groups",
    "get_filtered_labels_to_links_map",
    "get_filtered_links",
    "get_filtered_targets",
    "get_link_group",
    "get_link_group_info",
    "get_link_group_preferred_linkage",
    "get_public_link_group_nodes",
)
load(
    "@prelude//cxx:link_groups_types.bzl",
    "LinkGroupInfo",  # @unused Used as a type
)
load(
    "@prelude//cxx:linker.bzl",
    "get_default_shared_library_name",
    "get_shared_library_name_for_param",
)
load(
    "@prelude//linking:link_groups.bzl",
    "LinkGroupLib",  # @unused Used as a type
    "LinkGroupLibInfo",  # @unused Used as a type
)
load(
    "@prelude//linking:link_info.bzl",
    "LibOutputStyle",
    "LinkInfo",
    "LinkStrategy",
    "MergedLinkInfo",
    "get_link_args_for_strategy",
    "unpack_external_debug_info",
)
load(
    "@prelude//linking:linkable_graph.bzl",
    "LinkableGraph",
    "create_linkable_graph",
    "reduce_linkable_graph",
)
load(
    "@prelude//linking:shared_libraries.bzl",
    "SharedLibraryInfo",
)
load(
    "@prelude//linking:types.bzl",
    "Linkage",  # @unused Used as a type
)
load(
    "@prelude//utils:type_defs.bzl",
    "is_dict",
    "is_string",
)
load(
    ":build_params.bzl",
    "MetadataKind",  # @unused Used as a type
)
load(
    ":context.bzl",
    "CrateName",  # @unused Used as a type
    "DepCollectionContext",  # @unused Used as a type
)
load(":rust_toolchain.bzl", "PanicRuntime", "RustToolchainInfo")

# Link strategy for targets which do not set an explicit `link_style` attribute.
#
# These values are also used as the defaults for check/clippy subtargets on
# libraries, and are the only way in which metadata-fast output can be built.
#
# Internally at Meta, these are a good choice for a default because they allow
# sharing work between check builds and dev mode builds, which have shared link
# strategy, and so consume their dependencies as `static_pic`.
DEFAULT_STATIC_LINK_STRATEGY = LinkStrategy("static_pic")
DEFAULT_STATIC_LIB_OUTPUT_STYLE = LibOutputStyle("pic_archive")

RustProcMacroPlugin = plugins.kind()

# This provider is used for proc macros in those places where `RustLinkInfo` would typically be used
# for libraries. It represents a proc macro in the dependency graph, and contains as a field the
# `target_label` of that proc macro. The actual providers will always be accessed later through
# `ctx.plugins`
RustProcMacroMarker = provider(fields = {
    "label": typing.Any,
})

# Information which is keyed on link_style
RustLinkStrategyInfo = record(
    # Path to the rlib, rmeta, dylib, etc.
    outputs = field(dict[MetadataKind, Artifact]),
    # Transitive dependencies which are relevant to the consumer. For crate types which do not
    # propagate their deps (specifically proc macros), this set is empty
    # This does not include the proc macros, which are passed separately in `RustLinkInfo`
    transitive_deps = field(dict[MetadataKind, dict[Artifact, CrateName]]),
    transitive_proc_macro_deps = field(dict[RustProcMacroMarker, ()]),

    # Path to PDB file with Windows debug data.
    pdb = field(Artifact | None),
    # Debug info which is referenced -- but not included -- by the linkable rlib.
    external_debug_info = field(ArtifactTSet),
)

# Output of a Rust compilation
RustLinkInfo = provider(
    # @unsorted-dict-items
    fields = {
        # crate - crate name
        "crate": CrateName,
        # strategies - information about each LinkStrategy as RustLinkStrategyInfo
        "strategies": dict[LinkStrategy, RustLinkStrategyInfo],
        # Rust interacts with the native link graph in a non-standard way.
        #
        # The first difference is in the re-export behavior of Rust compared to C++. The native link
        # providers make an assumption that if one node in the link graph references a symbol in
        # another node in the link graph, there is also a corresponding edge in the link graph.
        # Specifically, the first node must declare a direct dependency on the second, a transitive
        # dependency is not enough. For C++, this just means that each library depends in the link
        # graph on its direct deps and their exported deps.
        #
        # For Rust, the situation is different. Because of re-exports and generics causing delayed
        # codegen, the generated object files for a Rust library can generate symbol references to
        # any of the library's transitive Rust dependencies, as well as to the immediate C++
        # dependencies of those libraries. So to account for that, each Rust library reports direct
        # dependencies on all of those libraries in the link graph. The `merged_link_infos` and
        # `linkable_graphs` lists are the providers from all of those libraries.
        #
        # The second difference is unique to the case where `advanced_unstable_linking` is not set
        # on the toolchain. Imagine we have a Rust library `:B` with its only one dependency `:A`,
        # another Rust library. The Rust rules give Rust -> Rust dependencies special treatment in
        # the non-`advanced_unstable_linking` case. As a result, the `MergedLinkInfo` provided from
        # `:B` is not a "superset" of the `MergedLinkInfo` provided from `:A` (concrete differences
        # discussed below).
        #
        # This distinction is implemented by effectively having each Rust library provide two sets
        # of link providers. The first is the link providers used across Rust -> Rust dependency
        # edges - this is what the fields below are. The second set is the one that is used by C++
        # and other non-Rust dependents, and is returned from the rule like normal. The second set
        # is a superset of the first, that is it includes anything that the first link providers
        # added.
        #
        # The concrete difference is that the Rust `MergedLinkInfo` provided by `:A` is only the
        # result of merging the `MergedLinkInfo`s from `:A`'s deps, and does not contain anything
        # about `:A`. Instead, when `:B` produces the native `MergedLinkInfo`, it will add a single
        # static library that bundles all transitive Rust deps, including `:A` (and similarly for
        # the DSO case).
        #
        # With `advanced_unstable_linkin`, Rust libraries essentially behave just like C++
        # libraries in the link graph, with the handling of transitive dependencies being the only
        # difference.
        "merged_link_infos": dict[ConfiguredTargetLabel, MergedLinkInfo],
        "linkable_graphs": list[LinkableGraph],
        "shared_libs": SharedLibraryInfo,
        # LinkGroupLibInfo intentionally omitted because the Rust -> Rust version
        # never needs to be different from the Rust -> native version
        #
        # Rust currently treats all native dependencies as being exported, in
        # the sense of C++ `exported_deps`. However, they are not only exported
        # from the Rust library that directly depends on them, they are also
        # exported through any further chains of Rust libraries. This list
        # tracks those dependencies
        #
        # FIXME(JakobDegen): We should not default to treating all native deps
        # as exported.
        "exported_link_deps": list[Dependency],
    },
)

def _adjust_link_strategy_for_rust_dependencies(toolchain_info: RustToolchainInfo, dep_link_strategy: LinkStrategy) -> LinkStrategy:
    if dep_link_strategy == LinkStrategy("shared") and not toolchain_info.advanced_unstable_linking:
        return DEFAULT_STATIC_LINK_STRATEGY
    else:
        return dep_link_strategy

def strategy_info(toolchain_info: RustToolchainInfo, info: RustLinkInfo, dep_link_strategy: LinkStrategy) -> RustLinkStrategyInfo:
    rust_dep_link_strategy = _adjust_link_strategy_for_rust_dependencies(toolchain_info, dep_link_strategy)

    return info.strategies[rust_dep_link_strategy]

# Any dependency of a Rust crate
RustOrNativeDependency = record(
    # The actual dependency
    dep = field(Dependency),
    # The local name, if any (for `named_deps`)
    name = field(None | str | ResolvedStringWithMacros),
    # Any flags for the dependency (`flagged_deps`), which are passed on to rustc.
    flags = field(list[str]),
)

RustDependency = record(
    info = field(RustLinkInfo),
    label = field(ConfiguredProvidersLabel),
    dep = field(Dependency),
    name = field(None | str | ResolvedStringWithMacros),
    flags = field(list[str]),
    proc_macro_marker = field([None, RustProcMacroMarker]),
)

# Information about cxx link groups that rust depends on
RustCxxLinkGroupInfo = record(
    # cxx link infos to link against
    filtered_links = field(list[LinkInfo]),
    # symbol files args to ensure we export the required symbols
    symbol_files_info = field(LinkInfo),
    # targets to link against
    filtered_targets = field(list[TargetLabel]),
    # information about the link groups
    link_group_info = field([LinkGroupInfo, None]),
    # shared libraries created from link groups
    link_group_libs = field(dict[str, [LinkGroupLib, None]]),
    # mapping from target labels to the corresponding link group link_info
    labels_to_links_map = field(dict[Label, LinkGroupLinkInfo]),
    # Target to link group name where it was actually linked into
    targets_consumed_by_link_groups = field(dict[Label, str]),
    # preferred linkage mode for link group libraries
    link_group_preferred_linkage = field(dict[Label, Linkage]),
)

def enable_link_groups(ctx: AnalysisContext):
    if not cxx_is_gnu(ctx):
        # check minimum requirements
        return False
    return ctx.attrs.auto_link_groups and ctx.attrs.link_group_map

# Returns all first-order dependencies.
def _do_resolve_deps(
        deps: list[Dependency],
        named_deps: dict[str, Dependency] | list[(ResolvedStringWithMacros, Dependency)],
        flagged_deps: list[(Dependency, list[str])] = []) -> list[RustOrNativeDependency]:
    named_deps_items = named_deps.items() if is_dict(named_deps) else named_deps

    return [
        RustOrNativeDependency(name = name, dep = dep, flags = flags)
        for name, dep, flags in [(None, dep, []) for dep in deps] +
                                [(name, dep, []) for name, dep in named_deps_items] +
                                [(None, dep, flags) for dep, flags in flagged_deps]
    ]

def gather_explicit_sysroot_deps(dep_ctx: DepCollectionContext) -> list[RustOrNativeDependency]:
    explicit_sysroot_deps = dep_ctx.explicit_sysroot_deps
    if not explicit_sysroot_deps:
        return []

    out = []
    if explicit_sysroot_deps.core:
        out.append(RustOrNativeDependency(
            dep = explicit_sysroot_deps.core,
            name = None,
            flags = ["noprelude", "nounused"],
        ))
    if explicit_sysroot_deps.std:
        out.append(RustOrNativeDependency(
            dep = explicit_sysroot_deps.std,
            name = None,
            # "force" is used here in order to link std internals (like alloc hooks) even if the rest
            # of std is otherwise unused (e.g. we are building a no_std crate that needs to link with
            # other std-enabled crates as a standalone dylib).
            flags = ["noprelude", "nounused", "force"],
        ))
    if explicit_sysroot_deps.proc_macro:
        out.append(RustOrNativeDependency(
            dep = explicit_sysroot_deps.proc_macro,
            name = None,
            flags = ["noprelude", "nounused"],
        ))

    # When advanced_unstable_linking is on, we only add the dep that matches the
    # panic runtime. Without advanced_unstable_linking, we just let rustc deal
    # with it
    if explicit_sysroot_deps.panic_unwind:
        if not dep_ctx.advanced_unstable_linking or dep_ctx.panic_runtime == PanicRuntime("unwind"):
            out.append(RustOrNativeDependency(
                dep = explicit_sysroot_deps.panic_unwind,
                name = None,
                flags = ["noprelude", "nounused"],
            ))
    if explicit_sysroot_deps.panic_abort:
        if not dep_ctx.advanced_unstable_linking or dep_ctx.panic_runtime == PanicRuntime("abort"):
            out.append(RustOrNativeDependency(
                dep = explicit_sysroot_deps.panic_abort,
                name = None,
                flags = ["noprelude", "nounused"],
            ))
    for d in explicit_sysroot_deps.others:
        out.append(RustOrNativeDependency(
            dep = d,
            name = None,
            flags = ["noprelude", "nounused"],
        ))
    return out

def resolve_deps(
        ctx: AnalysisContext,
        dep_ctx: DepCollectionContext) -> list[RustOrNativeDependency]:
    dependencies = _do_resolve_deps(
        deps = ctx.attrs.deps,
        named_deps = ctx.attrs.named_deps,
        flagged_deps = ctx.attrs.flagged_deps,
    )

    if dep_ctx.include_doc_deps:
        dependencies.extend(_do_resolve_deps(
            deps = getattr(ctx.attrs, "doc_deps", []),
            named_deps = getattr(ctx.attrs, "doc_named_deps", {}),
        ))

    return dependencies + gather_explicit_sysroot_deps(dep_ctx)

def resolve_rust_deps_inner(
        ctx: AnalysisContext,
        all_deps: list[RustOrNativeDependency]) -> list[RustDependency]:
    rust_deps = []
    available_proc_macros = get_available_proc_macros(ctx)
    for dep in all_deps:
        proc_macro_marker = dep.dep.get(RustProcMacroMarker)
        if proc_macro_marker != None:
            # Confusingly, this is not `proc_macro_marker.label`, since that has type
            # `target_label`, but this wants a `label`
            label = available_proc_macros[proc_macro_marker.label].label
            info = available_proc_macros[proc_macro_marker.label][RustLinkInfo]
        else:
            label = dep.dep.label
            info = dep.dep.get(RustLinkInfo)
            if info == None:
                continue

        rust_deps.append(RustDependency(
            info = info,
            label = label,
            dep = dep.dep,
            name = dep.name,
            flags = dep.flags,
            proc_macro_marker = proc_macro_marker,
        ))
    return rust_deps

def resolve_rust_deps(
        ctx: AnalysisContext,
        dep_ctx: DepCollectionContext) -> list[RustDependency]:
    all_deps = resolve_deps(ctx, dep_ctx)
    return resolve_rust_deps_inner(ctx, all_deps)

def get_available_proc_macros(ctx: AnalysisContext) -> dict[TargetLabel, Dependency]:
    return {x.label.raw_target(): x for x in ctx.plugins[RustProcMacroPlugin]}

# Returns native link dependencies.
def _native_link_dependencies(
        ctx: AnalysisContext,
        dep_ctx: DepCollectionContext) -> list[Dependency]:
    """
    Return all first-order native linkable dependencies of all transitive Rust
    libraries.

    This emulates v1's graph walk, where it traverses through Rust libraries
    looking for non-Rust native link infos (and terminating the search there).
    """
    first_order_deps = [dep.dep for dep in resolve_deps(ctx, dep_ctx)]

    return [
        d
        for d in first_order_deps
        if RustLinkInfo not in d and MergedLinkInfo in d
    ]

# Returns the rust link infos for non-proc macro deps.
#
# This is intended to be used to access the Rust -> Rust link providers
def _rust_non_proc_macro_link_infos(
        ctx: AnalysisContext,
        dep_ctx: DepCollectionContext) -> list[RustLinkInfo]:
    return [d.info for d in resolve_rust_deps(ctx, dep_ctx) if d.proc_macro_marker == None]

def inherited_exported_link_deps(ctx: AnalysisContext, dep_ctx: DepCollectionContext) -> list[Dependency]:
    deps = {}
    for dep in _native_link_dependencies(ctx, dep_ctx):
        deps[dep.label] = dep
    for dep in resolve_rust_deps(ctx, dep_ctx):
        if dep.proc_macro_marker != None:
            continue

        for dep in dep.info.exported_link_deps:
            deps[dep.label] = dep

    return deps.values()

def inherited_rust_cxx_link_group_info(
        ctx: AnalysisContext,
        dep_ctx: DepCollectionContext,
        link_strategy: [LinkStrategy, None] = None) -> RustCxxLinkGroupInfo:
    link_graphs = inherited_linkable_graphs(ctx, dep_ctx)

    # Assume a rust executable wants to use link groups if a link group map
    # is present
    link_group = get_link_group(ctx)
    link_group_info = get_link_group_info(ctx, link_graphs)
    link_groups = link_group_info.groups
    link_group_mappings = link_group_info.mappings
    link_group_preferred_linkage = get_link_group_preferred_linkage(link_groups.values())

    auto_link_group_specs = get_auto_link_group_specs(ctx, link_group_info)
    linkable_graph = create_linkable_graph(
        ctx,
        deps = link_graphs,
    )
    reduced_linkable_graph = reduce_linkable_graph(linkable_graph)
    linkable_graph_node_map = reduced_linkable_graph.nodes

    executable_deps = []
    for g in link_graphs:
        if g.label in linkable_graph_node_map:
            executable_deps.append(g.label)
        else:
            # handle labels that are mutated by version alias
            executable_deps.append(g.nodes.value.label)

    public_link_group_nodes = get_public_link_group_nodes(
        linkable_graph_node_map,
        link_group_mappings,
        executable_deps,
        link_group,
    )

    linked_link_groups = create_link_groups(
        ctx = ctx,
        link_groups = link_groups,
        link_strategy = link_strategy,
        linkable_graph = reduced_linkable_graph,
        link_group_mappings = link_group_mappings,
        link_group_preferred_linkage = link_group_preferred_linkage,
        executable_deps = executable_deps,
        linker_flags = [],
        link_group_specs = auto_link_group_specs,
        other_roots = [],
        prefer_stripped_objects = False,  # Does Rust ever use stripped objects?
        anonymous = ctx.attrs.anonymous_link_groups,
        public_nodes = public_link_group_nodes,
    )

    auto_link_groups = {}
    link_group_libs = {}

    for name, linked_link_group in linked_link_groups.libs.items():
        auto_link_groups[name] = linked_link_group.artifact
        if linked_link_group.library != None:
            link_group_libs[name] = linked_link_group.library

    roots = set(executable_deps)
    pic_behavior = PicBehavior("always_enabled") if link_strategy == LinkStrategy("static_pic") else PicBehavior("supported")
    is_executable_link = True
    exec_linkables = collect_linkables(
        reduced_linkable_graph,
        is_executable_link,
        link_strategy,
        link_group_preferred_linkage,
        pic_behavior,
        roots,
    )
    build_context = BuildLinkGroupsContext(
        public_nodes = public_link_group_nodes,
        linkable_graph = reduced_linkable_graph,
        link_groups = link_groups,
        link_group_mappings = link_group_mappings,
        link_group_preferred_linkage = link_group_preferred_linkage,
        link_strategy = link_strategy,
        pic_behavior = pic_behavior,
        link_group_libs = {
            name: (lib.label, lib.shared_link_infos)
            for name, lib in link_group_libs.items()
        },
        prefer_stripped = False,
        prefer_optimized = False,
    )
    labels_to_links = get_filtered_labels_to_links_map(
        link_group = link_group,
        linkables = exec_linkables,
        is_executable_link = is_executable_link,
        build_context = build_context,
        force_static_follows_dependents = True,
    )

    return RustCxxLinkGroupInfo(
        filtered_links = get_filtered_links(labels_to_links.map),
        symbol_files_info = LinkInfo(
            pre_flags = linked_link_groups.symbol_ldflags,
        ),
        filtered_targets = get_filtered_targets(labels_to_links.map),
        link_group_info = link_group_info,
        link_group_libs = link_group_libs,
        labels_to_links_map = labels_to_links.map,
        targets_consumed_by_link_groups = linked_link_groups.targets_consumed_by_link_groups,
        link_group_preferred_linkage = link_group_preferred_linkage,
    )

def inherited_merged_link_infos(
        ctx: AnalysisContext,
        dep_ctx: DepCollectionContext) -> dict[ConfiguredTargetLabel, MergedLinkInfo]:
    infos = {}
    for d in _native_link_dependencies(ctx, dep_ctx):
        g = d.get(MergedLinkInfo)
        if g:
            infos[d.label.configured_target()] = g
    for info in _rust_non_proc_macro_link_infos(ctx, dep_ctx):
        infos.update(info.merged_link_infos)
    return infos

def inherited_shared_libs(
        ctx: AnalysisContext,
        dep_ctx: DepCollectionContext) -> list[SharedLibraryInfo]:
    infos = []
    infos.extend([d[SharedLibraryInfo] for d in _native_link_dependencies(ctx, dep_ctx)])
    infos.extend([d.shared_libs for d in _rust_non_proc_macro_link_infos(ctx, dep_ctx)])
    return infos

def inherited_linkable_graphs(ctx: AnalysisContext, dep_ctx: DepCollectionContext) -> list[LinkableGraph]:
    deps = {}
    for d in _native_link_dependencies(ctx, dep_ctx):
        g = d.get(LinkableGraph)
        if g:
            deps[g.label] = g
    for info in _rust_non_proc_macro_link_infos(ctx, dep_ctx):
        for g in info.linkable_graphs:
            deps[g.label] = g
    return deps.values()

def inherited_link_group_lib_infos(ctx: AnalysisContext, dep_ctx: DepCollectionContext) -> list[LinkGroupLibInfo]:
    # There are no special Rust -> Rust versions of this provider
    deps = {}
    for d in resolve_deps(ctx, dep_ctx):
        i = d.dep.get(LinkGroupLibInfo)
        if i:
            deps[d.dep.label] = i
    return deps.values()

def inherited_rust_external_debug_info(
        ctx: AnalysisContext,
        dep_ctx: DepCollectionContext,
        link_strategy: LinkStrategy) -> list[ArtifactTSet]:
    toolchain_info = ctx.attrs._rust_toolchain[RustToolchainInfo]
    return [strategy_info(toolchain_info, d.info, link_strategy).external_debug_info for d in resolve_rust_deps(ctx, dep_ctx)]

def inherited_external_debug_info(
        ctx: AnalysisContext,
        dep_ctx: DepCollectionContext,
        dwo_output_directory: Artifact | None,
        dep_link_strategy: LinkStrategy) -> ArtifactTSet:
    inherited_debug_infos = []
    inherited_link_infos = []
    toolchain_info = ctx.attrs._rust_toolchain[RustToolchainInfo]

    for d in resolve_deps(ctx, dep_ctx):
        if RustLinkInfo in d.dep:
            inherited_debug_infos.append(strategy_info(toolchain_info, d.dep[RustLinkInfo], dep_link_strategy).external_debug_info)
            inherited_link_infos.extend(d.dep[RustLinkInfo].merged_link_infos.values())
        elif MergedLinkInfo in d.dep:
            inherited_link_infos.append(d.dep[MergedLinkInfo])

    link_args = get_link_args_for_strategy(ctx, inherited_link_infos, dep_link_strategy)
    inherited_debug_infos.append(unpack_external_debug_info(ctx.actions, link_args))

    return make_artifact_tset(
        actions = ctx.actions,
        label = ctx.label,
        artifacts = filter(None, [dwo_output_directory]),
        children = inherited_debug_infos,
    )

def normalize_crate(label: str | ResolvedStringWithMacros) -> str | ResolvedStringWithMacros:
    return label.replace("-", "_") if is_string(label) else label

def attr_simple_crate_for_filenames(ctx: AnalysisContext) -> str:
    """
    A "good enough" identifier to use in filenames. Buck wants to have filenames
    of artifacts figured out before we begin building them. Normally we want a
    crate foo to produce artifact libfoo.rlib; but if crate_dynamic is being
    used, the true crate name is not known until later. In this situation we use
    the rule's name in place of the true crate name in filenames.

    # produces libordinary.rlib
    rust_library(
        name = "ordinary",
        crate = "ordinary",
    )

    # produces libthrift_generated.rlib
    rust_library(
        name = "thrift-generated",
        crate_dynamic = ":get-namespace-from-thrift-file",
    )
    """
    return normalize_crate(ctx.attrs.crate or ctx.label.name)

def attr_crate(ctx: AnalysisContext) -> CrateName:
    """
    The true user-facing name of the crate, which may only be known at build
    time, not during analysis.
    """
    dynamic = getattr(ctx.attrs, "crate_dynamic", None)
    if dynamic:
        dynamic = dynamic.get(DefaultInfo).default_outputs[0]
    return CrateName(
        simple = normalize_crate(ctx.attrs.crate or ctx.label.name),
        dynamic = dynamic,
    )

def attr_soname(ctx: AnalysisContext) -> str:
    """
    Get the shared library name to set for the given rust library.
    """
    linker_info = get_cxx_toolchain_info(ctx).linker_info
    if ctx.attrs.soname != None:
        return get_shared_library_name_for_param(linker_info, ctx.attrs.soname)
    return get_default_shared_library_name(linker_info, ctx.label)

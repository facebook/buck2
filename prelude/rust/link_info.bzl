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
load(
    "@prelude//cxx:cxx_library_utility.bzl",
    "cxx_is_gnu",
)
load("@prelude//cxx:cxx_toolchain_types.bzl", "PicBehavior")
load(
    "@prelude//cxx:link_groups.bzl",
    "LinkGroupInfo",  # @unused Used as a type
    "LinkGroupLinkInfo",  # @unused Used as a type
    "create_link_groups",
    "find_relevant_roots",
    "get_filtered_labels_to_links_map",
    "get_filtered_links",
    "get_filtered_targets",
    "get_link_group",
    "get_link_group_info",
    "get_link_group_preferred_linkage",
)
load(
    "@prelude//linking:link_groups.bzl",
    "LinkGroupLib",  # @unused Used as a type
)
load(
    "@prelude//linking:link_info.bzl",
    "LinkInfo",
    "LinkStyle",
    "Linkage",  # @unused Used as a type
    "MergedLinkInfo",
    "get_link_args",
    "merge_link_infos",
    "unpack_external_debug_info",
)
load(
    "@prelude//linking:linkable_graph.bzl",
    "LinkableGraph",
    "create_linkable_graph",
    "get_linkable_graph_node_map_func",
)
load(
    "@prelude//linking:linkables.bzl",
    "linkables",
)
load(
    "@prelude//linking:shared_libraries.bzl",
    "SharedLibraryInfo",
)
load(
    "@prelude//utils:utils.bzl",
    "filter_and_map_idx",
)

# Link style for targets which do not set an explicit `link_style` attribute.
DEFAULT_STATIC_LINK_STYLE = LinkStyle("static_pic")

# Override dylib crates to static_pic, so that Rust code is always
# statically linked.
# In v1 we always linked Rust deps statically, even for "shared" link style
# That shouldn't be necessary, but fully shared needs some more debugging,
# so default to v1 behaviour. (Should be controlled with the `rust.force_rlib` option)
FORCE_RLIB = True

# Output of a Rust compilation
RustLinkInfo = provider(fields = [
    # crate - crate name
    "crate",
    # styles - information about each LinkStyle as RustLinkStyleInfo
    # {LinkStyle: RustLinkStyleInfo}
    "styles",
    # Propagate non-rust native linkable dependencies through rust libraries.
    "non_rust_exported_link_deps",
    # Propagate non-rust native linkable info through rust libraries.
    "non_rust_link_info",
    # Propagate non-rust shared libraries through rust libraries.
    "non_rust_shared_libs",
])

CrateName = record(
    simple = field(str),
    dynamic = field([Artifact, None]),
)

# Information which is keyed on link_style
RustLinkStyleInfo = record(
    # Path to library or binary
    rlib = field(Artifact),
    # Transitive dependencies which are relevant to the consumer. For crate types which do not
    # propagate their deps (specifically proc macros), this set is empty
    transitive_deps = field(dict[Artifact, CrateName.type]),

    # Path for library metadata (used for check or pipelining)
    rmeta = field(Artifact),
    # Transitive rmeta deps. This is the same dict as `transitive_deps`, except that it has the
    # rmeta and not the rlib artifact
    transitive_rmeta_deps = field(dict[Artifact, CrateName.type]),

    # Path to PDB file with Windows debug data.
    pdb = field([Artifact, None]),
    # Debug info which is referenced -- but not included -- by the linkable rlib.
    external_debug_info = field(ArtifactTSet.type),
)

def _adjust_link_style_for_rust_dependencies(dep_link_style: LinkStyle.type) -> LinkStyle.type:
    if FORCE_RLIB and dep_link_style == LinkStyle("shared"):
        return DEFAULT_STATIC_LINK_STYLE
    else:
        return dep_link_style

def style_info(info: RustLinkInfo.type, dep_link_style: LinkStyle.type) -> RustLinkStyleInfo.type:
    rust_dep_link_style = _adjust_link_style_for_rust_dependencies(dep_link_style)
    return info.styles[rust_dep_link_style]

# A Rust dependency
RustDependency = record(
    # The actual dependency
    dep = field(Dependency),
    # The local name, if any (for `named_deps`)
    name = field([None, str]),
    # Any flags for the dependency (`flagged_deps`), which are passed on to rustc.
    flags = field(list[str]),
)

# Information about cxx link groups that rust depends on
RustCxxLinkGroupInfo = record(
    # cxx link infos to link against
    filtered_links = field(list[LinkInfo.type]),
    # symbol files args to ensure we export the required symbols
    symbol_files_info = field(LinkInfo.type),
    # targets to link against
    filtered_targets = field(list["target_label"]),
    # information about the link groups
    link_group_info = field([LinkGroupInfo.type, None]),
    # shared libraries created from link groups
    link_group_libs = field(dict[str, [LinkGroupLib.type, None]]),
    # mapping from target labels to the corresponding link group link_info
    labels_to_links_map = field(dict[Label, LinkGroupLinkInfo.type]),
    # prefrred linkage mode for link group libraries
    link_group_preferred_linkage = field(dict[Label, Linkage.type]),
)

def enable_link_groups(
        ctx: AnalysisContext,
        link_style: [LinkStyle.type, None],
        specified_link_style: LinkStyle.type,
        is_binary: bool):
    if not (cxx_is_gnu(ctx) and is_binary):
        # check minium requirements
        return False
    if link_style == LinkStyle("shared") or link_style != specified_link_style:
        # check whether we should run link groups analysis for the given link style
        return False

    # check whether link groups is enabled
    return ctx.attrs.auto_link_groups and ctx.attrs.link_group_map

# Returns all first-order dependencies.
def _do_resolve_deps(
        deps: list[Dependency],
        named_deps: dict[str, Dependency],
        flagged_deps: list[(Dependency, list[str])] = []) -> list[RustDependency.type]:
    return [
        RustDependency(name = name, dep = dep, flags = flags)
        for name, dep, flags in [(None, dep, []) for dep in deps] +
                                [(name, dep, []) for name, dep in named_deps.items()] +
                                [(None, dep, flags) for dep, flags in flagged_deps]
    ]

def resolve_deps(
        ctx: AnalysisContext,
        include_doc_deps: bool = False) -> list[RustDependency.type]:
    # The `getattr`s are needed for when we're operating on
    # `prebuilt_rust_library` rules, which don't have those attrs.
    dependencies = _do_resolve_deps(
        deps = ctx.attrs.deps,
        named_deps = getattr(ctx.attrs, "named_deps", {}),
        flagged_deps = getattr(ctx.attrs, "flagged_deps", []),
    )

    if include_doc_deps:
        dependencies.extend(_do_resolve_deps(
            deps = ctx.attrs.doc_deps,
            named_deps = getattr(ctx.attrs, "doc_named_deps", {}),
        ))

    return dependencies

def _non_rust_linkable_graph(
        ctx: AnalysisContext,
        deps: list[Dependency]) -> LinkableGraph.type:
    linkable_graph = create_linkable_graph(
        ctx,
        children = filter(None, (
            [d.linkable_graph for d in linkables(deps)]
        )),
    )
    return linkable_graph

# Returns native link dependencies.
def _non_rust_link_deps(
        ctx: AnalysisContext,
        include_doc_deps: bool = False) -> list[Dependency]:
    """
    Return all first-order native linkable dependencies of all transitive Rust
    libraries.

    This emulates v1's graph walk, where it traverses through Rust libraries
    looking for non-Rust native link infos (and terminating the search there).
    """
    first_order_deps = [dep.dep for dep in resolve_deps(ctx, include_doc_deps)]
    return [
        d
        for d in first_order_deps
        if RustLinkInfo not in d and MergedLinkInfo in d
    ]

# Returns native link dependencies.
def _non_rust_link_infos(
        ctx: AnalysisContext,
        include_doc_deps: bool = False,
        link_style: [LinkStyle.type, None] = None) -> list[MergedLinkInfo.type]:
    """
    Return all first-order native link infos of all transitive Rust libraries.

    This emulates v1's graph walk, where it traverses through Rust libraries
    looking for non-Rust native link infos (and terminating the search there).
    MergedLinkInfo is a mapping from link style to all the transitive deps
    rolled up in a tset.
    """
    link_deps = _non_rust_link_deps(ctx, include_doc_deps)
    return [d[MergedLinkInfo] for d in link_deps]

# Returns native link dependencies.
def _non_rust_shared_lib_infos(
        ctx: AnalysisContext,
        include_doc_deps: bool = False) -> list[SharedLibraryInfo.type]:
    """
    Return all transitive shared libraries for non-Rust native linkabes.

    This emulates v1's graph walk, where it traverses through -- and ignores --
    Rust libraries to collect all transitive shared libraries.
    """
    first_order_deps = [dep.dep for dep in resolve_deps(ctx, include_doc_deps)]
    return [
        d[SharedLibraryInfo]
        for d in first_order_deps
        if RustLinkInfo not in d and SharedLibraryInfo in d
    ]

# Returns native link dependencies.
def _rust_link_infos(
        ctx: AnalysisContext,
        include_doc_deps: bool = False) -> list[RustLinkInfo.type]:
    first_order_deps = resolve_deps(ctx, include_doc_deps)
    return filter(None, [d.dep.get(RustLinkInfo) for d in first_order_deps])

def normalize_crate(label: str) -> str:
    return label.replace("-", "_")

def inherited_non_rust_exported_link_deps(ctx: AnalysisContext) -> list[Dependency]:
    deps = {}
    for dep in _non_rust_link_deps(ctx):
        deps[dep.label] = dep
    for info in _rust_link_infos(ctx):
        for dep in info.non_rust_exported_link_deps:
            deps[dep.label] = dep
    return deps.values()

def inherited_non_rust_link_group_info(
        ctx: AnalysisContext,
        include_doc_deps: bool = False,
        link_style: [LinkStyle.type, None] = None) -> RustCxxLinkGroupInfo:
    link_deps = _non_rust_link_deps(ctx, include_doc_deps) + inherited_non_rust_exported_link_deps(ctx)

    # Assume a rust executable wants to use link groups if a link group map
    # is present
    link_group = get_link_group(ctx)
    link_group_info = get_link_group_info(ctx, filter_and_map_idx(LinkableGraph, link_deps))
    link_groups = link_group_info.groups
    link_group_mappings = link_group_info.mappings
    link_group_preferred_linkage = get_link_group_preferred_linkage(link_groups.values())

    auto_link_group_specs = get_auto_link_group_specs(ctx, link_group_info)
    linkable_graph = _non_rust_linkable_graph(
        ctx,
        link_deps,
    )
    linkable_graph_node_map = get_linkable_graph_node_map_func(linkable_graph)()

    linked_link_groups = create_link_groups(
        ctx = ctx,
        link_groups = link_groups,
        link_group_mappings = link_group_mappings,
        link_group_preferred_linkage = link_group_preferred_linkage,
        executable_deps = [],  # TODO: do we need this?
        linker_flags = [],
        link_group_specs = auto_link_group_specs,
        root_link_group = link_group,
        linkable_graph_node_map = linkable_graph_node_map,
        other_roots = [],
        prefer_stripped_objects = False,  # Does Rust ever use stripped objects?
        anonymous = ctx.attrs.anonymous_link_groups,
    )

    auto_link_groups = {}
    link_group_libs = {}

    for name, linked_link_group in linked_link_groups.libs.items():
        auto_link_groups[name] = linked_link_group.artifact
        if linked_link_group.library != None:
            link_group_libs[name] = linked_link_group.library

    # Some third-party dependencies are stored in the linkable_graph_node_map as
    # third-party-buck, but the non_rust_link_deps contain the unresolved third-party
    # path. Filter out the unresolved paths from the roots to avoid traversal error.
    filtered_roots = []
    roots = ([d.label for d in link_deps] +
             find_relevant_roots(
                 link_group = link_group,
                 linkable_graph_node_map = linkable_graph_node_map,
                 link_group_mappings = link_group_mappings,
                 roots = [],
             ))
    for root in roots:
        if root in linkable_graph_node_map:
            filtered_roots.append(root)

    labels_to_links_map = get_filtered_labels_to_links_map(
        linkable_graph_node_map,
        link_group,
        link_groups,
        link_group_mappings,
        link_group_preferred_linkage,
        pic_behavior = PicBehavior("always_enabled") if link_style == LinkStyle("static_pic") else PicBehavior("supported"),
        link_group_libs = {
            name: (lib.label, lib.shared_link_infos)
            for name, lib in link_group_libs.items()
        },
        link_style = link_style,
        roots = filtered_roots,
        is_executable_link = True,
        prefer_stripped = False,
        force_static_follows_dependents = True,
    )

    # TODO(@christylee): Check that this works with unittests
    return RustCxxLinkGroupInfo(
        filtered_links = get_filtered_links(labels_to_links_map),
        symbol_files_info = LinkInfo(
            pre_flags = linked_link_groups.symbol_ldflags,
        ),
        filtered_targets = get_filtered_targets(labels_to_links_map),
        link_group_info = link_group_info,
        link_group_libs = link_group_libs,
        labels_to_links_map = labels_to_links_map,
        link_group_preferred_linkage = link_group_preferred_linkage,
    )

def inherited_non_rust_link_info(
        ctx: AnalysisContext,
        include_doc_deps: bool = False,
        link_style: [LinkStyle.type, None] = None) -> MergedLinkInfo.type:
    infos = []
    infos.extend(_non_rust_link_infos(ctx, include_doc_deps, link_style))
    infos.extend([d.non_rust_link_info for d in _rust_link_infos(ctx, include_doc_deps)])
    return merge_link_infos(ctx, infos)

def inherited_non_rust_shared_libs(
        ctx: AnalysisContext,
        include_doc_deps: bool = False) -> list[SharedLibraryInfo.type]:
    infos = []
    infos.extend(_non_rust_shared_lib_infos(ctx, include_doc_deps))
    infos.extend([d.non_rust_shared_libs for d in _rust_link_infos(ctx, include_doc_deps)])
    return infos

def inherited_external_debug_info(
        ctx: AnalysisContext,
        dwo_output_directory: [Artifact, None],
        dep_link_style: LinkStyle.type) -> ArtifactTSet:
    rust_dep_link_style = _adjust_link_style_for_rust_dependencies(dep_link_style)
    non_rust_dep_link_style = dep_link_style

    inherited_debug_infos = []
    inherited_non_rust_link_infos = []

    for d in resolve_deps(ctx):
        if RustLinkInfo in d.dep:
            inherited_debug_infos.append(d.dep[RustLinkInfo].styles[rust_dep_link_style].external_debug_info)
            inherited_non_rust_link_infos.append(d.dep[RustLinkInfo].non_rust_link_info)
        elif MergedLinkInfo in d.dep:
            inherited_non_rust_link_infos.append(d.dep[MergedLinkInfo])

    non_rust_merged_link_info = merge_link_infos(ctx, inherited_non_rust_link_infos)
    link_args = get_link_args(non_rust_merged_link_info, non_rust_dep_link_style)
    inherited_debug_infos.append(unpack_external_debug_info(ctx.actions, link_args))

    return make_artifact_tset(
        actions = ctx.actions,
        label = ctx.label,
        artifacts = filter(None, [dwo_output_directory]),
        children = inherited_debug_infos,
    )

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

def attr_crate(ctx: AnalysisContext) -> CrateName.type:
    """
    The true user-facing name of the crate, which may only be known at build
    time, not during analysis.
    """
    dynamic = getattr(ctx.attrs, "crate_dynamic", None)
    if dynamic:
        dynamic = dynamic.get(DefaultInfo).default_outputs[0]
    return CrateName(
        simple = ctx.attrs.crate or normalize_crate(ctx.label.name),
        dynamic = dynamic,
    )

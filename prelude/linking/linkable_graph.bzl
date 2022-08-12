load("@fbcode//buck2/prelude/python:python.bzl", "PythonLibraryInfo")
load("@fbcode//buck2/prelude/utils:utils.bzl", "expect")
load(
    ":link_info.bzl",
    "LinkInfo",  # @unused Used as a type
    "LinkInfos",
    "LinkStyle",
    "Linkage",
    "LinkedObject",
    "MergedLinkInfo",
    "get_actual_link_style",
    "get_link_styles_for_linkage",
    _get_link_info = "get_link_info",
)

###############################################################################
# Linkable Graph collects information on a node in the target graph that
# contains linkable output. This graph information may then be provided to any
# consumers of this target.
###############################################################################

LinkableNode = record(
    # Attribute labels on the target.
    labels = field([str.type], []),
    # Prefered linkage for this target.
    preferred_linkage = field(Linkage.type),
    # Link infos for all supported link styles.
    link_infos = field({LinkStyle.type: LinkInfos.type}),
    # Shared libraries provided by this target.  Used if this target is
    # excluded.
    shared_libs = field({str.type: LinkedObject.type}),
    # Linkable deps of this target.
    deps = field(["label"], []),
    # Exported linkable deps of this target.
    #
    # We distinguish between deps and exported deps so that when creating shared
    # libraries in a large graph we only need to link each library against its
    # deps and their (transitive) exported deps. This helps keep link lines smaller
    # and produces more efficient libs (for example, DT_NEEDED stays a manageable size).
    exported_deps = field(["label"], []),
)

# The LinkableGraph for a target holds all the transitive nodes, roots, and exclusions
# from all of its dependencies.
#
# TODO(cjhopman): Rather than flattening this at each node, we should build up an actual
# graph structure.
LinkableGraph = provider(fields = [
    # Target identifier of the graph.
    "label",  # "label"
    # Link information for all nodes that may be included in an accumalated
    # link action.
    # Note: certain roots (e.g. C++ Python extensions) will not be
    # represented here, as they are not `LinkInfo` providers.
    "nodes",  # {"label", LinkableNode.type}
    # All potential root notes for an omnibus link (e.g. C++ libraries,
    # C++ Python extensions).
    "roots",  # {"label", NativeLinkTarget.type}
    # All nodes that should be excluded from libomnibus.
    "excluded",  # {"label": None}
])

# The de-facto pattern for Rust Python extensions in fbsource is as follows:
#
# - Define a Rust library, make it preferred_linkage = "static"
# - Wrap it into an intermediary C++ library that declares PyInit_... as an
#   undefined symbol to force the Rust library to get linked into that C++
#   library. This has the same preferred_linkage.
# - Make a C++ Python extension that links to the intermediary C++ library.
#
# Note: I suspect the intermediary C++ library is here because the C++ Python
# extension requires a C++ library and doesn't allow a Python library as a dep,
# though I didn't check that.
#
# When we do Omnibus linking, the Python extension is an Omnibus root, and when
# it's created, the preferred_linkage clauses result in the Rust library being
# statically linked into the Omnibus root.
#
# Unfortunately, in v2, this breaks, because we don't support preferred_linkage
# for Rust libraries at this time: D33052438. As a result, the Rust library gets
# included in the Omnibus body and we attempt to link it libomnibus.so. At that
# point unfortunately all the code gets dropped because nothing is referencing
# it.
#
# More details here:
#
# https://fb.workplace.com/groups/347532827186692/permalink/3097248903896494/
#
# In an ideal world, we'd actually respect the preferred_linkage, but as noted in
# D33052438 we can't quite to this. In the above post, @jcg noted that this will
# work if we allow the Rust library to become an Omnibus root.
#
# This is what this ForceConsiderationOfOmnibusRoots does: if we have a Rust
# library and we ignore its `preferred_linkage = "static"` config, we add a
# `ForceConsiderationOfOmnibusRoots` provider as a flag, and then in
# `cxx_python_extension`, if we see that, then we make the dependency an
# Omnibus root.
#
# Reference:
#
# https://www.internalfb.com/code/fbsource/tools/build_defs/fbcode_macros/build_defs/rust_python_extension.bzl
ForceConsiderationOfOmnibusRoots = provider(fields = [])

def create_merged_linkable_graph(
        label: "label",
        deps: ["dependency"] = []) -> LinkableGraph.type:
    """
    Merge and propagate graph node from dependencies.
    """

    nodes = {}
    roots = {}
    excluded = {}

    # Merge in propagated info from first-order deps.
    for dep in deps:
        if dep[LinkableGraph] != None:
            sub_graph = dep[LinkableGraph]
            nodes.update(sub_graph.nodes)
            roots.update(sub_graph.roots)
            excluded.update(sub_graph.excluded)

    return LinkableGraph(label = label, nodes = nodes, roots = roots, excluded = excluded)

def add_linkable_node(
        graph: LinkableGraph.type,
        ctx: "context",
        preferred_linkage: Linkage.type,
        link_infos: {LinkStyle.type: LinkInfos.type} = {},
        shared_libs: {str.type: LinkedObject.type} = {},
        excluded = False,
        deps: ["dependency"] = [],
        exported_deps: ["dependency"] = []):
    """
    Native library rules should call this to add themselves as potential
    targets for a group link.
    """

    for link_style in get_link_styles_for_linkage(preferred_linkage):
        expect(
            link_style in link_infos,
            "must have {} link info".format(link_style),
        )

    graph.nodes[ctx.label] = LinkableNode(
        labels = ctx.attrs.labels,
        preferred_linkage = preferred_linkage,
        link_infos = link_infos,
        shared_libs = shared_libs,
        # Only include dependencies that are linkable.
        deps = linkable_deps(deps),
        exported_deps = linkable_deps(exported_deps),
    )

    if excluded:
        graph.excluded[ctx.label] = None

def linkable_deps(deps: ["dependency"]) -> ["label"]:
    labels = []

    for dep in deps:
        dep_info = linkable_graph(dep)
        if dep_info != None:
            labels.append(dep_info.label)

    return labels

def linkable_graph(dep: "dependency") -> [LinkableGraph.type, None]:
    """
    Helper to extract `LinkableGraph` from a dependency which also
    provides `MergedLinkInfo`.
    """

    # We only care about "linkable" deps.
    if dep[PythonLibraryInfo] != None or dep[MergedLinkInfo] == None:
        return None

    expect(
        dep[LinkableGraph] != None,
        "{} provides `MergedLinkInfo`".format(dep.label) +
        " but doesn't also provide `LinkableGraph`",
    )

    return dep[LinkableGraph]

def get_link_info(
        node: "LinkableNode",
        link_style: LinkStyle.type,
        prefer_stripped: bool.type = False) -> LinkInfo.type:
    return _get_link_info(
        node.link_infos[link_style],
        prefer_stripped = prefer_stripped,
    )

def get_deps_for_link(
        node: "LinkableNode",
        link_style: LinkStyle.type) -> ["label"]:
    """
    Return deps to follow when linking against this node with the given link
    style.
    """

    # Avoid making a copy of the list until we know have to modify it.
    deps = node.exported_deps

    # If we're linking statically, include non-exported deps.
    actual = get_actual_link_style(link_style, node.preferred_linkage)
    if actual != LinkStyle("shared") and node.deps:
        # Important that we don't mutate deps, but create a new list
        deps = deps + node.deps

    return deps

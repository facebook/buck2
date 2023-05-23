# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(
    "@prelude//cxx:groups.bzl",
    "compute_mappings",
    "parse_groups_definitions",
)
load(
    "@prelude//cxx:link_groups.bzl",
    "LinkGroupInfo",
)
load(
    "@prelude//linking:link_groups.bzl",
    "LinkGroupLibInfo",
)
load(
    "@prelude//linking:link_info.bzl",
    "MergedLinkInfo",
)
load(
    "@prelude//linking:linkable_graph.bzl",
    "LinkableGraph",
    "create_linkable_graph",
    "get_linkable_graph_node_map_func",
)
load(
    "@prelude//linking:shared_libraries.bzl",
    "SharedLibraryInfo",
)
load("@prelude//user:rule_spec.bzl", "RuleRegistrationSpec")
load("@prelude//decls/common.bzl", "Linkage", "Traversal")

def _v1_attrs(
        optional_root: bool.type = False,
        # Whether we should parse `root` fields as a `dependency`, instead of a `label`.
        root_is_dep: bool.type = True):
    if root_is_dep:
        attrs_root = attrs.dep(providers = [
            LinkGroupLibInfo,
            LinkableGraph,
            MergedLinkInfo,
            SharedLibraryInfo,
        ])
    else:
        attrs_root = attrs.label()

    if optional_root:
        attrs_root = attrs.option(attrs_root)

    return attrs.list(
        attrs.tuple(
            attrs.string(),
            attrs.list(
                attrs.tuple(
                    attrs_root,
                    attrs.enum(Traversal),
                    attrs.option(attrs.string()),
                    attrs.option(attrs.enum(Linkage)),
                ),
            ),
            attrs.option(
                attrs.dict(key = attrs.string(), value = attrs.any(), sorted = False),
            ),
        ),
    )

def link_group_map_attr():
    v2_attrs = attrs.dep(providers = [LinkGroupInfo])
    return attrs.option(
        attrs.one_of(
            v2_attrs,
            _v1_attrs(
                optional_root = True,
                # Inlined `link_group_map` will parse roots as `label`s, to avoid
                # bloating deps w/ unrelated mappings (e.g. it's common to use
                # a default mapping for all rules, which would otherwise add
                # unrelated deps to them).
                root_is_dep = False,
            ),
        ),
        default = None,
    )

def _impl(ctx: "context") -> ["provider"]:
    link_groups = parse_groups_definitions(ctx.attrs.map, lambda root: root.label)

    # Extract graphs from the roots via the raw attrs, as `parse_groups_definitions`
    # parses them as labels.
    linkable_graph = create_linkable_graph(
        ctx,
        children = [
            mapping[0][LinkableGraph]
            for entry in ctx.attrs.map
            for mapping in entry[1]
        ],
    )
    linkable_graph_node_map = get_linkable_graph_node_map_func(linkable_graph)()
    mappings = compute_mappings(groups = link_groups, graph_map = linkable_graph_node_map)
    return [
        DefaultInfo(),
        LinkGroupInfo(
            groups = link_groups,
            groups_hash = hash(str(link_groups)),
            mappings = mappings,
            # The consumer of this info may not have deps that cover that labels
            # referenced in our roots, so propagate graphs for them.
            # NOTE(agallagher): We do this to maintain existing behavior here
            # but it's not clear if it's actually desirable behavior.
            implicit_graphs = [linkable_graph],
        ),
    ]

registration_spec = RuleRegistrationSpec(
    name = "link_group_map",
    impl = _impl,
    attrs = {
        "map": _v1_attrs(),
    },
)

# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(
    "@prelude//cxx:groups.bzl",
    "make_info_subtarget_providers",
    "parse_groups_definitions",
)
load("@prelude//cxx:groups_types.bzl", "Traversal")
load(
    "@prelude//cxx:link_groups.bzl",
    "build_link_group_info",
)
load(
    "@prelude//cxx:link_groups_types.bzl",
    "LinkGroupInfo",  # @unused Used as a type
    "link_group_inlined_map_attr",
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
)
load(
    "@prelude//linking:shared_libraries.bzl",
    "SharedLibraryInfo",
)
load("@prelude//linking:types.bzl", "Linkage")
load("@prelude//user:rule_spec.bzl", "RuleRegistrationSpec")

def _v1_attrs(attrs_root):
    return attrs.list(
        attrs.tuple(
            # name
            attrs.string(),
            # list of mappings
            attrs.list(
                # a single mapping
                attrs.tuple(
                    # root node
                    attrs_root,
                    # traversal
                    attrs.enum(Traversal.values()),
                    # filters, either `None`, a single filter, or a list of filters
                    # (which must all match).
                    attrs.option(attrs.one_of(attrs.list(attrs.string()), attrs.string())),
                    # linkage
                    attrs.option(attrs.enum(Linkage.values())),
                ),
            ),
            # attributes
            attrs.option(
                attrs.dict(key = attrs.string(), value = attrs.any(), sorted = False),
            ),
        ),
    )

LINK_GROUP_MAP_ATTR = attrs.option(
    attrs.one_of(
        attrs.dep(providers = [LinkGroupInfo]),
        _v1_attrs(
            # Inlined `link_group_map` will parse roots as `label`s, to avoid
            # bloating deps w/ unrelated mappings (e.g. it's common to use
            # a default mapping for all rules, which would otherwise add
            # unrelated deps to them).
            attrs_root = attrs.option(attrs.label()),
        ),
    ),
    default = None,
)

def _impl(ctx: AnalysisContext) -> list[Provider]:
    # Extract graphs from the roots via the raw attrs, as `parse_groups_definitions`
    # parses them as labels.
    linkable_graph = create_linkable_graph(
        ctx,
        deps = [
            mapping[0][LinkableGraph]
            for entry in ctx.attrs.map
            for mapping in entry[1]
        ],
    )
    link_groups = parse_groups_definitions(ctx.attrs.map, lambda root: root.label)
    link_group_info = build_link_group_info(linkable_graph, link_groups)
    return [
        DefaultInfo(sub_targets = {
            "info": make_info_subtarget_providers(ctx, link_group_info.groups.values(), link_group_info.mappings),
        }),
        link_group_info,
    ]

registration_spec = RuleRegistrationSpec(
    name = "link_group_map",
    impl = _impl,
    attrs = {
        "map": link_group_inlined_map_attr(
            root_attr = attrs.dep(
                providers = [
                    LinkGroupLibInfo,
                    LinkableGraph,
                    MergedLinkInfo,
                    SharedLibraryInfo,
                ],
            ),
        ),
    },
)

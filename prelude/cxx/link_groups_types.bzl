# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(
    "@prelude//linking:link_info.bzl",
    "LibOutputStyle",
)
load("@prelude//linking:types.bzl", "Linkage")
load(":groups_types.bzl", "Group", "GroupFilterInfo", "Traversal")

# These are targets or link groups that will be added to .linker.argsfile
# Targets will be expanded to .o files, link groups will be added to NEEDS
LinkGroupsDebugLinkableEntry = record(
    name = field(TargetLabel | Label | str),
    output_style = field(LibOutputStyle),
)

# This is info about single output unit. It is either a final binary or
# one of link groups.
LinkGroupsDebugLinkableItem = record(
    ordered_linkables = field(list[LinkGroupsDebugLinkableEntry]),
)

LinkGroupsDebugLinkInfo = record(
    binary = field(LinkGroupsDebugLinkableItem),
    libs = field(dict[str, LinkGroupsDebugLinkableItem]),
)

LinkGroupInfo = provider(
    fields = {
        # Additional graphs needed to cover labels referenced by the groups above.
        # This is useful in cases where the consumer of this provider won't already
        # have deps covering these.
        # NOTE(agallagher): We do this to maintain existing behavior w/ the
        # standalone `link_group_map()` rule, but it's not clear if it's actually
        # desirable behavior.
        "graph": provider_field(typing.Any, default = None),  # LinkableGraph
        "groups": provider_field(dict[str, Group]),
        "groups_hash": provider_field(int),
        "mappings": provider_field(dict[Label, str]),
    },
)

LinkGroupDefinitions = provider(
    fields = {
        "definitions": provider_field(list[Group]),
    },
)

_FILTER_ATTR = attrs.one_of(attrs.dep(providers = [GroupFilterInfo]), attrs.string())

def link_group_inlined_map_attr(root_attr):
    return attrs.list(
        attrs.tuple(
            # name
            attrs.string(),
            # list of mappings
            attrs.list(
                # a single mapping
                attrs.tuple(
                    # root node
                    attrs.one_of(root_attr, attrs.list(root_attr)),
                    # traversal
                    attrs.enum(Traversal.values()),
                    # filters, either `None`, a single filter, or a list of filters
                    # (which must all match).
                    attrs.option(attrs.one_of(attrs.list(_FILTER_ATTR), _FILTER_ATTR)),
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
        attrs.dep(),  # LinkGroupInfo or LinkGroupInfoGenerator
        link_group_inlined_map_attr(
            # Inlined `link_group_map` will parse roots as `label`s, to avoid
            # bloating deps w/ unrelated mappings (e.g. it's common to use
            # a default mapping for all rules, which would otherwise add
            # unrelated deps to them).
            root_attr = attrs.option(attrs.label()),
        ),
    ),
    default = None,
)

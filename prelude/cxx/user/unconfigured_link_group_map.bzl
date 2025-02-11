# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(
    "@prelude//cxx:link_groups.bzl",
    "get_link_group_info_from_linkable_graph",
)
load(
    "@prelude//cxx:link_groups_types.bzl",
    "LinkGroupInfoGenerator",
    "link_group_inlined_map_attr",
)
load("@prelude//user:rule_spec.bzl", "RuleRegistrationSpec")

def _impl(ctx: AnalysisContext) -> list[Provider]:
    return [
        DefaultInfo(),
        LinkGroupInfoGenerator(
            generator = get_link_group_info_from_linkable_graph,
            link_group_map = ctx.attrs.link_group_map,
        ),
    ]

registration_spec = RuleRegistrationSpec(
    name = "unconfigured_link_group_map",
    impl = _impl,
    attrs = {
        "link_group_map": attrs.option(link_group_inlined_map_attr(
            root_attr = attrs.option(attrs.label()),
        )),
    },
)

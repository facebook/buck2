# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(
    "@prelude//cxx:groups.bzl",
    "parse_groups_definitions",
)
load(
    "@prelude//cxx:link_groups_types.bzl",
    "LinkGroupDefinitions",
    "link_group_inlined_map_attr",
)
load("@prelude//user:rule_spec.bzl", "RuleRegistrationSpec")

def _impl(ctx: AnalysisContext) -> list[Provider]:
    if ctx.attrs.link_group_map:
        definitions = parse_groups_definitions(ctx.attrs.link_group_map)
    else:
        definitions = []
    return [
        DefaultInfo(),
        LinkGroupDefinitions(
            definitions = definitions,
        ),
    ]

registration_spec = RuleRegistrationSpec(
    name = "unconfigured_link_group_map",
    impl = _impl,
    attrs = {
        "labels": attrs.list(attrs.string(), default = []),
        "link_group_map": attrs.option(link_group_inlined_map_attr(
            root_attr = attrs.option(attrs.label()),
        )),
    },
)

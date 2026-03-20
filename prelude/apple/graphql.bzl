# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load(
    "@prelude//cxx:cxx_library_utility.bzl",
    "cxx_attr_deps",
    "cxx_attr_exported_deps",
)

GraphQLInfo = provider(
    fields = {
        "config_name": provider_field(str),
        "enforce_colocation": provider_field(bool),
        "header_path_prefix": provider_field(str | None),
        "is_subconfig": provider_field(bool),
        "is_test_target": provider_field(bool),
        "srcs": provider_field(list[Artifact]),
        "target": provider_field(Label),
    },
)

def graphql_providers(ctx: AnalysisContext) -> list[Provider]:
    all_deps = cxx_attr_deps(ctx) + cxx_attr_exported_deps(ctx)

    for dep in all_deps:
        # We only want to propagate the provider from our direct
        # graphql_codegen() dep, not from any of the other library deps.
        if GraphQLInfo in dep and dep[GraphQLInfo].target == ctx.label:
            return [dep[GraphQLInfo]]

    return []

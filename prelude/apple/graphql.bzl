# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load(
    "@prelude//:artifact_tset.bzl",
    "ArtifactTSet",
    "make_artifact_tset",
)
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

GraphQLCodegenInput = provider(
    fields = {
        "artifacts": provider_field(ArtifactTSet),
    },
)

def _graphql_info_provider(label: Label, deps: list[Dependency]) -> list[Provider]:
    return [dep[GraphQLInfo] for dep in deps if GraphQLInfo in dep and dep[GraphQLInfo].target == label]

def graphql_codegen_sets(deps: list[Dependency]) -> list[ArtifactTSet]:
    return [dep[GraphQLCodegenInput].artifacts for dep in deps if GraphQLCodegenInput in dep]

def graphql_providers(ctx: AnalysisContext) -> list[Provider]:
    deps = cxx_attr_deps(ctx)
    providers = _graphql_info_provider(ctx.label, deps)
    if len(providers) > 1:
        fail("Multiple GraphQLInfo providers found in deps of {}".format(ctx.label.raw_target()))

    output_sets = graphql_codegen_sets(deps) + graphql_codegen_sets(cxx_attr_exported_deps(ctx))
    if output_sets:
        providers.append(
            GraphQLCodegenInput(
                artifacts = make_artifact_tset(actions = ctx.actions, children = output_sets),
            ),
        )

    return providers

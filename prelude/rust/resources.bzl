# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(
    "@prelude//:artifacts.bzl",
    "ArtifactOutputs",  # @unused Used as a type
    "single_artifact",
)
load("@prelude//:paths.bzl", "paths")
load("@prelude//utils:utils.bzl", "from_named_set")

def rust_attr_resources(ctx: AnalysisContext) -> dict[str, ArtifactOutputs]:
    """
    Return the resources provided by this rule, as a map of resource name to
    a tuple of the resource artifact and any "other" outputs exposed by it.
    """
    resources = {}

    for name, resource in from_named_set(ctx.attrs.resources).items():
        resources[paths.join(ctx.label.package, name)] = single_artifact(resource)

    return resources

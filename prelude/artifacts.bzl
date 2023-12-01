# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//:paths.bzl", "paths")
load("@prelude//dist:dist_info.bzl", "DistInfo")
load("@prelude//utils:arglike.bzl", "ArgLike")  # @unused Used as a type
load("@prelude//utils:expect.bzl", "expect")

# A group of artifacts.
ArtifactGroupInfo = provider(
    fields = {
        "artifacts": provider_field(typing.Any, default = None),  # ["artifact"]
    },
)

ArtifactOutputs = record(
    # Single output. This is the artifact whose path would go into the resources
    # JSON when this artifact is used as a resource.
    default_output = field(Artifact),

    # Other artifacts which need to be present in order to run the resource as
    # an executable. This includes shared library dependencies and resources.
    nondebug_runtime_files = field(list[ArgLike]),

    # Other outputs that would be materialized if this artifact is the output of
    # a build, or generally in any context where a user might run this artifact
    # in a debugger.
    #
    # This is a superset of nondebug_runtime_files and also includes external
    # debuginfo.
    other_outputs = field(list[ArgLike]),
)

def single_artifact(dep: Artifact | Dependency) -> ArtifactOutputs:
    if type(dep) == "artifact":
        return ArtifactOutputs(
            default_output = dep,
            nondebug_runtime_files = [],
            other_outputs = [],
        )

    if DefaultInfo in dep:
        info = dep[DefaultInfo]
        expect(
            len(info.default_outputs) == 1,
            "expected exactly one default output from {} ({})"
                .format(dep, info.default_outputs),
        )
        default_output = info.default_outputs[0]
        other_outputs = info.other_outputs

        dist_info = dep.get(DistInfo)
        nondebug_runtime_files = dist_info.nondebug_runtime_files if dist_info else other_outputs

        return ArtifactOutputs(
            default_output = default_output,
            nondebug_runtime_files = nondebug_runtime_files,
            other_outputs = other_outputs,
        )

    fail("unexpected dependency type: {}".format(type(dep)))

def unpack_artifacts(artifacts: list[Artifact | Dependency]) -> list[ArtifactOutputs]:
    """
    Unpack a heterogeneous list of Artifact and ArtifactGroupInfo into a list
    representing their outputs.
    """

    out = []

    for artifact in artifacts:
        if type(artifact) == "artifact":
            out.append(ArtifactOutputs(
                default_output = artifact,
                nondebug_runtime_files = [],
                other_outputs = [],
            ))
            continue

        if ArtifactGroupInfo in artifact:
            for artifact in artifact[ArtifactGroupInfo].artifacts:
                out.append(ArtifactOutputs(
                    default_output = artifact,
                    nondebug_runtime_files = [],
                    other_outputs = [],
                ))
            continue

        out.append(single_artifact(artifact))

    return out

def unpack_artifact_map(artifacts: dict[str, Artifact | Dependency]) -> dict[str, ArtifactOutputs]:
    """
    Unpack a heterogeneous dict of Artifact and ArtifactGroupInfo into a dict
    representing their outputs.
    """

    out = {}

    for name, artifact in artifacts.items():
        if type(artifact) == "artifact":
            out[name] = ArtifactOutputs(
                default_output = artifact,
                nondebug_runtime_files = [],
                other_outputs = [],
            )
            continue

        if ArtifactGroupInfo in artifact:
            for artifact in artifact[ArtifactGroupInfo].artifacts:
                out[paths.join(name, artifact.short_path)] = ArtifactOutputs(
                    default_output = artifact,
                    nondebug_runtime_files = [],
                    other_outputs = [],
                )
            continue

        out[name] = single_artifact(artifact)

    return out

# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//:paths.bzl", "paths")
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
    # an executable.
    other_outputs = field(list[ArgLike]),
)

def _from_default_info(dep: Dependency) -> ArtifactOutputs:
    info = dep[DefaultInfo]
    expect(
        len(info.default_outputs) == 1,
        "expected exactly one default output from {} ({})"
            .format(dep, info.default_outputs),
    )
    return ArtifactOutputs(
        default_output = info.default_outputs[0],
        other_outputs = info.other_outputs,
    )

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
                other_outputs = [],
            ))
            continue

        if ArtifactGroupInfo in artifact:
            for artifact in artifact[ArtifactGroupInfo].artifacts:
                out.append(ArtifactOutputs(
                    default_output = artifact,
                    other_outputs = [],
                ))
            continue

        if DefaultInfo in artifact:
            out.append(_from_default_info(artifact))
            continue

        fail("unexpected dependency type: {}".format(type(artifact)))

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
                other_outputs = [],
            )
            continue

        if ArtifactGroupInfo in artifact:
            for artifact in artifact[ArtifactGroupInfo].artifacts:
                out[paths.join(name, artifact.short_path)] = ArtifactOutputs(
                    default_output = artifact,
                    other_outputs = [],
                )
            continue

        if DefaultInfo in artifact:
            out[name] = _from_default_info(artifact)
            continue

        fail("unexpected dependency type: {}".format(type(artifact)))

    return out

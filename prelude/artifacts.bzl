# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(
    "@prelude//utils:utils.bzl",
    "expect",
)

# A group of artifacts.
ArtifactGroupInfo = provider(
    fields = [
        "artifacts",  # ["artifact"]
    ],
)

def unpack_artifacts(artifacts: [["artifact", "dependency"]]) -> ["artifact"]:
    """
    Unpack a list of `artifact` and `ArtifactGroupInfo` into a flattened list
    of `artifact`s
    """

    out = []

    for artifact in artifacts:
        if ArtifactGroupInfo in artifact:
            out.extend(artifact[ArtifactGroupInfo].artifacts)
        elif DefaultInfo in artifact:
            (artifact,) = artifact[DefaultInfo].default_outputs
            out.append(artifact)
        else:
            expect(type(artifact) == "artifact")
            out.append(artifact)

    return out

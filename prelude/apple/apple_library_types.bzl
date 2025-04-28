# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(
    "@prelude//:artifact_tset.bzl",
    "ArtifactTSet",
)

AppleLibraryInfo = provider(
    fields = {
        "labels": list[str],
        "public_framework_headers": ArtifactTSet,
        "swift_header": [Artifact, None],
        "target": Label,
    },
)

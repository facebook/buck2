# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(
    "@prelude//:artifact_tset.bzl",
    "ArtifactTSet",  # @unused Used as a type
)

RustcOutput = record(
    output = field(Artifact),
    stripped_output = field(Artifact),
    diag_txt = field(Artifact),
    diag_json = field(Artifact),
    # Only available on metadata-like emits
    clippy_txt = field(Artifact | None),
    clippy_json = field(Artifact | None),
    pdb = field([Artifact, None]),
    dwp_output = field([Artifact, None]),
    # Zero or more Split DWARF debug info files are emitted into this directory
    # with unpredictable filenames.
    dwo_output_directory = field([Artifact, None]),
    extra_external_debug_info = field(list[ArtifactTSet]),
)

def output_as_diag_subtargets(o: RustcOutput) -> dict[str, Artifact]:
    return {
        "check": o.output,
        "clippy.json": o.clippy_json,
        "clippy.txt": o.clippy_txt,
        "diag.json": o.diag_json,
        "diag.txt": o.diag_txt,
    }

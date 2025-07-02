# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load(
    "@prelude//:artifact_tset.bzl",
    "ArtifactTSet",  # @unused Used as a type
)

RustcOutput = record(
    output = field(Artifact),
    stripped_output = field(Artifact),
    diag_txt = field(Artifact),
    diag_json = field(Artifact),
    # Windows .lib artifact for linking against .dll
    import_library = field(Artifact | None),
    pdb = field(Artifact | None),
    dwp_output = field(Artifact | None),
    # Zero or more Split DWARF debug info files are emitted into this directory
    # with unpredictable filenames.
    dwo_output_directory = field(Artifact | None),
    extra_external_debug_info = field(list[ArtifactTSet]),
    # The output of the profiler. Set only if `rust_compile` was invoked with a
    # `profile_mode`
    profile_output = field(Artifact | None),
)

def output_as_diag_subtargets(o: RustcOutput, clippy: RustcOutput) -> dict[str, Artifact]:
    return {
        "check": o.output,
        "clippy.json": clippy.diag_json,
        "clippy.txt": clippy.diag_txt,
        "diag.json": o.diag_json,
        "diag.txt": o.diag_txt,
    }

# Access to additional outputs from Rust compilation.
#
# This provider is intended to be available from all rules that compile Rust
# code. As a result, it must be different from `RustLinkInfo`, since it should
# not exist on a prebuilt Rust library, but should exist on a binary.
RustcExtraOutputsInfo = provider(
    fields = {
        "clippy": RustcOutput,
        "clippy_incr": RustcOutput,
        "metadata": RustcOutput,
        "metadata_incr": RustcOutput,
    },
)

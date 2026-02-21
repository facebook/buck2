# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load(":link_info.bzl", "TransitiveDeps")

# Outputs that are generally associated with object code compilation, as opposed to linking
RustcCompileOutput = record(
    # A stripped version of the output, only available if the main output is not linked and
    # stripping makes sense for it
    stripped_output = Artifact | None,
    diag_txt = field(Artifact),
    diag_json = field(Artifact),
    # Zero or more Split DWARF debug info files are emitted into this directory
    # with unpredictable filenames.
    dwo_output_directory = field(Artifact | None),
    # The output of the profiler. Set only if `rust_compile` was invoked with a
    # `profile_mode`
    profile_output = field(Artifact | None),
    # LLVM remarks output. Set when profile_mode=ProfileMode("remarks").
    # These point to diag_txt/diag_json since remarks are part of the diagnostic stream.
    remarks_txt = field(Artifact | None),
    remarks_json = field(Artifact | None),
)

RustcLinkOutput = record(
    # Windows .lib artifact for linking against .dll
    import_library = field(Artifact | None),
    pdb = field(Artifact | None),
    dwp_output = field(Artifact | None),
)

RustcOutput = record(
    output = Artifact,
    singleton_tset = TransitiveDeps,
    compile_output = RustcCompileOutput,
    # As expected, only available when the combination of params actually
    # requires linking.
    link_output = RustcLinkOutput | None,
)

def output_as_diag_subtargets(o: RustcOutput, clippy: RustcOutput) -> dict[str, Artifact]:
    return {
        "check": o.output,
        "clippy.json": clippy.compile_output.diag_json,
        "clippy.txt": clippy.compile_output.diag_txt,
        "diag.json": o.compile_output.diag_json,
        "diag.txt": o.compile_output.diag_txt,
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
        "remarks": RustcOutput | None,
    },
)

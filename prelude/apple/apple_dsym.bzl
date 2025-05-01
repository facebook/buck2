# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//utils:arglike.bzl", "ArgLike")  # @unused Used as a type
load(":apple_toolchain_types.bzl", "AppleToolchainInfo")
load(":debug.bzl", "AppleSelectiveDebuggableMetadata")  # @unused Used as a type

DSYM_SUBTARGET = "dsym"
DSYM_INFO_SUBTARGET = "dsym-info"
EXTENDED_DSYM_INFO_SUBTARGET = "extended-dsym-info"
DWARF_AND_DSYM_SUBTARGET = "dwarf-and-dsym"

def get_apple_dsym(ctx: AnalysisContext, executable: Artifact, debug_info: list[ArgLike], action_identifier: str, output_path_override: [str, None] = None) -> Artifact:
    output_path = output_path_override or "{}.dSYM".format(executable.short_path)
    return get_apple_dsym_ext(ctx, executable, debug_info, action_identifier, output_path)

# TODO(T110672942): Things which are still unsupported:
# - oso_prefix
def get_apple_dsym_ext(ctx: AnalysisContext, executable: [ArgLike, Artifact], debug_info: list[ArgLike], action_identifier: str, output_path: str) -> Artifact:
    dsymutil = ctx.attrs._apple_toolchain[AppleToolchainInfo].dsymutil
    output = ctx.actions.declare_output(output_path, dir = True)
    cmd = cmd_args(
        [
            dsymutil,
            "--verify-dwarf={}".format(ctx.attrs._dsymutil_verify_dwarf),
            # Reproducers are not useful, we can reproduce from the action digest.
            "--reproducer=Off",
        ],
        # Mach-O executables don't contain DWARF data.
        # Instead, they contain paths to the object files which themselves contain DWARF data.
        # So, those object files are needed for dsymutil to be to create the dSYM bundle.
        hidden = debug_info,
    )
    if ctx.attrs.dsym_uses_parallel_linker:
        cmd.add("--linker=parallel")

    cmd.add(ctx.attrs._dsymutil_extra_flags)
    cmd.add(
        [
            "-o",
            output.as_output(),
            executable,
        ],
    )
    ctx.actions.run(cmd, category = "apple_dsym", identifier = action_identifier)
    return output

AppleDsymJsonInfo = record(
    # JSON object containing the list of dSYMs
    json_object = field(dict[str, typing.Any]),
    # A list of all artifacts referenced in `json_object`
    outputs = field(list[Artifact]),
)

def get_apple_dsym_info_json(
        binary_dsyms: list[Artifact],
        dep_dsyms: list[Artifact],
        metadata: list[AppleSelectiveDebuggableMetadata] | None = None) -> AppleDsymJsonInfo:
    dsym_info = {}

    if len(binary_dsyms) == 1:
        dsym_info["binary"] = binary_dsyms[0]
    else:
        fail("There can only be one binary dSYM")

    if dep_dsyms:
        # `dedupe` needed as it's possible for the same dSYM to bubble up
        # through multiple paths in a graph (e.g., including both a binary
        # + bundle in the `deps` field of a parent bundle).
        dsym_info["deps"] = dedupe(dep_dsyms)

    metadata_dsym_outputs = []
    if metadata != None:
        metadata_json_obj = {}
        for debuggable_metadata in metadata:
            metadata_json_obj[debuggable_metadata.dsym] = debuggable_metadata.metadata
            metadata_dsym_outputs += [debuggable_metadata.dsym, debuggable_metadata.metadata]
        dsym_info["selective_metadata"] = metadata_json_obj

    return AppleDsymJsonInfo(
        json_object = dsym_info,
        outputs = binary_dsyms + dep_dsyms + metadata_dsym_outputs,
    )

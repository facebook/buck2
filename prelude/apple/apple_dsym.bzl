# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//utils:arglike.bzl", "ArgLike")  # @unused Used as a type
load(":apple_toolchain_types.bzl", "AppleToolchainInfo")

DSYM_SUBTARGET = "dsym"
DSYM_INFO_SUBTARGET = "dsym-info"
DWARF_AND_DSYM_SUBTARGET = "dwarf-and-dsym"

def get_apple_dsym(ctx: AnalysisContext, executable: Artifact, debug_info: list[ArgLike], action_identifier: str, output_path_override: [str, None] = None) -> Artifact:
    output_path = output_path_override or "{}.dSYM".format(executable.short_path)
    return get_apple_dsym_ext(ctx, executable, debug_info, action_identifier, output_path)

# TODO(T110672942): Things which are still unsupported:
# - oso_prefix
# - dsym_verification
def get_apple_dsym_ext(ctx: AnalysisContext, executable: [ArgLike, Artifact], debug_info: list[ArgLike], action_identifier: str, output_path: str) -> Artifact:
    dsymutil = ctx.attrs._apple_toolchain[AppleToolchainInfo].dsymutil
    output = ctx.actions.declare_output(output_path, dir = True)

    cmd = cmd_args([dsymutil] + ctx.attrs._dsymutil_extra_flags + ["-o", output.as_output()])
    cmd.add(executable)

    # Mach-O executables don't contain DWARF data.
    # Instead, they contain paths to the object files which themselves contain DWARF data.
    #
    # So, those object files are needed for dsymutil to be to create the dSYM bundle.
    cmd.hidden(debug_info)
    ctx.actions.run(cmd, category = "apple_dsym", identifier = action_identifier)

    return output

def get_apple_dsym_info_json(binary_dsyms: list[Artifact], dep_dsyms: list[Artifact]) -> dict[str, typing.Any]:
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
    return dsym_info

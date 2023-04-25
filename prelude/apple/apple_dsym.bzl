# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//apple:apple_toolchain_types.bzl", "AppleToolchainInfo")

DSYM_SUBTARGET = "dsym"
DSYM_INFO_SUBTARGET = "dsym-info"
DWARF_AND_DSYM_SUBTARGET = "dwarf-and-dsym"
DEBUGINFO_SUBTARGET = "debuginfo"

AppleDebuggableInfo = provider(fields = [
    "dsyms",  # ["artifact"]
    "external_debug_info",  # "transitive_set"
])

AppleBundleDebuggableInfo = record(
    # Can be `None` for WatchKit stub
    binary_info = field([AppleDebuggableInfo.type, None]),
    dep_infos = field([AppleDebuggableInfo.type]),
    # Concat of `binary_info` and `dep_infos`
    all_infos = field([AppleDebuggableInfo.type]),
)

# TODO(T110672942): Things which are still unsupported:
# - pass in dsymutil_extra_flags
# - oso_prefix
# - dsym_verification
def get_apple_dsym(ctx: "context", executable: "artifact", external_debug_info: ["_arglike"], action_identifier: "string", output_path_override: ["string", None] = None) -> "artifact":
    dsymutil = ctx.attrs._apple_toolchain[AppleToolchainInfo].dsymutil
    output_path = output_path_override or "{}.dSYM".format(executable.short_path)
    output = ctx.actions.declare_output(output_path, dir = True)

    cmd = cmd_args([dsymutil, "-o", output.as_output(), executable])

    # Mach-O executables don't contain DWARF data.
    # Instead, they contain paths to the object files which themselves contain DWARF data.
    #
    # So, those object files are needed for dsymutil to be to create the dSYM bundle.
    cmd.hidden(external_debug_info)
    ctx.actions.run(cmd, category = "apple_dsym", identifier = action_identifier)

    return output

def get_apple_dsym_info(ctx: "context", binary_dsyms: ["artifact"], dep_dsyms: ["artifact"]) -> "artifact":
    dsym_info = {}

    # WatchOS stub does not have a dSYM, so it's possible that we get zero `binary_dsyms`
    if len(binary_dsyms) == 1:
        dsym_info["binary"] = binary_dsyms[0]
    elif len(binary_dsyms) > 1:
        fail("There cannot be more than one binary dSYM")

    if dep_dsyms:
        # `dedupe` needed as it's possible for the same dSYM to bubble up
        # through multiple paths in a graph (e.g., including both a binary
        # + bundle in the `deps` field of a parent bundle).
        dsym_info["deps"] = dedupe(dep_dsyms)

    return ctx.actions.write_json("dsym-info.json", dsym_info)

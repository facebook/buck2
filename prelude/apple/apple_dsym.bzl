load("@prelude//apple:apple_toolchain_types.bzl", "AppleToolchainInfo")

DSYM_SUBTARGET = "dsym"

AppleDebuggableInfo = provider(fields = [
    # ["artifact"]
    "dsyms",
])

# TODO(T110672942): Things which are still unsupported:
# - pass in dsymutil_extra_flags
# - oso_prefix
# - dsym_verification
def get_apple_dsym(ctx: "context", executable: "artifact", object_files: ["artifact"], action_identifier: "string") -> "artifact":
    dsymutil = ctx.attrs._apple_toolchain[AppleToolchainInfo].dsymutil
    output = ctx.actions.declare_output("{}.dSYM".format(executable.short_path))

    cmd = cmd_args([dsymutil, "-o", output.as_output(), executable])

    # Mach-O executables don't contain DWARF data.
    # Instead, they contain paths to the object files which themselves contain DWARF data.
    #
    # So, those object files are needed for dsymutil to be to create the dSYM bundle.
    cmd.hidden(object_files)
    ctx.actions.run(cmd, category = "apple_dsym", identifier = action_identifier)

    return output

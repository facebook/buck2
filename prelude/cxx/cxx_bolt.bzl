# BOLT (Binary Optimization Layout Tool) is a post link profile guided optimizer used for
# performance-critical services in fbcode: https://www.internalfb.com/intern/wiki/HHVM-BOLT/

load(":cxx_context.bzl", "get_cxx_toolchain_info")

def cxx_use_bolt(ctx: "context") -> bool.type:
    cxx_toolchain_info = get_cxx_toolchain_info(ctx)
    return cxx_toolchain_info.bolt_enabled and ctx.attr.bolt_profile != None

def bolt(ctx: "context", prebolt_output: "artifact", identifier: [str.type, None]) -> "artifact":
    postbolt_output = ctx.actions.declare_output(prebolt_output.short_path.removesuffix("-wrapper"))
    bolt_msdk = get_cxx_toolchain_info(ctx).binary_utilities_info.bolt_msdk

    if not bolt_msdk or not cxx_use_bolt(ctx):
        fail("Cannot use bolt if bolt_msdk is not available or bolt profile is not available")
    args = cmd_args()

    # bolt command format:
    # {llvm_bolt} {input_bin} -o $OUT -data={fdata} {args}
    args.add(
        cmd_args(bolt_msdk, format = "{}/bin/llvm-bolt"),
        prebolt_output,
        "-o",
        postbolt_output.as_output(),
        cmd_args(ctx.attr.bolt_profile, format = "-data={}"),
        ctx.attr.bolt_flags,
    )

    # TODO(T119572109): @christylee add gdb_index generation as a subtarget
    ctx.actions.run(
        args,
        category = "bolt",
        identifier = identifier,
        local_only = get_cxx_toolchain_info(ctx).linker_info.link_binaries_locally,
    )
    return postbolt_output

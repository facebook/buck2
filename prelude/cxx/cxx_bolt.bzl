# BOLT (Binary Optimization Layout Tool) is a post link profile guided optimizer used for
# performance-critical services in fbcode: https://www.internalfb.com/intern/wiki/HHVM-BOLT/

load("@fbcode//buck2/prelude:local_only.bzl", "link_cxx_binary_locally")
load(":cxx_context.bzl", "CxxContext")  # @unused Used as a type

def cxx_use_bolt(ctx: "context", cxx_context: CxxContext.type) -> bool.type:
    cxx_toolchain_info = cxx_context.cxx_toolchain_info
    return cxx_toolchain_info.bolt_enabled and ctx.attrs.bolt_profile != None

def bolt_gdb_index(ctx: "context", cxx_context: CxxContext.type, bolt_output: "artifact", identifier: [str.type, None]) -> "artifact":
    # Run gdb-indexer
    # gdb-indexer <input_binary> -o <output_binary>
    gdb_index_output_name = bolt_output.short_path.removesuffix("-pre_gdb_index") + "-gdb_index"
    gdb_index_output = cxx_context.actions.declare_output(gdb_index_output_name)
    gdb_index_args = cmd_args(
        ctx.attrs.bolt_gdb_index,
        bolt_output,
        "-o",
        gdb_index_output.as_output(),
    )
    cxx_context.actions.run(
        gdb_index_args,
        category = "gdb_index",
        identifier = identifier,
        local_only = link_cxx_binary_locally(ctx),
    )

    # Run objcopy
    # objcopy -R .gdb_index --add-section=.gdb_index=<gdb_index_binary> <input_binary> <output_binary>
    objcopy_output_name = gdb_index_output_name.removesuffix("-gdb_index")
    objcopy_output = cxx_context.actions.declare_output(objcopy_output_name)
    objcopy_args = cmd_args(
        cxx_context.cxx_toolchain_info.binary_utilities_info.objcopy,
        "-R",
        ".gdb_index",
        cmd_args(gdb_index_output, format = "--add-section=.gdb_index={}"),
        bolt_output,
        objcopy_output.as_output(),
    )
    cxx_context.actions.run(
        objcopy_args,
        category = "objcopy",
        identifier = identifier,
        local_only = link_cxx_binary_locally(ctx),
    )

    return objcopy_output

def bolt(ctx: "context", cxx_context: CxxContext.type, prebolt_output: "artifact", identifier: [str.type, None]) -> "artifact":
    output_name = prebolt_output.short_path.removesuffix("-wrapper") + ("-pre_gdb_index" if (ctx.attrs.bolt_gdb_index != None) else "")
    postbolt_output = cxx_context.actions.declare_output(output_name)
    bolt_msdk = cxx_context.cxx_toolchain_info.binary_utilities_info.bolt_msdk

    if not bolt_msdk or not cxx_use_bolt(ctx, cxx_context):
        fail("Cannot use bolt if bolt_msdk is not available or bolt profile is not available")
    args = cmd_args()

    # bolt command format:
    # {llvm_bolt} {input_bin} -o $OUT -data={fdata} {args}
    args.add(
        cmd_args(bolt_msdk, format = "{}/bin/llvm-bolt"),
        prebolt_output,
        "-o",
        postbolt_output.as_output(),
        cmd_args(ctx.attrs.bolt_profile, format = "-data={}"),
        ctx.attrs.bolt_flags,
    )

    cxx_context.actions.run(
        args,
        category = "bolt",
        identifier = identifier,
        local_only = cxx_context.cxx_toolchain_info.linker_info.link_binaries_locally,
    )

    if ctx.attrs.bolt_gdb_index != None:
        return bolt_gdb_index(ctx, cxx_context, postbolt_output, identifier)

    return postbolt_output

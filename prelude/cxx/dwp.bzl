load(":cxx_context.bzl", "get_cxx_toolchain_info")

def dwp_available(ctx: "context"):
    dwp = get_cxx_toolchain_info(ctx).binary_utilities_info.dwp
    return dwp != None

def run_dwp_action(
        ctx: "context",
        obj: "artifact",
        identifier: [str.type, None],
        referenced_objects: ["_arglike", ["artifact"]],
        dwp_output: "artifact"):
    args = cmd_args()
    dwp = get_cxx_toolchain_info(ctx).binary_utilities_info.dwp
    args.add("/bin/sh", "-c", '"$1" -o "$2" -e "$3" && touch "$2"', "")
    args.add(dwp, dwp_output.as_output(), obj)

    # All object/dwo files referenced in the library/executable are implicitly
    # processed by dwp.
    args.hidden(referenced_objects)

    ctx.actions.run(
        args,
        category = "dwp",
        identifier = identifier,
        # dwp produces ELF files on the same size scale as the corresponding @obj.
        # The files are a concatentation of input DWARF debug info.
        # Caching dwp has the same issues as caching binaries, so use the same local_only policy.
        local_only = get_cxx_toolchain_info(ctx).linker_info.link_binaries_locally,
    )

def dwp(
        ctx: "context",
        # Executable/library to extra dwo paths from.
        obj: "artifact",
        # An identifier that will uniquely name this link action in the context of a category. Useful for
        # differentiating multiple link actions in the same rule.
        identifier: [str.type, None],
        # All `.o`/`.dwo` paths referenced in `obj`.
        # TODO(T110378122): Ideally, referenced objects are a list of artifacts,
        # but currently we don't track them properly.  So, we just pass in the full
        # link line and extract all inputs from that, which is a bit of an
        # overspecification.
        referenced_objects: ["_arglike", ["artifact"]]) -> "artifact":
    # gdb/lldb expect to find a file named $file.dwp next to $file.
    output = ctx.actions.declare_output(obj.short_path + ".dwp")
    run_dwp_action(ctx, obj, identifier, referenced_objects, output)
    return output

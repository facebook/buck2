# Implementation of the `export_file` build rule.

def export_file_impl(ctx: "context") -> [DefaultInfo.type]:
    # mode is "copy" or "reference", defaulting to copy
    copy = ctx.attr.mode != "reference"

    if copy:
        dest = ctx.label.name if ctx.attr.out == None else ctx.attr.out
        output = ctx.actions.copy_file(dest, ctx.attr.src)
    elif ctx.attr.out != None:
        fail("export_file does not allow specifying `out` without also specifying `mode = 'copy'`")
    else:
        output = ctx.attr.src
    return [DefaultInfo(default_outputs = [output])]

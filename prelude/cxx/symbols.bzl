load(":cxx_context.bzl", "get_cxx_toolchain_info")

def extract_symbol_names(
        ctx: "context",
        name: str.type,
        objects: ["artifact"],
        category: str.type,
        identifier: [str.type, None] = None,
        undefined_only: bool.type = False,
        dynamic: bool.type = False,
        prefer_local: bool.type = False,
        local_only: bool.type = False,
        global_only: bool.type = False) -> "artifact":
    """
    Generate a file with a sorted list of symbol names extracted from the given
    native objects.
    """

    if not objects:
        fail("no objects provided")

    cxx_toolchain = get_cxx_toolchain_info(ctx)
    nm = cxx_toolchain.binary_utilities_info.nm
    output = ctx.actions.declare_output(name)

    # -A: Prepend all lines with the name of the input file to which it
    # corresponds.  Added only to make parsing the output a bit easier.
    # -P: Generate portable output format
    nm_flags = "-AP"
    if global_only:
        nm_flags += "g"
    if undefined_only:
        nm_flags += "u"

    # darwin objects don't have dynamic symbol tables.
    if dynamic and cxx_toolchain.linker_info.type != "darwin":
        nm_flags += "D"

    script = (
        "set -euo pipefail; " +
        '"$1" {} "${{@:2}}"'.format(nm_flags) +
        # Grab only the symbol name field.
        ' | cut -d" " -f2 ' +
        # Strip off ABI Version (@...) when using llvm-nm to keep compat with buck1
        " | cut -d@ -f1 " +
        # Sort and dedup symbols.  Use the `C` locale and do it in-memory to
        # make it significantly faster. CAUTION: if ten of these processes
        # run in parallel, they'll have cumulative allocations larger than RAM.
        " | LC_ALL=C sort -S 10% -u > {}"
    )

    ctx.actions.run(
        [
            "/bin/bash",
            "-c",
            cmd_args(output.as_output(), format = script),
            "",
            nm,
        ] +
        objects,
        category = category,
        identifier = identifier,
        prefer_local = prefer_local,
        local_only = local_only,
    )
    return output

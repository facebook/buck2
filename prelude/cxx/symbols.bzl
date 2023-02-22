# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//:paths.bzl", "paths")
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
    output = ctx.actions.declare_output(paths.join("__symbols__", name))

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
        weight_percentage = 15,  # 10% + a little padding
    )
    return output

def extract_undefined_syms(ctx: "context", output: "artifact", category_prefix: str.type, prefer_local: bool.type) -> "artifact":
    return extract_symbol_names(
        ctx,
        output.short_path + ".undefined_syms.txt",
        [output],
        dynamic = True,
        global_only = True,
        undefined_only = True,
        category = "{}_undefined_syms".format(category_prefix),
        identifier = output.short_path,
        prefer_local = prefer_local,
    )

def extract_global_syms(ctx: "context", output: "artifact", category_prefix: str.type, prefer_local: bool.type) -> "artifact":
    return extract_symbol_names(
        ctx,
        output.short_path + ".global_syms.txt",
        [output],
        dynamic = True,
        global_only = True,
        category = "{}_global_syms".format(category_prefix),
        identifier = output.short_path,
        prefer_local = prefer_local,
    )

def _create_symbols_file_from_script(
        actions: "actions",
        name: str.type,
        script: str.type,
        symbol_files: ["artifact"],
        category: str.type,
        prefer_local: bool.type,
        weight_percentage: int.type,
        identifier: [str.type, None] = None) -> "artifact":
    """
    Generate a symbols file from from the given objects and
    link args.
    """

    all_symbol_files = actions.write(name + ".symbols", symbol_files)
    all_symbol_files = cmd_args(all_symbol_files).hidden(symbol_files)
    output = actions.declare_output(name)
    cmd = [
        "/bin/bash",
        "-c",
        script,
        "",
        all_symbol_files,
        output.as_output(),
    ]
    actions.run(
        cmd,
        category = category,
        prefer_local = prefer_local,
        weight_percentage = weight_percentage,
        identifier = identifier,
    )
    return output

def get_undefined_symbols_args(
        ctx: "context",
        name: str.type,
        symbol_files: ["artifact"],
        category: [str.type, None] = None,
        identifier: [str.type, None] = None,
        prefer_local: bool.type = False) -> cmd_args.type:
    if get_cxx_toolchain_info(ctx).linker_info.type == "gnu":
        # linker script is only supported in gnu linkers
        linker_script = create_undefined_symbols_linker_script(
            ctx.actions,
            name,
            symbol_files,
            category,
            identifier,
            prefer_local,
        )
        return cmd_args(linker_script, format = "-Wl,--script={}")
    argsfile = create_undefined_symbols_argsfile(
        ctx.actions,
        name,
        symbol_files,
        category,
        identifier,
        prefer_local,
    )
    return cmd_args(argsfile, format = "@{}")

def create_undefined_symbols_argsfile(
        actions: "actions",
        name: str.type,
        symbol_files: ["artifact"],
        category: [str.type, None] = None,
        identifier: [str.type, None] = None,
        prefer_local: bool.type = False) -> "artifact":
    """
    Combine files with sorted lists of symbols names into an argsfile to pass
    to the linker to mark these symbols as undefined via `-u`.
    """
    return _create_symbols_file_from_script(
        actions = actions,
        name = name,
        script = """\
set -euo pipefail
tr '\\n' '\\0' < "$1" > "$2.files0.txt"
LC_ALL=C sort -S 10% -u -m --files0-from="$2.files0.txt" | sed "s/^/-u/" > "$2"
""",
        symbol_files = symbol_files,
        category = category,
        identifier = identifier,
        prefer_local = prefer_local,
        weight_percentage = 15,  # 10% + a little padding
    )

def create_undefined_symbols_linker_script(
        actions: "actions",
        name: str.type,
        symbol_files: ["artifact"],
        category: [str.type, None] = None,
        identifier: [str.type, None] = None,
        prefer_local: bool.type = False) -> "artifact":
    """
    Combine files with sorted lists of symbols names into a linker script
    to mark these symbols as undefined via EXTERN.
    """
    return _create_symbols_file_from_script(
        actions = actions,
        name = name,
        script = """\
set -euo pipefail;
echo "EXTERN(" > "$2";
tr '\\n' '\\0' < "$1" > "$2.files0.txt"
LC_ALL=C sort -S 10% -u -m --files0-from="$2.files0.txt" >> "$2";
echo ")" >> "$2";
""",
        symbol_files = symbol_files,
        category = category,
        identifier = identifier,
        prefer_local = prefer_local,
        weight_percentage = 15,  # 10% + a little padding
    )

def create_global_symbols_version_script(
        actions: "actions",
        name: str.type,
        symbol_files: ["artifact"],
        identifier: [str.type, None] = None,
        category: [str.type, None] = None,
        prefer_local: bool.type = False) -> "artifact":
    """
    Combine files with sorted lists of symbols names into an argsfile to pass
    to the linker to mark these symbols as undefined (e.g. `-m`).
    """
    return _create_symbols_file_from_script(
        actions = actions,
        name = name,
        script = """\
set -euo pipefail
echo "{" > "$2"
echo "  global:" >> "$2"
tr '\\n' '\\0' < "$1" > "$2.files0.txt"
LC_ALL=C sort -S 10% -u -m --files0-from="$2.files0.txt" | awk '{print "    \\""$1"\\";"}' >> "$2"
echo "  local: *;" >> "$2"
echo "};" >> "$2"
""",
        symbol_files = symbol_files,
        category = category,
        identifier = identifier,
        prefer_local = prefer_local,
        weight_percentage = 15,  # 10% + a little padding
    )

def create_dynamic_list_version_script(
        actions: "actions",
        name: str.type,
        symbol_files: ["artifact"],
        identifier: [str.type, None] = None,
        category: [str.type, None] = None,
        prefer_local: bool.type = False) -> "artifact":
    """
    Combine files with sorted lists of symbols names into a dynamic list version
    file that can be passed to the linked (e.g. via `--dynamic-list=<file>`).
    """
    return _create_symbols_file_from_script(
        actions = actions,
        name = name,
        script = """\
set -euo pipefail
echo "{" > "$2"
tr '\\n' '\\0' < "$1" > "$2.files0.txt"
LC_ALL=C sort -S 10% -u -m --files0-from="$2.files0.txt" | awk '{print "    \\""$1"\\";"}' >> "$2"
echo "};" >> "$2"
""",
        symbol_files = symbol_files,
        category = category,
        identifier = identifier,
        prefer_local = prefer_local,
        weight_percentage = 15,  # 10% + a little padding
    )

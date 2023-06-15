# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//haskell:haskell.bzl", "HaskellLibraryProvider")
load("@prelude//utils:utils.bzl", "flatten")

def _first_order_haskell_deps(ctx: "context") -> ["HaskellLibraryInfo"]:
    return dedupe(
        flatten(
            [
                dep[HaskellLibraryProvider].lib.values()
                for dep in ctx.attrs.deps
                if HaskellLibraryProvider in dep
            ],
        ),
    )

# Creates the start.ghci script used to load the packages during startup
def _write_start_ghci(ctx: "context", script_file: "artifact"):
    start_cmd = cmd_args()

    # Reason for unsetting `LD_PRELOAD` env var obtained from D6255224:
    # "Certain libraries (like allocators) cannot be loaded after the process
    # has started. When needing to use these libraries, send them to a
    # user-supplied script for handling them appropriately. Running the real
    # iserv with these libraries under LD_PRELOAD accomplishes this.
    # To ensure the LD_PRELOAD env doesn't make it to subsequently forked
    # processes, the very first action of start.ghci is to unset the variable."
    start_cmd.add("System.Environment.unsetEnv \"LD_PRELOAD\"")

    set_cmd = cmd_args(":set", delimiter = " ")
    first_order_deps = list(map(
        lambda dep: dep.name + "-" + dep.version,
        _first_order_haskell_deps(ctx),
    ))
    deduped_deps = {pkg: 1 for pkg in first_order_deps}.keys()
    package_list = cmd_args(
        deduped_deps,
        format = "-package {}",
        delimiter = " ",
    )
    set_cmd.add(package_list)
    set_cmd.add("\n")
    start_cmd.add(set_cmd)

    header_ghci = ctx.actions.declare_output("header.ghci")

    ctx.actions.write(header_ghci.as_output(), start_cmd)

    if ctx.attrs.ghci_init:
        append_ghci_init = cmd_args()
        append_ghci_init.add(
            ["sh", "-c", 'cat "$1" "$2" > "$3"', "--", header_ghci, ctx.attrs.ghci_init, script_file.as_output()],
        )
        ctx.actions.run(append_ghci_init, category = "append_ghci_init")
    else:
        ctx.actions.copy_file(script_file, header_ghci)

def haskell_ghci_impl(ctx: "context") -> ["provider"]:
    start_ghci_file = ctx.actions.declare_output("start.ghci")
    _write_start_ghci(ctx, start_ghci_file)

    outputs = [
        start_ghci_file,
    ]

    return [
        DefaultInfo(default_outputs = outputs),
    ]

# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//haskell:haskell.bzl", "HaskellLibraryProvider")
load("@prelude//utils:utils.bzl", "flatten")

# Symlink the ghci binary that will be used, e.g. the internal fork in Haxlsh
def _symlink_ghci_binary(ctx, ghci_bin: "artifact"):
    # TODO(T155760998): set ghci_ghc_path as a dependency instead of string
    ghci_bin_dep = ctx.attrs.ghci_bin_dep
    if not ghci_bin_dep:
        fail("GHC binary path not specified")

    # NOTE: In the buck1 version we'd symlink the binary only if a custom one
    # was provided, but in buck2 we're always setting `ghci_bin_dep` (i.e.
    # to default one if custom wasn't provided).
    src = ghci_bin_dep[DefaultInfo].default_outputs[0]
    ctx.actions.symlink_file(ghci_bin.as_output(), src)

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

    ghci_bin = ctx.actions.declare_output(ctx.attrs.name + ".bin/ghci")

    _symlink_ghci_binary(ctx, ghci_bin)

    outputs = [
        start_ghci_file,
        ghci_bin,
    ]

    return [
        DefaultInfo(default_outputs = outputs),
    ]

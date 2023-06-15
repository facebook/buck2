# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//:paths.bzl", "paths")
load("@prelude//haskell:haskell.bzl", "HaskellLibraryProvider", "HaskellToolchainInfo")
load(
    "@prelude//linking:linkable_graph.bzl",
    "LinkableRootInfo",
)
load(
    "@prelude//linking:shared_libraries.bzl",
    "SharedLibraryInfo",
    "traverse_shared_library_info",
)
load("@prelude//utils:utils.bzl", "flatten")

GHCiPreloadDepsInfo = record(
    preload_symlinks = {str.type: "artifact"},
    preload_deps_root = "artifact",
)

def _write_iserv_script(
        ctx: "context",
        preload_deps_info: GHCiPreloadDepsInfo.type,
        haskell_toolchain: HaskellToolchainInfo.type) -> "artifact":
    iserv_script_cmd = cmd_args(SCRIPT_HEADER.format("GHCi iserv script"))

    preload_libs = ":".join(
        [paths.join(
            "${DIR}",
            preload_deps_info.preload_deps_root.short_path,
            so,
        ) for so in sorted(preload_deps_info.preload_symlinks)],
    )

    if ctx.attrs.enable_profiling:
        ghci_iserv_path = haskell_toolchain.ghci_iserv_prof_path
    else:
        ghci_iserv_path = haskell_toolchain.ghci_iserv_path

    run_ghci = "LD_PRELOAD=\"$LD_PRELOAD\":{preload_libs} \
        PATH={binutils_path}:\"$PATH\" {ghci_iserv_path} \"$@\"".format(
        binutils_path = haskell_toolchain.ghci_binutils_path,
        ghci_iserv_path = ghci_iserv_path,
        preload_libs = preload_libs,
    )
    iserv_script_cmd.add(
        run_ghci,
    )

    iserv_script_name = "iserv"
    if ctx.attrs.enable_profiling:
        iserv_script_name += "-prof"

    iserv_script = ctx.actions.write(
        iserv_script_name,
        iserv_script_cmd,
        is_executable = True,
    )

    return iserv_script

def _build_preload_deps_root(
        ctx: "context",
        haskell_toolchain: HaskellToolchainInfo.type) -> GHCiPreloadDepsInfo.type:
    preload_deps = ctx.attrs.preload_deps

    preload_symlinks = {}
    preload_libs_root = ctx.label.name + ".preload-symlinks"

    for preload_dep in preload_deps:
        if SharedLibraryInfo in preload_dep:
            slib_info = preload_dep[SharedLibraryInfo]

            shlib = traverse_shared_library_info(slib_info).items()

            for shlib_name, shared_lib in shlib:
                preload_symlinks[shlib_name] = shared_lib.lib.output

        # TODO(T150785851): build or get SO for direct preload_deps
        # TODO(T150785851): find out why the only SOs missing are the ones from
        # the preload_deps themselves, even though the ones from their deps are
        # already there.
        if LinkableRootInfo in preload_dep:
            linkable_root_info = preload_dep[LinkableRootInfo]
            preload_so_name = linkable_root_info.name

            linkables = map(lambda x: x.objects, linkable_root_info.link_infos.default.linkables)

            object_file = flatten(linkables)[0]

            preload_so = ctx.actions.declare_output(preload_so_name)
            link = cmd_args(haskell_toolchain.linker)
            link.add(haskell_toolchain.linker_flags)
            link.add(ctx.attrs.linker_flags)
            link.add("-o", preload_so.as_output())

            link.add(
                "-shared",
                "-dynamic",
                "-optl",
                "-Wl,-soname",
                "-optl",
                "-Wl," + preload_so_name,
            )
            link.add(object_file)

            ctx.actions.run(
                link,
                category = "haskell_ghci_link",
                identifier = preload_so_name,
            )

            preload_symlinks[preload_so_name] = preload_so

    preload_deps_root = ctx.actions.symlinked_dir(preload_libs_root, preload_symlinks)
    return GHCiPreloadDepsInfo(
        preload_deps_root = preload_deps_root,
        preload_symlinks = preload_symlinks,
    )

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

    haskell_toolchain = ctx.attrs._haskell_toolchain[HaskellToolchainInfo]
    preload_deps_info = _build_preload_deps_root(ctx, haskell_toolchain)

    iserv_script = _write_iserv_script(ctx, preload_deps_info, haskell_toolchain)

    outputs = [
        start_ghci_file,
        ghci_bin,
        preload_deps_info.preload_deps_root,
        iserv_script,
    ]

    return [
        DefaultInfo(default_outputs = outputs),
    ]

# TODO(gustavoavena): parameterize header to print correct error msg
# @lint-ignore-every LICENSELINT
SCRIPT_HEADER = """\
#!/bin/bash

DIR="$(dirname "$(readlink -f "${{BASH_SOURCE[0]}}")")"
if ! test -d "$DIR"; then
  echo Cannot locate directory containing {}; exit 1
fi
"""

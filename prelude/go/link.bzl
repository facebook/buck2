load("@prelude//cxx:cxx_library_utility.bzl", "cxx_inherited_link_info")
load("@prelude//cxx:cxx_toolchain_types.bzl", "CxxToolchainInfo")
load(
    "@prelude//linking:link_info.bzl",
    "LinkStyle",
    "get_link_args",
    "unpack_link_args",
)
load(":packages.bzl", "merge_pkgs")
load(":toolchain.bzl", "GoToolchainInfo", "get_toolchain_cmd_args")

# Provider wrapping packages used for linking.
GoPkgLinkInfo = provider(fields = [
    "pkgs",  # {str.type: "artifact"}
])

def get_inherited_link_pkgs(deps: ["dependency"]) -> {str.type: "artifact"}:
    return merge_pkgs([d[GoPkgLinkInfo].pkgs for d in deps if GoPkgLinkInfo in d])

def link(ctx: "context", main: "artifact", pkgs: {str.type: "artifact"} = {}, deps: ["dependency"] = [], link_mode = None):
    go_toolchain = ctx.attrs._go_toolchain[GoToolchainInfo]
    output = ctx.actions.declare_output(ctx.label.name)

    cmd = get_toolchain_cmd_args(go_toolchain)

    cmd.add(go_toolchain.linker)

    cmd.add("-o", output.as_output())
    cmd.add("-buildmode", "exe")  # TODO(agallagher): support other modes
    cmd.add("-buildid=")  # Setting to a static buildid helps make the binary reproducible.

    # Add inherited Go pkgs to library search path.
    all_pkgs = merge_pkgs([pkgs, get_inherited_link_pkgs(deps)])
    pkgs_dir = ctx.actions.symlinked_dir(
        "__link_pkgs__",
        {name + path.extension: path for name, path in all_pkgs.items()},
    )
    cmd.add("-L", pkgs_dir)

    # Gather external link args from deps.
    ext_links = get_link_args(
        cxx_inherited_link_info(ctx, deps),
        # TODO: support other link styles?
        LinkStyle("static"),
    )
    ext_link_args = unpack_link_args(ext_links)

    if not link_mode:
        link_mode = "external"
    cmd.add("-linkmode", link_mode)

    if link_mode == "external":
        # Delegate to C++ linker...
        # TODO: It feels a bit inefficient to generate a wrapper file for every
        # link.  Is there some way to etract the first arg of `RunInfo`?  Or maybe
        # we can generate te platform-specific stuff once and re-use?
        cxx_toolchain = ctx.attrs._cxx_toolchain[CxxToolchainInfo]
        cxx_link_cmd = cmd_args(
            [
                cxx_toolchain.linker_info.linker,
                cxx_toolchain.linker_info.linker_flags,
                go_toolchain.external_linker_flags,
                ext_link_args,
                "\"$@\"",
            ],
            delimiter = " ",
        )
        linker_wrapper, macro_files = ctx.actions.write(
            "__{}_cxx_link_wrapper__.sh".format(ctx.label.name),
            ["#!/bin/sh", cxx_link_cmd],
            allow_args = True,
            is_executable = True,
        )
        cmd.add("-extld", linker_wrapper).hidden(cxx_link_cmd).hidden(macro_files)

    if ctx.attrs.linker_flags:
        cmd.add(ctx.attrs.linker_flags)

    cmd.add(main)

    ctx.actions.run(cmd, category = "go_link")

    return output

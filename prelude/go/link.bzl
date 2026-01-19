# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//cxx:cxx_library_utility.bzl", "cxx_inherited_link_info")
load(
    "@prelude//cxx:cxx_link_utility.bzl",
    "ExecutableSharedLibArguments",
    "executable_shared_lib_arguments",
    "make_link_args",
)
load("@prelude//cxx:cxx_toolchain_types.bzl", "CxxToolchainInfo")
load("@prelude//cxx:linker.bzl", "get_default_shared_library_name", "get_shared_library_name_linker_flags")
load(
    "@prelude//linking:link_info.bzl",
    "LinkStyle",
    "get_link_args_for_strategy",
    "to_link_strategy",
)
load(
    "@prelude//linking:shared_libraries.bzl",
    "SharedLibraryInfo",
    "merge_shared_libraries",
    "traverse_shared_library_info",
)
load("@prelude//linking:stamp_build_info.bzl", "stamp_build_info")
load("@prelude//os_lookup:defs.bzl", "Os", "OsLookup")
load(
    "@prelude//utils:utils.bzl",
    "filter_and_map_idx",
)
load(
    ":packages.bzl",
    "GoPkg",  # @Unused used as type
    "GoStdlib",
    "make_importcfg",
    "merge_pkgs",
)
load(":toolchain.bzl", "GoToolchainInfo", "get_toolchain_env_vars")

# Provider wrapping packages used for linking.
GoPkgLinkInfo = provider(fields = {
    "pkgs": provider_field(typing.Any, default = None),  # {str: "artifact"}
})

GoBuildMode = enum(
    "exe",  # non-pic executable
    "pie",  # pic executable
    "c_shared",  # pic C-shared library
    "c_archive",  # pic C-static library
)

def _build_mode_param(mode: GoBuildMode) -> str:
    if mode == GoBuildMode("exe"):
        return "exe"
    if mode == GoBuildMode("pie"):
        return "pie"
    if mode == GoBuildMode("c_shared"):
        return "c-shared"
    if mode == GoBuildMode("c_archive"):
        return "c-archive"
    fail("unexpected: {}", mode)

def get_inherited_link_pkgs(deps: list[Dependency]) -> dict[str, GoPkg]:
    return merge_pkgs([d[GoPkgLinkInfo].pkgs for d in deps if GoPkgLinkInfo in d])

# TODO(cjhopman): Is link_style a LibOutputStyle or a LinkStrategy here? Based
# on returning an empty thing for link_style != shared, it seems likely its
# intended to be LibOutputStyle, but it's called in places that are passing what
# appears to be a LinkStrategy.
def _process_shared_dependencies(
        ctx: AnalysisContext,
        artifact: Artifact,
        deps: list[Dependency],
        link_style: LinkStyle) -> ExecutableSharedLibArguments:
    """
    Provides files and linker args needed to for binaries with shared library linkage.
    - the runtime files needed to run binary linked with shared libraries
    - linker arguments for shared libraries
    """
    if link_style != LinkStyle("shared"):
        return ExecutableSharedLibArguments()

    shlib_info = merge_shared_libraries(
        ctx.actions,
        deps = filter_and_map_idx(SharedLibraryInfo, deps),
    )
    shared_libs = traverse_shared_library_info(shlib_info, transformation_provider = None)

    return executable_shared_lib_arguments(
        ctx,
        ctx.attrs._cxx_toolchain[CxxToolchainInfo],
        artifact,
        shared_libs,
    )

def link(
        ctx: AnalysisContext,
        main: GoPkg,
        cgo_enabled: bool,
        pkgs: dict[str, GoPkg] = {},
        deps: list[Dependency] = [],
        build_mode: GoBuildMode = GoBuildMode("exe"),
        link_mode: [str, None] = None,
        link_style: LinkStyle = LinkStyle("static"),
        linker_flags: list[typing.Any] = [],
        external_linker_flags: list[typing.Any] = []):
    go_toolchain = ctx.attrs._go_toolchain[GoToolchainInfo]

    if not cgo_enabled and (go_toolchain.asan or go_toolchain.race):
        fail("`race=True` and `asan=True` are only supported when `cgo_enabled=True`")

    if go_toolchain.env_go_os == "windows":
        executable_extension = ".exe"
        shared_extension = ".dll"
        archive_extension = ".lib"
    else:
        executable_extension = ""
        shared_extension = ".so"
        archive_extension = ".a"

    if build_mode == GoBuildMode("c_shared"):
        file_extension = shared_extension
        use_shared_code = True  # PIC
        link_style = LinkStyle("shared")
    elif build_mode == GoBuildMode("c_archive"):
        file_extension = archive_extension
        use_shared_code = True  # PIC
        link_style = LinkStyle("static_pic")
    elif build_mode == GoBuildMode("pie"):
        file_extension = executable_extension
        use_shared_code = True  # PIC
        link_style = LinkStyle("static_pic")
    else:  # GoBuildMode("exe")
        file_extension = executable_extension
        use_shared_code = False  # non-PIC
    final_output_name = ctx.label.name + file_extension
    output = ctx.actions.declare_output(ctx.label.name + "-tmp" + file_extension, has_content_based_path = True)

    cmd = cmd_args()

    cmd.add(go_toolchain.go_wrapper)
    cmd.add(["--go", go_toolchain.linker])
    cmd.add("--")
    cmd.add(go_toolchain.linker_flags)

    cmd.add("-o", output.as_output())
    cmd.add("-buildmode=" + _build_mode_param(build_mode))
    cmd.add("-buildid=")  # Setting to a static buildid helps make the binary reproducible.

    if go_toolchain.race:
        cmd.add("-race")

    if go_toolchain.asan:
        cmd.add("-asan")

    # Add inherited Go pkgs to library search path.
    all_pkgs = merge_pkgs([
        pkgs,
        get_inherited_link_pkgs(deps),
    ])

    identifier_prefix = ctx.label.name + "_" + _build_mode_param(build_mode)

    go_stdlib = ctx.attrs._go_stdlib[GoStdlib]
    importcfg = make_importcfg(ctx.actions, go_toolchain, go_stdlib, identifier_prefix, all_pkgs, use_shared_code, link = True)

    cmd.add("-importcfg", importcfg)

    executable_args = _process_shared_dependencies(ctx, output, deps, link_style)

    if link_mode == None:
        if build_mode == GoBuildMode("c_shared"):
            link_mode = "external"
        if build_mode == GoBuildMode("c_archive"):
            link_mode = "external"

    if link_mode != None:
        cmd.add("-linkmode", link_mode)

    cxx_toolchain_available = CxxToolchainInfo in ctx.attrs._cxx_toolchain
    if cxx_toolchain_available:
        cxx_toolchain = ctx.attrs._cxx_toolchain[CxxToolchainInfo]
        is_win = ctx.attrs._exec_os_type[OsLookup].os == Os("windows")

        # Gather external link args from deps.
        ext_links = get_link_args_for_strategy(
            ctx,
            cxx_inherited_link_info(deps),
            to_link_strategy(link_style),
            prefer_stripped = False,
            transformation_spec_context = None,
        )
        ext_link_args_output = make_link_args(
            ctx,
            ctx.actions,
            cxx_toolchain,
            [ext_links],
        )
        ext_link_args = cmd_args(hidden = ext_link_args_output.hidden)
        ext_link_args.add(cmd_args(executable_args.extra_link_args, quote = "shell"))
        ext_link_args.add(external_linker_flags)
        ext_link_args.add(ext_link_args_output.link_args)

        if build_mode == GoBuildMode("c_shared") and go_toolchain.env_go_os != "windows":
            soname = get_default_shared_library_name(cxx_toolchain.linker_info, ctx.label)
            soname_flags = get_shared_library_name_linker_flags(cxx_toolchain.linker_info.type, soname)
            ext_link_args.add(soname_flags)

        # Delegate to C++ linker...
        # TODO: It feels a bit inefficient to generate a wrapper file for every
        # link.  Is there some way to etract the first arg of `RunInfo`?  Or maybe
        # we can generate the platform-specific stuff once and re-use?
        ext_link_argfile, _ = ctx.actions.write(
            output.short_path + ".go_ext_link_argsfile",
            ext_link_args,
            allow_args = True,
            has_content_based_path = True,
        )
        cxx_link_cmd = cmd_args(
            [
                cxx_toolchain.linker_info.linker,
                cmd_args(ext_link_argfile, format = "@{}"),
                "%*" if is_win else "\"$@\"",
            ],
            delimiter = " ",
        )
        linker_wrapper, _ = ctx.actions.write(
            "__{}_cxx_link_wrapper__.{}".format(identifier_prefix, "bat" if is_win else "sh"),
            ([] if is_win else ["#!/bin/sh"]) + [cxx_link_cmd],
            allow_args = True,
            is_executable = True,
            has_content_based_path = True,
        )
        cmd.add("-extld", linker_wrapper, cmd_args(hidden = [cxx_link_cmd, ext_link_args, ext_link_args_output.hidden]))
        cmd.add("-extldflags", cmd_args(
            cxx_toolchain.linker_info.linker_flags,
            go_toolchain.external_linker_flags,
            delimiter = " ",
            quote = "shell",
        ))

    cmd.add(linker_flags)

    cmd.add(main.pkg_shared if use_shared_code else main.pkg)

    env = get_toolchain_env_vars(go_toolchain)

    ctx.actions.run(cmd, env = env, category = "go_link", identifier = identifier_prefix)

    # stamp only executable targets
    if build_mode in [GoBuildMode("exe"), GoBuildMode("pie")]:
        output = stamp_build_info(ctx, output, has_content_based_path = True)

    final_output = ctx.actions.copy_file(final_output_name, output)

    return (final_output, executable_args.runtime_files, executable_args.external_debug_info)

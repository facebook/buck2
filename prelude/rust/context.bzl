# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//cxx:cxx_context.bzl", "get_cxx_toolchain_info")
load("@prelude//cxx:cxx_toolchain_types.bzl", "CxxToolchainInfo", "LinkerInfo")
load(
    "@prelude//cxx:linker.bzl",
    "get_default_shared_library_name",
    "get_shared_library_name_for_param",
)
load("@prelude//linking:link_info.bzl", "LinkStrategy")
load("@prelude//os_lookup:defs.bzl", "Os", "OsLookup")
load("@prelude//rust/tools:attrs.bzl", "RustInternalToolsInfo")
load("@prelude//utils:cmd_script.bzl", "cmd_script")
load(":build_params.bzl", "BuildParams", "CrateType", "Emit", "ProfileMode")
load(":rust_toolchain.bzl", "PanicRuntime", "RustExplicitSysrootDeps", "RustToolchainInfo")
load(":sources.bzl", "symlinked_srcs")

CrateName = record(
    simple = field(str | ResolvedStringWithMacros),
    dynamic = field(Artifact | None),
)

# Struct for sharing common args between rustc and rustdoc
# (rustdoc just relays bunch of the same args to rustc when trying to gen docs)
CommonArgsInfo = record(
    args = field(cmd_args),
    subdir = field(str),
    tempfile = field(str),
    crate_type = field(CrateType),
    params = field(BuildParams),
    emit = field(Emit),
    emit_requires_linking = field(bool),
    crate_map = field(list[(CrateName, Label)]),
)

# Information that determines how dependencies should be collected
DepCollectionContext = record(
    advanced_unstable_linking = field(bool),
    include_doc_deps = field(bool),
    # Is the target a proc-macro target? This is ignored if `include_doc_deps`
    # is set, since doc tests in proc macro crates are not built with
    # `--extern proc_macro`
    is_proc_macro = field(bool),
    # From the toolchain, if available
    explicit_sysroot_deps = field(RustExplicitSysrootDeps | None),
    # Only needed if `advanced_unstable_linking` is set
    panic_runtime = field(PanicRuntime),
)

# Compile info which is reusable between multiple compilation command performed
# by the same rule.
CompileContext = record(
    # Clippy wrapper (wrapping clippy-driver so it has the same CLI as rustc).
    clippy_wrapper = field(cmd_args),
    # Memoized common args for reuse.
    common_args = field(dict[(CrateType, Emit, LinkStrategy, bool, bool, bool, ProfileMode), CommonArgsInfo]),
    cxx_toolchain_info = field(CxxToolchainInfo),
    dep_ctx = field(DepCollectionContext),
    exec_is_windows = field(bool),
    internal_tools_info = field(RustInternalToolsInfo),
    # The linker that we pass to `-Clinker`. This needs to be a wrapper script for two reasons:
    #  1. The `linker` in the cxx toolchain is a `cmd_args` which might have multiple args, we need
    #     this to be a single script.
    #  2. We need to pass some linker args that are applied before the ones rustc passes. There's
    #     unstable `-Zpre-link-arg` for that, but until that stabilizes, we pass them from within
    #     this script.
    linker_with_pre_args = field(cmd_args),
    path_sep = field(str),
    # Dylib name override, if any was provided by the target's `soname` attribute.
    soname = field(str | None),
    # Symlink root containing all sources.
    symlinked_srcs = field(Artifact),
    # Linker args to pass the linker wrapper to rustc.
    sysroot_args = field(cmd_args),
    toolchain_info = field(RustToolchainInfo),
    transitive_dependency_dirs = field(set[Artifact]),
)

def compile_context(ctx: AnalysisContext, binary: bool = False) -> CompileContext:
    toolchain_info = ctx.attrs._rust_toolchain[RustToolchainInfo]
    internal_tools_info = ctx.attrs._rust_internal_tools_toolchain[RustInternalToolsInfo]
    cxx_toolchain_info = get_cxx_toolchain_info(ctx)

    linker_with_pre_args = _linker(ctx, cxx_toolchain_info.linker_info, binary = binary)
    clippy_wrapper = _clippy_wrapper(ctx, toolchain_info)

    dep_ctx = DepCollectionContext(
        advanced_unstable_linking = toolchain_info.advanced_unstable_linking,
        include_doc_deps = False,
        is_proc_macro = getattr(ctx.attrs, "proc_macro", False),
        explicit_sysroot_deps = toolchain_info.explicit_sysroot_deps,
        panic_runtime = toolchain_info.panic_runtime,
    )

    # When we pass explicit sysroot deps, we need to override the default
    # sysroot to avoid accidentally linking against the prebuilt sysroot libs
    # provided by the toolchain.
    if toolchain_info.explicit_sysroot_deps:
        # Construct an empty sysroot dir with the similar structure as the real one
        # in order to appease rustc. For instance, even with -Zexternal-clangrt, rustc will emit a
        # -L{sysroot}/lib/rustlib/{target}/lib on the link line, which will fail if the sysroot dir is empty.
        empty_sysroot = ctx.actions.copied_dir("empty_dir", {
            "lib/rustlib/{}/lib".format(toolchain_info.rustc_target_triple): ctx.actions.copied_dir("__empty__", {}),
        })
        sysroot_args = cmd_args("--sysroot=", empty_sysroot, delimiter = "")
    elif toolchain_info.sysroot_path:
        sysroot_args = cmd_args("--sysroot=", toolchain_info.sysroot_path, delimiter = "")
    else:
        sysroot_args = cmd_args()

    exec_is_windows = ctx.attrs._exec_os_type[OsLookup].os == Os("windows")
    path_sep = "\\" if exec_is_windows else "/"

    return CompileContext(
        clippy_wrapper = clippy_wrapper,
        common_args = {},
        cxx_toolchain_info = cxx_toolchain_info,
        dep_ctx = dep_ctx,
        exec_is_windows = exec_is_windows,
        internal_tools_info = internal_tools_info,
        linker_with_pre_args = linker_with_pre_args,
        path_sep = path_sep,
        soname = _attr_soname(ctx),
        symlinked_srcs = symlinked_srcs(ctx),
        sysroot_args = sysroot_args,
        toolchain_info = toolchain_info,
        transitive_dependency_dirs = set(),
    )

def _linker(
        ctx: AnalysisContext,
        linker_info: LinkerInfo,
        binary: bool = False) -> cmd_args:
    linker = cmd_args(
        linker_info.linker,
        linker_info.linker_flags or [],
        # For "binary" rules, add C++ toolchain binary-specific linker flags.
        # TODO(agallagher): This feels a bit wrong -- it might be better to have
        # the Rust toolchain have it's own `binary_linker_flags` instead of
        # implicltly using the one from the C++ toolchain.
        linker_info.binary_linker_flags if binary else [],
        ctx.attrs.linker_flags,
    )

    return cmd_script(
        actions = ctx.actions,
        name = "linker_wrapper",
        cmd = linker,
        language = ctx.attrs._exec_os_type[OsLookup].script,
    )

# Return wrapper script for clippy-driver to make sure sysroot is set right
# We need to make sure clippy is using the same sysroot - compiler, std libraries -
# as rustc itself, so explicitly invoke rustc to get the path. This is a
# (small - ~15ms per invocation) perf hit but only applies when generating
# specifically requested clippy diagnostics.
def _clippy_wrapper(
        ctx: AnalysisContext,
        toolchain_info: RustToolchainInfo) -> cmd_args:
    clippy_driver = cmd_args(toolchain_info.clippy_driver)
    rustc_print_sysroot = cmd_args(toolchain_info.compiler, "--print=sysroot", delimiter = " ")
    if toolchain_info.rustc_target_triple:
        rustc_print_sysroot.add("--target={}".format(toolchain_info.rustc_target_triple))

    skip_setting_sysroot = toolchain_info.explicit_sysroot_deps != None or toolchain_info.sysroot_path != None

    if ctx.attrs._exec_os_type[OsLookup].os == Os("windows"):
        wrapper_file, _ = ctx.actions.write(
            ctx.actions.declare_output("__clippy_driver_wrapper.bat"),
            [
                "@echo off",
                "set __CLIPPY_INTERNAL_TESTS=true",
            ] + [
                cmd_args(rustc_print_sysroot, format = 'FOR /F "tokens=* USEBACKQ" %%F IN (`{}`) DO (set SYSROOT=%%F)') if not skip_setting_sysroot else "",
                cmd_args(clippy_driver, format = "{} %*"),
            ],
            allow_args = True,
        )
    else:
        wrapper_file, _ = ctx.actions.write(
            ctx.actions.declare_output("__clippy_driver_wrapper.sh"),
            [
                "#!/usr/bin/env bash",
                # Force clippy to be clippy: https://github.com/rust-lang/rust-clippy/blob/e405c68b3c1265daa9a091ed9b4b5c5a38c0c0ba/src/driver.rs#L334
                "export __CLIPPY_INTERNAL_TESTS=true",
            ] + (
                [] if skip_setting_sysroot else [cmd_args(rustc_print_sysroot, format = "export SYSROOT=$({})")]
            ) + [
                cmd_args(clippy_driver, format = "{} \"$@\"\n"),
            ],
            is_executable = True,
            allow_args = True,
        )

    return cmd_args(wrapper_file, hidden = [clippy_driver, rustc_print_sysroot])

def _attr_soname(ctx: AnalysisContext) -> str:
    """
    Get the shared library name to set for the given rust library.
    """
    linker_info = get_cxx_toolchain_info(ctx).linker_info
    if getattr(ctx.attrs, "soname", None) != None:
        return get_shared_library_name_for_param(linker_info, ctx.attrs.soname)
    return get_default_shared_library_name(linker_info, ctx.label)

# Filenames used for various emitted forms
# `None` for a prefix or suffix means use the build_param version
_EMIT_PREFIX_SUFFIX = {
    Emit("asm"): ("", ".s"),
    Emit("llvm-bc"): ("", ".bc"),
    Emit("llvm-ir"): ("", ".ll"),
    Emit("llvm-ir-noopt"): ("", ".ll"),
    Emit("obj"): ("", ".o"),
    Emit("metadata-fast"): ("lib", ".rmeta"),  # even binaries get called 'libfoo.rmeta'
    Emit("metadata-full"): (None, None),  # Hollow rlibs, so they get the same name
    Emit("link"): (None, None),  # crate type and reloc model dependent
    Emit("dep-info"): ("", ".d"),
    Emit("mir"): (None, ".mir"),
    Emit("expand"): (None, ".rs"),
    Emit("clippy"): ("lib", ".rmeta"),  # Treated like metadata-fast
}

# Return the filename for a particular emitted artifact type
def output_filename(
        compile_ctx: CompileContext,
        cratename: str,
        emit: Emit,
        buildparams: BuildParams,
        extra: str | None = None) -> str:
    # Allow for overriding the soname via the `soname` attribute.
    if emit == Emit("link") and buildparams.crate_type in (CrateType("dylib"), CrateType("cdylib")):
        return compile_ctx.soname

    epfx, esfx = _EMIT_PREFIX_SUFFIX[emit]
    prefix = epfx if epfx != None else buildparams.prefix
    suffix = esfx if esfx != None else buildparams.suffix
    return prefix + cratename + (extra or "") + suffix

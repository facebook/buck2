# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//cxx:cxx_toolchain_types.bzl", "CxxToolchainInfo")
load("@prelude//linking:link_info.bzl", "LinkStrategy")
load(":build_params.bzl", "CrateType", "Emit")
load(":rust_toolchain.bzl", "PanicRuntime", "RustExplicitSysrootDeps", "RustToolchainInfo")

CrateName = record(
    simple = field(str),
    dynamic = field([Artifact, None]),
)

# Struct for sharing common args between rustc and rustdoc
# (rustdoc just relays bunch of the same args to rustc when trying to gen docs)
CommonArgsInfo = record(
    args = field(cmd_args),
    subdir = field(str),
    tempfile = field(str),
    short_cmd = field(str),
    is_check = field(bool),
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
    toolchain_info = field(RustToolchainInfo),
    cxx_toolchain_info = field(CxxToolchainInfo),
    dep_ctx = field(DepCollectionContext),
    # Symlink root containing all sources.
    symlinked_srcs = field(Artifact),
    # Linker args to pass the linker wrapper to rustc.
    linker_args = field(cmd_args),
    # Clippy wrapper (wrapping clippy-driver so it has the same CLI as rustc).
    clippy_wrapper = field(cmd_args),
    # Memoized common args for reuse.
    common_args = field(dict[(CrateType, Emit, LinkStrategy, bool), CommonArgsInfo]),
    transitive_dependency_dirs = field(dict[Artifact, None]),
    sysroot_args = field(cmd_args),
)

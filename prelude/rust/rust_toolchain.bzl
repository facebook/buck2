# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# FIXME(JakobDegen): These all have default values for historical reasons. Some of them certainly
# should, but some of them probably shouldn't?
# @unsorted-dict-items
_rust_toolchain_attrs = {
    # Report unused dependencies
    "report_unused_deps": provider_field(bool, default = False),
    # Rustc target triple to use
    # https://doc.rust-lang.org/rustc/platform-support.html
    "rustc_target_triple": provider_field(str | None, default = None),
    # Baseline compiler config
    "rustc_flags": provider_field(list[typing.Any], default = []),
    # Extra flags when building binaries
    "rustc_binary_flags": provider_field(list[typing.Any], default = []),
    # Extra flags for doing check builds
    "rustc_check_flags": provider_field(list[typing.Any], default = []),
    # Extra flags for doing building tests
    "rustc_test_flags": provider_field(list[typing.Any], default = []),
    # Extra flags when coverage is enabled for a target
    # FIXME(JakobDegen): Can't use `list[str]` here, because then the default is wrong, but can't
    # use a non-empty list as the default because lists are mutable
    "rustc_coverage_flags": provider_field(typing.Any, default = ("-Cinstrument-coverage",)),
    # Extra flags for rustdoc invocations
    "rustdoc_flags": provider_field(list[typing.Any], default = []),
    # Use rmeta for lib->lib dependencies, and only block
    # linking on rlib crates. The hope is that rmeta builds
    # are quick and this increases effective parallelism.
    "pipelined": provider_field(bool, default = False),
    # When you `buck test` a library, also compile and run example code in its
    # documentation comments.
    "doctests": provider_field(bool, default = False),
    # Filter out failures when we just need diagnostics. That is,
    # a rule which fails with a compilation failure will report
    # success as an RE action, but a "failure filter" action will
    # report the failure if some downstream action needs one of the
    # artifacts. If all you need is diagnostics, then it will report
    # success. This doubles the number of actions, so it should only
    # be explicitly enabled when needed.
    "failure_filter": provider_field(bool, default = False),
    # The Rust compiler (rustc)
    "compiler": provider_field(RunInfo | None, default = None),
    # A Rust compiler that can be used "standalone", without special settings
    # from Rust rules (e.g. this keeps the default sysroot).
    "compiler_standalone": provider_field(RunInfo | None, default = None),
    # Rust documentation extractor (rustdoc)
    "rustdoc": provider_field(RunInfo | None, default = None),
    # Clippy (linter) version of the compiler
    "clippy_driver": provider_field(RunInfo | None, default = None),
    # Wrapper for rustc in actions
    "rustc_action": provider_field(RunInfo | None, default = None),
    # Wrapper for rustdoc-generated test executables
    "rustdoc_test_with_resources": provider_field(RunInfo | None, default = None),
    # Failure filter action
    "failure_filter_action": provider_field(RunInfo | None, default = None),
    # The default edition to use, if not specified.
    "default_edition": provider_field(str | None, default = None),
    # Lints
    "allow_lints": provider_field(list[typing.Any], default = []),
    "deny_lints": provider_field(list[typing.Any], default = []),
    "warn_lints": provider_field(list[typing.Any], default = []),
    # Clippy configuration file clippy.toml
    "clippy_toml": provider_field(Artifact | None, default = None),
    # URL prefix (e.g. /path/to/docs) where crates' docs are hosted. Used for
    # linking types in signatures to their definition in another crate.
    "extern_html_root_url_prefix": provider_field(str | None, default = None),
    # Utilities used for building flagfiles containing dynamic crate names
    "concat_tool": provider_field(RunInfo | None, default = None),
    "transitive_dependency_symlinks_tool": provider_field(RunInfo | None, default = None),
    # Passing true here enables the unstable feature using `rlib` format
    # instead of `staticlib` when linking rust targets into native (e.g.
    # C/C++) targets.
    "native_unbundle_deps": provider_field(bool, default = False),
}

RustToolchainInfo = provider(fields = _rust_toolchain_attrs)

def ctx_toolchain_info(ctx: AnalysisContext) -> RustToolchainInfo:
    return ctx.attrs._rust_toolchain[RustToolchainInfo]

# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# Typically, rustc has access to a "sysroot," which is a directory tree with a known layout that
# contains a number of pre-compiled rlibs that are available by default. This includes, for example,
# the standard library. If explicit sysroot deps are passed on the toolchain, the sysroot is not
# made available to rustc. Instead, all crates which would normally be members of the sysroot must
# be passed explicitly here.
#
# Most sysroot deps typically behave as if they were transitive dependencies of the crate being
# compiled. That means they are not guaranteed to be included in the link, and the user must write
# `extern crate;` to access them. The crates singled out here behave differently in that regard,
# which is why they are singled out.
#
# Toolchains are free to omit any crates they like from this list; those crates will simply not be
# available in the compilation.
RustExplicitSysrootDeps = record(
    core = Dependency | None,
    proc_macro = Dependency | None,
    std = Dependency | None,
    panic_unwind = Dependency | None,
    panic_abort = Dependency | None,
    others = list[Dependency],
)

PanicRuntime = enum("unwind", "abort", "none")

# FIXME(JakobDegen): These all have default values for historical reasons. Some of them certainly
# should, but some of them probably shouldn't?
# @unsorted-dict-items
rust_toolchain_attrs = {
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
    # Setting this enables additional behaviors that improves linking at the
    # cost of using unstable implementation details of rustc. At the moment,
    # this is only used for linking rlibs into C++/C builds, instead of using
    # staticlibs, but that's expected to change.
    #
    # FIXME(JakobDegen): This should require `explicit_sysroot_deps` in the
    # future.
    "advanced_unstable_linking": provider_field(bool, default = False),
    # See the documentation on the type for details
    "explicit_sysroot_deps": provider_field(RustExplicitSysrootDeps | None, default = None),
    # The panic runtime to use. This is a part of the target definition and is
    # normally inferred by rustc. This field:
    #
    #  - Should be set to `"none"` on nostd targets
    #  - Must be set correctly if `explicit_sysroot_deps` and
    #    `advanced_unstable_linking` are used. You can find the correct value
    #    for a given target triple via `rustc --print target-spec-json`
    #  - Otherwise can typically be safely defaulted to `"unwind"`. It is,
    #    however, still the preferred way of configuring `-Cpanic=abort`, since
    #    it makes sure that the flag is consistent across the crate graph.
    #
    # It's worth pointing out that the way that rustc handles this is a bit
    # weird. It requires the panic runtime to be a nostd crate, despite the fact
    # that it is only ever useful in combination with std. We don't impose such
    # a requirement.
    #
    # FIXME(JakobDegen): Fix `enum` so that we can set `unwind` as the default
    "panic_runtime": provider_field(PanicRuntime),
}

RustToolchainInfo = provider(fields = rust_toolchain_attrs)

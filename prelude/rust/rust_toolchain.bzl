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
    # Rustc flags, except that they are applied on the command line after the
    # target's rustc flags
    "extra_rustc_flags": provider_field(list[typing.Any], default = []),
    # Extra flags when building binaries
    "rustc_binary_flags": provider_field(list[typing.Any], default = []),
    # Extra flags for doing building tests
    "rustc_test_flags": provider_field(list[typing.Any], default = []),
    # Extra flags when coverage is enabled for a target
    # FIXME(JakobDegen): Can't use `list[str]` here, because then the default is wrong, but can't
    # use a non-empty list as the default because lists are mutable
    "rustc_coverage_flags": provider_field(typing.Any, default = ("-Cinstrument-coverage",)),
    # Extra flags for rustdoc invocations
    "rustdoc_flags": provider_field(list[typing.Any], default = []),
    # When you `buck test` a library, also compile and run example code in its
    # documentation comments.
    "doctests": provider_field(bool, default = False),
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
    # Wrapper for rustdoc coverage
    "rustdoc_coverage": provider_field(RunInfo | None, default = None),
    # These two scripts are used to implement deferred linking, where the link action
    # is separate from the rustc invocation action. The benefit here is that we can
    # decouple the action graph such that rustc can compile libs without waiting for
    # the link step from shared lib dependencies from completing.
    "deferred_link_action": provider_field(RunInfo | None, default = None),
    "extract_link_action": provider_field(RunInfo | None, default = None),
    # Failure filter action
    "failure_filter_action": provider_field(RunInfo | None, default = None),
    # The default edition to use, if not specified.
    "default_edition": provider_field(str | None, default = None),
    # Lints
    "allow_lints": provider_field(list[typing.Any], default = []),
    "deny_lints": provider_field(list[typing.Any], default = []),
    "warn_lints": provider_field(list[typing.Any], default = []),
    # Deny-on-Check lints are handled differently depending on the build.
    #
    # For check builds, e.g. [check], [diag.json], [clippy.json] subtargets, or the default target
    # for `rust_library` rules, these lints will be applied as Deny Lints. Importantly, this means
    # that when you call `buck build :rust_lib` or use tools like arc rust-check or rustfix, these
    # lints will be surfaced as errors.
    #
    # However, for "regular" builds, e.g. when building tests or binaries, or building this target
    # as a dependency of another target, these flags will be surfaced only as warnings. The primary
    # benefit here is that you can develop + test your code as normal and will not be blocked by
    # these lints. However, once you run rust check, or submit your code to phabricator, these
    # lints will prevent you from landing your code. This way we can introduce lints that we'd like
    # to deny from our codebase without slowing down your inner dev loop, or encouraging you to
    # --cap-warns=lint for your projects.
    "deny_on_check_lints": provider_field(list[typing.Any], default = []),
    # Clippy configuration file clippy.toml
    "clippy_toml": provider_field(Artifact | None, default = None),
    # Utilities used for building flagfiles containing dynamic crate names
    "transitive_dependency_symlinks_tool": provider_field(RunInfo | None, default = None),
    # Setting this enables additional behaviors that improves linking at the
    # cost of using unstable implementation details of rustc. At the moment,
    # this is only used for linking rlibs into C++/C builds, instead of using
    # staticlibs, but that's expected to change.
    #
    # FIXME(JakobDegen): This should require `explicit_sysroot_deps` in the
    # future.
    "advanced_unstable_linking": provider_field(bool, default = False),
    # Override the implicit sysroot with the provided Artifact containing a directory to
    # a prebuilt sysroot. Will be forwarded to rustc as `--sysroot=<sysroot_path>`. Only
    # one of this and `explicit_sysroot_deps` may be set.
    "sysroot_path": provider_field(Artifact | None, default = None),
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

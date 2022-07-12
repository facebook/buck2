RustToolchainInfo = provider(fields = [
    # Report unused dependencies
    "report_unused_deps",
    # Baseline compiler config
    "rustc_flags",
    # Extra flags when building binaries
    "rustc_binary_flags",
    # Extra flags for doing check builds
    "rustc_check_flags",
    # Extra flags for doing building tests
    "rustc_test_flags",
    # Extra flags for rustdoc invocations
    "rustdoc_flags",
    # Use rmeta for lib->lib dependencies, and only block
    # linking on rlib crates. The hope is that rmeta builds
    # are quick and this increases effective parallelism.
    # Currently blocked by https://github.com/rust-lang/rust/issues/85401
    "pipelined",
    # Generate save-analysis by default with metadata builds.
    # Is unstable, so it should be configurable.
    "save_analysis",
    # Filter out failures when we just need diagnostics. That is,
    # a rule which fails with a compilation failure will report
    # success as an RE action, but a "failure filter" action will
    # report the failure if some downstream action needs one of the
    # artifacts. If all you need is diagnostics, then it will report
    # success. This doubles the number of actions, so it should only
    # be explicitly enabled when needed.
    "failure_filter",
    # The Rust compiler (rustc)
    "compiler",
    # Rust documentation extractor (rustdoc)
    "rustdoc",
    # Clippy (linter) version of the compiler
    "clippy_driver",
    # Wrapper for rustc in actions
    "rustc_action",
    # Failure filter action
    "failure_filter_action",
    # The default edition to use, if not specified.
    "default_edition",
    # Lints
    "allow_lints",
    "deny_lints",
    "warn_lints",
    # Prefix (/intern/rustdoc in our case) where fbcode crates' docs are hosted.
    # Used for linking types in signatures to their definition in another crate.
    "extern_html_root_url_prefix",
])

# Stores "platform"/flavor name used to resolve *platform_* arguments
RustPlatformInfo = provider(fields = [
    "name",
])

def ctx_toolchain_info(ctx: "context") -> "RustToolchainInfo":
    return ctx.attrs._rust_toolchain[RustToolchainInfo]

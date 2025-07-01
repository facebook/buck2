# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//rust:build.bzl", "crate_root", "process_env")
load(
    "@prelude//rust:context.bzl",
    "CompileContext",  # @unused Used as a type
    "CrateName",  # @unused Used as a type
    "DepCollectionContext",  # @unused Used as a type
)
load("@prelude//rust:link_info.bzl", "attr_crate", "get_available_proc_macros", "resolve_rust_deps")

RustAnalyzerInfo = provider(
    fields = {
        "available_proc_macros": list[Dependency],
        # The name of the crate for the target.
        "crate": CrateName,
        # The root source for the rust target (typically lib.rs, main.rs), relative to the buck target file.
        "crate_root": str,
        # The processed env as produced by the buck build prelude. Some env vars like `OUT_DIR` and `CARGO_MANIFEST_DIR`
        # will be made into absolute paths.
        "env": dict[str, cmd_args],
        # The list of rust deps needed for RustAnalyzer to function. Namely, this excludes things like
        # exec deps used as inputs to genrules and other non-rust dependencies.
        "rust_deps": list[Dependency],
        # The list of recursive rust dependencies for this target, including proc macros. Useful for
        # identifying the targets needing to be collected into Rust Analyzer's crate graph. Notably,
        # excludes rust dependencies that are used in build tools (e.g. build scripts).
        "transitive_target_set": set[ConfiguredTargetLabel],
    },
)

def _compute_rust_deps(
        ctx: AnalysisContext,
        dep_ctx: DepCollectionContext) -> list[Dependency]:
    dep_ctx = DepCollectionContext(
        advanced_unstable_linking = dep_ctx.advanced_unstable_linking,
        # Include doc deps here for any doctests that may be present in the target.
        include_doc_deps = True,
        is_proc_macro = dep_ctx.is_proc_macro,
        # Rust Analyzer handles the sysroot separately. We omit the sysroot deps here and will
        # instead pass a path to the sysroot as a separate config.
        explicit_sysroot_deps = None,
        panic_runtime = dep_ctx.panic_runtime,
    )

    first_order_deps = resolve_rust_deps(ctx, dep_ctx)
    available_proc_macros = get_available_proc_macros(ctx)

    return [dep.dep for dep in first_order_deps] + available_proc_macros.values()

def _compute_transitive_target_set(
        ctx: AnalysisContext,
        first_order_deps: list[Dependency]) -> set[ConfiguredTargetLabel]:
    transitive_targets = set([ctx.label.configured_target()])
    for dep in first_order_deps:
        target_sets = dep[RustAnalyzerInfo].transitive_target_set
        for target_set in target_sets:
            transitive_targets.add(target_set)
    return transitive_targets

def _compute_env(
        ctx: AnalysisContext,
        compile_ctx: CompileContext) -> dict[str, cmd_args]:
    # Disable rustc_action processing, as rust-project will handle windows + any escaping necessary.
    plain_env, path_env = process_env(compile_ctx, ctx.attrs.env, False)
    return plain_env | path_env

def rust_analyzer_provider(
        ctx: AnalysisContext,
        compile_ctx: CompileContext,
        default_roots: list[str]) -> RustAnalyzerInfo:
    rust_deps = _compute_rust_deps(ctx, compile_ctx.dep_ctx)
    return RustAnalyzerInfo(
        available_proc_macros = get_available_proc_macros(ctx).values(),
        crate = attr_crate(ctx),
        crate_root = crate_root(ctx, default_roots),
        env = _compute_env(ctx, compile_ctx),
        rust_deps = rust_deps,
        transitive_target_set = _compute_transitive_target_set(ctx, rust_deps),
    )

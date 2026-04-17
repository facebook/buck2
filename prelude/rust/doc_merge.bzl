# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

"""Reusable building block for producing a merged rustdoc HTML tree
across a set of rust crates, using RFC 3662's `rustdoc --merge=finalize`.

`prelude//rust:doc_merge.bxl` is a thin wrapper around `rustdoc_merge`
for the common case. Import `rustdoc_merge` from your own BXL when you
want to customise *which* crates go into the merged tree — e.g. bespoke
target-universe handling, filtering by package, or (in future) skipping
crates that should link out to docs.rs instead of being bundled.

Typical custom BXL:

    load("@prelude//rust:doc_merge.bzl", "rustdoc_merge")

    def _impl(ctx: bxl.Context) -> None:
        universe = ctx.target_universe("root//my/lib/...")
        rust = ctx.cquery().kind("^rust_library$", universe.target_set())
        targets = [t for t in rust if should_include(t)]
        out = rustdoc_merge(ctx, targets)
        ctx.output.print(ctx.output.ensure(out).abs_path())
"""

load("@prelude//rust:outputs.bzl", "RustcExtraOutputsInfo")
load("@prelude//rust:rust_toolchain.bzl", "RustToolchainInfo")
load("@prelude//utils:argfile.bzl", "at_argfile")

_MERGE_TOOL = "prelude//rust/tools:rustdoc_merge"

def rustdoc_merge(
        ctx: bxl.Context,
        targets: list[bxl.ConfiguredTargetNode]) -> Artifact:
    """Produce a merged rustdoc HTML tree for `targets`.

    `targets` must be configured rust rules whose `RustcExtraOutputsInfo`
    provider carries non-None `rustdoc_parts` / `rustdoc_html` artifacts
    (i.e. `rust_library` or `rust_binary`). Targets that don't carry the
    provider are skipped silently; callers should filter in advance if
    they want stricter behaviour.

    Returns the merged output directory as an Artifact — the caller is
    responsible for `ctx.output.ensure(...)` or any further use.
    """
    if not targets:
        fail("rustdoc_merge: targets list is empty")

    analyses = ctx.analysis(targets)
    parts_artifacts = []
    html_artifacts = []
    for _label, analysis in analyses.items():
        providers = analysis.providers()
        if RustcExtraOutputsInfo not in providers:
            continue
        extra = providers[RustcExtraOutputsInfo]
        if extra.rustdoc_parts != None:
            parts_artifacts.append(extra.rustdoc_parts)
        if extra.rustdoc_html != None:
            html_artifacts.append(extra.rustdoc_html)

    if not parts_artifacts:
        fail("rustdoc_merge: no rust targets with rustdoc_parts found")

    # Every rust rule has `_rust_toolchain` wired via `toolchains_common.rust()`,
    # so we can read the rustdoc binary off any of the resolved targets.
    rust_toolchain = targets[0].resolved_attrs_eager(ctx)._rust_toolchain[RustToolchainInfo]

    bxl_actions = ctx.bxl_actions(exec_deps = [_MERGE_TOOL])
    actions = bxl_actions.actions
    merge_tool = bxl_actions.exec_deps[bxl_actions.exec_deps.keys()[0]][RunInfo]

    out_dir = actions.declare_output("merged-rustdoc", dir = True)

    argfile_content = cmd_args(
        cmd_args(out_dir.as_output(), format = "--out-dir={}"),
        cmd_args(rust_toolchain.rustdoc, format = "--rustdoc={}"),
        [cmd_args(h, format = "--html-dir={}") for h in html_artifacts],
        [cmd_args(p, format = "--parts-dir={}") for p in parts_artifacts],
        [cmd_args(f, format = "--rustdoc-flag={}") for f in rust_toolchain.rustdoc_flags],
        [cmd_args(t, format = "--theme={}") for t in rust_toolchain.rustdoc_themes],
    )
    if rust_toolchain.default_edition != None:
        argfile_content.add(cmd_args(rust_toolchain.default_edition, format = "--edition={}"))

    cmd = cmd_args(
        merge_tool,
        at_argfile(
            actions = actions,
            name = "rustdoc_merge.argfile",
            args = argfile_content,
            allow_args = True,
        ),
    )

    actions.run(cmd, category = "rustdoc_merge")
    return out_dir

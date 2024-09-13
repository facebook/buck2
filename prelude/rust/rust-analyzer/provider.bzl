# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//rust:build.bzl", "crate_root")

RustAnalyzerInfo = provider(
    fields = {
        # The root source for the rust target (typically lib.rs, main.rs), relative to the buck target file.
        "crate_root": str,
    },
)

def rust_analyzer_provider(
        ctx: AnalysisContext,
        default_roots: list[str]) -> RustAnalyzerInfo:
    return RustAnalyzerInfo(
        crate_root = crate_root(ctx, default_roots),
    )

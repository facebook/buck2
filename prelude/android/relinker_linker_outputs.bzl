# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

"""
Extra relinker outputs support (e.g., LLVM stats).

Reads from ctx.attrs.extra_relinker_outputs attribute (not buck config directly).
Buck config is read at macro level and translated to attribute value.
"""

load("@prelude//linking:link_info.bzl", "ExtraLinkerOutputs")
load("@prelude//utils:arglike.bzl", "ArgLike")  # @unused Used as a type

def _llvm_stats_outputs_factory(output_path: str) -> typing.Callable:
    """Factory for relinker LLVM stats outputs."""

    def factory(ctx: AnalysisContext) -> ExtraLinkerOutputs:
        stats_file = ctx.actions.declare_output(output_path + ".stats")
        return ExtraLinkerOutputs(
            artifacts = {"llvm-stats": stats_file},
            providers = {"llvm-stats": [DefaultInfo(default_output = stats_file)]},
        )

    return factory

def _llvm_stats_flags_factory() -> typing.Callable:
    """Factory for relinker LLVM stats linker flags."""

    def factory(_ctx: AnalysisContext, outputs: dict[str, Artifact]) -> list[ArgLike]:
        return [
            cmd_args(outputs["llvm-stats"].as_output(), format = "-Wl,-plugin-opt=stats-file={}"),
        ]

    return factory

def _get_extra_relinker_output_types(ctx: AnalysisContext) -> list[str]:
    """Get list of enabled extra relinker output types from attribute."""
    return getattr(ctx.attrs, "extra_relinker_outputs", [])

def get_extra_relinker_args(ctx: AnalysisContext, output_path: str) -> dict[str, typing.Any]:
    """Get extra linker outputs factory dict for all enabled relinker outputs."""
    output_types = _get_extra_relinker_output_types(ctx)
    if not output_types:
        return {}

    def combined_outputs_factory(ctx: AnalysisContext) -> ExtraLinkerOutputs:
        artifacts = {}
        providers = {}
        for output_type in output_types:
            if output_type == "llvm-stats":
                result = _llvm_stats_outputs_factory(output_path)(ctx)
                artifacts |= result.artifacts
                providers |= result.providers
        return ExtraLinkerOutputs(artifacts = artifacts, providers = providers)

    def combined_flags_factory(ctx: AnalysisContext, outputs: dict[str, Artifact]) -> list[ArgLike]:
        flags = []
        for output_type in output_types:
            if output_type == "llvm-stats":
                flags += _llvm_stats_flags_factory()(ctx, outputs)
        return flags

    return {
        "extra_linker_outputs_factory": combined_outputs_factory,
        "extra_linker_outputs_flags_factory": combined_flags_factory,
    }

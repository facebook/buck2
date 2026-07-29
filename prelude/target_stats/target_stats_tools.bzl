# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# Reusable, rule-agnostic bundle of the target_stats tool executables. A
# concrete instance is attached (optionally) to a platform toolchain
# (cxx_toolchain, android_toolchain, ...) so any supporting rule can reach the
# tools via its toolchain when target_stats is enabled. New platforms can wire
# it onto their own toolchains without changing the rules that consume it.

TargetStatsToolsInfo = provider(
    fields = {
        # Runs the intermediary that extracts cycle target-data from file_stats
        # outputs.
        "extract_target_data": provider_field(RunInfo),
        # Runs the per-library Apple file-cycle tool.
        "file_cycles": provider_field(RunInfo),
        # Runs the per-file metrics tool.
        "file_stats": provider_field(RunInfo),
        # Runs the per-library Android package-cycle tool.
        "package_cycles": provider_field(RunInfo),
    },
)

def _target_stats_tools_impl(ctx: AnalysisContext) -> list[Provider]:
    return [
        DefaultInfo(),
        TargetStatsToolsInfo(
            file_cycles = ctx.attrs.file_cycles[RunInfo],
            file_stats = ctx.attrs.file_stats[RunInfo],
            package_cycles = ctx.attrs.package_cycles[RunInfo],
            extract_target_data = ctx.attrs.extract_target_data[RunInfo],
        ),
    ]

target_stats_tools = rule(
    impl = _target_stats_tools_impl,
    attrs = {
        "extract_target_data": attrs.exec_dep(providers = [RunInfo]),
        "file_cycles": attrs.exec_dep(providers = [RunInfo]),
        "file_stats": attrs.exec_dep(providers = [RunInfo]),
        "package_cycles": attrs.exec_dep(providers = [RunInfo]),
    },
)

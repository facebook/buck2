# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# Per-target codebase statistics, propagated across the dependency graph.
#
# A supporting rule (apple_library, cxx_library, android_library, ...) that has
# target_stats enabled builds a per-target manifest JSON (aggregate metrics +
# cycles + file->json mapping) and returns a TargetStatsInfo. The provider
# carries a transitive set whose value is this target's TargetStatsRecord and
# whose children are the tsets of its dependencies, so a consumer can request
# the whole dependency graph's stats from a single top-level target.

TargetStatsRecord = record(
    # The (unconfigured) label of the target these stats are for.
    label = field(str),
    # The per-target manifest JSON: aggregate info, the cycles output, and the
    # mapping of source file -> its file_stats JSON. Produced with
    # ctx.actions.write_json(..., with_inputs = True) so it carries the per-file
    # JSONs + cycles artifact as inputs.
    manifest = field(Artifact),
)

def _project_manifests(record: TargetStatsRecord) -> list[Artifact]:
    return [record.manifest]

# Transitive set of TargetStatsRecord. The "manifests" projection yields every
# transitive target's manifest, used to build the [all_target_stats] subtarget.
TargetStatsInfoTSet = transitive_set(
    args_projections = {
        "manifests": _project_manifests,
    },
)

TargetStatsInfo = provider(
    fields = {
        # The (unconfigured) label of the target.
        "label": provider_field(str),
        # tset with this target's TargetStatsRecord as the value and its
        # dependencies' tsets as the children.
        "tset": provider_field(TargetStatsInfoTSet),
    },
)

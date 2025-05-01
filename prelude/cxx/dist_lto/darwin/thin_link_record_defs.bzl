# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# These records are documented in the dist_lto_planner_darwin.py script and must be kept in sync with those definitions

BitcodeMergeState = enum(
    "STANDALONE",
    "ABSORBED",
    "ROOT",
    "NOT_LOADED",
)

ObjectFileOptimizationPlan = record(
    imports = field(list[int]),
    archive_imports = field(list[int]),
    is_bitcode = field(bool),
    merge_state = field(str | None, None),  # BitcodeMergeState
    loaded_by_linker = field(bool),
)

ArchiveMemberOptimizationPlan = record(
    is_bitcode = field(bool),
    path = field(str),
    imports = field(list[int]),
    archive_imports = field(list[int]),
    index_shard_file_path = field(str | None, None),
    loaded_by_linker = field(bool),
    merge_state = field(str | None, None),  # BitcodeMergeState
    merged_bitcode_path = field(str | None, None),
)

ArchiveOptimizationPlan = record(
    object_plans = field(list[ArchiveMemberOptimizationPlan]),
    base_dir = field(str),
)

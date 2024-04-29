# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//linking:types.bzl", "Linkage")
load(
    "@prelude//utils:build_target_pattern.bzl",
    "BuildTargetPattern",
)

# Label for special group mapping which makes every target associated with it to be included in all groups
MATCH_ALL_LABEL = "MATCH_ALL"

# Label for special group mapping which makes every target associated with it to be linked directly
# against the final binary
NO_MATCH_LABEL = "NO_MATCH"

Traversal = enum(
    # Includes the target and all of it's transitive dependencies in the group.
    "tree",
    # Includes only the target in the group.
    "node",
    # Uses pattern and separates all targets by full folder path.
    "subfolders",
    # Includes targets found in the transitive deps of *any* roots.
    # Filters for these mappings will be applied to the intersected deps.
    "intersect_any_roots",
)

# Optional type of filtering
FilterType = enum(
    # Filters for targets with labels matching the regex pattern defined after `label:`.
    "label",
    # Filters for targets for the build target pattern defined after "pattern:".
    "pattern",
    # Filters for targets matching the regex pattern defined after "target_regex:".
    "target_regex",
)

BuildTargetFilter = record(
    pattern = field(BuildTargetPattern),
    _type = field(FilterType, FilterType("pattern")),
)

LabelFilter = record(
    regex = regex,
    _type = field(FilterType, FilterType("label")),
)

TargetRegexFilter = record(
    regex = regex,
    _type = field(FilterType, FilterType("target_regex")),
)

# Representation of a parsed group mapping
GroupMapping = record(
    # The root to apply this mapping to.
    roots = field(list[Label], []),
    # The type of traversal to use.
    traversal = field(Traversal, Traversal("tree")),
    # Optional filter type to apply to the traversal.
    filters = field(list[[BuildTargetFilter, LabelFilter, TargetRegexFilter]], []),
    # Preferred linkage for this target when added to a link group.
    preferred_linkage = field([Linkage, None], None),
)

# Representation of group attributes
GroupAttrs = record(
    # Use distributed thinlto to build the link group shared library.
    enable_distributed_thinlto = field(bool, False),
    # Enable this link group if the binary's node count exceeds the given threshold
    enable_if_node_count_exceeds = field([int, None], None),
    # Discard all dependencies in the link group, useful for dropping unused dependencies
    # from the build graph.
    discard_group = field(bool, False),
    # Adds additional linker flags used to link the link group shared object.
    linker_flags = field(list, []),
    # Adds additional linker flags to apply to dependents that link against the
    # link group's shared object.
    exported_linker_flags = field(list, []),
    # Requires root nodes in specs to always exist in dependency graph.
    # Otherwise fails.
    requires_root_node_exists = field(bool, True),
    # For certain wide-scale generic link groups we want to enable
    # initial duplicate analysis. This is useful for detecting dduplicated symbols problem early
    # for automatoc link groups that we not aware about (e.g. evicting whole root package folder into link group)
    prohibit_file_duplicates = field(bool, False),
)

# Types of group traversal
GroupDefinition = enum(
    # Group is explicitly defined in mapping provided by user.
    # That is the default behavior.
    "explicit",
    # Group is implicitly created during mapping computations.
    # For example, group can be created for "subfolders" traversal.
    "implicit",
)

# Representation of a parsed group
Group = record(
    # The name for this group.
    name = str,
    # The mappings that are part of this group.
    mappings = list[GroupMapping],
    attrs = GroupAttrs,
    definition_type = field(GroupDefinition, GroupDefinition("explicit")),
)

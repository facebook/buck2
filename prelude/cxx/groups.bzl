load(
    "@fbcode//buck2/prelude/linking:link_info.bzl",
    "Linkage",
)
load(
    "@fbcode//buck2/prelude/utils:build_target_pattern.bzl",
    "BuildTargetPattern",
    "parse_build_target_pattern",
)

# Types of group traversal
Traversal = enum(
    # Includes the target and all of it's transitive dependencies in the group.
    "tree",
    # Includes only the target in the group.
    "node",
)

# Optional type of filtering
FilterType = enum(
    # Filters for targets with labels matching the regex pattern defined after `label:`.
    "label",
    # Filters for targets for the build target pattern defined after "pattern:".
    "pattern",
)

# Label for special group mapping which makes every target associated with it to be included in all groups
MATCH_ALL_LABEL = "MATCH_ALL"

# Label for excluding targets from all group mapping
NO_MATCH_LABEL = "NO_MATCH"

# Representation of a parsed group mapping
GroupMapping = record(
    # The target to which to apply this mapping.
    target = "dependency",
    # The type of traversal to use.
    traversal = field(Traversal.type, Traversal("tree")),
    # Optional filter type to apply to the traversal. If present,
    # either `label_regex` or `build_target_pattern` is required.
    filter_type = field([FilterType.type, None], None),
    # Optional label regex filter to apply to the traversal. If present,
    # the `filter_type` is required.
    label_regex = field(["regex", None], None),
    # Optional build target pattern to apply to the traversal. If present,
    # the `filter_type` is required.
    build_target_pattern = field([BuildTargetPattern.type, None], None),
    # Preferred linkage for this target when added to a link group.
    preferred_linkage = field([Linkage.type, None], None),
)

# Representation of a parsed group
Group = record(
    # The name for this group.
    name = str.type,
    # The mappings that are part of this group.
    mappings = [GroupMapping.type],
)

ResourceGraph = provider(fields = [
    # Target identifier of the graph.
    "label",  # "label"
    # All nodes of the resources DAG indexed by target label.
    "nodes",  # {"label", ResourceNode.type}
])

def parse_groups_definitions(map: list.type) -> [Group.type]:
    link_groups = []
    for name, mappings in map:
        parsed_mappings = []
        for entry in mappings:
            traversal = _parse_traversal_from_mapping(entry[1])
            filter_type, label_regex, build_target_pattern = _parse_filter_from_mapping(entry[2])
            mapping = GroupMapping(target = entry[0], traversal = traversal, filter_type = filter_type, label_regex = label_regex, build_target_pattern = build_target_pattern, preferred_linkage = Linkage(entry[3]) if len(entry) > 3 and entry[3] else None)
            parsed_mappings.append(mapping)

        link_group = Group(name = name, mappings = parsed_mappings)
        link_groups.append(link_group)

    return link_groups

def _parse_traversal_from_mapping(entry: str.type) -> Traversal.type:
    if entry == "tree":
        return Traversal("tree")
    elif entry == "node":
        return Traversal("node")
    else:
        fail("Unrecognized link group traversal type: " + entry)

def _parse_filter_from_mapping(entry: [str.type, None]) -> [(FilterType.type, "regex", None), (FilterType.type, None, BuildTargetPattern.type), (None, None, None)]:
    filter_type = None
    label_regex = None
    build_target_pattern = None
    if entry:
        # We need the anchors "^"" and "$" because experimental_regex match anywhere in the text,
        # while we want full text match for link_group label text.
        if entry.startswith("label"):
            filter_type = FilterType("label")
            label_regex = experimental_regex("^{}$".format(entry[6:]))
        elif entry.startswith("tag"):
            filter_type = FilterType("label")
            label_regex = experimental_regex("^{}$".format(entry[4:]))
        elif entry.startswith("pattern"):
            filter_type = FilterType("pattern")
            build_target_pattern = parse_build_target_pattern(entry[8:])
        else:
            fail("Invalid link group mapping filter: {}\nFilter must begin with `label:` or `pattern:`.".format(entry))
    return filter_type, label_regex, build_target_pattern

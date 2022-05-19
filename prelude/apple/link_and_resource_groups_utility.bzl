load(
    "@fbcode//buck2/prelude/utils:build_target_pattern.bzl",
    "BuildTargetPattern",
    "parse_build_target_pattern",
)

# Types of link group traversal
Traversal = enum(
    # Includes the target and all of it's transitive dependencies in the link group.
    "tree",
    # Includes only the target in the link group.
    "node",
)

# Optional type of filtering
FilterType = enum(
    # Filters for targets with labels matching the regex pattern defined after `label:`.
    "label",
    # Filters for targets for the build target pattern defined after "pattern:".
    "pattern",
)

# Label for special link group mapping which makes every target associated with it to be linked against all groups
MATCH_ALL_LABEL = "MATCH_ALL"

# Label for excluding targets from all link group mapping
NO_MATCH_LABEL = "NO_MATCH"

# Representation of a parsed link group mapping
LinkGroupMapping = record(
    # The target to which to apply this mapping.
    target = "dependency",
    # The type of traversal to use.
    traversal = field(Traversal.type, Traversal("tree")),
    # Optional filter type to apply to the traversal. If present,
    # either `label_regex` or `build_target_pattern` is required.
    filter_type = field([FilterType.type, None], None),
    # Optional label regex filter to apply to the traversal. If present,
    # the `filter_type` is required.
    label_regex = field([str.type, None], None),
    # Optional build target pattern to apply to the traversal. If present,
    # the `filter_type` is required.
    build_target_pattern = field([BuildTargetPattern.type, None], None),
)

# Representation of a parsed link group
LinkGroup = record(
    # The name for this link group.
    name = str.type,
    # The mappings that are part of this link group.
    mappings = [LinkGroupMapping.type],
)

def parse_link_groups_definitions(map: list.type) -> [LinkGroup.type]:
    link_groups = []
    for name, mappings in map:
        parsed_mappings = []
        for entry in mappings:
            traversal = _parse_traversal_from_mapping(entry[1])
            filter_type, label_regex, build_target_pattern = _parse_filter_from_mapping(entry[2])
            mapping = LinkGroupMapping(target = entry[0], traversal = traversal, filter_type = filter_type, label_regex = label_regex, build_target_pattern = build_target_pattern)
            parsed_mappings.append(mapping)

        link_group = LinkGroup(name = name, mappings = parsed_mappings)
        link_groups.append(link_group)

    return link_groups

def _parse_traversal_from_mapping(entry: str.type) -> Traversal.type:
    if entry == "tree":
        return Traversal("tree")
    elif entry == "node":
        return Traversal("node")
    else:
        fail("Unrecognized link group traversal type: " + entry)

def _parse_filter_from_mapping(entry: [str.type, None]) -> [(FilterType.type, str.type, None), (FilterType.type, None, BuildTargetPattern.type), (None, None, None)]:
    filter_type = None
    label_regex = None
    build_target_pattern = None
    if entry:
        if entry.startswith("label") or entry.startswith("tag"):
            filter_type = FilterType("label")
            label_regex = entry[6:]
        elif entry.startswith("pattern"):
            filter_type = FilterType("pattern")
            build_target_pattern = parse_build_target_pattern(entry[8:])
        else:
            fail("Invalid link group mapping filter: {}\nFilter must begin with `label:` or `pattern:`.".format(entry))
    return filter_type, label_regex, build_target_pattern

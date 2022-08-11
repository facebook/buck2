load(
    "@fbcode//buck2/prelude/linking:link_info.bzl",
    "Linkage",
)
load(
    "@fbcode//buck2/prelude/linking:linkable_graph.bzl",
    "LinkableGraph",  # @unused Used as a type
)
load(
    "@fbcode//buck2/prelude/utils:build_target_pattern.bzl",
    "BuildTargetPattern",
    "label_matches_build_target_pattern",
    "parse_build_target_pattern",
)
load(
    "@fbcode//buck2/prelude/utils:graph_utils.bzl",
    "breadth_first_traversal_by",
)
load(
    "@fbcode//buck2/prelude/utils:utils.bzl",
    "expect",
    "map_idx",
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

GroupsMappings = record(
    groups = [Group.type],
    mappings = {"label": str.type},
)

def parse_groups_definitions(map: list.type) -> [Group.type]:
    groups = []
    for name, mappings in map:
        parsed_mappings = []
        for entry in mappings:
            traversal = _parse_traversal_from_mapping(entry[1])
            filter_type, label_regex, build_target_pattern = _parse_filter_from_mapping(entry[2])
            mapping = GroupMapping(target = entry[0], traversal = traversal, filter_type = filter_type, label_regex = label_regex, build_target_pattern = build_target_pattern, preferred_linkage = Linkage(entry[3]) if len(entry) > 3 and entry[3] else None)
            parsed_mappings.append(mapping)

        group = Group(name = name, mappings = parsed_mappings)
        groups.append(group)

    return groups

def _parse_traversal_from_mapping(entry: str.type) -> Traversal.type:
    if entry == "tree":
        return Traversal("tree")
    elif entry == "node":
        return Traversal("node")
    else:
        fail("Unrecognized group traversal type: " + entry)

def _parse_filter_from_mapping(entry: [str.type, None]) -> [(FilterType.type, "regex", None), (FilterType.type, None, BuildTargetPattern.type), (None, None, None)]:
    filter_type = None
    label_regex = None
    build_target_pattern = None
    if entry:
        # We need the anchors "^"" and "$" because experimental_regex match anywhere in the text,
        # while we want full text match for group label text.
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
            fail("Invalid group mapping filter: {}\nFilter must begin with `label:` or `pattern:`.".format(entry))
    return filter_type, label_regex, build_target_pattern

def get_group_mappings_and_info(group_info_type: "_a", deps: ["dependency"], groups: [Group.type], graph: [LinkableGraph.type, ResourceGraph.type]) -> ({"label": str.type}, ["_a", None]):
    if not groups:
        return {}, None

    # If we have precomputed group info in deps, validate and use them
    computed_groups_infos = filter(None, map_idx(group_info_type, deps))
    if computed_groups_infos:
        # While link groups can have different mappings, in practice, only one mapping is used for a specific build graph,
        # as otherwise ensuring the right links end up in the right group is hard to get right.
        # For now, ensure that only one mapping is used. This can be relaxed if needed.
        groups_hash_set = {info.groups_hash: True for info in computed_groups_infos}
        expect(len(groups_hash_set.keys()) == 1, "All precomputed groups mappings must be equivalent!")
        computed_groups_info = computed_groups_infos[0]
        expect(hash(str(groups)) == computed_groups_info.groups_hash, "The group spec used for a build must be the same.")
        return computed_groups_info.mappings, computed_groups_info

    mappings = _compute_mappings(groups, graph)
    return mappings, (group_info_type(groups_hash = hash(str(groups)), mappings = mappings) if mappings else None)

def _compute_mappings(groups: [Group.type], graph: [LinkableGraph.type, ResourceGraph.type]) -> {"label": str.type}:
    """
    Returns the group mappings {target label -> group name} based on the provided groups and graph.
    """
    target_to_group_map = {}
    node_traversed_targets = {}

    for group in groups:
        for mapping in group.mappings:
            targets_in_group = _find_targets_in_mapping(graph, mapping)
            for target in targets_in_group:
                _update_target_to_group_mapping(graph, target_to_group_map, node_traversed_targets, group.name, mapping, target)

    return target_to_group_map

def _find_targets_in_mapping(
        graph: [LinkableGraph.type, ResourceGraph.type],
        mapping: GroupMapping.type) -> ["label"]:
    # If we have no filtering, we don't need to do any traversal to find targets to include.
    if mapping.filter_type == None:
        return [mapping.target.label]

    # Else find all dependencies that match the filter.
    matching_targets = {}

    def matches_target(
            target,  # "label"
            labels) -> bool.type:  # labels: [str.type]
        if mapping.filter_type == FilterType("label"):
            return any([mapping.label_regex.match(label) for label in labels])
        else:
            return label_matches_build_target_pattern(target, mapping.build_target_pattern)

    def find_matching_targets(node):  # "label" -> ["label"]:
        graph_node = graph.nodes[node]
        if matches_target(node, graph_node.labels):
            matching_targets[node] = None
            if mapping.traversal == Traversal("tree"):
                # We can stop traversing the tree at this point because we've added the
                # build target to the list of all targets that will be traversed by the
                # algorithm that applies the groups.
                return []
        return graph_node.deps + graph_node.exported_deps

    breadth_first_traversal_by(graph.nodes, [mapping.target.label], find_matching_targets)

    return matching_targets.keys()

# Types removed to avoid unnecessary type checking which degrades performance.
def _update_target_to_group_mapping(
        graph,  # [LinkableGraph.type, ResourceGraph.type]
        target_to_group_map,  #: {"label": str.type}
        node_traversed_targets,  #: {"label": None}
        group,  #  str.type,
        mapping,  # GroupMapping.type,
        target):  # "label"
    def assign_target_to_group(
            target: "label",
            node_traversal: bool.type) -> bool.type:
        # If the target hasn't already been assigned to a group, assign it to the
        # first group claiming the target. Return whether the target was already assigned.
        if target not in target_to_group_map:
            target_to_group_map[target] = group
            if node_traversal:
                node_traversed_targets[target] = None
            return False
        else:
            return True

    def transitively_add_targets_to_group_mapping(node: "label") -> ["label"]:
        previously_processed = assign_target_to_group(target = node, node_traversal = False)

        # If the node has been previously processed, and it was via tree (not node), all child nodes have been assigned
        if previously_processed and node not in node_traversed_targets:
            return []
        graph_node = graph.nodes[node]
        return graph_node.deps + graph_node.exported_deps

    if mapping.traversal == Traversal("node"):
        assign_target_to_group(target = target, node_traversal = True)
    else:  # tree
        breadth_first_traversal_by(graph.nodes, [target], transitively_add_targets_to_group_mapping)

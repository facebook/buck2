load(
    "@fbcode//buck2/prelude/linking:link_info.bzl",
    "LinkInfo",  # @unused Used as a type
    "LinkStyle",
    "Linkage",
    "get_actual_link_style",
)
load(
    "@fbcode//buck2/prelude/linking:linkable_graph.bzl",
    "LinkableGraph",  # @unused Used as a type
    "get_link_info",
    "linkable_deps",
)
load(
    "@fbcode//buck2/prelude/utils:build_target_pattern.bzl",
    "label_matches_build_target_pattern",
)
load(
    "@fbcode//buck2/prelude/utils:graph_utils.bzl",
    "breadth_first_traversal_by",
)
load(
    ":groups.bzl",
    "FilterType",  # @unused Used as a type
    "Group",  # @unused Used as a type
    "GroupMapping",  # @unused Used as a type
    "MATCH_ALL_LABEL",
    "NO_MATCH_LABEL",
    "ResourceGraph",  # @unused Used as a type
    "Traversal",  # @unused Used as a type
    "parse_groups_definitions",
)

LINK_GROUP_MAP_DATABASE_SUB_TARGET = "link-group-map-database"
LINK_GROUP_MAP_FILE_NAME = "link_group_map.json"

def get_link_group(ctx: "context") -> [str.type, None]:
    return ctx.attrs.link_group

def get_link_groups(ctx: "context") -> [Group.type]:
    """
    Parses the currently analyzed context for any link group definitions
    and returns a list of all link groups with their mappings.
    """
    link_group_map = ctx.attrs.link_group_map

    if not link_group_map:
        return []

    return parse_groups_definitions(link_group_map)

def get_link_group_preferred_linkage(link_groups: [Group.type]) -> {"label": Linkage.type}:
    return {
        mapping.target.label: mapping.preferred_linkage
        for group in link_groups
        for mapping in group.mappings
        if mapping.preferred_linkage != None
    }

def get_link_group_mappings(link_groups: [Group.type], linkable_graph: [LinkableGraph.type, ResourceGraph.type]) -> {"label": str.type}:
    """
    Returns the link group mappings {target label -> link group name}
    based on the provided link groups and linkable graph.
    """
    target_to_link_group_map = {}
    node_traversed_targets = {}

    for group in link_groups:
        for mapping in group.mappings:
            targets_in_group = _find_targets_in_mapping(linkable_graph, mapping)
            for target in targets_in_group:
                _update_target_to_group_mapping(linkable_graph, target_to_link_group_map, node_traversed_targets, group.name, mapping, target)

    return target_to_link_group_map

LinkGroupLinkInfo = record(
    link_info = field(LinkInfo.type),
    link_style = field(LinkStyle.type),
)

def get_filtered_labels_to_links_map(
        linkable_graph: LinkableGraph.type,
        link_group: [str.type, None],
        link_group_mappings: [{"label": str.type}, None],
        link_group_preferred_linkage: {"label": Linkage.type},
        link_style: LinkStyle.type,
        non_exported_deps: ["dependency"],
        prefer_stripped: bool.type = False,
        is_library: bool.type = True) -> {"label": LinkGroupLinkInfo.type}:
    """
    Given a linkable graph, link style and link group mappings, finds all links
    to consider for linking traversing the graph as necessary and then
    identifies which link infos and targets belong the in the provided link group.
    If no link group is provided, all unmatched link infos are returned.
    """

    deps = linkable_deps(non_exported_deps)

    def get_traversed_deps(node: "label") -> ["label"]:
        linkable_node = linkable_graph.nodes[node]  # buildifier: disable=uninitialized

        # Always link against exported deps
        node_linkables = list(linkable_node.exported_deps)

        # If the preferred linkage is `static` or `any` with a link style that is
        # not shared, we need to link against the deps too.
        should_traverse = False
        if linkable_node.preferred_linkage == Linkage("static"):
            should_traverse = True
        elif linkable_node.preferred_linkage == Linkage("any"):
            should_traverse = link_style != Linkage("shared")

        if should_traverse:
            node_linkables += linkable_node.deps

        return node_linkables

    # Get all potential linkable targets
    linkables = breadth_first_traversal_by(
        linkable_graph.nodes,
        deps,
        get_traversed_deps,
    )

    linkable_map = {}

    def add_link(target: "label", link_style: LinkStyle.type):
        linkable_map[target] = LinkGroupLinkInfo(
            link_info = get_link_info(linkable_graph.nodes[target], link_style, prefer_stripped),
            link_style = link_style,
        )  # buildifier: disable=uninitialized

    for target in linkables:
        node = linkable_graph.nodes[target]
        actual_link_style = get_actual_link_style(link_style, link_group_preferred_linkage.get(target, node.preferred_linkage))

        # Always link any shared dependencies
        if actual_link_style == LinkStyle("shared"):
            add_link(target, LinkStyle("shared"))
        else:  # static or static_pic
            target_link_group = link_group_mappings.get(target)

            # Don't put targets with NO_MATCH_LABEL into a link group
            if is_library and target_link_group == NO_MATCH_LABEL:
                continue

            # Ungrouped linkable targets belong to the unlabeled executable
            if not target_link_group and not link_group:
                add_link(target, actual_link_style)
                # Targets labeled NO_MATCH belong to the unlabeled executable

            elif not is_library and target_link_group == NO_MATCH_LABEL:
                add_link(target, actual_link_style)
            elif target_link_group == MATCH_ALL_LABEL or target_link_group == link_group:
                # If this belongs to the match all link group or the group currently being evaluated
                add_link(target, actual_link_style)

    return linkable_map

def get_filtered_links(labels_to_links_map: {"label": LinkGroupLinkInfo.type}):
    return [link_group_info.link_info for link_group_info in labels_to_links_map.values()]

def get_filtered_targets(labels_to_links_map: {"label": LinkGroupLinkInfo.type}):
    return [label.raw_target() for label in labels_to_links_map.keys()]

def get_link_group_map_json(ctx: "context", targets: ["target_label"]) -> DefaultInfo.type:
    json_map = ctx.actions.write_json(LINK_GROUP_MAP_FILE_NAME, sorted(targets))
    return DefaultInfo(default_outputs = [json_map])

def _find_targets_in_mapping(
        linkable_graph: [LinkableGraph.type, ResourceGraph.type],
        mapping: GroupMapping.type) -> ["label"]:
    # If we have no filtering, we don't need to do any traversal to find targets to link.
    if mapping.filter_type == None:
        return [mapping.target.label]

    # Else find all dependencies that match the filter.
    matching_targets = {}

    def matches_target(target: "label", labels: [str.type]) -> bool.type:
        if mapping.filter_type == FilterType("label"):
            return any([mapping.label_regex.match(label) for label in labels])
        else:
            return label_matches_build_target_pattern(target, mapping.build_target_pattern)

    def find_matching_targets(node: "label") -> ["label"]:
        graph_node = linkable_graph.nodes[node]
        if matches_target(node, graph_node.labels):
            matching_targets[node] = None
            if mapping.traversal == Traversal("tree"):
                # We can stop traversing the tree at this point because we've added the
                # build target to the list of all targets that will be traversed by the
                # algorithm that applies the link groups.
                return []
        return graph_node.deps + graph_node.exported_deps

    breadth_first_traversal_by(linkable_graph.nodes, [mapping.target.label], find_matching_targets)

    return matching_targets.keys()

def _update_target_to_group_mapping(
        linkable_graph: [LinkableGraph.type, ResourceGraph.type],
        target_to_link_group_map: {"label": str.type},
        node_traversed_targets: {"label": None},
        link_group: str.type,
        mapping: GroupMapping.type,
        target: "label"):
    def assign_target_to_link_group(
            target: "label",
            node_traversal: bool.type) -> bool.type:
        # If the target hasn't already been assigned to a link group, assign it to the
        # first group claiming the target. Return whether the target was already assigned.
        if target not in target_to_link_group_map:
            target_to_link_group_map[target] = link_group
            if node_traversal:
                node_traversed_targets[target] = None
            return False
        else:
            return True

    def transitively_add_targets_to_group_mapping(node: "label") -> ["label"]:
        previously_processed = assign_target_to_link_group(target = node, node_traversal = False)

        # If the node has been previously processed, and it was via tree (not node), all child nodes have been assigned
        if previously_processed and node not in node_traversed_targets:
            return []
        graph_node = linkable_graph.nodes[node]
        return graph_node.deps + graph_node.exported_deps

    if mapping.traversal == Traversal("node"):
        _ = assign_target_to_link_group(target = target, node_traversal = True)
    else:  # tree
        breadth_first_traversal_by(linkable_graph.nodes, [target], transitively_add_targets_to_group_mapping)

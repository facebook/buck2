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
    "@fbcode//buck2/prelude/utils:graph_utils.bzl",
    "breadth_first_traversal_by",
)
load(
    ":groups.bzl",
    "Group",  # @unused Used as a type
    "MATCH_ALL_LABEL",
    "NO_MATCH_LABEL",
    "parse_groups_definitions",
)

LINK_GROUP_MAP_DATABASE_SUB_TARGET = "link-group-map-database"
LINK_GROUP_MAP_FILE_NAME = "link_group_map.json"

LinkGroupLinkInfo = record(
    link_info = field(LinkInfo.type),
    link_style = field(LinkStyle.type),
)

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

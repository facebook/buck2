# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//cxx:groups_types.bzl", "Traversal")
load("@prelude//linking:types.bzl", "Linkage")
load(
    "@prelude//utils:build_target_pattern.bzl",
    "BuildTargetPattern",
    "parse_build_target_pattern",
)
load(
    "@prelude//utils:graph_utils.bzl",
    "depth_first_traversal_by",
)
load(
    "@prelude//utils:strings.bzl",
    "strip_prefix",
)
load(
    "@prelude//utils:utils.bzl",
    "value_or",
)
load(
    ":groups_types.bzl",
    "Group",
    "GroupAttrs",
    "GroupDefinition",
    "GroupFilterInfo",
    "GroupMapping",
)

_VALID_ATTRS = [
    "enable_distributed_thinlto",
    "enable_if_node_count_exceeds",
    "discard_group",
    "linker_flags",
    "linker_script",
    "exported_linker_flags",
    "no_as_needed",
    "requires_root_node_exists",
    "prohibit_file_duplicates",
    "prefer_optimized_experimental",
]

# Traversal types in this list will only assign the node
# to a target (as opposed to the transitive deps of the node's tree).
_TRAVERSALS_TO_ASSIGN_NODE = [
    Traversal("node"),
    Traversal("subfolders"),
    # TODO (dust): Possible perf optimization:
    # When intersecting configured targets, it's not possible to intersect
    # a parent without also intersecting it's children.
    #
    # As a result, there's a possible perf optimization to assign 'tree'
    # to intersected targets instead, and leverage that to avoid traversing
    # the entire tree of every root.
    #
    # For example:
    # If iterating the tree of 'root2' we find a node which
    # was also present in 'root1', we can skip traversing the subtree
    # because it's evitable that everything is going to match there too.
    Traversal("intersect_any_roots"),
]

# Creates a group from an existing group, overwriting any properties provided
def create_group(
        group: Group,
        name: [None, str] = None,
        mappings: [None, list[GroupMapping]] = None,
        attrs: [None, GroupAttrs] = None,
        definition_type: [None, GroupDefinition] = None):
    return Group(
        name = value_or(name, group.name),
        mappings = value_or(mappings, group.mappings),
        attrs = value_or(attrs, group.attrs),
        definition_type = value_or(definition_type, group.definition_type),
    )

def get_roots_from_mapping(mapping):
    deps = mapping[0] if type(mapping[0]) == "list" else [mapping[0]]
    return filter(None, deps)

def parse_groups_definitions(
        map: list,
        # Function to parse a root label from the input type, allowing different
        # callers to have different top-level types for the `root`s.
        parse_root: typing.Callable = lambda d: d) -> list[Group]:
    groups = []
    group_names = set()
    for map_entry in map:
        name = map_entry[0]

        # Dedup the link group specs, take the deinition from the first definition
        if name in group_names:
            continue
        group_names.add(name)
        mappings = map_entry[1]
        attrs = (map_entry[2] or {}) if len(map_entry) > 2 else {}

        for attr in attrs:
            if attr not in _VALID_ATTRS:
                fail("invalid attr '{}' for link group '{}' found. Valid attributes are {}.".format(attr, name, _VALID_ATTRS))
        group_attrs = GroupAttrs(
            enable_distributed_thinlto = attrs.get("enable_distributed_thinlto", False),
            enable_if_node_count_exceeds = attrs.get("enable_if_node_count_exceeds", None),
            discard_group = attrs.get("discard_group", False),
            linker_flags = attrs.get("linker_flags", []),
            linker_script = attrs.get("linker_script", None),
            exported_linker_flags = attrs.get("exported_linker_flags", []),
            no_as_needed = attrs.get("no_as_needed", False),
            requires_root_node_exists = attrs.get("requires_root_node_exists", True),
            prohibit_file_duplicates = attrs.get("prohibit_file_duplicates", False),
            prefer_optimized_experimental = attrs.get("prefer_optimized_experimental", False),
        )

        parsed_mappings = []
        for entry in mappings:
            traversal = _parse_traversal_from_mapping(entry[1])
            mapping = GroupMapping(
                roots = filter(None, [parse_root(root) for root in get_roots_from_mapping(entry)]),
                traversal = traversal,
                filters = _parse_filter_from_mapping(entry[2]),
                preferred_linkage = Linkage(entry[3]) if len(entry) > 3 and entry[3] else None,
            )
            num_roots = len(mapping.roots) if mapping.roots else 0
            if num_roots > 1 and mapping.traversal != Traversal("intersect_any_roots"):
                fail("Invariant. A link_group mapping with traversal type: {} can only have 1 root node. {} found.".format(mapping.traversal, mapping.roots))
            elif mapping.traversal == Traversal("intersect_any_roots") and num_roots < 2:
                fail("Invariant. A link_group mapping with traversal type 'intersect' must have at least 2 root nodes. {} found.".format(mapping.roots))

            parsed_mappings.append(mapping)

        group = Group(
            name = name,
            mappings = parsed_mappings,
            attrs = group_attrs,
            definition_type = GroupDefinition("explicit"),
        )
        groups.append(group)

    return groups

def _parse_traversal_from_mapping(entry: str) -> Traversal:
    if entry == "tree":
        return Traversal("tree")
    elif entry == "node":
        return Traversal("node")
    elif entry == "subfolders":
        return Traversal("subfolders")
    elif entry == "intersect_any_roots":
        return Traversal("intersect_any_roots")
    else:
        fail("Unrecognized group traversal type: " + entry)

def _parse_filter(entry: str) -> GroupFilterInfo:
    for prefix in ("label:", "tag:"):
        label_regex = strip_prefix(prefix, entry)
        if label_regex != None:
            # We need the anchors "^"" and "$" because experimental_regex match
            # anywhere in the text, while we want full text match for group label
            # text.
            regex_expr = regex("^{}$".format(label_regex), fancy = False)

            def matches_regex(_t, labels):
                for label in labels:
                    if regex_expr.match(label):
                        return True
                return False

            return GroupFilterInfo(
                matches = matches_regex,
                info = {"regex": str(regex_expr)},
            )

        target_regex = strip_prefix("target_regex:", entry)
        if target_regex != None:
            regex_expr = regex("^{}$".format(target_regex), fancy = True)

            def matches_regex(t, _labels):
                return regex_expr.match(str(t.raw_target()))

            return GroupFilterInfo(
                matches = matches_regex,
                info = {"target_regex": str(regex_expr)},
            )

    pattern = strip_prefix("pattern:", entry)
    if pattern != None:
        build_target_pattern = parse_build_target_pattern(pattern)

        def matches_target_pattern(t, _labels):
            return build_target_pattern.matches(t)

        return GroupFilterInfo(
            matches = matches_target_pattern,
            info = _make_json_info_for_build_target_pattern(build_target_pattern),
        )

    fail("Invalid group mapping filter: {}\nFilter must begin with `label:`, `tag:`, `target_regex` or `pattern:`.".format(entry))

def _parse_filter_from_mapping(entry: [list[str | Dependency], str, Dependency, None]) -> list[GroupFilterInfo]:
    if type(entry) == type([]):
        return [_parse_filter(e) if isinstance(e, str) else e[GroupFilterInfo] for e in entry]
    elif isinstance(entry, Dependency):
        return [entry[GroupFilterInfo]]
    elif type(entry) == type(""):
        return [_parse_filter(entry)]
    return []

def compute_mappings(groups_map: dict[str, Group], graph_map: dict[Label, typing.Any]) -> dict[Label, str]:
    """
    Returns the group mappings {target label -> group name} based on the provided groups_map and graph.
    """
    if not groups_map:
        return {}

    target_to_group_map = {}
    node_traversed_targets = {}

    for group in groups_map.values():
        for mapping in group.mappings:
            targets_in_mapping = _find_targets_in_mapping(graph_map, mapping)
            for target in targets_in_mapping:
                # If the target doesn't exist in our graph, skip the mapping.
                if target not in graph_map:
                    continue
                _update_target_to_group_mapping(graph_map, target_to_group_map, node_traversed_targets, group, groups_map, mapping, target)

    return target_to_group_map

def get_dedupped_roots_from_groups(groups: list[Group]) -> list[Label]:
    roots = {}
    for group in groups:
        for mapping in group.mappings:
            if not mapping.roots:
                continue

            for root in mapping.roots:
                roots[root] = True

    return list(roots.keys())

def _find_targets_in_mapping(
        graph_map: dict[Label, typing.Any],
        mapping: GroupMapping) -> list[Label]:
    # If we have no filtering, we don't need to do any traversal to find targets to include.
    if not mapping.filters:
        if not mapping.roots:
            # Some link groups may want to partially define their mapping roots based on constraint
            # that potentially can be resolved to `None`.
            #
            # E.g:
            # ```
            # ("evict-mkl", [
            #   (":mkl_ilp64_omp", "node", None, "shared"),
            #   (select(
            #     {"DEFAULT": None, "ovr_config//runtime:platform010": "//IntelComposerXE:mkl_ilp64_omp" }),
            #     "node", None, "shared"
            #   ),
            # ])
            # ```
            # Second mapping will be resolved to `(None, "node", None, "shared")` and will not handle anything.
            # There is no convenient way to gracefully handle that in user-facing link groups API.
            return []

        elif mapping.traversal != Traversal("intersect_any_roots"):
            return mapping.roots

    # Else find all dependencies that match the filter.
    matching_targets = {}

    def populate_matching_targets(node):  # Label -> bool:
        graph_node = graph_map[node]

        # This callsite was migrated away from `lazy.is_any()`
        # because we saw a non-trivial increase in retained bytes
        # associated with the lambda required by the function.
        if mapping.filters:
            for filter in mapping.filters:
                if not filter.matches(node, graph_node.labels):
                    return True

        matching_targets[node] = None
        if mapping.traversal == Traversal("tree"):
            # We can stop traversing the tree at this point because we've added the
            # build target to the list of all targets that will be traversed by the
            # algorithm that applies the groups.
            return False
        return True

    def populate_matching_targets_bfs_wrapper(node):  # (Label) -> list
        if populate_matching_targets(node):
            graph_node = graph_map[node]
            return graph_node.deps + graph_node.exported_deps
        return []

    if not mapping.roots:
        for node in graph_map:
            populate_matching_targets(node)
    elif mapping.traversal == Traversal("intersect_any_roots"):
        targets_to_counter = {}
        for root in mapping.roots:
            # This is a captured variable inside `populate_matching_targets`.
            # We reset it for each root we visit so that we don't have results
            # from other roots.
            matching_targets = {}
            depth_first_traversal_by(graph_map, [root], populate_matching_targets_bfs_wrapper)
            for t in matching_targets:
                targets_to_counter[t] = targets_to_counter.get(t, 0) + 1

        return [
            t
            for t, count in targets_to_counter.items()
            if count > 1
        ]
    else:
        depth_first_traversal_by(graph_map, mapping.roots, populate_matching_targets_bfs_wrapper)

    return matching_targets.keys()

# Extracted from `_update_target_to_group_mapping` to avoid function allocations inside the loop
def _assign_target_to_group(
        target_to_group_map,  #: {"label": str}
        node_traversed_targets,  #: {"label": None}
        group,  #  Group,
        groups_map,  # {str: Group}
        mapping,  # GroupMapping
        target,  # Label
        node_traversal):  # bool
    # If the target hasn't already been assigned to a group, assign it to the
    # first group claiming the target. Return whether the target was already assigned.
    if target not in target_to_group_map:
        if mapping.traversal == Traversal("subfolders"):
            generated_group_name = _generate_group_subfolder_name(group.name, target.package)
            _add_to_implicit_link_group(generated_group_name, group, groups_map, target_to_group_map, target)
        else:
            target_to_group_map[target] = group.name

        if node_traversal:
            node_traversed_targets[target] = None
        return False
    else:
        return True

# Extracted from `_update_target_to_group_mapping` to avoid function allocations inside the loop
def _transitively_add_targets_to_group_mapping(
        assign_target_to_group,  # (Label, bool) -> bool
        node_traversed_targets,  #: {"label": None}
        graph_map,  # {"label": "_b"}
        node):  # ([Label]) -> None
    previously_processed = assign_target_to_group(node, False)

    # If the node has been previously processed, and it was via tree (not node), all child nodes have been assigned
    if previously_processed and node not in node_traversed_targets:
        return None
    graph_node = graph_map[node]
    return graph_node.deps + graph_node.exported_deps

# Types removed to avoid unnecessary type checking which degrades performance.
def _update_target_to_group_mapping(
        graph_map,  # {"label": "_b"}
        target_to_group_map,  #: {"label": str}
        node_traversed_targets,  #: {"label": None}
        group,  #  Group,
        groups_map,  # {str: Group}
        mapping,  # GroupMapping
        target):  # Label
    assign_target_to_group = partial(_assign_target_to_group, target_to_group_map, node_traversed_targets, group, groups_map, mapping)  # (Label, bool) -> bool
    transitively_add_targets_to_group_mapping = partial(_transitively_add_targets_to_group_mapping, assign_target_to_group, node_traversed_targets, graph_map)  # (Label) -> list[Label]

    if mapping.traversal in _TRAVERSALS_TO_ASSIGN_NODE:
        assign_target_to_group(target, True)
    else:  # tree
        depth_first_traversal_by(graph_map, [target], transitively_add_targets_to_group_mapping)

def _add_to_implicit_link_group(
        generated_group_name,  # str
        group,  # Group
        groups_map,  # {str: Group}
        target_to_group_map,  # {Label: str}
        target):  # Label
    target_to_group_map[target] = generated_group_name
    if generated_group_name not in groups_map:
        groups_map[generated_group_name] = create_group(
            group = group,
            name = generated_group_name,
            definition_type = GroupDefinition("implicit"),
        )
    elif groups_map[generated_group_name].definition_type == GroupDefinition("explicit"):
        hashed_group_name = _hash_group_name(group.name, generated_group_name)
        _add_to_implicit_link_group(hashed_group_name, group, groups_map, target_to_group_map, target)

def _generate_group_subfolder_name(
        group,  # str,
        package):  # str
    """ Dynamically generating link group name for "subfolders" traversal."""
    name = group + "_" + package.replace("/", "_")

    if len(name) > 246:
        # Maximum filename size in unix is 255.
        # We prefix all libraries with "lib" (3 symbols) and suffix with ".dylib" (6 symbols) or ".so" (3 symbols).
        # Assuming ".dylib" suffix cause it's the longest, we can allow (255 - 3 - 6) = 246 symbols for the rest of the name.
        name = _hash_group_name(group, name)
    return name

def _hash_group_name(prefix: str, name: str) -> str:
    """
    Creates new name via simple hashing.
    Hash algorithm is stable in starlark: https://fburl.com/code/ptegkov6
    """
    return "{}_{}".format(prefix, str(hash(name)))

def _make_json_info_for_build_target_pattern(build_target_pattern: BuildTargetPattern) -> dict[str, typing.Any]:
    # `BuildTargetPattern` contains lambdas which are not serializable, so
    # have to generate the JSON representation
    return {
        "cell": build_target_pattern.cell,
        "kind": build_target_pattern.kind,
        "name": build_target_pattern.name,
        "path": build_target_pattern.path,
    }

def _make_json_info_for_group_mapping(group_mapping: GroupMapping) -> dict[str, typing.Any]:
    return {
        "filters": [
            filter.info
            for filter in group_mapping.filters
        ],
        "preferred_linkage": group_mapping.preferred_linkage,
        "roots": group_mapping.roots,
        "traversal": group_mapping.traversal,
    }

def _make_json_info_for_group(group: Group) -> dict[str, typing.Any]:
    return {
        "attrs": group.attrs,
        "mappings": [_make_json_info_for_group_mapping(mapping) for mapping in group.mappings],
        "name": group.name,
    }

def make_info_subtarget_providers(ctx: AnalysisContext, groups: list[Group], mappings: dict[Label, str]) -> list[Provider]:
    info_json = {
        "groups": {group.name: _make_json_info_for_group(group) for group in groups},
        "mappings": mappings,
    }
    json_output = ctx.actions.write_json("link_group_map_info.json", info_json, pretty = True)
    return [DefaultInfo(default_output = json_output)]

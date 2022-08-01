load(
    "@fbcode//buck2/prelude/cxx:groups.bzl",
    "Group",  # @unused Used as a type
    "MATCH_ALL_LABEL",
    "ResourceGraph",  # @unused Used as a type
    "parse_groups_definitions",
)
load(
    "@fbcode//buck2/prelude/utils:graph_utils.bzl",
    "breadth_first_traversal_by",
)
load(":apple_asset_catalog_types.bzl", "AppleAssetCatalogSpec")
load(":apple_core_data_types.bzl", "AppleCoreDataSpec")
load(":apple_resource_types.bzl", "AppleResourceSpec")

ResourceNode = record(
    # Attribute labels on the target.
    labels = field([str.type], []),
    # Deps of this target which might have resources transitively.
    deps = field(["label"], []),
    # Exported deps of this target which might have resources transitively.
    exported_deps = field(["label"], []),
    # Actual resource data, present when node corresponds to `apple_resource` target.
    resource_spec = field([AppleResourceSpec.type, None], None),
    # Actual asset catalog data, present when node corresponds to `apple_asset_catalog` target.
    asset_catalog_spec = field([AppleAssetCatalogSpec.type, None], None),
    # Actual core data, present when node corresponds to `core_data_model` target
    core_data_spec = field([AppleCoreDataSpec.type, None], None),
)

ResourceGroupInfo = provider(fields = [
    "groups_hash",  # str.type
    "mappings",  # {"label": str.type}
])

def create_resource_graph(
        root: "label",
        labels: [str.type],
        deps: ["dependency"],
        exported_deps: ["dependency"],
        resource_spec: [AppleResourceSpec.type, None] = None,
        asset_catalog_spec: [AppleAssetCatalogSpec.type, None] = None,
        core_data_spec: [AppleCoreDataSpec.type, None] = None) -> ResourceGraph.type:
    nodes = {
        root: ResourceNode(
            labels = labels,
            deps = _with_resources_deps(deps),
            exported_deps = _with_resources_deps(exported_deps),
            resource_spec = resource_spec,
            asset_catalog_spec = asset_catalog_spec,
            core_data_spec = core_data_spec,
        ),
    }
    all_deps = deps + exported_deps
    graphs = filter(None, [d[ResourceGraph] for d in all_deps])
    for g in graphs:
        nodes.update(g.nodes)
    return ResourceGraph(label = root, nodes = nodes)

def _with_resources_deps(deps: ["dependency"]) -> ["label"]:
    """
    Filters dependencies and returns only those which are relevant
    to working with resources i.e. those which contains resource graph provider.
    """
    graphs = filter(None, [d[ResourceGraph] for d in deps])
    return [g.label for g in graphs]

def get_resource_groups(ctx: "context") -> [Group.type]:
    """
    Parses the currently analyzed context for any resource group definitions
    and returns a list of all resource groups with their mappings.
    """
    resource_group_map = ctx.attrs.resource_group_map

    if not resource_group_map:
        return []

    return parse_groups_definitions(resource_group_map)

def get_filtered_resources(
        resource_graph: ResourceGraph.type,
        resource_group: [str.type, None],
        resource_group_mappings: [{"label": str.type}, None]) -> ([AppleResourceSpec.type], [AppleAssetCatalogSpec.type], [AppleCoreDataSpec.type]):
    """
    Walks the provided DAG and collects resources matching resource groups definition.
    """

    def get_traversed_deps(target: "label") -> ["label"]:
        node = resource_graph.nodes[target]  # buildifier: disable=uninitialized
        return node.exported_deps + node.deps

    targets = breadth_first_traversal_by(
        resource_graph.nodes,
        get_traversed_deps(resource_graph.label),
        get_traversed_deps,
    )

    resource_specs = []
    asset_catalog_specs = []
    core_data_specs = []

    for target in targets:
        target_resource_group = resource_group_mappings.get(target)

        # Ungrouped targets belong to the unlabeled bundle
        if ((not target_resource_group and not resource_group) or
            # Does it match special "MATCH_ALL" mapping?
            target_resource_group == MATCH_ALL_LABEL or
            # Does it match currently evaluated group?
            target_resource_group == resource_group):
            node = resource_graph.nodes[target]
            resource_spec = node.resource_spec
            if resource_spec:
                resource_specs.append(resource_spec)
            asset_catalog_spec = node.asset_catalog_spec
            if asset_catalog_spec:
                asset_catalog_specs.append(asset_catalog_spec)
            core_data_spec = node.core_data_spec
            if core_data_spec:
                core_data_specs.append(core_data_spec)

    return resource_specs, asset_catalog_specs, core_data_specs

load("@fbcode//buck2/prelude:attributes.bzl", "Traversal")
load(
    "@fbcode//buck2/prelude/apple:resource_groups.bzl",
    "ResourceGroupInfo",
    "create_resource_graph",
)
load(
    "@fbcode//buck2/prelude/cxx:groups.bzl",
    "compute_mappings",
    "parse_groups_definitions",
)
load("@fbcode//buck2/prelude/user:rule_spec.bzl", "RuleRegistrationSpec")

def v1_attrs():
    return attrs.list(attrs.tuple(attrs.string(), attrs.list(attrs.tuple(attrs.dep(), attrs.enum(Traversal), attrs.option(attrs.string())))))

def resource_group_map_attr():
    v2_attrs = attrs.dep(providers = [ResourceGroupInfo])
    return attrs.option(attrs.one_of(v2_attrs, v1_attrs()), default = None)

def _impl(ctx: "context") -> ["provider"]:
    resource_groups = parse_groups_definitions(ctx.attrs.map)
    resource_groups_deps = [mapping.target for group in resource_groups for mapping in group.mappings]
    resource_graph = create_resource_graph(
        root = ctx.label,
        labels = [],
        deps = resource_groups_deps,
        exported_deps = [],
    )
    mappings = compute_mappings(groups = resource_groups, graph = resource_graph)
    return [
        DefaultInfo(),
        ResourceGroupInfo(groups = resource_groups, groups_hash = hash(str(resource_groups)), mappings = mappings),
    ]

registration_spec = RuleRegistrationSpec(
    name = "resource_group_map",
    impl = _impl,
    attrs = {
        "map": v1_attrs(),
    },
)

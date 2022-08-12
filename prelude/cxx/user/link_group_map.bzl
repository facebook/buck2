load("@fbcode//buck2/prelude:attributes.bzl", "Linkage", "Traversal")
load("@fbcode//buck2/prelude/user:rule_spec.bzl", "RuleRegistrationSpec")

def _v1_attrs():
    return attrs.list(attrs.tuple(attrs.string(), attrs.list(attrs.tuple(attrs.dep(), attrs.enum(Traversal), attrs.option(attrs.string()), attrs.option(attrs.enum(Linkage))))))

def link_group_map_attr():
    return attrs.option(_v1_attrs(), default = None)

def _impl(_ctx: "context") -> ["provider"]:
    return [
        DefaultInfo(),
    ]

registration_spec = RuleRegistrationSpec(
    name = "link_group_map",
    impl = _impl,
    attrs = {
        "map": _v1_attrs(),
    },
)
